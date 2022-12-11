(defpackage :capitalized-export
  (:use :cl)
  (:export "MAKE-CAPITALIZED-EXPORT-READTABLE"))

(in-package :capitalized-export)

(defvar *debug* nil)

;; readtable-case :invert

;; User types     Symbol
;; Zebra       -> Zebra
;; ZEBRA       -> zebra
;; zebra       -> ZEBRA
;; a           -> A
;; A           -> a

(defun capitalized-according-to-invert-p (string)
  (or (and (= 1 (length string))
           (lower-case-p (aref string 0)))
      (and (< 1 (length string))
           (upper-case-p (aref string 0))
           (loop :for k :from 1 :below (length string)
                 :always (lower-case-p (aref string k))))))

(defun escape-object (x)
  (vector 'escape x))

(defun escaped-object-p (x)
  (and (vectorp x)
       (not (zerop (length x)))
       (eq (aref x 0) 'escape)))

(defun escaped-object-value (x)
  (aref x 1))

(defun make-inverted-readtable ()
  (let* ((rt (copy-readtable nil))
         (standard (copy-readtable nil))
         (vertical-special-case
           (lambda (s c)
             (assert (eql #\| c))
             (unread-char c s)
             (let ((*readtable* standard))
               (escape-object (read s t s t))))))
    (setf (readtable-case rt) :invert)
    (set-macro-character #\| vertical-special-case nil rt)
    rt))

(defun map-tree-leaves (fn tree)
  (let ((seen-table (make-hash-table :test 'eq)) ; original cons -> marker
        (value-table (make-hash-table :test 'eq))) ; marker -> walked cons
    (labels ((walk (tr)
               (typecase tr
                 (cons
                  ;; If tr has been seen before, return the
                  ;; corresponding marker.
                  (cond ((gethash tr seen-table))
                        ;; Otherwise mark as seen and store the walked
                        ;; tree in value-table.
                        (t (let ((marker (list nil)))
                             (setf (gethash tr seen-table) marker)
                             (setf (gethash marker value-table)
                                   (cons (walk (car tr))
                                         (walk (cdr tr))))))))
                 (atom (funcall fn tr))))
             (replace-markers (tr)
               (typecase tr
                 (cons
                  (let ((car (gethash (car tr) value-table))
                        (cdr (gethash (cdr tr) value-table)))
                    (if car
                        (rplaca tr car)
                        (replace-markers (car tr)))
                    (if cdr
                        (rplacd tr cdr)
                        (replace-markers (cdr tr)))))
                 (atom nil))))
      (let ((new (walk tree)))
        (replace-markers new)
        new))))

(defun make-capitalized-export-readtable ()
  (buffering-readtable:make-buffering-readtable
   :inner-readtable (make-inverted-readtable)
   :translate-all
   (lambda (forms)
     (let* ((exports ())
            (collect-capitalized
              (lambda (object)
                (cond ((escaped-object-p object)
                       (escaped-object-value object))
                      ((and (symbolp object)
                            (capitalized-according-to-invert-p
                             (symbol-name object)))
                       (let ((upcased-symbol (intern (string-upcase object)
                                                     *package*)))
                         (push upcased-symbol exports)
                         upcased-symbol))
                      (t object))))
            (translated-forms
               (append (map-tree-leaves collect-capitalized forms)
                       `((export ',exports)))))
       (when *debug*
         (format t "Before:~%")
         (with-standard-io-syntax
           (let ((*print-pretty* t)
                 (*print-circle* t)
                 (*package* (find-package :capitalized-export)))
             (print forms)))
         (terpri)
         (format t "After:~%")
         (with-standard-io-syntax
           (let ((*print-pretty* t)
                 (*print-circle* t)
                 (*package* (find-package :capitalized-export)))
             (print translated-forms))))
       translated-forms))))
