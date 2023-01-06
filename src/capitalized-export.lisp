(defpackage :capitalized-export
  (:use :cl)
  (:export "MAKE-CAPITALIZED-EXPORT-READTABLE"))

(in-package :capitalized-export)

(defvar *debug* nil)

;; readtable-case :invert
;;
;; User types     Symbol
;; Zebra       -> Zebra
;; ZEBRA       -> zebra
;; zebra       -> ZEBRA
;; a           -> A
;; A           -> a

;; Want to export "Foo", "F" and "*Foo*".
(defun matches-export-pattern-p (string)
  (or (and (= 1 (length string))
           (lower-case-p (aref string 0)))
      (and (< 1 (length string))
           (loop :with state = :looking-for-alpha-char
                 :for c :across string
                 :when (alpha-char-p c)
                   :do (case state
                         (:looking-for-alpha-char
                          (if (upper-case-p c)
                              (setf state :only-lower-case)
                              (return nil)))
                         (:only-lower-case
                          (when (upper-case-p c)
                            (return nil))))
                 :finally (return (eq :only-lower-case state))))))

(defun escape-object (x)
  (vector 'escape x))

(defun escaped-object-p (x)
  (and (vectorp x)
       (not (zerop (length x)))
       (eq (aref x 0) 'escape)))

(defun escaped-object-value (x)
  (aref x 1))

(defun make-inverted-readtable (&optional (inner-readtable *readtable*))
  (let* ((inverted (copy-readtable inner-readtable))
         (vertical-special-case
           (lambda (s c)
             (assert (eql #\| c))
             (unread-char c s)
             (let ((*readtable* inner-readtable))
               (escape-object (read s t s t))))))
    (setf (readtable-case inverted) :invert)
    (set-macro-character #\| vertical-special-case nil inverted)
    inverted))

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

(defun resolve-capitalized-symbol (symbol)
  ;; Should be really rare.
  (unless (eq (symbol-package symbol) *package*)
    (error "~
CAPITALIZED-EXPORT: Export functionality using capitalized symbols
with a different home package than the current one is not implemented.
Please export in the usual way instead.

Tried to export ~S.
"  symbol))
  (intern (string-upcase symbol) *package*))

(defun collect-capitalized-symbols (form)
  (let* (new-tree
         (capitalized ())
         (collect-capitalized
           (lambda (object)
             (cond ((escaped-object-p object)
                    (escaped-object-value object))
                   ((and (symbolp object)
                         (matches-export-pattern-p
                          (symbol-name object)))
                    (let ((upcased-symbol
                            (resolve-capitalized-symbol object)))
                      (push upcased-symbol capitalized)
                      upcased-symbol))
                   (t object)))))
    (setf new-tree (map-tree-leaves collect-capitalized form))
    (values capitalized
            new-tree)))

(defun chomp (string)
  (if (eql #\Newline (alexandria:last-elt string))
      (subseq string 0 (1- (length string)))
      string))

(defclass capitalized-export-analyzer ()
  ((exports :initarg :exports :accessor exports :initform nil)
   (donep :initarg :donep :accessor donep :initform nil)
   (readtable :initarg :readtable
              :accessor analyzer-readtable
              :initform (make-inverted-readtable))
   (package :initarg :package :accessor analyzer-package :initform *package*)))

(defmethod done ((a capitalized-export-analyzer))
  (setf (donep a) t))

(defmethod analyze-file ((a capitalized-export-analyzer) file)
  (unless (donep a)
    (prog1 (analyze-string a (alexandria:read-file-into-string file))
      (done a))))

(defmethod analyze-string ((a capitalized-export-analyzer) string)
  (when *debug*
    (format t "; Analyzing string ~S~%" (chomp string)))
  (with-accessors ((exports exports)
                   (package analyzer-package)) a
    (let* ((*readtable* (analyzer-readtable a))
           (*package* package)
           (forms (buffering-readtable::read-all-from-string string))
           (capitalized nil))
      (dolist (form forms)
        (setf capitalized (append (collect-capitalized-symbols form)
                                  capitalized)))
      (when (and *debug* capitalized)
        (format t "; Found ~A capitalized symbols.~%" (length capitalized)))
      (setf exports (append capitalized exports)))))

(defun wrap-readtable-macro-characters (readtable fn)
  (let ((rt (copy-readtable readtable)))
    (loop :for i :from 0 :to 255
          :do (multiple-value-bind (reader-macro-fn nt)
                  (get-macro-character (code-char i) readtable)
                (when reader-macro-fn
                  (set-macro-character (code-char i)
                                       (lambda (s c)
                                         (funcall fn s c reader-macro-fn))
                                       nt
                                       rt))))
    rt))

(defvar *toplevel* t)

(setf *debug* t)

;;; Replaces the final newline in a file with an (export ...) form.
(defun make-capitalized-export-readtable ()
  (let ((analyzer (make-instance 'capitalized-export-analyzer))
        (done nil)
        wrapped-rt)
    (labels ((error-handler (c)
               ;; On any error, do not attempt to build an export
               ;; list.
               (declare (ignore c))
               (warn "CAPITALIZED-EXPORT: Error encountered while reading. Cannot determine export list.")
               (setf done t))
             (generate-exports ()
               ;; Make an export form (once) based on the collected
               ;; capitalized symbols.
               (assert *toplevel*)
               (when *debug*
                 (format t "; Exported ~A.~%" (exports analyzer)))
               (setf done t)
               `(export ',(exports analyzer)))
             (newline-reader-macro-fn (s c)
               (declare (ignore c))
               ;; Place exports last.
               (if (or done (listen s))
                   (values)
                   (generate-exports))))
      ;; Wrap the readtable to capture the input using an echo stream, so
      (setf wrapped-rt (wrap-readtable-macro-characters
                        *readtable*
                        (lambda (s c fn)
                          ;; Echo input from the stream to the analyzer.
                          (if (and *toplevel* (not done))
                              (let* ((capture (make-string-output-stream)))
                                (write-char c capture)
                                (with-open-stream (echo (make-echo-stream s capture))
                                  ;; Do not need to consider (values) here.
                                  (let ((form (handler-bind ((serious-condition #'error-handler))
                                                (let ((*toplevel* nil))
                                                  (funcall fn echo c)))))
                                    (analyze-string analyzer (get-output-stream-string capture))
                                    ;; Wrap in a progn if FN happened
                                    ;; to consume the last newline
                                    ;; (e.g. a ';' comment).  This
                                    ;; will confuse SLIME's labeling
                                    ;; of compiler notes, but for a
                                    ;; comment it doesn't really
                                    ;; matter.
                                    (if (not (listen s))
                                        `(progn
                                           ,form
                                           ,(generate-exports))
                                        form))))
                              ;; If not done, error handler is active here.
                              (funcall fn s c)))))
      (set-macro-character #\Newline
                           #'newline-reader-macro-fn
                           nil
                           wrapped-rt)
      wrapped-rt)))
