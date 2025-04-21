(defpackage :capitalized-export
  (:use :cl)
  (:export #:default-scanner
           #:generate-export-form
           #:make-capitalized-export-readtable
           #:scan-string
           #:scanner-export-from-package
           #:scanner-exports
           #:scanner-read-into-package
           #:scanner-readtable))

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

;; Matches "Foo", "F", "Foo-Bar", "FoO-bAr" and "*Foo*".
(defun matches-lax-export-pattern-p (string)
  ;; STRING is the name of the symbol returned by the inverted
  ;; readtable.
  (or (and (= 1 (length string))
           (lower-case-p (aref string 0))) ; User typed "A".
      (and (< 1 (length string))
           ;; Heuristic: First letter capitalized, string contains a lower
           ;; case letter.
           (loop :with state = :looking-for-upper-case
                 :for c :across string
                 :when (alpha-char-p c)
                 :do (case state
                       (:looking-for-upper-case
                        (if (upper-case-p c)
                            (setf state :looking-for-lower-case)
                            (return nil)))
                       (:looking-for-lower-case
                        (when (lower-case-p c)
                          (return t))))
                 :finally (return nil)))))

;; Matches "Foo", "F" and "*Foo*" (only allow the first letter to be
;; capitalized).
(defun matches-restricted-export-pattern-p (string)
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

;;; This is used to escape |Foo| symbols.
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
                         (matches-lax-export-pattern-p
                          (symbol-name object)))
                    (push object capitalized)
                    object)
                   (t object)))))
    (setf new-tree (map-tree-leaves collect-capitalized form))
    (values capitalized
            new-tree)))

;;; Scanner functions.

(defgeneric scan-string (scanner string)
  (:documentation "Scan STRING for capitalized symbols."))

(defgeneric scanner-exports (scanner)
  (:documentation "The exports found by the scanner."))

(defgeneric scanner-readtable (scanner)
  (:documentation "The readtable used by the scanner."))

(defgeneric scanner-export-from-package (scanner)
  (:documentation
   "The package from which the capitalized symbols should be exported."))

(defgeneric scanner-read-into-package (scanner)
  (:documentation
   "The package in which SCANNER-STRING will perform reading."))

(defgeneric generate-export-form (scanner)
  (:documentation
   "Generate the export form to be returned at the end of the file."))

(defclass default-scanner ()
  ((exports :accessor scanner-exports
            :initform nil)
   (readtable :accessor scanner-readtable
              :initform (make-inverted-readtable))
   (package-export :initarg :export-from-package
                   :accessor scanner-export-from-package
                   :initform nil)
   (package-read :initarg :read-into-package
                 :accessor scanner-read-into-package
                 :initform nil)))

;; :PACKAGE can be used to set both read and export packages.
(defmethod initialize-instance :after ((scanner default-scanner)
                                       &key package)
  (with-accessors ((read-into-package scanner-read-into-package)
                   (export-from-package scanner-export-from-package))
      scanner
    (when (and package (not read-into-package))
      (setf read-into-package package))
    (when (and package (not export-from-package))
      (setf export-from-package package))))

(defun chomp (string)
  (if (eql #\Newline (alexandria:last-elt string))
      (subseq string 0 (1- (length string)))
      string))

(defun read-all-from-stream (stream)
  (loop :for form = (read stream nil stream nil)
        :until (eq form stream)
        :collect form))

(defun read-all-from-string (string)
  (with-input-from-string (string-stream string)
    (read-all-from-stream string-stream)))

;;; Collect (append) capitalized symbols found in string. Access them
;;; using the accessor SCANNER-EXPORTS.
(defmethod scan-string ((c default-scanner) string)
  (when *debug*
    (format t "; Analyzing string ~S~%" (chomp string)))
  (with-accessors ((exports scanner-exports)
                   (read-package scanner-read-into-package)) c
    (let* ((*readtable* (scanner-readtable c))
           (*package* read-package)
           (forms (read-all-from-string string))
           (capitalized nil))
      (dolist (form forms)
        (let ((form-capitalized-symbols
                (collect-capitalized-symbols form)))
          (setf capitalized (append form-capitalized-symbols
                                    capitalized))))
      (when (and *debug* capitalized)
        (let ((len (length capitalized)))
          (format t "; Found ~A capitalized ~a.~%"
                  len
                  (if (= 1 len)
                      "symbol"
                      "symbols"))))
      (setf capitalized (mapcar #'resolve-capitalized-symbol capitalized)
            exports (append capitalized exports)))))

;;; Import first, in case the export package for some reason
;;; doesn't contain the symbols (for instance, if creating "api
;;; packages"). This may happen if the export-from-package is
;;; different from the read-into-package.
;;;
;;; If we didn't care about that we could simply do:
;;;
;;; `(export ',(scanner-exports scanner)
;;;          ',(package-name (scanner-export-from-package scanner))))
;;;
(defmethod generate-export-form ((scanner default-scanner))
  `(let ((exports ',(scanner-exports scanner))
         (export-package
           ',(package-name (scanner-export-from-package scanner))))
     (import exports export-package)
     (export exports export-package)))

;;; Creating the readtable.

#-ecl
(defun wrap-readtable-macro-characters (readtable fn)
  (let ((rt (copy-readtable readtable)))
    (loop :for i :from 0 :to 255
          :do (multiple-value-bind (reader-macro-fn nt)
                  (get-macro-character (code-char i) readtable)
                (when reader-macro-fn
                  (set-macro-character (code-char i)
                                       (lambda (s c)
                                         (funcall fn s c reader-macro-fn :null))
                                       nt
                                       rt))))
    rt))

;; For ECL we handle dispatch macro characters explicitly.
#+ecl
(defun wrap-readtable-macro-characters (readtable fn)
  (let ((rt (copy-readtable readtable))
        (char-range 255))
    (flet ((handle-dispatch-character (char)
             (loop :for i :from 0 :to char-range
                   :do (let* ((reader-macro-fn
                                (get-dispatch-macro-character char
                                                              (code-char i)
                                                              readtable)))
                         (when reader-macro-fn
                           (set-dispatch-macro-character
                            char
                            (code-char i)
                            (lambda (s c n)
                              (funcall fn s c reader-macro-fn n))
                            rt)))))
           (handle-macro-character (char)
             (multiple-value-bind (reader-macro-fn nt)
                 (get-macro-character char readtable)
               (when reader-macro-fn
                 (set-macro-character char
                                      (lambda (s c)
                                        (funcall fn s c reader-macro-fn :null))
                                      nt
                                      rt))))
           (dispatch-macro-character-p (char rt)
             (and (get-macro-character char rt)
                  (handler-case
                      ;; If char is not a dispatch macro character, an
                      ;; error should be signalled.
                      (progn (get-dispatch-macro-character
                              char #\a rt) ; can test any char
                             t)
                    (error (c) nil)))))
      (loop :for i :from 0 :to char-range
            :for char = (code-char i)
            :do (cond ((dispatch-macro-character-p char rt)
                       (handle-dispatch-character char))
                      ((get-macro-character char rt)
                       (handle-macro-character char))))
      rt)))

(defvar *toplevel* t)

;;; Replaces the final newline in a file with an (export ...) form.
(defun make-capitalized-export-readtable (&key (package *package*)
                                               scanner)
  "Creates a readtable that will arrange for an export form to be
generated at the end of a stream, if the stream ends in a newline.  If
SCANNER is nil, then the default scanner will be used.  The default
scanner will export all capitalized symbols encountered while reading
the stream from the package PACKAGE."
  (setf package (find-package package))
  (unless scanner
    (setf scanner (make-instance 'default-scanner
                    :package package)))
  (let ((done nil) ; We're done after encountering a newline followed by EOF.
        wrapped-rt
        first-stream-encountered)
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
                 (format t "; Exported ~A.~%" (scanner-exports scanner)))
               (setf done t)
               (generate-export-form scanner))
             (newline-reader-macro-fn (s c)
               (declare (ignore c))
               ;; Place exports last.
               (if (or done
                       (not (eq first-stream-encountered s))
                       (listen s))
                   (values)
                   (generate-exports))))
      ;; Wrap the readtable to capture the input using an echo
      ;; stream.
      (setf wrapped-rt (wrap-readtable-macro-characters
                        *readtable*
                        (lambda (s c fn dispatch-numeric-argument)
                          (when (null first-stream-encountered)
                            (setf first-stream-encountered s))
                          ;; Echo each toplevel form to the
                          ;; scanner. If done is T, then do nothing
                          ;; out of the ordinary, i.e. just call the
                          ;; wrapped functions. Done becomes T when
                          ;; the final newline is encountered.
                          (if (and *toplevel*
                                   (not done)
                                   (eq s first-stream-encountered))
                              (let* ((capture (make-string-output-stream)))
                                (write-char c capture)
                                (with-open-stream (echo (make-echo-stream s capture))
                                  ;; FINISH-OUTPUT seems to be
                                  ;; necessary, otherwise SLIME gets
                                  ;; confused when labeling compiler
                                  ;; notes.
                                  (let ((values (handler-bind ((serious-condition #'error-handler))
                                                  (let ((*toplevel* nil))
                                                    (prog1 (multiple-value-list
                                                            (if (eq :null dispatch-numeric-argument)
                                                                (funcall fn echo c) ; Not dispatch-macro-character
                                                                (funcall fn echo c dispatch-numeric-argument)))
                                                      (finish-output echo))))))
                                    (unless done
                                      (scan-string scanner (get-output-stream-string capture)))
                                    ;; Wrap in a progn if FN happened
                                    ;; to consume the last newline
                                    ;; (e.g. a ';' comment).  This
                                    ;; will confuse SLIME's labeling
                                    ;; of compiler notes, but for a
                                    ;; comment it doesn't really
                                    ;; matter.
                                    (if (not (listen s))
                                        `(progn
                                           ,@values
                                           ,(generate-exports))
                                        (values-list values)))))
                              ;; If not done, error handler is active here.
                              (values-list
                               (prog1 (multiple-value-list
                                       (if (eq :null dispatch-numeric-argument)
                                           (funcall fn s c) ; Not dispatch-macro-character
                                           (funcall fn s c dispatch-numeric-argument)))
                                 (finish-output s)))))))
      (set-macro-character #\Newline
                           #'newline-reader-macro-fn
                           nil
                           wrapped-rt)
      wrapped-rt)))
