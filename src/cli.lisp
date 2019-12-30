(defpackage #:getac/cli
  (:use #:cl)
  (:import-from #:getac
                #:run)
  (:export #:option
           #:defoption
           #:option-name
           #:option-handler
           #:find-option
           #:cli-error
           #:invalid-option
           #:missing-option-value
           #:print-options-usage
           #:parse-argv
           #:cli-main
           #:main))
(in-package #:getac/cli)

(define-condition cli-error (error) ())
(define-condition invalid-option (cli-error)
  ((name :initarg :name))
  (:report (lambda (error stream)
             (format stream "Invalid option: ~A" (slot-value error 'name)))))
(define-condition missing-option-value (cli-error)
  ((name :initarg :name))
  (:report (lambda (error stream)
             (format stream "Missing value for the option: ~A" (slot-value error 'name)))))

(defvar *options* '())
(defvar *short-options* '())

(defstruct option
  name
  docstring
  short
  lambda-list
  handler)

(defmacro defoption (name (&key short) lambda-list &body body)
  (let ((option (gensym "OPTION"))
        (g-short (gensym "SHORT")))
    ;; Currently support only zero or one arguments
    (assert (or (null lambda-list)
                (null (rest lambda-list))))
    (multiple-value-bind (docstring body)
        (if (and (rest body)
                 (stringp (first body)))
            (values (first body) (rest body))
            (values nil body))
      `(let* ((,g-short ,short)
              (,option (make-option :name ,name
                                    :docstring ,docstring
                                    :short ,g-short
                                    :lambda-list ',lambda-list
                                    :handler (lambda ,lambda-list ,@body))))
         (push (cons (format nil "--~A" ,name) ,option) *options*)
         (when ,g-short
           (push (cons (format nil "-~A" ,g-short) ,option) *short-options*))
         ,option))))

(defun find-option (name &optional errorp)
  (let ((option (or (cdr (assoc name *options* :test 'equal))
                    (cdr (assoc name *short-options* :test 'equal)))))
    (when (and (null option)
               errorp)
      (error 'invalid-option :name name))
    option))

(defun print-usage-of-option (option stream)
  (format stream "~&    ~@[-~A, ~]--~A~@[=~{<~(~A~)>~^ ~}~]~%~@[        ~A~%~]"
          (option-short option)
          (option-name option)
          (option-lambda-list option)
          (option-docstring option)))

(defun print-options-usage (&optional (stream *error-output*))
  (dolist (option (reverse *options*))
    (print-usage-of-option (cdr option) stream)))

(defun option-string-p (string)
  (and (stringp string)
       (<= 2 (length string))
       (char= #\- (aref string 0))))

(defun parse-argv (argv)
  (loop for arg = (pop argv)
        while arg
        if (option-string-p arg)
        append (let ((=-pos (position #\= arg :start 1)))
                 (multiple-value-bind (option-name value)
                     (if =-pos
                         (values (subseq arg 0 =-pos)
                                 (subseq arg (1+ =-pos)))
                         (values arg nil))
                   (let ((option (find-option option-name t)))
                     (when (and (option-lambda-list option)
                                (null value))
                       (setf value (pop argv))
                       (when (or (null value)
                                 (option-string-p value))
                         (error 'missing-option-value
                                :name option-name)))
                     (apply (option-handler option)
                            (and value (list value)))))) into results
        else
        do (return (values results (cons arg argv)))
        finally (return (values results nil))))

(defun print-usage (&key (quit t))
  (format *error-output*
          "~&Usage: getac [options] <file>~2%OPTIONS:~%")
  (print-options-usage *error-output*)
  (when quit
    (uiop:quit -1)))

(defoption "test" (:short "t") (file)
  "Specify a file to read test cases. (Default: <file>.txt)"
  (list :test file))

(defoption "filetype" (:short "f") (type)
  "File type to test. The default will be detected by the file extension."
  (list :filetype type))

(defoption "timeout" (:short "T") (seconds)
  #.(format nil "Time limit for each test cases. (Default: ~A)" getac:*default-timeout*)
  (let ((sec (handler-case (read-from-string seconds)
               (error ()
                 (error "Invalid value for timeout: ~A" seconds)))))
    (unless (numberp sec)
      (error "Invalid value for timeout: ~A" seconds))
    (list :timeout sec)))

(defoption "disable-colors" () ()
  "Turn off colors."
  (setf getac:*enable-colors* nil)
  (values))

(defoption "no-fail-fast" (:short "F") ()
  "Don't quit when a failure."
  (list :fail-fast nil))

(defoption "version" (:short "V") ()
  "Print version."
  (format *error-output* "~&getac v~A running on ~A ~A~@[ (with ASDF ~A)~]~%"
          (asdf:component-version (asdf:find-system '#:getac))
          (lisp-implementation-type)
          (lisp-implementation-version)
          #+asdf (asdf:asdf-version) #-asdf nil)
  (uiop:quit -1))

(defoption "help" (:short "h") ()
  "Show help."
  (print-usage :quit t))

(defun cli-main (&rest argv)
  (unless argv
    (print-usage))
  (handler-case
      (multiple-value-bind (options argv)
          (handler-case (parse-argv argv)
            (cli-error (e)
              (format *error-output* "~&~A~%" e)
              (print-usage)))
        (let ((filename (pop argv)))
          (unless filename
            (format *error-output* "~&Missing a file to test.~%")
            (print-usage))
          (when argv
            ;; Allow options after the filename
            (multiple-value-bind (more-options argv)
                (handler-case (parse-argv argv)
                  (cli-error (e)
                    (format *error-output* "~&~A~%" e)
                    (print-usage)))
              (when argv
                (format *error-output* "Extra arguments: ~{~A~^ ~}~%" argv)
                (print-usage))
              (setf options (append options more-options))))

          (or (handler-case (apply #'getac:run filename options)
                (simple-error (e)
                              (format *error-output* "~&~A~%" e)
                              nil)
                (error (e)
                       (format *error-output* "~&~A: ~A~%" (type-of e) e)
                       nil))
              (uiop:quit -1))))
   #+sbcl (sb-sys:interactive-interrupt () (uiop:quit -1 t))))

(defun main (&optional (argv nil argv-supplied-p))
  (let ((argv (if argv-supplied-p
                  (rest argv)
                  (or #+sbcl (rest sb-ext:*posix-argv*)
                      #-sbcl uiop:*command-line-arguments*))))
    (apply #'cli-main argv)))
