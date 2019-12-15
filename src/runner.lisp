(defpackage #:get-accepted/runner
  (:use #:cl
        #:get-accepted/utils)
  (:export #:*default-commands*
           #:run-file
           #:subprocess-error
           #:execution-error
           #:compilation-error
           #:subprocess-error-command
           #:subprocess-error-output))
(in-package #:get-accepted/runner)

(defvar *default-commands*
  '(("lisp" . (("execute" . ("ros" "run" "--" "--script" file))))
    ("python" . (("execute" . ("python" "-B" file))))
    ("python2" . (("execute" . ("python2" "-B" file))))
    ("python3" . (("execute" . ("python3" "-B" file))))
    ("pypy" . (("execute" . ("pypy" file))))
    ("pypy2" . (("execute" . ("pypy2" file))))
    ("pypy3" . (("execute" . ("pypy3" file))))
    ("ruby" . (("execute" . ("ruby" "--disable-gems" file))))
    ("go" . (("compile" . ("go" "build" "-o" compile-to file))
             ("execute" . (compile-to))))))

(defun render-command (command-template values)
  (mapcar (lambda (x)
            (typecase x
              (symbol
                (let ((pair (assoc (symbol-name x) values :test 'equalp :key #'symbol-name)))
                  (unless pair
                    (error "Unexpected variable: ~A" x))
                  (cdr pair)))
              (otherwise (princ-to-string x))))
          command-template))

(define-condition subprocess-error (error)
  ((command :initarg :command
            :reader subprocess-error-command)
   (output :initarg :output
           :reader subprocess-error-output)))

(define-condition execution-error (subprocess-error) ())
(define-condition compilation-error (subprocess-error) ())

(defmacro with-took-ms (took-ms &body body)
  (let ((before (gensym "BEFORE")))
    `(let ((,before (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (setf ,took-ms
               (* (- (get-internal-real-time) ,before)
                  #.(if (= internal-time-units-per-second 1000)
                        1
                        (/ internal-time-units-per-second 1000.0))))))))

(defun execute-code (command-template file input &optional compile-to)
  (check-type command-template cons)
  (check-type file pathname)
  (check-type input string)
  (let ((command (render-command command-template
                                 `((file . ,(uiop:native-namestring file))
                                   (compile-to . ,(and compile-to
                                                       (uiop:native-namestring compile-to))))))
        (errout (make-string-output-stream))
        (took-ms 0))
    (with-input-from-string (in input)
      (values
        (handler-case (with-took-ms took-ms
                        (uiop:run-program command
                                          :input in
                                          :output :string
                                          :error-output errout))
          (uiop:subprocess-error ()
            (error 'execution-error
                   :command command
                   :output (get-output-stream-string errout))))
        took-ms))))

(defun compile-code (command-template file &optional compile-to)
  (check-type command-template cons)
  (check-type file pathname)
  (check-type compile-to (or null pathname))
  (let* ((compile-to (or compile-to
                         (uiop:with-temporary-file (:pathname file :keep t) file)))
         (command (render-command command-template
                                  `((file . ,(uiop:native-namestring file))
                                    (compile-to . ,(uiop:native-namestring compile-to)))))
         (errout (make-string-output-stream)))
    (handler-case (uiop:run-program command
                                    :input t
                                    :output t
                                    :error-output errout)
      (uiop:subprocess-error ()
        (error 'compilation-error
               :command command
               :output (get-output-stream-string errout))))
    compile-to))

(defun detect-filetype (file)
  (check-type file pathname)
  (let ((type (pathname-type file)))
    (cond
      ((string= type "py") "python")
      ((string= type "rb") "ruby")
      (t type))))

(defun run-file (file input &key compile-to filetype)
  (let* ((file (normalize-pathname file))
         (filetype (or filetype
                       (detect-filetype file)))
         (commands (cdr (assoc filetype *default-commands* :test 'string=))))
    (unless commands
      (error "Unknown file type: ~A" filetype))

    ;; Compilation
    (let ((compilation-command (cdr (assoc "compile" commands :test 'string=))))
      (when compilation-command
        (compile-code compilation-command file (and compile-to
                                                    (normalize-pathname compile-to)))))
    ;; Execution
    (let ((execution-command (cdr (assoc "execute" commands :test 'string=))))
      (when execution-command
        (execute-code execution-command file input compile-to)))))
