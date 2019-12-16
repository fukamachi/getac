(defpackage #:getac/runner
  (:use #:cl
        #:getac/utils)
  (:export #:*default-commands*
           #:run-file
           #:subprocess-error
           #:execution-error
           #:compilation-error
           #:subprocess-error-command
           #:subprocess-error-output))
(in-package #:getac/runner)

(defparameter *cpp-include-locations*
  (list #+unix "/usr/include/boost"
        #+darwin "/usr/local/include/boost"))

(defvar *default-commands*
  `(("lisp" . (("execute" . ("ros" "run" "--" "--script" file))))
    ("python" . (("execute" . ("python" "-B" file))))
    ("python2" . (("execute" . ("python2" "-B" file))))
    ("python3" . (("execute" . ("python3" "-B" file))))
    ("pypy" . (("execute" . ("pypy" file))))
    ("pypy2" . (("execute" . ("pypy2" file))))
    ("pypy3" . (("execute" . ("pypy3" file))))
    ("ruby" . (("execute" . ("ruby" "--disable-gems" file))))
    ("go" . (("compile" . ("go" "build" "-o" compile-to file))
             ("execute" . (compile-to))))
    ("java" . (("compile" . ("javac" "-d" compile-in file))
               ("execute" . ("java" "-cp" compile-in filename))))
    ("javascript" . (("execute" . ("node" file))))
    ("clojure" . (("execute" . ("clj" file))))
    ("c" . (("compile" . ("gcc" "-std=gnu11" "-O2" "-o" compile-to file "-lm"))
            ("execute" . (compile-to))))
    ("cpp" . (("compile" . ("g++" "-std=gnu++1y" "-O2" ,@(mapcar (lambda (dir) (format nil "-I~A" dir))
                                                                 *cpp-include-locations*)
                            "-o" compile-to file))
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
                                   (filename . ,(pathname-name file))
                                   (compile-to . ,(and compile-to
                                                       (uiop:native-namestring compile-to)))
                                   (compile-in . ,(and compile-to
                                                       (uiop:native-namestring (uiop:pathname-directory-pathname compile-to)))))))
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
                                    (compile-to . ,(uiop:native-namestring compile-to))
                                    (compile-in . ,(uiop:native-namestring (uiop:pathname-directory-pathname compile-to))))))
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
      ((string= type "js") "javascript")
      ((string= type "clj") "clojure")
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
        (setf compile-to
              (compile-code compilation-command
                            file
                            (and compile-to
                                 (normalize-pathname compile-to))))))
    ;; Execution
    (let ((execution-command (cdr (assoc "execute" commands :test 'string=))))
      (when execution-command
        (execute-code execution-command file input compile-to)))))
