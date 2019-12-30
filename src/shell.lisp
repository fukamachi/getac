(defpackage #:getac/shell
  (:use #:cl)
  (:export #:run-program))
(in-package #:getac/shell)

(defun run-program (command &rest initargs)
  (let ((process (apply #'uiop:launch-program command initargs)))
    (unwind-protect
        (let ((code (uiop:wait-process process)))
          (unless (zerop code)
            (error 'uiop:subprocess-error
                   :code code
                   :command command
                   :process process))
          code)
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process :urgent t)))))
