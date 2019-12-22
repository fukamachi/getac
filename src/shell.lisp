(defpackage #:getac/shell
  (:use #:cl)
  (:export #:run-program))
(in-package #:getac/shell)

(defun run-program (command &rest initargs)
  (let ((process (apply #'uiop:launch-program command initargs)))
    (unwind-protect (uiop:wait-process process)
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process :urgent t)))))
