(defpackage #:getac/shell
  (:use #:cl)
  (:export #:run-program))
(in-package #:getac/shell)

(defun run-program (command &key input (output :stream))
  (let ((process (uiop:launch-program command
                                      :input input
                                      :output output
                                      :error-output :stream)))
    (unwind-protect
        (let ((code (uiop:wait-process process)))
          (unless (zerop code)
            (error 'uiop:subprocess-error
                   :code code
                   :command command
                   :process process))
          (values code
                  (uiop:process-info-output process)
                  (uiop:process-info-error-output process)))
      (when (uiop:process-alive-p process)
        (uiop:terminate-process process :urgent t)))))
