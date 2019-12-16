(defpackage #:getac/timer
  (:use #:cl)
  (:export #:max-execution-time*
           #:with-deadline
           #:deadline-timeout
           #:deadline-timeout-seconds))
(in-package #:getac/timer)

(defparameter *max-execution-time* 10) ;; in seconds

(define-condition deadline-timeout (error)
  ((seconds :initarg :seconds
            :reader deadline-timeout-seconds)))

(defmacro with-deadline (&body body)
  #+sbcl
  `(handler-bind ((sb-sys:deadline-timeout
                    (lambda (e)
                      (error 'deadline-timeout :seconds *max-execution-time*))))
     (sb-sys:with-deadline (:seconds *max-execution-time*)
       ,@body))
  #-sbcl
  `(progn ,@body))
