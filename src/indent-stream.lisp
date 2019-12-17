(defpackage #:getac/indent-stream
  (:use #:cl
        #:trivial-gray-streams)
  (:export #:indent-stream
           #:make-indent-stream
           #:stream-fresh-line-p
           #:with-indent))
(in-package #:getac/indent-stream)

(defclass indent-stream (trivial-gray-stream-mixin
                         fundamental-character-output-stream)
  ((stream :initarg :stream
           :accessor stream-real-stream)
   (level :initarg :level
          :initform 0
          :accessor stream-indent-level)
   (prefix :initarg :prefix
           :initform nil
           :accessor stream-prefix)

   (fresh-line-p :initform t
                 :accessor stream-fresh-line-p)))

(defun make-indent-stream (stream &rest initargs &key level prefix)
  (declare (ignore level prefix))
  (apply #'make-instance 'indent-stream
         :stream stream
         initargs))

(defmacro with-indent ((stream level) &body body)
  (let ((g-level (gensym "LEVEL"))
        (g-stream (gensym "STREAM")))
    `(let ((,g-level ,level)
           (,g-stream ,stream))
       (incf (stream-indent-level ,g-stream) ,g-level)
       (unwind-protect (progn ,@body)
         (decf (stream-indent-level ,g-stream) ,g-level)))))

(defun new-line-char-p (char)
  (and (member char '(#\Newline #\Linefeed #\Return) :test #'char=)
       t))

(defmethod stream-write-char ((stream indent-stream) char)
  (let ((*standard-output* (stream-real-stream stream)))
    (cond
      ((new-line-char-p char)
       (write-char char)
       (setf (stream-fresh-line-p stream) t))
      ((stream-fresh-line-p stream)
       (write-string
         (make-string (stream-indent-level stream) :initial-element #\Space))
       (when (stream-prefix stream)
         (write-string (stream-prefix stream)))
       (write-char char)
       (setf (stream-fresh-line-p stream) nil))
      (t
       (write-char char)))))

(defmethod stream-line-column ((stream indent-stream))
  (+ (stream-indent-level stream)
     (length (stream-prefix stream))
     (call-next-method)))

(defmethod stream-start-line-p ((stream indent-stream))
  (stream-fresh-line-p stream))

(defmethod stream-finish-output ((stream indent-stream))
  (stream-finish-output (stream-real-stream stream)))

(defmethod stream-force-output ((stream indent-stream))
  (stream-force-output (stream-real-stream stream)))

(defmethod stream-clear-output ((stream indent-stream))
  (stream-clear-output (stream-real-stream stream)))
