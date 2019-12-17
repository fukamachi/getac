(defpackage #:getac/testcase
  (:use #:cl
        #:getac/utils)
  (:export #:read-from-stream
           #:read-from-file
           #:default-testcase-file))
(in-package #:getac/testcase)

(defparameter *testcase-file-extension* "txt")

(defun header-line-p (line)
  (let ((start (position-if (lambda (x)
                              (char/= x #\=))
                            line)))
    (or (and (null start)
             (<= 4 (length line))
             (values t nil))
        (and (<= 2 start)
             (let ((end (position-if (lambda (x)
                                       (char= x #\=))
                                     line
                                     :start start)))
               (and end
                    (<= 2 (- (length line) end))
                    (not (find-if (lambda (x) (char/= x #\=)) line :start end))
                    (values t (string-trim '(#\Space #\Tab) (subseq line start end)))))))))

(defun delimiter-line-p (line)
  (and (<= 4 (length line))
       (every (lambda (x) (char= x #\-)) line)))

(defparameter *default-test-name* "test case ~D")

(defun read-from-stream (stream)
  (let ((eof nil)
        (results '())
        (header-line (read-line stream nil nil))
        (gen-test-count 0))
    (loop
      (when eof
        (return))
      (multiple-value-bind (headerp test-name)
          (header-line-p header-line)
        (let ((test-name (or test-name
                             (format nil *default-test-name* (incf gen-test-count))))
              (input
                (let ((buffer (make-string-output-stream)))
                  (unless headerp
                    (format buffer "~A~%" header-line))
                  (loop
                    (let ((line (read-line stream nil nil)))
                      (cond
                        ((null line)
                         (setf eof t)
                         (return))
                        ((delimiter-line-p line)
                         (return))
                        (t (format buffer "~A~%" line)))))
                  (get-output-stream-string buffer))))
          (unless eof
            (let ((buffer (make-string-output-stream)))
              (loop
                (let ((line (read-line stream nil nil)))
                  (cond
                    ((null line)
                     (push (list test-name input (get-output-stream-string buffer))
                           results)
                     (setf eof t)
                     (return))
                    ((header-line-p line)
                     (push (list test-name input (get-output-stream-string buffer))
                           results)
                     (setf header-line line)
                     (return))
                    (t
                     (format buffer "~A~%" line))))))))))
    (nreverse results)))

(defun read-from-file (file)
  (let ((file (normalize-pathname file)))
    (uiop:with-input-file (in file)
      (read-from-stream in))))

(defun default-testcase-file (file)
  (let ((file (normalize-pathname file)))
    (make-pathname :name (pathname-name file)
                   :type *testcase-file-extension*
                   :defaults file)))
