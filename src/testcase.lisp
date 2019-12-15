(defpackage #:get-accepted/testcase
  (:use #:cl
        #:get-accepted/utils)
  (:export #:read-from-stream
           #:read-from-file
           #:default-testcase-file))
(in-package #:get-accepted/testcase)

(defparameter *testcase-file-extension* "in")

(defun header-line-p (line)
  (let ((start (position-if (lambda (x)
                              (char/= x #\=))
                            line)))
    (and start
         (let ((end (position-if (lambda (x)
                                   (char= x #\=))
                                 line
                                 :start start)))
           (and end
                (= start (- (length line) end))
                (not (find-if (lambda (x) (char/= x #\=)) line :start end))
                (values t (string-trim '(#\Space #\Tab) (subseq line start end))))))))

(defun delimiter-line-p (line)
  (and (<= 4 (length line))
       (every (lambda (x) (char= x #\-)) line)))

(defun read-from-stream (stream)
  (let ((eof nil)
        (results '())
        (header-line (read-line stream nil nil)))
    (loop
      (when eof
        (return))
      (unless header-line
        (error "Unexpected EOF while reading a test case header"))
      (multiple-value-bind (headerp test-name)
          (header-line-p header-line)
        (unless headerp
          (error "Not test case header: ~S" header-line))
        (let ((input
                (let ((buffer (make-string-output-stream)))
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
