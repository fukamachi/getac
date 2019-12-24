(defpackage #:getac/reporter
  (:use #:cl)
  (:import-from #:getac/runner
                #:subprocess-error-command
                #:subprocess-error-output)
  (:import-from #:getac/diff
                #:edit-operations)
  (:import-from #:getac/indent-stream
                #:make-indent-stream
                #:stream-fresh-line-p
                #:new-line-char-p
                #:with-indent)
  (:export #:*enable-colors*
           #:report-accepted
           #:report-wrong-answer
           #:report-runtime-error
           #:report-compilation-error
           #:report-time-limit-exceeded
           #:report-canceled
           #:print-summary))
(in-package #:getac/reporter)

(defvar *enable-colors*
  (not
    (or (equal (uiop:getenv "EMACS") "t")
        (uiop:getenv "INSIDE_EMACS"))))

(defparameter *background-color-codes*
  '((:green . 42)
    (:red . 41)
    (:white . 47)))

(defparameter *color-codes*
  '((:green . 32)
    (:red . 31)
    (:aqua . 36)
    (:gray . 90)))

(defun start-color (color &optional stream)
  (when *enable-colors*
    (let ((code (cdr (assoc color *color-codes*))))
      (unless code
        (error "Unsupported color: ~A" color))
      (format stream "~C[~Am" #\Esc code)))
  (values))

(defun reset-color (&optional stream)
  (when *enable-colors*
    (format stream "~C[0m" #\Esc))
  (values))

(defun color-text (color text)
  (if (= (length text) 0)
      text
      (with-output-to-string (s)
        (start-color color s)
        (format s text)
        (reset-color s))))

(defun print-badge (label color &optional (stream *standard-output*))
  (check-type label string)
  (check-type stream stream)
  (let ((code (cdr (assoc color *background-color-codes*))))
    (unless code
      (error "Unsupported color: ~S" color))
    (if *enable-colors*
        (format stream "~C[30;~Am  ~A  ~C[0m" #\Esc code label #\Esc)
        (format stream "[ ~A ]" label))))

(defun print-first-line (label color &optional test-name took-ms (stream *standard-output*))
  (format stream "~& ")
  (print-badge label color stream)
  (when test-name
    (format stream " ~A" test-name))
  (when took-ms
    (format stream " ~A" (color-text :gray (format nil "(~A ms)" took-ms))))
  (fresh-line stream))

(defun report-accepted (test-name took-ms)
  (print-first-line "AC" :green test-name took-ms))

(defun diff (a b)
  (cond
    ((null a)
     (let ((stream (make-indent-stream *standard-output*
                                       :prefix (color-text :green "+ ")
                                       :level 3)))
       (princ b stream)))
    ((null b)
     (let ((stream (make-indent-stream *standard-output*
                                       :prefix (color-text :red "- ")
                                       :level 3)))
       (princ a stream)))
    (t
     (let ((ops (edit-operations a b)))
       (if (every (lambda (x) (char= x #\M)) ops)
           (let ((stream (make-indent-stream *standard-output* :level 5)))
             (princ a stream))
           (progn
             (let ((stream (make-indent-stream *standard-output*
                                               :prefix (color-text :red "- ")))
                   (ops (remove #\D ops)))
               (with-indent (stream +3)
                            (let ((color nil))
                              (loop
                                for op = (pop ops)
                                for ch across b
                                if (or (null op)
                                       (char= op #\M))
                                do (when color
                                     (reset-color stream)
                                     (setf color nil))
                                (princ ch stream)
                                else
                                do (unless color
                                     (start-color :red stream)
                                     (setf color t))
                                (princ ch stream))
                              (when color
                                (reset-color stream)))
                            (when (or (zerop (length b))
                                      (not (new-line-char-p (aref b (1- (length b))))))
                              (format stream "~&~A~%"
                                      (color-text :gray "[no newline at the end]")))))
             (format t "~&")
             (let ((stream (make-indent-stream *standard-output*
                                               :prefix (color-text :green "+ ")))
                   (ops (remove #\I ops)))
               (with-indent (stream +3)
                            (let ((color nil))
                              (loop
                                for op = (pop ops)
                                for ch across a
                                if (or (null op)
                                       (char= op #\M))
                                do (when color
                                     (reset-color stream)
                                     (setf color nil))
                                (princ ch stream)
                                else
                                do (unless color
                                     (start-color :green stream)
                                     (setf color t))
                                (princ ch stream))
                              (when color
                                (reset-color stream)))
                            (when (or (zerop (length a))
                                      (not (new-line-char-p (aref a (1- (length a))))))
                              (format stream "~&~A~%"
                                      (color-text :gray "[no newline at the end]"))))))))))
  (format t "~&"))

(defun %print-input (input)
  (format t "~2&")
  (let ((stream (make-indent-stream *standard-output* :level 3)))
    (princ input stream)))

(defun by-line-iter (str)
  (let ((start 0)
        (end (length str))
        eofp)
    (lambda ()
      (unless eofp
        (let ((pos (position #\Newline str :test #'char= :start start)))
          (if pos
              (prog1 (subseq str start (1+ pos))
                (setf start (1+ pos))
                (when (= start end)
                  (setf eofp t)))
              (prog1 (subseq str start)
                (setf eofp t))))))))

(defun report-wrong-answer (test-name input expected actual took-ms)
  (print-first-line "WA" :red test-name took-ms)
  (%print-input input)
  (format t "~2&   ~A ~A~2%"
          (color-text :red "- actual")
          (color-text :green "+ expected"))
  (loop with expected-iter = (by-line-iter expected)
        with actual-iter = (by-line-iter actual)
        for expected-line = (funcall expected-iter)
        for actual-line = (funcall actual-iter)
        while (or expected-line actual-line)
        do (diff expected-line actual-line)))

(defun report-compilation-error (error)
  (print-first-line "CE" :red "Compilation failed.")
  (let ((stream (make-indent-stream *standard-output*)))
    (with-indent (stream +3)
      (format stream "~&~A~%"
              (color-text :gray
                          (format nil "While executing ~{~A~^ ~}~2%  ~A"
                                  (subprocess-error-command error)
                                  (subprocess-error-output error)))))))

(defun report-runtime-error (test-name input error)
  (print-first-line "RE" :red test-name)
  (%print-input input)
  (let ((stream (make-indent-stream *standard-output*)))
    (with-indent (stream +3)
      (format stream "~2&~A~%"
              (color-text :gray
                          (format nil "While executing ~{~A~^ ~}~2%  ~A"
                                  (subprocess-error-command error)
                                  (subprocess-error-output error)))))))

(defun report-time-limit-exceeded (test-name seconds)
  (print-first-line "TLE" :red test-name)
  (format t "~&   ~A~%"
          (color-text :gray
                      (format nil "Took ~A seconds and time limit exceeded" seconds))))

(defun report-canceled (test-name input seconds)
  (print-first-line "TLE" :red test-name)
  (%print-input input)
  (format t "~2&   ~A~%"
          (color-text :gray
                      (format nil "Canceled after ~A seconds" seconds))))

(defun print-summary (passed-count failed-count skipped-count)
  (princ
    (if (= failed-count 0)
        (color-text :green
                    (format nil "~2&✓ All ~D test case~:*~P passed~%"
                            passed-count))
        (color-text :red
                    (format nil "~2&× ~D of ~D test case~:*~P failed~%"
                            failed-count
                            (+ passed-count failed-count)))))
  (unless (= skipped-count 0)
    (princ (color-text :aqua
                       (format nil "● ~D test case~:*~P skipped~%" skipped-count)))))
