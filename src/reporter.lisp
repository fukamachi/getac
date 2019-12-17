(defpackage #:getac/reporter
  (:use #:cl)
  (:import-from #:getac/runner
                #:subprocess-error-command
                #:subprocess-error-output)
  (:import-from #:getac/diff
                #:lcs)
  (:import-from #:getac/indent-stream
                #:make-indent-stream
                #:stream-fresh-line-p
                #:with-indent)
  (:export #:*enable-colors*
           #:report-accepted
           #:report-wrong-answer
           #:report-runtime-error
           #:report-compilation-error
           #:report-time-limit-exceeded
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

(defun color-text (color text)
  (if *enable-colors*
      (if (= (length text) 0)
          text
          (let ((code (cdr (assoc color *color-codes*))))
            (unless code
              (error "Unsupported color: ~A" color))
            (format nil "~C[~Am~A~C[0m"
                    #\Esc
                    code
                    text
                    #\Esc)))
      text))

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
  (let ((lcs (lcs a b)))
    (let ((stream (make-indent-stream *standard-output*
                                      :prefix (color-text :red "- "))))
      (with-indent (stream +3)
        (loop with (lcs-char . lcs-rest) = lcs
              for i from 0
              for ch across b
              if (null lcs-char)
              do (princ (if (graphic-char-p ch)
                            (color-text :red (princ-to-string ch))
                            ch)
                        stream)
              else if (char= ch lcs-char)
              do (princ ch stream)
                 (setf lcs-char (pop lcs-rest))
              else
              do (princ (color-text :red (princ-to-string ch)) stream))
        (unless (stream-fresh-line-p stream)
          (format stream "~&~A~%"
                  (color-text :gray "[no newline at the end]")))))
    (format t "~&")
    (let ((stream (make-indent-stream *standard-output*
                                      :prefix (color-text :green "+ "))))
      (with-indent (stream +3)
        (loop with (lcs-char . lcs-rest) = lcs
              for i from 0
              for ch across a
              if (null lcs-char)
              do (princ (if (graphic-char-p ch)
                            (color-text :green (princ-to-string ch))
                            ch)
                        stream)
              else if (char= ch lcs-char)
              do (princ ch stream)
                 (setf lcs-char (pop lcs-rest))
              else
              do (princ (color-text :green (princ-to-string ch)) stream))
        (unless (stream-fresh-line-p stream)
          (format stream "~&~A~%"
                  (color-text :gray "[no newline at the end]")))))
    (format t "~2&")))

(defun %print-input (input)
  (format t "~2&")
  (let ((stream (make-indent-stream *standard-output* :level 3)))
    (princ input stream)))

(defun report-wrong-answer (test-name input expected actual took-ms)
  (print-first-line "WA" :red test-name took-ms)
  (%print-input input)
  (format t "~2&   ~A ~A~2%"
          (color-text :red "- actual")
          (color-text :green "+ expected"))
  (diff expected actual))

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

(defun report-time-limit-exceeded (test-name input seconds)
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
                    (format nil "~&× ~D of ~D test case~:*~P failed~%"
                            failed-count
                            (+ passed-count failed-count)))))
  (unless (= skipped-count 0)
    (princ (color-text :aqua
                       (format nil "● ~D test case~:*~P skipped~%" skipped-count)))))
