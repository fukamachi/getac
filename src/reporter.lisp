(defpackage #:get-accepted/reporter
  (:use #:cl)
  (:import-from #:get-accepted/runner
                #:subprocess-error-command
                #:subprocess-error-output)
  (:export #:*enable-colors*
           #:report-accepted
           #:report-wrong-answer
           #:report-runtime-error
           #:report-compilation-error
           #:report-unknown
           #:print-summary))
(in-package #:get-accepted/reporter)

(defvar *enable-colors*
  (not
    (or (equal (uiop:getenv "EMACS") "t")
        (uiop:getenv "INSIDE_EMACS"))))

(defparameter *color-codes*
  '((:green . 42)
    (:red . 41)
    (:gray . 90)
    (:white . 47)
    (:fggreen . 32)
    (:fgred . 31)))

(defun color-text (color text)
  (if *enable-colors*
      (if (= (length text) 0)
          text
          (let ((code (cdr (assoc color *color-codes*))))
            (assert code)
            (format nil "~C[~Am~A~C[0m"
                    #\Esc
                    code
                    text
                    #\Esc)))
      text))

(defun print-badge (label color &optional (stream *standard-output*))
  (check-type label string)
  (check-type stream stream)
  (let ((code (cdr (assoc color *color-codes*))))
    (unless code
      (error "Unsupported color: ~S" color))
    (if *enable-colors*
        (format stream "~C[30;~Am  ~A  ~C[0m" #\Esc code label #\Esc)
        (format stream "[ ~A ]" label))))

(defun print-first-line (label color test-name &optional took-ms (stream *standard-output*))
  (fresh-line stream)
  (print-badge label color stream)
  (format stream " ~A" test-name)
  (when took-ms
    (format stream " ~A" (color-text :gray (format nil "(~A ms)" took-ms))))
  (fresh-line stream))

(defun report-accepted (test-name took-ms)
  (print-first-line "AC" :green test-name took-ms))

(defun report-wrong-answer (test-name expected actual took-ms)
  (print-first-line "WA" :red test-name took-ms)
  (format t "~&  Expected: ~S~%  Actual: ~S~%" expected actual))

(defun report-compilation-error (test-name error)
  (declare (ignore error))
  (print-first-line "CE" :red test-name))

(defun report-runtime-error (test-name error)
  (print-first-line "RE" :red test-name)
  (format t "~&  ~A~%"
          (color-text :gray
                      (format nil "While executing ~{~A~^ ~}~2%  ~A"
                              (subprocess-error-command error)
                              (subprocess-error-output error)))))

(defun report-unknown (test-name)
  (print-first-line "UNKNOWN" :white test-name))

(defun print-summary (passed-count failed-count)
  (princ
    (if (= failed-count 0)
        (color-text :fggreen
                    (format nil "~2&✓ All ~D test case~:*~P passed~%"
                            passed-count))
        (color-text :fgred
                    (format nil "~2&× ~D of ~D test case~:*~P failed~%"
                            failed-count
                            (+ passed-count failed-count))))))
