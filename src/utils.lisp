(defpackage #:get-accepted/utils
  (:use #:cl)
  (:export #:normalize-pathname))
(in-package #:get-accepted/utils)

(defun normalize-pathname (filename)
  (let ((file (probe-file filename)))
    (unless file
      (error "File not exists: ~A" filename))
    file))
