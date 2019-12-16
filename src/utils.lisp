(defpackage #:getac/utils
  (:use #:cl)
  (:export #:normalize-pathname))
(in-package #:getac/utils)

(defun normalize-pathname (filename)
  (let ((file (probe-file filename)))
    (unless file
      (error "File not exists: ~A" filename))
    file))
