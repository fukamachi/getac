(defpackage #:getac/utils
  (:use #:cl)
  (:export #:normalize-pathname))
(in-package #:getac/utils)

(defun normalize-pathname (filename)
  (let ((file (probe-file filename)))
    (cond
      ((null file)
       (error "File not exists: ~A" filename))
      ((uiop:directory-pathname-p file)
       (error "Is a directory: ~A" filename))
      (t file))))
