(defpackage #:getac/utils
  (:use #:cl)
  (:export #:normalize-pathname))
(in-package #:getac/utils)

(defun normalize-pathname (filename &optional allow-directory)
  (let ((file (probe-file filename)))
    (cond
      ((null file)
       (error "File not exists: ~A" filename))
      ((and (uiop:directory-pathname-p file)
            (not allow-directory))
       (error "Is a directory: ~A" filename))
      (t file))))
