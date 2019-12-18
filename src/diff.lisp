(defpackage #:getac/diff
  (:use #:cl)
  (:export #:edit-operations))
(in-package #:getac/diff)

(declaim (type hash-table *memoized*))
(defvar *memoized*)

(declaim (ftype (function (simple-string simple-string fixnum fixnum fixnum fixnum) (values list fixnum))
                %compute-edit-operations compute-edit-operations))

(defun %compute-edit-operations (a b start1 start2 end1 end2)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (simple-string a b)
           (fixnum start1 start2 end1 end2))
    (let ((a-len (- end1 start1))
        (b-len (- end2 start2)))
    (declare (fixnum a-len b-len))
    (cond
      ((= 0 a-len)
       (values (make-list b-len :initial-element #\A)
               b-len))
      ((= 0 b-len)
       (values (make-list a-len :initial-element #\D)
               a-len))
      (t
       (if (char= (aref a start1) (aref b start2))
           (multiple-value-bind (ops distance)
               (compute-edit-operations a b (1+ start1) (1+ start2) end1 end2)
             (values (cons #\M ops) distance))
           (multiple-value-bind (ops1 d1)
               (compute-edit-operations a b start1 (1+ start2) end1 end2)
             (when (zerop d1)
               (return-from %compute-edit-operations
                            (values (cons #\I ops1) 1)))
             (multiple-value-bind (ops2 d2)
                 (compute-edit-operations a b (1+ start1) start2 end1 end2)
               (when (zerop d2)
                 (return-from %compute-edit-operations
                              (values (cons #\D ops2) 1)))
               (multiple-value-bind (ops3 d3)
                   (compute-edit-operations a b (1+ start1) (1+ start2) end1 end2)
                 (let ((min-distance (min d1 d2 d3)))
                   (declare (fixnum min-distance))
                   (values (cond
                             ((= d1 min-distance)
                              (cons #\I ops1))
                             ((= d2 min-distance)
                              (cons #\D ops2))
                             (t (cons #\R ops3)))
                           (1+ min-distance)))))))))))

(defun compute-edit-operations (a b start1 start2 end1 end2)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (simple-string a b)
           (fixnum start1 start2 end1 end2))
  "Call %compute-edit-operations with memoization."
  (let ((result (gethash (list start1 start2 end1 end2) *memoized*)))
    (declare (list result))
    (apply #'values (or result
                        (setf (gethash (list start1 start2 end1 end2)
                                       *memoized*)
                              (multiple-value-list (%compute-edit-operations a b start1 start2 end1 end2)))))))

(declaim (ftype (function (simple-string simple-string) (values list fixnum))
                edit-operations))

(defun edit-operations (a b)
  (declare (optimize (speed 3) (safety 2) (debug 2))
           (simple-string a b))
  "Return the operations to make A be the same as B as a list.

Each operations are a character which is one of 4 characters -- #\A for addition, #\D for deletion, #\R for replacement and #\M for matched ones."
  (let ((end1 (length a))
        (end2 (length b))
        (*memoized* (make-hash-table :test 'equal)))
    (declare (fixnum end1 end2))
    (compute-edit-operations a b 0 0 end1 end2)))
