(defpackage #:getac/diff
  (:use #:cl)
  (:export #:lcs))
(in-package #:getac/diff)

(defun compute-lcs (a b start1 start2 end1 end2)
  (when (or (<= end1 start1)
            (<= end2 start2))
    (return-from compute-lcs nil))

  (let ((c1 (aref a start1))
        (c2 (aref b start2)))
    (if (char= c1 c2)
        (cons c1 (compute-lcs a b (1+ start1) (1+ start2) end1 end2))
        (let ((i start1)
              (j start2)
              i-end-reached j-end-reached)
          (loop
            (unless i-end-reached
              (if (= (1+ i) end1)
                  (setf i-end-reached t)
                  (incf i)))
            (unless j-end-reached
              (if (= (1+ j) end2)
                  (setf j-end-reached t)
                  (incf j)))
            (when (and i-end-reached
                       j-end-reached)
              (return
                (compute-lcs a b (1+ start1) (1+ start2) end1 end2)))
            (let ((d1 (aref a i))
                  (d2 (aref b j)))
              (when (or (char= d1 c2)
                        (char= d2 c1))
                (return-from compute-lcs
                             (compute-lcs a b
                                          (if (char= d1 c2)
                                              i
                                              start1)
                                          (if (char= d2 c1)
                                              j
                                              start2)
                                          end1
                                          end2)))))))))

(defun lcs (a b)
  (compute-lcs a b 0 0 (length a) (length b)))
