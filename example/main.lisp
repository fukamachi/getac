;; Code for https://atcoder.jp/contests/abs/tasks/practice_1

(defun resolve ()
  (let ((a (read))
        (b (read))
        (c (read))
        (s (read-line)))
    (format t "~D ~A~%" (+ a b c) s)))

(resolve)
