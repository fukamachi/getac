(defpackage #:get-accepted
  (:use #:cl
        #:get-accepted/utils)
  (:import-from #:get-accepted/runner
                #:*default-commands*
                #:run-file
                #:execution-error
                #:compilation-error)
  (:import-from #:get-accepted/testcase
                #:read-from-file
                #:default-testcase-file)
  (:import-from #:get-accepted/reporter
                #:*enable-colors*
                #:report-accepted
                #:report-wrong-answer
                #:report-compilation-error
                #:report-runtime-error
                #:report-unknown
                #:print-summary)
  (:export #:*default-commands*
           #:*enable-colors*
           #:run))
(in-package #:get-accepted)

(defun run (file &key test filetype)
  (let* ((file (normalize-pathname file))
         (testcase-file (or test
                            (default-testcase-file file)))
         (passedp t))
    (loop with passed-count = 0
          for all-test-count from 0
          for (test-name input expected) in (read-from-file testcase-file)
          do (handler-case
               (multiple-value-bind (result took-ms)
                   (run-file file input :filetype filetype)
                 (cond
                   ((not (stringp result))
                    (report-unknown test-name))
                   ((equal result expected)
                    (report-accepted test-name took-ms)
                    (incf passed-count))
                   (t
                    (report-wrong-answer test-name expected result took-ms))))
               (compilation-error (e)
                                  (setf passedp nil)
                                  (report-compilation-error test-name e))
               (execution-error (e)
                                (setf passedp nil)
                                (report-runtime-error test-name e)))
          finally
            (print-summary passed-count (- all-test-count passed-count)))
    passedp))
