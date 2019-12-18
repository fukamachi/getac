(defpackage #:getac
  (:use #:cl
        #:getac/utils)
  (:import-from #:getac/runner
                #:detect-filetype
                #:compile-main-file
                #:make-execution-handler
                #:execution-error
                #:compilation-error)
  (:import-from #:getac/testcase
                #:read-from-file
                #:default-testcase-file)
  (:import-from #:getac/reporter
                #:*enable-colors*
                #:report-accepted
                #:report-wrong-answer
                #:report-compilation-error
                #:report-runtime-error
                #:report-time-limit-exceeded
                #:print-summary)
  (:import-from #:getac/timer
                #:*max-execution-time*
                #:deadline-timeout
                #:deadline-timeout-seconds)
  (:export #:*enable-colors*
           #:*max-execution-time*
           #:run))
(in-package #:getac)

(defun run (file &key test filetype (fail-fast t))
  (let* ((file (normalize-pathname file))
         (filetype (or filetype
                       (detect-filetype file)))
         (testcase-file (or test
                            (default-testcase-file file))))
    (let* ((compile-to (handler-case (compile-main-file file filetype)
                         (compilation-error (e)
                           (report-compilation-error e)
                           (return-from run nil))))
           (handler (make-execution-handler file filetype :compile-to compile-to))
           (test-cases (read-from-file testcase-file))
           (all-test-count (length test-cases))
           (passed-count 0)
           (failed-count 0))
      (format t "~&Running ~A test cases...~2%" all-test-count)
      (loop for (test-name input expected) in test-cases
            do (handler-case
                   (multiple-value-bind (result took-ms)
                       (funcall handler input)
                     (check-type result string)
                     (if (equal result expected)
                         (progn
                           (incf passed-count)
                           (report-accepted test-name took-ms))
                         (progn
                           (report-wrong-answer test-name input expected result took-ms)
                           (incf failed-count)
                           (when fail-fast
                             (return))
                           (write-char #\Newline))))
                 (execution-error (e)
                   (report-runtime-error test-name input e)
                   (incf failed-count)
                   (when fail-fast
                     (return)))
                 (deadline-timeout (e)
                   (report-time-limit-exceeded test-name input (deadline-timeout-seconds e))
                   (incf failed-count)
                   (when fail-fast
                     (return)))))
      (print-summary passed-count failed-count (- all-test-count
                                                  passed-count
                                                  failed-count))
      (= failed-count 0))))
