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
                #:report-unknown
                #:print-summary)
  (:export #:*enable-colors*
           #:run))
(in-package #:getac)

(defun run (file &key test filetype)
  (let* ((file (normalize-pathname file))
         (filetype (or filetype
                       (detect-filetype file)))
         (testcase-file (or test
                            (default-testcase-file file)))
         (passedp t))
    (let* ((compile-to (handler-case (compile-main-file file filetype)
                         (compilation-error (e)
                           (report-compilation-error e)
                           (return-from run nil))))
           (handler (make-execution-handler file filetype :compile-to compile-to)))
      (loop with passed-count = 0
            for all-test-count from 0
            for (test-name input expected) in (read-from-file testcase-file)
            do (handler-case
                   (multiple-value-bind (result took-ms)
                       (funcall handler input)
                     (cond
                       ((not (stringp result))
                        (report-unknown test-name))
                       ((equal result expected)
                        (report-accepted test-name took-ms)
                        (incf passed-count))
                       (t
                        (report-wrong-answer test-name expected result took-ms))))
                 (execution-error (e)
                   (setf passedp nil)
                   (report-runtime-error test-name e)))
            finally
            (print-summary passed-count (- all-test-count passed-count))))
    passedp))
