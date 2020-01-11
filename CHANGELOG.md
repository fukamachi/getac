# getac Changelog

## To be released, v0.9.2

* Allow to read test cases from a directory containing separated input/output files.
* bugfix: Fix to print standard error while execution. (reported by @nganhkhoa at [#5](https://github.com/fukamachi/getac/issues/5))

## 2019-12-31, v0.9.1

* Add Haskell support (thanks to @nganhkhoa)
* Include bookmarklet.js to generate testcases (Experimental)
* Print also the versions of Lisp and ASDF by 'getac --version'
* bugfix: Ensure compilation/execution processes are terminated even when TLE.
* bugfix: Fix a TYPE-ERROR if the testcase ends with a newline.

## 2019-12-20, v0.9.0

* First major release
