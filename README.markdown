# getac

Unit testing CLI tool for competitive programming.

![Screenshot](images/screencast.gif)

## Supported languages

* C
* C++
* Clojure
* Common Lisp
* Go
* Java
* Node.js
* Python
* Ruby

## Usage

```
$ getac -h
Usage: getac [options] <file>

OPTIONS:
    -t, --test=<file>
        Specify a file to read test cases. (Default: '*.in')
    -f, --filetype=<type>
        File type to test. The default will be detected by the file extension.
    --disable-colors
        Turn off colors.
    -F, --no-fail-fast
        Don't quit when a failure.
    -V, --version
        Print version.
    -h, --help
        Show help.

# Run with the default Python
$ getac main.py

# Specify test cases
$ getac -t test-cases.in main.py

# Run with PyPy3
$ getac -f pypy3 main.py
```

## Getting started

1. Write your code in a file (ex. `main.py`)
2. Write your test cases in "*.in" (ex. `main.in`)
3. Run `getac main.py`

## Usage

### Format of test cases

```
==== example1 ====
Input texts are here...
------------------
Expected result is here...
==== example2 ====
Second test case
------------------
Expected result is here again...
```

## Installation

Install [Roswell](https://github.com/roswell/roswell) if not already installed.

```
$ ros install fukamachi/getac
```

After the installation, `getac` command will be installed at `~/.roswell/bin`.

## Upgrade

```
$ ros update getac
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2019 Eitaro Fukamachi (e.arrows@gmail.com)

## License

See [LICENSE](LICENSE).
