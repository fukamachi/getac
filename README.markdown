# get-accepted

Unit testing CLI tool for competitive programming.

![Screenshot](images/screenshot.png)

## Supported languages

* Common Lisp
* Python
* Go
* Ruby
* Java

## Usage

```
$ get-accepted -h
Usage: get-accepted [options] [file]

OPTIONS:
    -t, --test [file]
        Specify a file to read test cases. (Default: '*.in')
    -f, --filetype [type]
        File type to test. The default will be detected by the file extension.
    --disable-colors
        Turn off colors. (Default: false)
    --h, --help
        Show help.
```

## Getting started

1. Write your code in a file like "main.lisp"
2. Write your test cases in "*.in" like "main.in"
3. Run `get-accepted main.lisp`

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
$ ros install fukamachi/get-accepted
```

After the installation, `get-accepted` command will be installed at `~/.roswell/bin`.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2019 Eitaro Fukamachi (e.arrows@gmail.com)

## License

See [LICENSE](LICENSE).
