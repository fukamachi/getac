# getac

Quick unit testing CLI tool for competitive programming.

![Screenshot](images/screencast.gif)

## Usage

```
$ getac -h
Usage: getac [options] <file>

OPTIONS:
    -t, --test=<file>
        Specify a file to read test cases. (Default: <file>.txt)
    -f, --filetype=<type>
        File type to test. The default will be detected by the file extension.
    -T, --timeout=<seconds>
        Time limit for each test cases. (Default: 2)
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

## Getting started

1. Write your code in a file (ex. `main.py`)

```python
a = int(input())
b, c = map(int, input().split())
s = input()
print(a + b + c, s)
```

2. Write your test cases in "*.txt" (ex. `main.txt`)

```
==== example1 ====
1
2 3
test
--------------
6 test
==== example2 ====
2
3 4
myonmyon
-------
9 myonmyon
```

See [Format of test cases](#format-of-test-cases) for getting its syntax.

3. Run `getac main.py`

![Screenshot of successed tests](images/screenshot-1.png)

If some test cases are failed, it shows 'WA' (wrong answer).

![Screenshot of failed tests](images/screenshot-2.png)

## Format of test cases

Let's start with this very minimal example which contains a single test case:

```
Input texts are here...
------------------
Expected result is here...
```

The input text and its expected results are separated with `----` (more than 4 hyphens).

When you include multiple test cases in the same file, those have to be divided with `====` (more than 4 equal signs).

```
Input texts are here...
------------------
Expected result is here...
==================
Second test case
------------------
Expected result is here again...
```

The name of each test cases can be written in the middle of separators, like `==== example1 ====`.

Here's the full example:

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
* Scheme

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2019 Eitaro Fukamachi (e.arrows@gmail.com)

## License

See [LICENSE](LICENSE).
