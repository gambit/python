# python
**ATTENTION:** This is a work in progress. _Caveat emptor_!

``` scheme
> \import calendar
> (display (\calendar.month 2022 09))
   September 2022
Mo Tu We Th Fr Sa Su
          1  2  3  4
 5  6  7  8  9 10 11
12 13 14 15 16 17 18
19 20 21 22 23 24 25
26 27 28 29 30
```

This module is an interface to Python for Gambit Scheme. It wraps and exposes
the low-level CPython C API using the Gambit Scheme C FFI managing the CPython
GIL and reference counting. It implements a Foreign Procedure Call mechanism
which bridges the Gambit and CPython threading models. It also offers a
convenient syntactic interface to write Python expressions in Scheme. This
module allows the use of packages from the Python Package Index's (PyPI)
repository of almost 400,000 packages.

For more details, see the preprint for our Scheme Workshop 2022 article here: [A
Foreign Function Interface between Gambit Scheme and
CPython](https://andykeep.com/SchemeWorkshop2022/scheme2022-final22.pdf).

## Getting started

### Requirements
This module has a few mandatory requirements:

- You must have a recent version of Gambit compiled with the
  `--enable-multiple-threaded-vms` option.
- You must have a dynamically linked version of CPython >= 3.7 installed. This
  module will link against the CPython shared library.
- A Windows, Linux or macOS operating system (other OSes have not been tested but
  might work).

### Installation
You can also install and compile the program at the command-line as such:

``` sh
gsi -install github.com/gambit/python
gsc github.com/gambit/python
```

You can also download and compile the module lazily by importing it from within
`gsi` or any program compiled with the C backend:

``` scheme
(import (github.com/gambit/python))
```


Gambit will download and compile the code. During compilation, the module will
perform an automatic discovery of the installed CPython executable. Various C
compiler options will be determined by introspection. This module will create a
`virtualenv` to manage its packages.

Users can configure the compilation with environment variables:

- `GAMBIT_PYTHON_EXECUTABLE` is the path to the CPython executable (supersedes `GAMBIT_PYTHON_VERSION`)
- `GAMBIT_PYTHON_VERSION` is the CPython version to use (e.g. `3.7`)
- `GAMBIT_PYTHON_VENV` is the directory where to put the `virtualenv`
- `GAMBIT_PYTHON_DEBUG` is a flag to show debug information (`yes`)

A default installation _should_ work out of the box. By default, the Python
`virtualenv` will be placed under `~~userlib/.venv${GAMBIT_PYTHON_VERSION}`.

## Usage

This module dynamically links to the CPython shared library. On some systems and
configurations, you _must_ preload the shared library. For example, on a Linux
system you most probably need to invoke the `gsi` binary as such:

``` sh
LD_PRELOAD=/path/to/Python-3.10.5/lib/libpython3.10.so gsi
```

To use the syntactic interface, you must import the `(_six python)` module. To
use the low-level interface and instantiate the CPython VM, you must import the
`github.com/gambit/python` module. Combined, these are:

``` scheme
(import (_six python)
        (github.com/gambit/python))
```

### Installing packages

To install packages from PyPI, simply run `(pip-install "package-name")`.
Advanced users can always load the virtual environment as they would any other,
or use other packaging mechanisms if they wish.

## Examples

The [examples](examples/) can be run as follows:

``` sh
gsi github.com/gambit/python/examples/EXAMPLE_NAME_WITHOUT_SCM
```

So to run the `requests` example, you would do:

``` sh
gsi github.com/gambit/python/examples/requests
```

Some examples require packages to be installed from the PyPI. These have to be
manually installed using `pip-install`.

## Testing

To test the module, run the following command:

``` sh
gsi github.com/gambit/python/test
```

There is a specific test for memory leaks that you can run as:

``` sh
gsi github.com/gambit/python/test/leaks
```


## License and copyright

This software is distributed under the same terms as
[Gambit](https://github.com/gambit/gambit).

&copy; Marc Feeley 2021-2022

&copy; Marc-André Bélanger 2021-2022
