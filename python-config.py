# python-config.py
#
# This script tries to find the correct ldflags and cflags through python3
# introspection. See CPython's Misc/python-config.in and configure.ac for
# details.


import sys
import sysconfig
import platform
import pathlib


class MSVC:
    def __init__(self):
        self.name = "cl"
        self.cflags = []
        self.ldflags = ["-link"]

    def add_library_path(self, path):
        self.ldflags.append('-LIBPATH:"' + path + '"')

    def add_library(self, lib):
        self.ldflags.append(lib + ".lib")

    def add_libraries(self, libs):
        for lib in libs:
            self.add_library(lib)

    def add_include_path(self, path):
        self.cflags.append('-I"' + path + '"')


class GnuLikeCompiler:
    def __init__(self, name):
        self.name = name
        self.cflags = []
        self.ldflags = []

    def add_library_path(self, path):
        self.ldflags.append('-L"' + path + '"')

    def add_library(self, lib):
        self.ldflags.append('-l' + lib)

    def add_libraries(self, libs):
        for lib in libs:
            self.add_library(lib)

    def add_include_path(self, path):
        self.cflags.append('-I"' + path + '"')


getvar = sysconfig.get_config_var


def extend_with_config_var(array, name):
    var = getvar(name)
    if var is not None:
        array.extend(var.split())


def find_compiler():
    try:
        CC = getvar("CC").lower()
        if "clang" in CC:
            compiler = GnuLikeCompiler("clang")
        elif "gcc" in CC:
            compiler = GnuLikeCompiler("gcc")
        else:
            raise RuntimeError("Unknown compiler")
    except Exception:
        pycompiler = platform.python_compiler().lower()
        if "msc" in pycompiler:
            compiler = MSVC()
        elif "clang" in pycompiler:
            compiler = GnuLikeCompiler("clang")
        elif "gcc" in pycompiler:
            compiler = GnuLikeCompiler("gcc")
        else:
            raise RuntimeError("Unknown compiler")

    return compiler


# Detect the platform/system

system = platform.system().lower()
if system not in ["linux", "darwin", "windows"]:
    raise RuntimeError("Unsupported system")


# Detect the compiler

compiler = find_compiler()


# Get common Python configuration variables

VERSION = getvar("VERSION")
LIBDIR = getvar("LIBDIR")
CONFINCLUDEDIR = getvar("CONFINCLUDEDIR")
try:
    abiflags = sys.abiflags
except Exception:
    abiflags = getvar("abiflags") or ""


# Configure system specific variables

if system == "windows" and LIBDIR is None:
    # Assume libpath is %PYTHONPREFIX%\\libs on Windows
    prefix = pathlib.Path(sysconfig.get_config_var("prefix"))
    libs = prefix / "libs"
    if not libs.exists():
        raise RuntimeError("Unable to find python C libraries")
    LIBDIR = str(libs)

elif system == "darwin":
    # Set @rpath when using clang
    PYTHONFRAMEWORKPREFIX = getvar("PYTHONFRAMEWORKPREFIX")
    if PYTHONFRAMEWORKPREFIX != "":
        compiler.cflags.append("-Wl,-rpath -Wl," + PYTHONFRAMEWORKPREFIX)


# Configure ldflags

compiler.add_library_path(LIBDIR)
compiler.add_library("python" + VERSION + abiflags)
extend_with_config_var(compiler.ldflags, "LIBS")
extend_with_config_var(compiler.ldflags, "SYSLIBS")

if not getvar("Py_ENABLE_SHARED"):
    LIBPL = getvar("LIBPL")
    if LIBPL is not None:
        compiler.add_library_path(LIBPL)

if not getvar("PYTHONFRAMEWORK"):
    extend_with_config_var(compiler.ldflags, "LINKFORSHARED")


# Configure cflags

compiler.add_include_path(sysconfig.get_path("include"))
compiler.add_include_path(sysconfig.get_path("platinclude"))
if CONFINCLUDEDIR is not None:  # Can be None on Windows
    compiler.add_include_path(CONFINCLUDEDIR + "/python" + VERSION + abiflags)
extend_with_config_var(compiler.cflags, "CFLAGS")


# The output is parsed by gsc, one line at a time:

print(VERSION)
print(compiler.name)
print(" ".join(compiler.ldflags))
print(" ".join(compiler.cflags))
print(LIBDIR)
