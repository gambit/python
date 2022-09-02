;;; on macOS compile with:
;;;
;;; gsc/gsc -:= -exe -cc-options -I/usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/Headers -ld-options /usr/local/opt/python@3.10/Frameworks/Python.framework/Versions/3.10/lib/libpython3.10.dylib python-fpc-proto1.scm

;;; The first part of this file is copied from lib/_ffi/python/python.scm
;;; with modifications for error handling and GIL acquire/release.




;;;============================================================================

;;; File: "python.scm"

;;; Copyright (c) 2020-2022 by Marc Feeley, All Rights Reserved.
;;; Copyright (c) 2020-2022 by Marc-André Bélanger, All Rights Reserved.

;;;============================================================================

;;; Python FFI.

;; (##supply-module _ffi/python)

;; (##namespace ("_ffi/python#"))              ;; in _ffi/python#

;; (##include "~~lib/gambit/prim/prim#.scm")   ;; map fx+ to ##fx+, etc
(##include "~~lib/_gambit#.scm")            ;; for macro-check-procedure,
;;                                             ;; macro-absent-obj, etc
;; (##include "~~lib/gambit#.scm")             ;; shell-command

;; (##include "python#.scm")                    ;; correctly map pyffi ops

(##declare (extended-bindings) (standard-bindings) (block)) ;; ##fx+ is bound to fixnum addition, etc
(declare (not safe))          ;; claim code has no type errors
;;(declare (block))             ;; claim no global is assigned

;;;----------------------------------------------------------------------------


;; Generate meta information to link to Python libs.
(define-syntax gen-meta-info
  (lambda (src)
    (define (string-strip-trailing-return! str)
      (if (string? str)
          (let ((newlen (- (string-length str) 1)))
            (if (char=? #\return (string-ref str newlen))
                (string-shrink! str newlen))))
      str)

    (define os (substring (symbol->string (caddr (system-type))) 0 3))
    (define default-venv-path #f)
    (define venv-path #f)
    ;; Determine CPython interpreter to use.
    (define version (getenv "GAMBIT_PYTHON_VERSION" "3.10"))
    (define python (string-append "python" version))
    ;; Its alias in the virtualenv
    (define python3 #f)

    ;; Check if the CPython executable exists, or early exit if it does not
    (and (not (= 0 (car (shell-command (string-append "command -v " python) #t))))
         (println "CPython version " version " not found, will not compile the FFI.")
         (exit 0))

    ;; Only compile on macOS and Linux for now.
    (cond
     ((or (equal? "dar" os) (equal? "lin" os))
      (set! default-venv-path "~/.gambit_venv")
      (set! venv-path (getenv "GAMBIT_VENV" default-venv-path))
      (set! python3 (string-append venv-path "/bin/python3")))
     ;; TODO: Windows, exit gracefully
     (else (exit 0)))

    ;; NOTE: Try to create the venv first to avoid differing venv and env python version
    ;; issues on macOS. We take the current env python3 and run from there.
    (shell-command (string-append python " -m venv " venv-path))

    ;; NOTE: Only after putting everything in a venv do we proceed with introspection
    (let ((sh
           (parameterize ((current-directory
                           (path-directory (##source-path src))))
             (shell-command (string-append python3 " " (current-directory) "/python-config.py") #t))))

      (and (not (= 0 (car sh)))
           (println "Error executing python3-config.py, will not compile the CPython FFI." sh)
           (exit 0))

      (let* ((res
              (call-with-input-string (cdr sh)
                (lambda (port)
                  (read-all port (lambda (p) (string-strip-trailing-return! (read-line p)))))))
             (pyver   (list-ref res 0))
             (pycc    (list-ref res 1))
             (ldflags (list-ref res 2))
             (cflags  (list-ref res 3))
             (libdir  (list-ref res 4)))

        ;; Get proper PYTHONPATH from venv bin
        (let* ((s (cdr
                   (shell-command
                    (string-append python3 " -c 'import sys; print(\"\"+\":\".join(sys.path))'") #t)))
               (pythonpath (substring s 0 (- (string-length s) 2))))

          `(begin
             (define PYVER ,pyver)
             (define LIBDIR ,libdir)
             (define PYTHONPATH ,pythonpath)
             (define VENV-PATH ,venv-path)
             (##meta-info ld-options ,ldflags)
             (##meta-info cc-options ,cflags)))))))

(gen-meta-info)

;;;----------------------------------------------------------------------------

;; Get Python C API.

(c-declare #<<end-of-c-declare

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef PyObject *PyObjectPtr;

PyTypeObject *Fraction_cls = NULL;
PyTypeObject *_SchemeObject_cls = NULL;

#define DEBUG_LOWLEVEL_
#define DEBUG_PYTHON_REFCNT_

#if 1 || defined(DEBUG_PYTHON_REFCNT)

// Taken from https://stackoverflow.com/a/46202119
static void debug_print_repr(PyObject *obj) {

  PyObject* repr = PyObject_Repr(obj);
  PyObject* str = PyUnicode_AsEncodedString(repr, "utf-8", "~E~");
  const char *bytes = PyBytes_AS_STRING(str);

  printf("REPR: %s\n", bytes);
  fflush(stdout);

  Py_XDECREF(repr);
  Py_XDECREF(str);
}

#endif

#ifdef DEBUG_PYTHON_REFCNT

#define PYOBJECTPTR_INCREF(obj, where) \
do { \
  Py_INCREF(obj); \
  printf(where " REFCNT(%p)=%ld after INCREF\n", obj, Py_REFCNT(obj)); \
  fflush(stdout); \
} while (0)

#define PYOBJECTPTR_DECREF(obj, where) \
do { \
  printf(where " REFCNT(%p)=%ld before DECREF\n", obj, Py_REFCNT(obj)); \
  if (Py_REFCNT(obj) == 1) { \
    printf("##### WILL FREE "); \
    debug_print_repr(obj); \
  } \
  fflush(stdout); \
  Py_DECREF(obj); \
} while (0)

#define PYOBJECTPTR_REFCNT_SHOW(obj, where) \
do { \
  if (obj != NULL) { \
    printf(where " REFCNT(%p)=%ld\n", obj, Py_REFCNT(obj)); \
    fflush(stdout); \
  } \
} while (0)

#else

#define PYOBJECTPTR_INCREF(obj, where) Py_INCREF(obj)
#define PYOBJECTPTR_DECREF(obj, where) Py_DECREF(obj)
#define PYOBJECTPTR_REFCNT_SHOW(obj, where)

#endif

#define GIL_ACQUIRE() PyGILState_STATE ___gilstate = PyGILState_Ensure()
#define GIL_RELEASE() PyGILState_Release(___gilstate)

___SCMOBJ release_PyObjectPtr(void *obj) {

  if (Py_IsInitialized()) { // Avoid mem management after Python is shutdown
    GIL_ACQUIRE();
    PYOBJECTPTR_DECREF(___CAST(PyObjectPtr, obj), "release_PyObjectPtr");
    GIL_RELEASE();
  }

  return ___FIX(___NO_ERR);
}

end-of-c-declare
)

;;;----------------------------------------------------------------------------

;; Define PyObject* foreign type.

(c-define-type PyObject "PyObject")

(c-define-type _PyObject*
               (nonnull-pointer
                PyObject
                (PyObject*
                 PyObject*/None
                 PyObject*/bool
                 PyObject*/int
                 PyObject*/float
                 PyObject*/complex
                 PyObject*/Fraction
                 PyObject*/bytes
                 PyObject*/bytearray
                 PyObject*/str
                 PyObject*/list
                 PyObject*/dict
                 PyObject*/frozenset
                 PyObject*/set
                 PyObject*/tuple
                 PyObject*/module
                 PyObject*/type
                 PyObject*/function
                 PyObject*/builtin_function_or_method
                 PyObject*/method
                 PyObject*/method_descriptor
                 PyObject*/cell
                 PyObject*/SchemeObject
                 )))

(c-define-type PyObject*
               "void*"
               "PYOBJECTPTR_to_SCMOBJ"
               "SCMOBJ_to_PYOBJECTPTR"
               #t)

(c-define-type PyObject*!own
               "void*"
               "PYOBJECTPTR_OWN_to_SCMOBJ"
               "SCMOBJ_to_PYOBJECTPTR_OWN"
               #t)

;;;----------------------------------------------------------------------------

;; Define PyObject* subtypes.

(define-macro (define-python-subtype-type subtype)
  (define type (string-append "PyObjectPtr_" subtype))
  (define _name (string->symbol (string-append "_PyObject*/" subtype)))
  (define name (string->symbol (string-append "PyObject*/" subtype)))
  (define name-own (string->symbol (string-append "PyObject*!own/" subtype)))
  (define TYPE (string-append "PYOBJECTPTR_" (string-upcase subtype)))
  (define TYPE-OWN (string-append "PYOBJECTPTR_OWN_" (string-upcase subtype)))
  (define to-scmobj (string-append TYPE "_to_SCMOBJ"))
  (define from-scmobj (string-append "SCMOBJ_to_" TYPE))
  (define to-scmobj-own (string-append TYPE-OWN "_to_SCMOBJ"))
  (define from-scmobj-own (string-append "SCMOBJ_to_" TYPE-OWN))
  `(begin
     (c-declare ,(string-append "typedef PyObjectPtr " type ";"))
     (c-define-type ,_name (nonnull-pointer PyObject ,name))
     (c-define-type ,name "void*" ,to-scmobj ,from-scmobj #t)
     (c-define-type ,name-own "void*" ,to-scmobj-own ,from-scmobj-own #t)))

(define-python-subtype-type "None")
(define-python-subtype-type "bool")
(define-python-subtype-type "int")
(define-python-subtype-type "float")
(define-python-subtype-type "complex")
(define-python-subtype-type "Fraction")
(define-python-subtype-type "bytes")
(define-python-subtype-type "bytearray")
(define-python-subtype-type "str")
(define-python-subtype-type "list")
(define-python-subtype-type "dict")
(define-python-subtype-type "frozenset")
(define-python-subtype-type "set")
(define-python-subtype-type "tuple")
(define-python-subtype-type "module")
(define-python-subtype-type "type")
(define-python-subtype-type "function")
(define-python-subtype-type "builtin_function_or_method")
(define-python-subtype-type "method")
(define-python-subtype-type "method_descriptor")
(define-python-subtype-type "cell")
(define-python-subtype-type "SchemeObject")

;;;----------------------------------------------------------------------------

;; Define PyTypeObject* foreign type.

;; NOTE: Not sure yet if we want to use raw PyTypeObjects.

(c-define-type PyTypeObject "PyTypeObject")

(c-define-type PyTypeObject*
               (nonnull-pointer PyTypeObject (PyTypeObject*)))

;;;----------------------------------------------------------------------------

;; Generator of converter macros.

(define-macro (define-converter-macros _SUBTYPE _OWN release)
  `(c-declare ,(string-append "

#define ___BEGIN_CFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst,i) \
  if ((___err = SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst,i) " release "}

#define ___BEGIN_CFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst) \
  if ((___err = PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst,i) \
  if ((___err = PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst,i) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst) \
  if ((___err = SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst) " release "}
")))

;;;----------------------------------------------------------------------------

;; Converter for Python* type that detects the subtype.

(c-declare #<<end-of-c-declare

___SCMOBJ PYOBJECTPTR_to_SCMOBJ(PyObjectPtr src, ___SCMOBJ *dst, int arg_num) {

  ___SCMOBJ tag;

  if (src == NULL)
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);

#ifdef ___C_TAG_PyObject_2a__2f_None
  if (src == Py_None)
    tag = ___C_TAG_PyObject_2a__2f_None;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bool
  if (src == Py_False || src == Py_True)
    tag = ___C_TAG_PyObject_2a__2f_bool;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_int
  if (PyLong_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_int;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_float
  if (PyFloat_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_float;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_complex
  if (PyComplex_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_complex;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_Fraction
  if (Py_TYPE(src) == Fraction_cls)
    tag = ___C_TAG_PyObject_2a__2f_Fraction;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytes
  if (PyBytes_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_bytes;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytearray
  if (PyByteArray_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_bytearray;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_str
  if (PyUnicode_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_str;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_list
  if (PyList_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_list;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_dict
  if (PyDict_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_dict;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_frozenset
  if (PyFrozenSet_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_frozenset;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_set
  if (PyAnySet_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_set;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_tuple
  if (PyTuple_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_tuple;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_module
  if (PyModule_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_module;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_type
  if (PyType_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_type;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_function
  if (PyFunction_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_function;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_builtin__function__or__method
  if (!strcmp(src->ob_type->tp_name, "builtin_function_or_method"))
    tag = ___C_TAG_PyObject_2a__2f_builtin__function__or__method;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_method
  if (PyMethod_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_method;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_method__descriptor
  if (!strcmp(src->ob_type->tp_name, "method_descriptor"))
    tag = ___C_TAG_PyObject_2a__2f_method__descriptor;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_cell
  if (PyCell_Check(src))
    tag = ___C_TAG_PyObject_2a__2f_cell;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_SchemeObject
  if (Py_TYPE(src) == _SchemeObject_cls)
    tag = ___C_TAG_PyObject_2a__2f_SchemeObject;
  else
#endif

  tag = ___C_TAG_PyObject_2a_;

  PYOBJECTPTR_REFCNT_SHOW(src, "PYOBJECTPTR_to_SCMOBJ");

  return ___EXT(___NONNULLPOINTER_to_SCMOBJ)(___PSTATE,
                                             src,
                                             tag,
                                             release_PyObjectPtr,
                                             dst,
                                             arg_num);
}

___SCMOBJ PYOBJECTPTR_OWN_to_SCMOBJ(PyObjectPtr src, ___SCMOBJ *dst, int arg_num) {
  if (src == NULL)
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);
  PYOBJECTPTR_INCREF(src, "PYOBJECTPTR_OWN_to_SCMOBJ");
  return PYOBJECTPTR_to_SCMOBJ(src, dst, arg_num);
}

___SCMOBJ SCMOBJ_to_PYOBJECTPTR(___SCMOBJ src, void **dst, int arg_num) {

  ___processor_state ___ps = ___PSTATE;

#define CONVERT_TO_NONNULLPOINTER(tag) \
  ___EXT(___SCMOBJ_to_NONNULLPOINTER)(___PSP src, dst, tag, arg_num)

#define TRY_CONVERT_TO_NONNULLPOINTER(tag) \
  if (CONVERT_TO_NONNULLPOINTER(tag) == ___FIX(___NO_ERR)) \
    return ___FIX(___NO_ERR)

#ifdef ___C_TAG_PyObject_2a__2f_None
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_None);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bool
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_bool);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_int
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_int);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_float
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_float);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_complex
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_complex);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_Fraction
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_Fraction);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytes
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_bytes);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytearray
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_bytearray);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_str
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_str);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_list
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_list);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_dict
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_dict);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_frozenset
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_frozenset);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_set
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_set);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_tuple
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_tuple);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_module
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_module);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_type
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_type);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_function
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_function);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_builtin__function__or__method
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_builtin__function__or__method);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_method
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_method);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_method__descriptor
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_method__descriptor);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_cell
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_cell);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_SchemeObject
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_SchemeObject);
#endif

  return CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a_);
}

end-of-c-declare
)

(define-converter-macros "" "" "")
(define-converter-macros "" "_OWN" "___EXT(___release_foreign) (src); ")

;;;----------------------------------------------------------------------------

;; Converters for Python* subtypes.

(define-macro (define-subtype-converters subtype check)
  (define _SUBTYPE (string-append "_" (string-upcase subtype)))
  (define tag (string-append "___C_TAG_PyObject_2a__2f_" subtype))
  `(begin
     (c-declare
       ,(string-append "

#ifdef " tag "

___SCMOBJ PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(PyObjectPtr_" subtype " src, ___SCMOBJ *dst, int arg_num) {

  if (src == NULL || !(" check "))
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);

  PYOBJECTPTR_REFCNT_SHOW(src, \"PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ\");

  return ___EXT(___NONNULLPOINTER_to_SCMOBJ)(___PSTATE,
                                             src,
                                             " tag ",
                                             release_PyObjectPtr,
                                             dst,
                                             arg_num);
}

___SCMOBJ PYOBJECTPTR_OWN" _SUBTYPE "_to_SCMOBJ(PyObjectPtr_" subtype " src, ___SCMOBJ *dst, int arg_num) {

  if (src == NULL || !(" check "))
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);

  PYOBJECTPTR_INCREF(src, \"PYOBJECTPTR_OWN" _SUBTYPE "_to_SCMOBJ\");

  return ___EXT(___NONNULLPOINTER_to_SCMOBJ)(___PSTATE,
                                             src,
                                             " tag ",
                                             release_PyObjectPtr,
                                             dst,
                                             arg_num);
}

___SCMOBJ SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(___SCMOBJ src, void **dst, int arg_num) {

  return ___EXT(___SCMOBJ_to_NONNULLPOINTER)(___PSA(___PSTATE)
                                             src,
                                             dst,
                                             " tag ",
                                             arg_num);
}

#endif

"))
     (define-converter-macros ,_SUBTYPE "" "")
     (define-converter-macros ,_SUBTYPE "_OWN" "___EXT(___release_foreign) (src); ")))

(define-subtype-converters "None"      "src == Py_None")
(define-subtype-converters "bool"      "src == Py_False || src == Py_True")
(define-subtype-converters "int"       "PyLong_Check(src)")
(define-subtype-converters "float"     "PyFloat_Check(src)")
(define-subtype-converters "complex"   "PyComplex_Check(src)")
(define-subtype-converters "Fraction"  "Py_TYPE(src) == Fraction_cls")
(define-subtype-converters "bytes"     "PyBytes_Check(src)")
(define-subtype-converters "bytearray" "PyByteArray_Check(src)")
(define-subtype-converters "str"       "PyUnicode_Check(src)")
(define-subtype-converters "list"      "PyList_Check(src)")
(define-subtype-converters "dict"      "PyDict_Check(src)")
(define-subtype-converters "frozenset" "PyFrozenSet_Check(src)")
(define-subtype-converters "set"       "PyAnySet_Check(src) && !PyFrozenSet_Check(src)")
(define-subtype-converters "tuple"     "PyTuple_Check(src)")
(define-subtype-converters "module"    "PyModule_Check(src)")
(define-subtype-converters "type"      "PyType_Check(src)")
(define-subtype-converters "function"  "PyFunction_Check(src)")
(define-subtype-converters "builtin_function_or_method"  "!strcmp(src->ob_type->tp_name, \"builtin_function_or_method\")")
(define-subtype-converters "method"    "PyMethod_Check(src)")
(define-subtype-converters "method_descriptor"  "!strcmp(src->ob_type->tp_name, \"method_descriptor\")")
(define-subtype-converters "cell"      "PyCell_Check(src)")
(define-subtype-converters "SchemeObject" "Py_TYPE(src) == _SchemeObject_cls")

;;;----------------------------------------------------------------------------

(c-declare #<<end-of-c-declare


___SCMOBJ python_error_handler;

void set_err(___SCMOBJ *err, ___SCMOBJ *errdata, ___SCMOBJ *errhandler) {

  ___SCMOBJ e;
  ___SCMOBJ val_scmobj;
  ___SCMOBJ tb_scmobj;
  PyObjectPtr type;
  PyObjectPtr val;
  PyObjectPtr tb;

  PyErr_Fetch(&type, &val, &tb);

  // Handle a NULL traceback object possibly returned by PyErr_Fetch
  if (tb == NULL) {
    tb = Py_None;
  }

#ifdef DEBUG_PYTHON_REFCNT
  debug_print_repr(type);
  debug_print_repr(val);
  debug_print_repr(tb);
#endif

  PYOBJECTPTR_INCREF(type, "set_err");
  PYOBJECTPTR_INCREF(val, "set_err");
  PYOBJECTPTR_INCREF(tb, "set_err");

  PyErr_NormalizeException(&type, &val, &tb);

  if ((e = PYOBJECTPTR_to_SCMOBJ(val, &val_scmobj, ___RETURN_POS))
      == ___FIX(___NO_ERR)) {
    if ((e = PYOBJECTPTR_to_SCMOBJ(tb, &tb_scmobj, ___RETURN_POS))
        != ___FIX(___NO_ERR)) {
      ___EXT(___release_scmobj) (val_scmobj);
    } else {
      *errdata = ___EXT(___make_pair) (___PSTATE, val_scmobj, tb_scmobj);
      ___EXT(___release_scmobj) (val_scmobj);
      ___EXT(___release_scmobj) (tb_scmobj);
      if (___FIXNUMP(*errdata)) {
        e = *errdata;
      }
    }
  }

  PYOBJECTPTR_DECREF(type, "set_err");

  if (e != ___FIX(___NO_ERR)) {
    PYOBJECTPTR_DECREF(val, "set_err");
    PYOBJECTPTR_DECREF(tb, "set_err");
  }

  *err = e;
  *errhandler = python_error_handler;
}

PyObjectPtr check_PyObjectPtr(PyObjectPtr result, ___SCMOBJ *err, ___SCMOBJ *errdata, ___SCMOBJ *errhandler) {
  if (result == NULL) set_err(err, errdata, errhandler);
  return result;
}

int check_int(int result, ___SCMOBJ *err, ___SCMOBJ *errdata, ___SCMOBJ *errhandler) {
  /*TODO*/
  return result;
}

ssize_t check_ssize_t(ssize_t result, ___SCMOBJ *err, ___SCMOBJ *errdata, ___SCMOBJ *errhandler) {
  /*TODO*/
  return result;
}

___SCMOBJ check_scheme_object(___SCMOBJ result, ___SCMOBJ *err, ___SCMOBJ *errdata, ___SCMOBJ *errhandler) {
  /*TODO*/
  return result;
}

#define return_with_check_PyObjectPtr(call) \
do { \
  PyObjectPtr result; \
  GIL_ACQUIRE(); \
  result = check_PyObjectPtr(call, &___err, &___errdata, &___errhandler); \
  GIL_RELEASE(); \
  ___return(result); \
} while (0)

#define return_with_check_int(call) \
do { \
  int result; \
  GIL_ACQUIRE(); \
  result = check_int(call, &___err, &___errdata, &___errhandler); \
  GIL_RELEASE(); \
  ___return(result); \
} while (0)

#define return_with_check_ssize__t(call) \
do { \
  ssize_t result; \
  GIL_ACQUIRE(); \
  result = check_ssize_t(call, &___err, &___errdata, &___errhandler); \
  GIL_RELEASE(); \
  ___return(result); \
} while (0)

#define return_with_check_void(call) \
do { \
  GIL_ACQUIRE(); \
  call; \
  GIL_RELEASE(); \
  ___return; \
} while (0)

#define return_with_check_scheme_2d_object(call) \
do { \
  ___SCMOBJ result; \
  GIL_ACQUIRE(); \
  result = check_scheme_object(call, &___err, &___errdata, &___errhandler); \
  GIL_RELEASE(); \
  ___return(result); \
} while (0)

end-of-c-declare
)

(##c-code "python_error_handler = ___ARG1;" python-error-handler)

(define-type python-exception
  id: A9EC1C11-A6D8-4357-99E6-655B75ADC09E
  type-exhibitor: python-exception-type
  data
  proc
  args)

(##structure-display-exception-handler-register!
 (##type-id (python-exception-type))
 (lambda (exc port)
   (if port
       (let* ((val-tb (python-exception-data exc))
              (val (car val-tb))
              (tb (cdr val-tb)))
         (display (string-append "Python raised "
                                 (PyObject*/str->string
                                  (PyObject_Repr val))
                                 "\n"
                                 (PyObject*/str->string
                                  (PyObject_Repr tb))
                                 "\n"
                                 )
                  port)
         )
       (cons (python-exception-proc exc)
             (python-exception-args exc)))))

(define (python-error-handler code data proc . args)
  (raise (make-python-exception data proc args)))

;;;----------------------------------------------------------------------------

;; Interface to Python API.

(define-macro (def-api name result-type arg-types)
  (let* ((result-type-str
          (symbol->string result-type))
         (base-result-type-str
          (if (eqv? 0 (##string-contains result-type-str "PyObject*"))
              "PyObjectPtr"
              result-type-str)))
    `(define ,name
       (c-lambda ,arg-types
                 ,result-type
         ,(string-append "return_with_check_"
                         (##string->c-id base-result-type-str)
                         "("
                         (symbol->string name)
                         "("
                         (string-concatenate
                          (map (lambda (i)
                                 (string-append "___arg" (number->string i)))
                               (iota (length arg-types) 1))
                          ",")
                         "));")))))

(define Py_eval_input   ((c-lambda () int "___return(Py_eval_input);")))
(define Py_file_input   ((c-lambda () int "___return(Py_file_input);")))
(define Py_single_input ((c-lambda () int "___return(Py_single_input);")))

(def-api Py_Initialize            void             ())
(def-api Py_Finalize              void             ())

(def-api PyBool_FromLong          PyObject*/bool   (long))

(def-api PyLong_FromUnicodeObject PyObject*/int    (PyObject*/str int))

(def-api PyUnicode_FromString     PyObject*/str    (nonnull-UTF-8-string))

(def-api PyRun_SimpleString       int              (nonnull-UTF-8-string))

(def-api PyRun_String             PyObject*        (nonnull-UTF-8-string
                                                    int
                                                    PyObject*/dict
                                                    PyObject*/dict))

(def-api PyImport_AddModuleObject PyObject*/module (PyObject*/str))
(def-api PyImport_AddModule       PyObject*/module (nonnull-UTF-8-string))
(def-api PyImport_ImportModule    PyObject*/module (nonnull-UTF-8-string))
(def-api PyImport_ImportModuleEx  PyObject*/module (nonnull-UTF-8-string
                                                    PyObject*/dict
                                                    PyObject*/dict
                                                    PyObject*/list))

(def-api PyModule_GetDict         PyObject*/dict   (PyObject*/module))

(def-api PyDict_New               PyObject*/dict   ())
(def-api PyDict_Size              ssize_t          (PyObject*/dict))
(def-api PyDict_Items             PyObject*/list   (PyObject*/dict))
(def-api PyDict_Keys              PyObject*/list   (PyObject*/dict))
(def-api PyDict_Values            PyObject*/list   (PyObject*/dict))
(def-api PyDict_GetItem           PyObject*        (PyObject*/dict
                                                    PyObject*))
(def-api PyDict_SetItem           int              (PyObject*/dict
                                                    PyObject*
                                                    PyObject*))
(def-api PyDict_GetItemString     PyObject*        (PyObject*/dict
                                                    nonnull-UTF-8-string))
(def-api PyDict_SetItemString     int              (PyObject*/dict
                                                    nonnull-UTF-8-string
                                                    PyObject*))

(def-api PyCell_New               PyObject*/cell   (PyObject*))
(def-api PyCell_Get               PyObject*        (PyObject*/cell))
(def-api PyCell_Set               int              (PyObject*/cell
                                                    PyObject*))

(def-api PyList_New               PyObject*/list   (int))

(def-api PyTuple_GetItem          PyObject*        (PyObject*/tuple
                                                    ssize_t))

(def-api PyObject_CallObject      PyObject*        (PyObject*
                                                    PyObject*/tuple))
(def-api PyObject_CallMethod      PyObject*        (PyObject*
                                                    nonnull-UTF-8-string
                                                    nonnull-UTF-8-string))

(def-api PyObject_GetAttrString   PyObject*        (PyObject*
                                                    nonnull-UTF-8-string))
(def-api PyObject_HasAttrString   int              (PyObject*
                                                    nonnull-UTF-8-string))

(def-api PyObject_Length          ssize_t          (PyObject*))

(def-api PyObject_Repr            PyObject*/str    (PyObject*))

(def-api Py_SetPath               void             (nonnull-wchar_t-string))
(def-api Py_SetProgramName        void             (nonnull-wchar_t-string))
(def-api PySys_SetArgv            void             (int nonnull-wchar_t-string-list))
(def-api PySys_SetArgvEx          void             (int nonnull-wchar_t-string-list int))
(def-api Py_SetPythonHome         void             (nonnull-wchar_t-string))

(def-api PyCallable_Check         int              (PyObject*))

;; NOTE: Maybe migrate to `def-api'
(c-define-type PyThreadState "PyThreadState")
(c-define-type PyThreadState* (nonnull-pointer PyThreadState))
(define Py_NewInterpreter
  (c-lambda () PyThreadState* "Py_NewInterpreter"))

;; Get object type from struct field, no new reference.
(define PyObject*-type
  (c-lambda (_PyObject*) PyTypeObject*
    "___return(___arg1->ob_type);"))

(define PyObject*-type-name
  (c-lambda (_PyObject*) nonnull-UTF-8-string
    "___return(___CAST(char*,___arg1->ob_type->tp_name));"))

;; Use for debugging
(define _Py_REFCNT
  (c-lambda (PyObject*) ssize_t
    "___return(Py_REFCNT(___arg1));"))

;;;----------------------------------------------------------------------------

;; Converters between Scheme and subtypes of Python* foreign objects.

;; TODO: check for errors and implement conversion of other subtypes...

(define PyObject*/None->void
  (c-lambda (PyObject*/None) scheme-object "

___return(___VOID);

"))

(define void->PyObject*/None
  (c-lambda (scheme-object) PyObject*/None "

___SCMOBJ src = ___arg1;
PyObjectPtr dst = NULL;

GIL_ACQUIRE();

if (___EQP(src, ___VOID)) {
  dst = Py_None;
  PYOBJECTPTR_INCREF(dst, \"void->PyObject*/None\");
}

GIL_RELEASE();

___return(dst);

"))

(define PyObject*/bool->boolean
  (c-lambda (PyObject*/bool) scheme-object "

___SCMOBJ dst;

GIL_ACQUIRE();

dst = ___BOOLEAN(___arg1 != Py_False);

GIL_RELEASE();

___return(dst);

"))

(define boolean->PyObject*/bool
  (c-lambda (scheme-object) PyObject*/bool "

___SCMOBJ src = ___arg1;
PyObjectPtr dst = NULL;

GIL_ACQUIRE();

if (___BOOLEANP(src)) {
  dst = ___FALSEP(src) ? Py_False : Py_True;
  PYOBJECTPTR_INCREF(dst, \"boolean->PyObject*/bool\");
}

GIL_RELEASE();

___return(dst);

"))

(define (PyObject*/int->exact-integer src)
  (let ((dst
         ((c-lambda (PyObject*/int) scheme-object "

PyObjectPtr src = ___arg1;
___SCMOBJ dst = ___FAL;

int overflow;
___LONGLONG val;

GIL_ACQUIRE();

val = PyLong_AsLongLongAndOverflow(src, &overflow);

if (!overflow) {

  if (___EXT(___LONGLONG_to_SCMOBJ)(___PSTATE,
                                    val,
                                    &dst,
                                    ___RETURN_POS)
      != ___FIX(___NO_ERR))
    dst = ___FAL;

} else {

  size_t nb_bits = _PyLong_NumBits(src) + 1; /* add 1 for sign */
  size_t nb_adigits = (nb_bits + ___BIG_ABASE_WIDTH - 1) / ___BIG_ABASE_WIDTH;
  size_t nb_bytes = nb_adigits * (___BIG_ABASE_WIDTH>>3);

  dst = ___EXT(___alloc_scmobj) (___ps, ___sBIGNUM, nb_bytes);
  if (___FIXNUMP(dst))
    dst = ___FAL;
  else {
    if (_PyLong_AsByteArray(___CAST(PyLongObject*,src),
                            ___CAST(unsigned char*,___BODY_AS(dst, ___tSUBTYPED)),
                            nb_bytes,
#ifdef ___LITTLE_ENDIAN
                            1
#else
                            0
#endif
                            , 1)) {
      dst = ___FAL;
    }
  }

}

GIL_RELEASE();

___return(___EXT(___release_scmobj) (dst));

")
          src)))
    (if dst
        (if (##bignum? dst)
            (##bignum.normalize! dst)
            dst)
        (error "PyObject*/int->exact-integer conversion error"))))

(define exact-integer->PyObject*/int
  (c-lambda (scheme-object) PyObject*/int "

___SCMOBJ src = ___arg1;
PyObjectPtr dst = NULL;

GIL_ACQUIRE();

if (___FIXNUMP(src)) {
  dst = PyLong_FromLongLong(___INT(src));
} else {

#ifdef ___LITTLE_ENDIAN
  /*
   * Conversion is simple when words are represented in little endian
   * because bignums are also stored with the big digits from the least
   * signigicant digit to the most significant digit.  So when viewed
   * as an array of bytes the bytes are from least significant to most
   * significant.
   */
  dst = _PyLong_FromByteArray(
          ___CAST(const unsigned char*,___BODY_AS(src,___tSUBTYPED)),
          ___HD_BYTES(___SUBTYPED_HEADER(src)),
          1,  /* little_endian */
          1); /* is_signed */
#endif

#ifdef ___BIG_ENDIAN
  /* TODO: use _PyLong_FromByteArray(...) after copying bignum  */
#endif
}

PYOBJECTPTR_REFCNT_SHOW(dst, \"exact-integer->PyObject*/int\");

GIL_RELEASE();

___return(dst);

"))

(define PyObject*/float->flonum
  (c-lambda (PyObject*/float) double "

double dst;

GIL_ACQUIRE();

dst = PyFloat_AS_DOUBLE(___arg1);

GIL_RELEASE();

___return(dst);

"))

(define flonum->PyObject*/float
  (c-lambda (double) PyObject*/float "

PyObjectPtr dst;

GIL_ACQUIRE();

dst = PyFloat_FromDouble(___arg1);

PYOBJECTPTR_REFCNT_SHOW(dst, \"flonum->PyObject*/float\");

GIL_RELEASE();

___return(dst);

"))

(define (PyObject*/complex->cpxnum src)
  (or ((c-lambda (PyObject*/complex) scheme-object "

PyObjectPtr src = ___arg1;
___SCMOBJ dst = ___FAL;
___SCMOBJ real_scmobj;
___SCMOBJ imag_scmobj;
double real;
double imag;

GIL_ACQUIRE();

real = PyComplex_RealAsDouble(src);
imag = PyComplex_ImagAsDouble(src);

___BEGIN_SFUN_DOUBLE_TO_SCMOBJ(real,real_scmobj,___RETURN_POS)
___BEGIN_SFUN_DOUBLE_TO_SCMOBJ(imag,imag_scmobj,___RETURN_POS)

dst = ___EXT(___alloc_scmobj) (___ps, ___sCPXNUM, ___CPXNUM_SIZE<<___LWS);
if (___FIXNUMP(dst))
  dst = ___FAL;
else
  {
    ___CPXNUMREAL(dst) = real_scmobj;
    ___CPXNUMIMAG(dst) = imag_scmobj;
    ___EXT(___release_scmobj) (dst);
  }

___END_SFUN_DOUBLE_TO_SCMOBJ(imag,imag_scmobj,___RETURN_POS)
___END_SFUN_DOUBLE_TO_SCMOBJ(real,real_scmobj,___RETURN_POS)

GIL_RELEASE();

___return(dst);

")
       src)
      (error "PyObject*/complex->cpxnum conversion error")))

(define flonums->PyObject*/complex
  (c-lambda (double double) PyObject*/complex "

double real = ___arg1;
double imag = ___arg2;

PyObjectPtr dst;

GIL_ACQUIRE();

dst = PyComplex_FromDoubles(real, imag);

PYOBJECTPTR_REFCNT_SHOW(dst, \"flonums->PyObject*/complex\");

GIL_RELEASE();

___return(dst);

"))

(define (PyObject*/Fraction->ratnum src)
  (let ((dst
         ((c-lambda (PyObject*/Fraction) scheme-object "

PyObjectPtr src = ___arg1;
___SCMOBJ dst = ___FAL;
___SCMOBJ num_scmobj;
___SCMOBJ den_scmobj;
PyObjectPtr num;
PyObjectPtr den;

GIL_ACQUIRE();

num = PyObject_GetAttrString(src, \"_numerator\");
den = PyObject_GetAttrString(src, \"_denominator\");

if (PYOBJECTPTR_to_SCMOBJ(num, &num_scmobj, ___RETURN_POS)
    == ___FIX(___NO_ERR)) {
  if (PYOBJECTPTR_to_SCMOBJ(den, &den_scmobj, ___RETURN_POS)
      == ___FIX(___NO_ERR)) {
    dst = ___EXT(___alloc_scmobj) (___ps, ___sVECTOR, 2<<___LWS);
    if (___FIXNUMP(dst))
      dst = ___FAL;
    else
      {
        ___FIELD(dst, 0) = num_scmobj;
        ___FIELD(dst, 1) = den_scmobj;
        ___EXT(___release_scmobj) (dst);
      }
    ___EXT(___release_scmobj) (den_scmobj);
  }
  ___EXT(___release_scmobj) (num_scmobj);
}

GIL_RELEASE();

___return(dst);

")
          src)))
    (if dst
        (begin
          (vector-set! dst 0 (PyObject*->object (vector-ref dst 0)))
          (vector-set! dst 1 (PyObject*->object (vector-ref dst 1)))
          (if (eqv? (vector-ref dst 1) 1)
              (vector-ref dst 0)
              (##subtype-set! dst 2))) ;; ratnum subtype = 2
        (error "PyObject*/Fraction->ratnum conversion error"))))

(define ints->PyObject*/Fraction
  (c-lambda (PyObject*/int PyObject*/int) PyObject*/Fraction "

PyObjectPtr num = ___arg1;
PyObjectPtr den = ___arg2;
PyObjectPtr dst;

GIL_ACQUIRE();

dst = PyObject_CallFunctionObjArgs(___CAST(PyObjectPtr,Fraction_cls), num, den, NULL);

GIL_RELEASE();

___return(dst);

"))

(define (PyObject*/str->string src)
  (or ((c-lambda (PyObject*/str) scheme-object "

PyObjectPtr src = ___arg1;
___SCMOBJ dst = ___FAL;

GIL_ACQUIRE();

if (!PyUnicode_READY(src)) { /* convert to canonical representation */

  Py_ssize_t len = PyUnicode_GET_LENGTH(src);

  dst = ___EXT(___alloc_scmobj) (___PSTATE, ___sSTRING, len << ___LCS);

  if (___FIXNUMP(dst))
    dst = ___FAL;
  else
    switch (PyUnicode_KIND(src)) {
      case PyUnicode_1BYTE_KIND:
        {
          Py_UCS1 *data = PyUnicode_1BYTE_DATA(src);
          while (len-- > 0)
            ___STRINGSET(dst, ___FIX(len), ___CHR(data[len]));
          break;
        }
      case PyUnicode_2BYTE_KIND:
        {
          Py_UCS2 *data = PyUnicode_2BYTE_DATA(src);
          while (len-- > 0)
            ___STRINGSET(dst, ___FIX(len), ___CHR(data[len]));
          break;
        }
      case PyUnicode_4BYTE_KIND:
        {
          Py_UCS4 *data = PyUnicode_4BYTE_DATA(src);
          while (len-- > 0)
            ___STRINGSET(dst, ___FIX(len), ___CHR(data[len]));
          break;
        }
    }
}

GIL_RELEASE();

___return(___EXT(___release_scmobj) (dst));

")
          src)
      (error "PyObject*/str->string conversion error")))

(define string->PyObject*/str
  (c-lambda (scheme-object) PyObject*/str "

___SCMOBJ src = ___arg1;
PyObjectPtr dst;

___SCMOBJ ___temp; // used by ___STRINGP

GIL_ACQUIRE();

if (!___STRINGP(src)) {
  dst = NULL;
} else {
  dst = PyUnicode_FromKindAndData(___CS_SELECT(PyUnicode_1BYTE_KIND,
                                               PyUnicode_2BYTE_KIND,
                                               PyUnicode_4BYTE_KIND),
                                  ___CAST(void*,
                                          ___BODY_AS(src,___tSUBTYPED)),
                                  ___INT(___STRINGLENGTH(src)));
  PYOBJECTPTR_REFCNT_SHOW(dst, \"string->PyObject*/str\");
}

GIL_RELEASE();

___return(dst);

"))

;; Convert from Python to Gambit kwargs notation
(define (kwargs->keywords keys vals)
  (define (join i v)
    (list v (string->keyword i)))
  (if (pair? keys)
      (let loop ((keys keys) (vals vals) (kwargs '()))
        (if (pair? keys)
            (loop (cdr keys) (cdr vals) (append (join (car keys) (car vals)) kwargs))
            (reverse kwargs)))
      '()))

(define object->SchemeObject
  (c-lambda (scheme-object) PyObject*/SchemeObject "

___SCMOBJ src = ___arg1;
PyObjectPtr dst;

void *ptr;

GIL_ACQUIRE();

ptr = ___EXT(___alloc_rc)(___PSP 0);

if (ptr == NULL) {
  // Heap overflow
  dst = NULL;
} else {

  ___EXT(___set_data_rc)(ptr, src);

  // Create an instance of a _SchemeObject class
  PyObject* obj_capsule = PyCapsule_New(ptr, NULL, NULL);

  // TODO: check for heap overflow
  dst = PyObject_CallFunctionObjArgs(___CAST(PyObjectPtr,_SchemeObject_cls), obj_capsule, NULL);

  if (dst == NULL) {
    ___EXT(___release_rc)(ptr);
  }
}

GIL_RELEASE();

___return(dst);

"))

(define scheme object->SchemeObject)

(define SchemeObject?
  (c-lambda (PyObject*) bool "

PyObject* src = ___arg1;
___BOOL result;

/* call to GIL_ACQUIRE() not needed here */

result = (Py_TYPE(src) == _SchemeObject_cls);

/* call to GIL_RELEASE() not needed here */

___return(result);

"))

(define SchemeObject->object
  (c-lambda (PyObject*/SchemeObject) scheme-object "

PyObject *src = ___arg1;
___SCMOBJ dst;

GIL_ACQUIRE();

PyObject *capsule = PyObject_GetAttrString(src, \"obj_capsule\");
void *rc = PyCapsule_GetPointer(capsule, NULL);

dst = ___EXT(___data_rc)(rc);

GIL_RELEASE();

___return(dst);

"))

(define (procedure->SchemeProcedure proc)
  (python-SchemeProcedure (object->SchemeObject proc)))

(define (PyObject*/list->vector src)
  (or ((c-lambda (PyObject*/list) scheme-object "

PyObjectPtr src = ___arg1;
Py_ssize_t len;
___SCMOBJ dst;

GIL_ACQUIRE();

len = PyList_GET_SIZE(src);

dst = ___EXT(___make_vector) (___PSTATE, len, ___FIX(0));

if (___FIXNUMP(dst)) {
  dst = ___FAL;
} else {
  Py_ssize_t i;
  for (i=0; i<len; i++) {
    PyObjectPtr item = PyList_GET_ITEM(src, i);
    ___SCMOBJ item_scmobj;
    if (PYOBJECTPTR_OWN_to_SCMOBJ(item, &item_scmobj, ___RETURN_POS)
        == ___FIX(___NO_ERR)) {
      ___VECTORSET(dst, ___FIX(i), ___EXT(___release_scmobj) (item_scmobj))
    } else {
      ___EXT(___release_scmobj) (dst);
      dst = ___FAL;
      break;
    }
  }
}

GIL_RELEASE();

___return(___EXT(___release_scmobj) (dst));

")
          src)
      (error "PyObject*/list->vector conversion error")))

(define vector->PyObject*/list
  (c-lambda (scheme-object) PyObject*/list "

___SCMOBJ src = ___arg1;
PyObjectPtr dst;

___SCMOBJ ___temp; // used by ___VECTORP

GIL_ACQUIRE();

if (!___VECTORP(src)) {
  dst = NULL;
} else {
  Py_ssize_t len = ___INT(___VECTORLENGTH(src));
  dst = PyList_New(len);
  if (dst != NULL) {
    Py_ssize_t i;
    for (i=0; i<len; i++) {
      ___SCMOBJ item = ___VECTORREF(src,___FIX(i));
      void* item_py;
      if (SCMOBJ_to_PYOBJECTPTR(item, &item_py, ___RETURN_POS)
          == ___FIX(___NO_ERR)) {
        PYOBJECTPTR_INCREF(___CAST(PyObjectPtr,item_py), \"vector->PyObject*/list\");
        PyList_SET_ITEM(dst, i, ___CAST(PyObjectPtr,item_py));
      } else {
        PYOBJECTPTR_DECREF(dst, \"vector->PyObject*/list\");
        dst = NULL;
        break;
      }
    }
    PYOBJECTPTR_REFCNT_SHOW(dst, \"vector->PyObject*/list\");
  }
}

GIL_RELEASE();

___return(dst);

"))

(define (PyObject*/tuple->vector src)
  (or ((c-lambda (PyObject*/tuple) scheme-object "

PyObjectPtr src = ___arg1;
Py_ssize_t len;
___SCMOBJ dst;

GIL_ACQUIRE();

len = PyTuple_GET_SIZE(src);

dst = ___EXT(___make_vector) (___PSTATE, len, ___FIX(0));

if (___FIXNUMP(dst)) {
  dst = ___FAL;
} else {
  Py_ssize_t i;
  for (i=0; i<len; i++) {

    PyObjectPtr item = PyTuple_GET_ITEM(src, i);

    ___SCMOBJ item_scmobj;
    if (PYOBJECTPTR_OWN_to_SCMOBJ(item, &item_scmobj, ___RETURN_POS)
        == ___FIX(___NO_ERR)) {
      ___VECTORSET(dst, ___FIX(i), ___EXT(___release_scmobj) (item_scmobj))
    } else {
      ___EXT(___release_scmobj) (dst);
      dst = ___FAL;
      break;
    }
  }
}

GIL_RELEASE();

___return(___EXT(___release_scmobj) (dst));

")
          src)
      (error "PyObject*/tuple->vector conversion error")))

(define (PyObject*/list->list src)
  (vector->list (PyObject*/list->vector src)))

(define (list->PyObject*/list src)
  (vector->PyObject*/list (list->vector src)))

(define (vector->PyObject*/tuple vect)
  (vector->PyObject*/tuple-aux vect))

(define vector->PyObject*/tuple-aux
  (c-lambda (scheme-object) PyObject*/tuple "

___SCMOBJ src = ___arg1;
PyObjectPtr dst;

___SCMOBJ ___temp; // used by ___VECTORP

GIL_ACQUIRE();

if (!___VECTORP(src)) {
  dst = NULL;
} else {
  Py_ssize_t len = ___INT(___VECTORLENGTH(src));
  dst = PyTuple_New(len);
  if (dst != NULL) {
    Py_ssize_t i;
    for (i=0; i<len; i++) {
      ___SCMOBJ item = ___VECTORREF(src,___FIX(i));
      void* item_py;
      if (SCMOBJ_to_PYOBJECTPTR(item, &item_py, ___RETURN_POS)
          == ___FIX(___NO_ERR)) {
        PYOBJECTPTR_INCREF(___CAST(PyObjectPtr,item_py), \"vector->PyObject*/tuple\");
        PyTuple_SET_ITEM(dst, i, ___CAST(PyObjectPtr,item_py));
      } else {
        PYOBJECTPTR_DECREF(dst, \"vector->PyObject*/tuple\");
        dst = NULL;
        break;
      }
    }
    PYOBJECTPTR_REFCNT_SHOW(dst, \"vector->PyObject*/tuple\");
  }
}

GIL_RELEASE();

___return(dst);

"))

(define (PyObject*/tuple->list src)
  (vector->list (PyObject*/tuple->vector src)))

(define (list->PyObject*/tuple src)
  (vector->PyObject*/tuple (list->vector src)))

(define (PyObject*/bytes->u8vector src)
  (or ((c-lambda (PyObject*/bytes) scheme-object "

PyObjectPtr src = ___arg1;
Py_ssize_t len;
___SCMOBJ dst;

GIL_ACQUIRE();

len = PyBytes_GET_SIZE(src);

dst = ___EXT(___alloc_scmobj) (___PSTATE, ___sU8VECTOR, len);

if (___FIXNUMP(dst)) {
  dst = ___FAL;
} else {
  memmove(___BODY_AS(dst,___tSUBTYPED), PyBytes_AS_STRING(src), len);
}

GIL_RELEASE();

___return(___EXT(___release_scmobj) (dst));

")
          src)
      (error "PyObject*/bytes->u8vector conversion error")))

(define u8vector->PyObject*/bytes
  (c-lambda (scheme-object) PyObject*/bytes "

___SCMOBJ src = ___arg1;
PyObjectPtr dst;

___SCMOBJ ___temp; // used by ___U8VECTORP

GIL_ACQUIRE();

if (!___U8VECTORP(src)) {
  dst = NULL;
} else {
  Py_ssize_t len = ___INT(___U8VECTORLENGTH(src));
  dst = PyBytes_FromStringAndSize(
          ___CAST(char*,___BODY_AS(src,___tSUBTYPED)),
          len);
  PYOBJECTPTR_REFCNT_SHOW(dst, \"u8vector->PyObject*/bytes\");
}

GIL_RELEASE();

___return(dst);

"))

(define s8vector->PyObject*/bytes
  (c-lambda (scheme-object) PyObject*/bytes "

___SCMOBJ src = ___arg1;
PyObjectPtr dst;

___SCMOBJ ___temp; // used by ___S8VECTORP

GIL_ACQUIRE();

if (!___S8VECTORP(src)) {
  dst = NULL;
} else {
  Py_ssize_t len = ___INT(___S8VECTORLENGTH(src));
  dst = PyBytes_FromStringAndSize(
          ___CAST(char*,___BODY_AS(src,___tSUBTYPED)),
          len);
  PYOBJECTPTR_REFCNT_SHOW(dst, \"u8vector->PyObject*/bytes\");
}

GIL_RELEASE();

___return(dst);

"))

(define (PyObject*/bytearray->u8vector src)
  (or ((c-lambda (PyObject*/bytearray) scheme-object "

PyObjectPtr src = ___arg1;
Py_ssize_t len;
___SCMOBJ dst;

GIL_ACQUIRE();

len = PyByteArray_GET_SIZE(src);

dst = ___EXT(___alloc_scmobj) (___PSTATE, ___sU8VECTOR, len);

if (___FIXNUMP(dst)) {
  dst = ___FAL;
} else {
  memmove(___BODY_AS(dst,___tSUBTYPED), PyByteArray_AS_STRING(src), len);
}

GIL_RELEASE();

___return(___EXT(___release_scmobj) (dst));

")
          src)
      (error "PyObject*/bytearray->u8vector conversion error")))

(define u8vector->PyObject*/bytearray
  (c-lambda (scheme-object) PyObject*/bytearray "

___SCMOBJ src = ___arg1;
PyObjectPtr dst;

___SCMOBJ ___temp; // used by ___U8VECTORP

GIL_ACQUIRE();

if (!___U8VECTORP(src)) {
  dst = NULL;
} else {
  Py_ssize_t len = ___INT(___U8VECTORLENGTH(src));
  dst = PyByteArray_FromStringAndSize(
          ___CAST(char*,___BODY_AS(src,___tSUBTYPED)),
          len);
  PYOBJECTPTR_REFCNT_SHOW(dst, \"u8vector->PyObject*/bytearray\");
}

GIL_RELEASE();

___return(dst);

"))

;;;----------------------------------------------------------------------------

;; Generic converters.

(define (PyObject*->object src)

  (define (conv src)
;;    (let ((converter (table-ref PyObject*-converters (PyObject*-type-name src) #f)))
;;      (if converter
;;          (converter src)
          (case (car (##foreign-tags src))
            ((PyObject*/None)                        (PyObject*/None->void src))
            ((PyObject*/bool)                        (PyObject*/bool->boolean src))
            ((PyObject*/int)                         (PyObject*/int->exact-integer src))
            ((PyObject*/float)                       (PyObject*/float->flonum src))
            ((PyObject*/complex)                     (PyObject*/complex->cpxnum src))
            ((PyObject*/Fraction)                    (PyObject*/Fraction->ratnum src))
            ((PyObject*/str)                         (PyObject*/str->string src))
            ((PyObject*/bytes)                       (PyObject*/bytes->u8vector src))
            ((PyObject*/bytearray)                   (PyObject*/bytearray->u8vector src))
            ((PyObject*/list)                        (list-conv src))
            ((PyObject*/tuple)                       (vector-conv src))
            ((PyObject*/dict)                        (table-conv src))
            ((PyObject*/function
              PyObject*/builtin_function_or_method
              PyObject*/method
              PyObject*/method_descriptor)           (procedure-conv src))
            ((PyObject*/cell)                        (PyCell_Get src))
            (else
             (cond ((= 1 (PyCallable_Check src))     (procedure-conv src))
                   ((SchemeObject? src)              (SchemeObject->object src))
                   (else                             src))))
;;))
)

  (define (list-conv src)
    (let* ((vect (PyObject*/list->vector src))
           (len (vector-length vect)))
      (let loop ((i (fx- len 1)) (lst '()))
        (if (fx< i 0)
            lst
            (loop (fx- i 1)
                  (cons (conv (vector-ref vect i))
                        lst))))))

  (define (vector-conv src)
    (let ((vect (PyObject*/tuple->vector src)))
      (let loop ((i (fx- (vector-length vect) 1)))
        (if (fx< i 0)
            vect
            (begin
              (vector-set! vect i (conv (vector-ref vect i)))
              (loop (fx- i 1)))))))

  (define (table-conv src)
    (let ((table (make-table)))
      (for-each (lambda (key)
                  (let ((val (PyDict_GetItem src key)))
                    (table-set! table
                                (PyObject*->object key)
                                (PyObject*->object val))))
                (PyObject*/list->list (PyDict_Keys src)))
      table))

  ;; TODO: Handle **kwargs
  (define (procedure-conv callable)
    (define (valid-kw? rest)
      (and (pair? rest)
           (not (keyword? (car rest)))))
    (lambda (#!optional
             (arg1 (macro-absent-obj))
             (arg2 (macro-absent-obj))
             (arg3 (macro-absent-obj))
             #!rest
             other)

      (define (generic args)
        (let loop ((args args) (*args '()) (kw-keys '()) (kw-vals '()))
          (if (pair? args)
              (let ((arg (car args))
                    (rest (cdr args)))
                (if (keyword? arg)
                    (if (valid-kw? rest)
                        (loop (cdr rest)
                              *args
                              (cons (keyword->string arg) kw-keys)
                              (cons (car rest) kw-vals))
                        (error "Keyword argument has no value" args))
                    (loop (cdr args) (cons (car args) *args) kw-keys kw-vals))
                (if (null? kw-keys)
                    (sfpc-call callable (list->vector (reverse *args)))
                    (sfpc-call-with-kw callable (list->vector (reverse *args)) kw-keys kw-vals))))))

      (cond ((eq? arg1 (macro-absent-obj))
             (sfpc-call callable '#()))
            ((keyword? arg1)
             (cond ((eq? arg2 (macro-absent-obj))
                    (generic (list arg1)))
                   ((eq? arg3 (macro-absent-obj))
                    (generic (list arg1 arg2)))
                   (else
                    (generic (cons arg1 (cons arg2 (cons arg3 other)))))))
            ((eq? arg2 (macro-absent-obj))
             (sfpc-call callable (vector arg1)))
            ((keyword? arg2)
             (cond ((eq? arg3 (macro-absent-obj))
                    (generic (list arg1 arg2)))
                   (else
                    (generic (cons arg1 (cons arg2 (cons arg3 other)))))))
            ((eq? arg3 (macro-absent-obj))
             (sfpc-call callable (vector arg1 arg2)))
            ((keyword? arg3)
             (generic (cons arg1 (cons arg2 (cons arg3 other)))))
            ((null? other)
             (sfpc-call callable (vector arg1 arg2 arg3)))
            (else
             (generic (cons arg1 (cons arg2 (cons arg3 other))))))))

  (if (##foreign? src)
      (conv src)
      src))

(define (PyObject*-or-subtype? tag)
  (case tag
    ((PyObject*
      PyObject*/None
      PyObject*/bool
      PyObject*/int
      PyObject*/float
      PyObject*/complex
      PyObject*/Fraction
      PyObject*/bytes
      PyObject*/bytearray
      PyObject*/str
      PyObject*/list
      PyObject*/dict
      PyObject*/frozenset
      PyObject*/set
      PyObject*/tuple
      PyObject*/module
      PyObject*/type
      PyObject*/function
      PyObject*/builtin_function_or_method
      PyObject*/method
      PyObject*/method_descriptor
      PyObject*/cell
      PyObject*/SchemeObject)
     #t)
    (else
     #f)))

(define (object->PyObject* src)

  (define (conv src)
    (cond ((eq? src (void))             (void->PyObject*/None src))
          ((boolean? src)               (boolean->PyObject*/bool src))
          ((exact-integer? src)         (exact-integer->PyObject*/int src))
          ((flonum? src)                (flonum->PyObject*/float src))
          ((##cpxnum? src)              (flonums->PyObject*/complex
                                         (##inexact (##cpxnum-real src))
                                         (##inexact (##cpxnum-imag src))))
          ((##ratnum? src)              (ints->PyObject*/Fraction
                                         (object->PyObject* (##numerator src))
                                         (object->PyObject* (##denominator src))))
          ((string? src)                (string->PyObject*/str src))
          ((char? src)                  (exact-integer->PyObject*/int (char->integer src)))
          ((u8vector? src)              (u8vector->PyObject*/bytes src))
          ((s8vector? src)              (s8vector->PyObject*/bytes src))
          ((or (null? src) (pair? src)) (list-conv src))
          ((vector? src)                (vector-conv src))
          ((table? src)                 (table-conv src))
          ((symbol? src)                (string->PyObject*/str (symbol->string src)))
          ((and (##foreign? src)
                (PyObject*-or-subtype?
                 (car (##foreign-tags src))))
           src)
          ((procedure? src)             (procedure->SchemeProcedure src))
          (else
           (error "can't convert" src))))

  (define (list-conv src)
    (let loop1 ((probe src) (len 0))
      (if (pair? probe)
          (loop1 (cdr probe) (fx+ len 1))
          (let ((vect
                 (if (null? probe)
                     (make-vector len)
                     (make-vector (fx+ len 1) (conv probe)))))
            (let loop2 ((probe src) (i 0))
              (if (and (pair? probe) (fx< i (vector-length vect)))
                  (begin
                    (vector-set! vect i (conv (car probe)))
                    (loop2 (cdr probe) (fx+ i 1)))
                  (vector->PyObject*/list vect)))))))

  (define (vector-conv src)
    (let* ((len (vector-length src))
           (vect (make-vector len)))
      (let loop ((i (fx- len 1)))
        (if (fx< i 0)
            (vector->PyObject*/tuple vect)
            (begin
              (vector-set! vect i (conv (vector-ref src i)))
              (loop (fx- i 1)))))))

  (define (u8vector-conv src)
    (let* ((len (vector-length src))
           (vect (make-vector len)))
      (let loop ((i (fx- len 1)))
        (if (fx< i 0)
            (vector->PyObject*/tuple vect)
            (begin
              (vector-set! vect i (conv (vector-ref src i)))
              (loop (fx- i 1)))))))

  (define (table-conv src)
    (let ((dst (PyDict_New)))
      (table-for-each
       (lambda (key val)
         (PyDict_SetItem dst
                         (object->PyObject* key)
                         (object->PyObject* val)))
       src)
      dst))

  (conv src))

;;;----------------------------------------------------------------------------

;; TODO: get rid of this by improving Gambit C interface.

(define dummy
  (list
   (c-lambda () _PyObject* "___return(NULL);")
   (c-lambda () _PyObject*/None "___return(NULL);")
   (c-lambda () _PyObject*/bool "___return(NULL);")
   (c-lambda () _PyObject*/int "___return(NULL);")
   (c-lambda () _PyObject*/float "___return(NULL);")
   (c-lambda () _PyObject*/complex "___return(NULL);")
   (c-lambda () _PyObject*/Fraction "___return(NULL);")
   (c-lambda () _PyObject*/bytes "___return(NULL);")
   (c-lambda () _PyObject*/bytearray "___return(NULL);")
   (c-lambda () _PyObject*/str "___return(NULL);")
   (c-lambda () _PyObject*/list "___return(NULL);")
   (c-lambda () _PyObject*/dict "___return(NULL);")
   (c-lambda () _PyObject*/frozenset "___return(NULL);")
   (c-lambda () _PyObject*/set "___return(NULL);")
   (c-lambda () _PyObject*/tuple "___return(NULL);")
   (c-lambda () _PyObject*/module "___return(NULL);")
   (c-lambda () _PyObject*/type "___return(NULL);")
   (c-lambda () _PyObject*/function "___return(NULL);")
   (c-lambda () _PyObject*/builtin_function_or_method "___return(NULL);")
   (c-lambda () _PyObject*/method "___return(NULL);")
   (c-lambda () _PyObject*/method_descriptor "___return(NULL);")
   (c-lambda () _PyObject*/cell "___return(NULL);")
   (c-lambda () _PyObject*/SchemeObject "___return(NULL);")))

;;;----------------------------------------------------------------------------

;; Call Python callables from Scheme.

(define (PyObject_CallFunctionObjArgs callable . args)
  (PyObject_CallFunctionObjArgs* callable args))

;; TODO: Handle **kwargs in Python call
(define (PyObject_CallFunctionObjArgs* callable args)
  (if (not (pair? args))
      (PyObject_CallNoArgs callable)
      (let ((arg1 (car args))
            (rest (cdr args)))
        (if (not (pair? rest))
            (PyObject_CallOneArg callable arg1)
            (let ((arg2 (car rest))
                  (rest (cdr rest)))
              (if (not (pair? rest))
                  (PyObject_CallFunctionObjArgs2 callable arg1 arg2)
                  (let ((arg3 (car rest))
                        (rest (cdr rest)))
                    (if (not (pair? rest))
                        (PyObject_CallFunctionObjArgs3 callable arg1 arg2 arg3)
                        (let ((arg4 (car rest))
                              (rest (cdr rest)))
                          (if (not (pair? rest))
                              (PyObject_CallFunctionObjArgs4 callable arg1 arg2 arg3 arg4)
                              (PyObject_CallObject
                               callable
                               (list->PyObject*/tuple args))))))))))))

(define PyObject_CallFunctionObjArgs0
  (c-lambda (PyObject*) PyObject* "

return_with_check_PyObjectPtr(PyObject_CallFunctionObjArgs(___arg1, NULL));

"))

(define PyObject_CallFunctionObjArgs1
  (c-lambda (PyObject* PyObject*) PyObject* "

return_with_check_PyObjectPtr(PyObject_CallFunctionObjArgs(___arg1, ___arg2, NULL));

"))

(define PyObject_CallFunctionObjArgs2
  (c-lambda (PyObject* PyObject* PyObject*) PyObject* "

return_with_check_PyObjectPtr(PyObject_CallFunctionObjArgs(___arg1, ___arg2, ___arg3, NULL));

"))

(define PyObject_CallFunctionObjArgs3
  (c-lambda (PyObject* PyObject* PyObject* PyObject*) PyObject* "

return_with_check_PyObjectPtr(PyObject_CallFunctionObjArgs(___arg1, ___arg2, ___arg3, ___arg4, NULL));

"))

(define PyObject_CallFunctionObjArgs4
  (c-lambda (PyObject* PyObject* PyObject* PyObject* PyObject*) PyObject* "

return_with_check_PyObjectPtr(PyObject_CallFunctionObjArgs(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, NULL));

"))

;;;----------------------------------------------------------------------------













;;;----------------------------------------------------------------------------

;;; new stuff

(c-declare #<<end-of-c-declare



#include <stdio.h>
#include <pthread.h>

#if 0
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#endif



void sleep_ms(int milliseconds) {

  struct timespec t = {
    (int)(milliseconds / 1000),
    (milliseconds % 1000) * 1000000
  };

  while (nanosleep (&t , &t) < 0) ;
}


/* converters */

___SCMOBJ convert_from_python(PyObject *val) {

  ___SCMOBJ result;

#ifdef DEBUG_LOWLEVEL
  printf("convert_from_python() enter\n");
#endif

  GIL_ACQUIRE();

  ___SCMOBJ err = PYOBJECTPTR_to_SCMOBJ(val, &result, ___RETURN_POS);

  if (err != ___FIX(___NO_ERR)) {
    printf("could not convert PyObject* to SCMOBJ\n");
    exit(1);
  }

#ifdef DEBUG_LOWLEVEL
  printf("convert_from_python() exit\n");
#endif

  GIL_RELEASE();

  return result;
}

PyObject *convert_to_python(___SCMOBJ val) {

  void *ptr;
  PyObject *result;

#ifdef DEBUG_LOWLEVEL
  printf("convert_to_python() enter\n");
#endif

  GIL_ACQUIRE();

  ___SCMOBJ err = SCMOBJ_to_PYOBJECTPTR(val, &ptr, ___RETURN_POS);

  if (err != ___FIX(___NO_ERR)) {
    printf("could not convert SCMOBJ to PyObject*\n");
    exit(1);
  }

  result = ptr;

  PYOBJECTPTR_REFCNT_SHOW(result, "convert_to_python");

#ifdef DEBUG_LOWLEVEL
  printf("convert_to_python() exit\n");
#endif

  GIL_RELEASE();

  return result;
}



typedef struct fpc_state_struct
  {
    ___procedural_interrupt_header header; /* used as a procedural interrupt */
    ___processor_state pstate;             /* Gambit processor */
    PyObject *message;                     /* inter-realm message */
    PyObject *capsule;                     /* Python ref to this fpc_state */
    ___MUTEX_DECL(wait_mut);               /* for waiting for peer's message */
    ___thread python_thread;               /* OS thread that runs Python code */
  } fpc_state;



const char *python_code = "\
\n\
_sys = __import__(\"sys\")\n\
_threading = __import__(\"threading\")\n\
_pfpc = __import__(\"pfpc\")\n\
_fractions = __import__(\"fractions\")\n\
_empty_dict = dict()\n\
\n\
def _pfpc_get_fpc_state():\n\
  ct = _threading.current_thread()\n\
  if not hasattr(ct, \"_fpc_state\"):\n\
    ct._fpc_state = _pfpc.start_buddy()\n\
    _pfpc_loop(ct._fpc_state) # wait for buddy thread to be started\n\
  return ct._fpc_state\n\
\n\
_pfpc_send = _pfpc.send\n\
_pfpc_recv = _pfpc.recv\n\
\n\
#_op_return = 'return'\n\
#_op_call   = 'call'\n\
#_op_raise  = 'raise'\n\
#_op_error  = 'error'\n\
#_op_get_eval = 'get-eval'\n\
#_op_get_exec = 'get-exec'\n\
#_op_terminate = 'terminate'\n\
\n\
_op_return = 0\n\
_op_call   = 1\n\
_op_raise  = 2\n\
_op_error  = 3\n\
_op_get_eval = 4\n\
_op_get_exec = 5\n\
_op_terminate = 6\n\
\n\
def _pfpc_loop(fpc_state):\n\
  while True:\n\
    message = _pfpc_recv(fpc_state)\n\
#    print(\"pfpc_loop message =\", repr(message))\n\
    op = message[0]\n\
    if op == _op_call:\n\
      try:\n\
        if len(message) > 3:\n\
          message = (_op_return, message[1](*message[2], **dict(zip(message[3], message[4]))))\n\
        else:\n\
          message = (_op_return, message[1](*message[2]))\n\
      except BaseException as exc:\n\
        message = (_op_raise, exc, repr(exc))\n\
    elif op == _op_return:\n\
      return message[1]\n\
    elif op == _op_raise:\n\
      raise message[1]\n\
    elif op == _op_get_eval:\n\
      message = (_op_return, lambda e: eval(e, globals()))\n\
    elif op == _op_get_exec:\n\
      message = (_op_return, lambda e: exec(e, globals()))\n\
    elif op == _op_terminate:\n\
      _sys.exit()\n\
    else:\n\
      message = (_op_error, op)\n\
    _pfpc_send(fpc_state, message)\n\
\n\
def _pfpc_call(fn, args, kw_keys, kw_vals):\n\
  fpc_state = _pfpc_get_fpc_state()\n\
  _pfpc_send(fpc_state, (_op_call, fn, args, kw_keys, kw_vals))\n\
  return _pfpc_loop(fpc_state)\n\
\n\
def _pfpc_start(fpc_state):\n\
  _threading.current_thread()._fpc_state = fpc_state\n\
  _pfpc_loop(fpc_state)\n\
\n\
def _SchemeProcedure(scheme_proc):\n\
  def fun(*args, **kwargs):\n\
    kw_keys = list(kwargs.keys())\n\
    kw_vals = list(kwargs.values())\n\
    return _pfpc_call(scheme_proc, args, kw_keys, kw_vals)\n\
  return foreign(fun)\n\
\n\
foreign = lambda x: (lambda:x).__closure__[0]\n\
\n\
class _SchemeObject(BaseException):\n\
  def __init__(self, obj_capsule):\n\
    self.obj_capsule = obj_capsule\n\
  def __del__(self):\n\
    _pfpc.free(self.obj_capsule)\n\
\n\
def set_global(k, v):\n\
  globals()[k] = v\n\
\n\
";




void python_thread_main(___thread *self) {

  /* TODO: add error checking */

  fpc_state *python_fpc_state = ___CAST(fpc_state*, self->data_ptr);

  GIL_ACQUIRE();

  PyObject *m = PyImport_AddModule("__main__");
  PyObject *v = PyObject_GetAttrString(m, "_pfpc_start");

  PyObject_CallOneArg(v, python_fpc_state->capsule); /* call _pfpc_start */

  GIL_RELEASE();
}


___SCMOBJ procedural_interrupt_execute_fn(void *self, ___SCMOBJ op) {

#ifdef DEBUG_LOWLEVEL
  printf("procedural_interrupt_execute_fn() enter\n");
#endif

  if (op != ___FAL) {

    fpc_state *python_fpc_state = ___CAST(fpc_state*, self);

    ___SCMOBJ scheme_fpc_state =
      ___EXT(___data_rc)(___CAST(void*,python_fpc_state));

//    printf("scheme_fpc_state = %p\n", scheme_fpc_state);
//    printf("python_fpc_state->pstate = %p\n", python_fpc_state->pstate);

    if (scheme_fpc_state == ___FAL) {

      ___processor_state ___ps = python_fpc_state->pstate; /* same as ___PSTATE */
      ___SCMOBJ python_fpc_state_scmobj;

//      printf("___PSTATE = %p\n", ___PSTATE);
//      printf("___ps = %p\n", ___ps);

      scheme_fpc_state = ___EXT(___make_vector)(___ps, 4, ___NUL);

      if (___FIXNUMP(scheme_fpc_state)) {
        printf("heap overflow\n");
        exit(1);
      }

      ___EXT(___set_data_rc)(python_fpc_state, scheme_fpc_state);

      ___EXT(___register_rc)(___PSP python_fpc_state);

      ___FIELD(scheme_fpc_state, 1) = ___GLO__23__23_start_2d_buddy;

      if (___EXT(___POINTER_to_SCMOBJ)(___ps,
                                       ___CAST(void*,python_fpc_state),
                                       ___C_TAG_fpc__state_2a_,
                                       ___RELEASE_POINTER,
                                       &python_fpc_state_scmobj,
                                       ___RETURN_POS)
          != ___FIX(___NO_ERR)) {
        printf("could not convert python_fpc_state to foreign\n");
        exit(1);
      }

      ___FIELD(scheme_fpc_state, 3) = python_fpc_state_scmobj;

//      ___EXT(___release_scmobj)(python_fpc_state_scmobj);
    }

#ifdef DEBUG_LOWLEVEL
    printf("procedural_interrupt_execute_fn() calling ___raise_high_level_interrupt_pstate\n");
#endif

    ___EXT(___raise_high_level_interrupt_pstate)(python_fpc_state->pstate,
                                                 scheme_fpc_state);
  }

#ifdef DEBUG_LOWLEVEL
  printf("procedural_interrupt_execute_fn() exit\n");
#endif

  return ___FIX(___NO_ERR);
}


fpc_state *alloc_python_fpc_state(___processor_state ___ps) {

  PyObject *capsule;

  fpc_state *python_fpc_state = ___EXT(___alloc_rc_no_register)(sizeof(fpc_state));

  if (python_fpc_state == NULL) {
    printf("could not allocate python_fpc_state\n");
    exit(1); /* TODO: better error handling! */
  }

  ___EXT(___init_procedural_interrupt)(___CAST(void*, python_fpc_state),
                                       procedural_interrupt_execute_fn);

  python_fpc_state->pstate = ___ps;

  capsule = PyCapsule_New(python_fpc_state, NULL, NULL);

  if (capsule == NULL) {
    printf("could not allocate capsule\n");
    /* TODO: use ___release_rc */
    ___EXT(___release_rc)(python_fpc_state);
    exit(1); /* TODO: better error handling! */
  }

  PYOBJECTPTR_REFCNT_SHOW(capsule, "alloc_python_fpc_state");

  python_fpc_state->capsule = capsule;

#ifdef DEBUG_LOWLEVEL
  printf("alloc_python_fpc_state() calling ___MUTEX_LOCK(python_fpc_state->wait_mut);\n");
#endif

  ___MUTEX_INIT(python_fpc_state->wait_mut);
  ___MUTEX_LOCK(python_fpc_state->wait_mut);

  python_fpc_state->python_thread.data_ptr = ___CAST(void*, python_fpc_state);

  return python_fpc_state;
}

void setup_python_fpc_state(___SCMOBJ scheme_fpc_state) {

  ___processor_state ___ps = ___PSTATE;
  fpc_state *python_fpc_state;

#ifdef DEBUG_LOWLEVEL
  printf("setup_python_fpc_state() enter\n");
#endif

  python_fpc_state = alloc_python_fpc_state(___PSTATE);

  ___FIELD(___FIELD(scheme_fpc_state, 3),___FOREIGN_PTR) = ___CAST(___WORD, python_fpc_state);

  ___EXT(___set_data_rc)(python_fpc_state, scheme_fpc_state);

  ___EXT(___register_rc)(___PSP python_fpc_state);

  /* Start the buddy Python thread */

  python_fpc_state->python_thread.start_fn = python_thread_main;

  if (___EXT(___thread_create)(&python_fpc_state->python_thread)
      != ___FIX(___NO_ERR)) {
    printf("can't create Python thread (was Gambit configured with --enable-multiple-threaded-vms?)\n");
    exit(1); /* TODO: better error handling! */
  }

#ifdef DEBUG_LOWLEVEL
  printf("setup_python_fpc_state() exit\n");
#endif
}


void cleanup_python_fpc_state(___SCMOBJ scheme_fpc_state) {

  fpc_state *python_fpc_state =
    ___CAST(fpc_state*,___FIELD(___FIELD(scheme_fpc_state, 3),___FOREIGN_PTR));

#ifdef DEBUG_LOWLEVEL
  printf("cleanup_python_fpc_state() enter\n");
#endif

  if (___EXT(___thread_join)(&python_fpc_state->python_thread)
      != ___FIX(___NO_ERR)) {
    printf("can't join Python thread\n");
    exit(1); /* TODO: better error handling! */
  }

#ifdef DEBUG_LOWLEVEL
  printf("cleanup_python_fpc_state() exit\n");
#endif
}


void sfpc_send(___SCMOBJ scheme_fpc_state, PyObject *message) {

  PYOBJECTPTR_INCREF(message, "sfpc_send");

  fpc_state *python_fpc_state =
    ___CAST(fpc_state*,___FIELD(___FIELD(scheme_fpc_state, 3),___FOREIGN_PTR));

#ifdef DEBUG_LOWLEVEL
  printf("sfpc_send() setting python_fpc_state->message\n");
#endif

  python_fpc_state->message = message;

#ifdef DEBUG_LOWLEVEL
  printf("sfpc_send() calling ___MUTEX_UNLOCK(python_fpc_state->wait_mut);\n");
#endif

  ___MUTEX_UNLOCK(python_fpc_state->wait_mut);
}


PyObject *sfpc_recv(___SCMOBJ scheme_fpc_state) {

  fpc_state *python_fpc_state =
    ___CAST(fpc_state*,___FIELD(___FIELD(scheme_fpc_state, 3),___FOREIGN_PTR));

#ifdef DEBUG_LOWLEVEL
  printf("sfpc_recv() returning python_fpc_state->message\n");
#endif

  return python_fpc_state->message;
}



static PyObject *pfpc_send(PyObject *self, PyObject *args) {

  PyObject *capsule;
  PyObject *message;
  PyArg_ParseTuple(args, "OO", &capsule, &message);

  PYOBJECTPTR_REFCNT_SHOW(capsule, "pfpc_send");
  PYOBJECTPTR_REFCNT_SHOW(message, "pfpc_send");

  fpc_state *python_fpc_state =
    ___CAST(fpc_state*, PyCapsule_GetPointer(capsule, NULL));

//  PYOBJECTPTR_DECREF(capsule, "pfpc_send");

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_send() enter\n");
#endif

  /* send message to Scheme thread */

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_send() setting python_fpc_state->message\n");
#endif

  PYOBJECTPTR_INCREF(message, "pfpc_send");

  python_fpc_state->message = message;

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_send() calling ___raise_procedural_interrupt_pstate\n");
#endif

  ___EXT(___raise_procedural_interrupt_pstate)(python_fpc_state->pstate,
                                               ___CAST(void*,python_fpc_state));

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_send() exit\n");
#endif

  Py_INCREF(Py_None);

  return Py_None;
}


static PyObject *pfpc_recv(PyObject *self, PyObject *args) {

  PyObject *capsule;
  PyArg_ParseTuple(args, "O", &capsule);

  PYOBJECTPTR_REFCNT_SHOW(capsule, "pfpc_recv");

  fpc_state *python_fpc_state =
    ___CAST(fpc_state*, PyCapsule_GetPointer(capsule, NULL));

//  PYOBJECTPTR_DECREF(capsule, "pfpc_recv");

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_recv() calling ___MUTEX_LOCK(python_fpc_state->wait_mut);\n");
#endif

  Py_BEGIN_ALLOW_THREADS
  ___MUTEX_LOCK(python_fpc_state->wait_mut);
  Py_END_ALLOW_THREADS

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_recv() returning python_fpc_state->message\n");
#endif

  return python_fpc_state->message;
}


static PyObject *pfpc_free(PyObject *self, PyObject *args) {

  PyObject *capsule;
  PyArg_ParseTuple(args, "O", &capsule);

  PYOBJECTPTR_REFCNT_SHOW(capsule, "pfpc_free");

  void *ptr = PyCapsule_GetPointer(capsule, NULL);

  PYOBJECTPTR_DECREF(capsule, "pfpc_free");

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_free calling ___release_rc(%p)\n", ptr);
#endif

  ___EXT(___release_rc)(ptr);

  Py_INCREF(Py_None);

  return Py_None;
}


___SCMOBJ ___thread_init_from_self
   ___P((___thread *thread),
        (thread)
___thread *thread;)
{
#ifdef ___USE_POSIX_THREAD_SYSTEM

  thread->thread_id = pthread_self ();

#endif

#ifdef ___USE_WIN32_THREAD_SYSTEM

  thread->thread_handle = GetCurrentThread ();
  thread->thread_id = GetCurrentThreadId ();

#endif

return ___FIX(___NO_ERR);
//  return thread_init_common (thread);
}

static PyObject *pfpc_start_buddy(PyObject *self, PyObject *args) {

  ___processor_state ___ps =
    ___PSTATE_FROM_PROCESSOR_ID(0, &___GSTATE->vmstate0);

  fpc_state *python_fpc_state;
  PyObject *capsule;

#ifdef DEBUG_LOWLEVEL
  printf("pfpc_start_buddy() enter\n");
#endif

  python_fpc_state = alloc_python_fpc_state(___ps);

  ___EXT(___thread_init_from_self)(&python_fpc_state->python_thread);


  ___EXT(___raise_procedural_interrupt_pstate)(___ps,
                                               ___CAST(void*,python_fpc_state));


#ifdef DEBUG_LOWLEVEL
  printf("pfpc_start_buddy() exit\n");
#endif

  return python_fpc_state->capsule;
}


static PyMethodDef pfpc_methods[] = {
  {"send",  pfpc_send, METH_VARARGS, "Send to buddy thread."},
  {"recv",  pfpc_recv, METH_VARARGS, "Receive from buddy thread."},
  {"free",  pfpc_free, METH_VARARGS, "Free Scheme object."},
  {"start_buddy", pfpc_start_buddy, METH_VARARGS, "Start buddy Scheme thread."},
  {NULL, NULL, 0, NULL}
};


static struct PyModuleDef pfpc_module = {
    PyModuleDef_HEAD_INIT,
    "pfpc",   /* name of module */
    NULL,     /* module documentation, may be NULL */
    -1,       /* size of per-interpreter state of the module,
                 or -1 if the module keeps state in global variables. */
    pfpc_methods
};


PyMODINIT_FUNC PyInit_pfpc(void) {
  return PyModule_Create(&pfpc_module);
}


___BOOL initialize(void) {

  if (PyImport_AppendInittab("pfpc", PyInit_pfpc) == -1) return 0;

  Py_Initialize();

  PyRun_SimpleString(python_code);

  PyObject *__main__ = PyImport_AddModule("__main__");
  PyObject *_fractions = PyObject_GetAttrString(__main__, "_fractions");

  _SchemeObject_cls = ___CAST(PyTypeObject*, PyObject_GetAttrString(__main__, "_SchemeObject"));
  Fraction_cls = ___CAST(PyTypeObject*, PyObject_GetAttrString(_fractions, "Fraction"));

  PyEval_SaveThread();

  return 1;
}


void finalize(void) {
  Py_Finalize();
}



end-of-c-declare
)

;;;----------------------------------------------------------------------------

;;(define PyObject*-converters (make-table))

;;(define (PyObject*-register-converter type-name conv)
;;  (let ((val (table-ref PyObject*-converters type-name #f)))
;;    (if val
;;        (begin
;;          (table-set! PyObject*-converters type-name #f)
;;          (table-set! PyObject*-converters type-name conv))
;;        (table-set! PyObject*-converters type-name conv))))

(define (##py-function-memoized descr)
  (let* ((x (##unbox descr)))
    (if (##string? x)
        (let ((host-fn (python-eval x)))
          (##set-box! descr host-fn)
          host-fn)
        x)))

;;;----------------------------------------------------------------------------

(define initialize
  (c-lambda () bool "initialize"))

(define finalize
  (c-lambda () void "finalize"))

(c-define-type fpc_state* (pointer "fpc_state"))

(define (make-null-fpc_state*)
  (let ((x ((c-lambda () fpc_state* "___return(___CAST(void*,1));"))))
    ((c-lambda (scheme-object) void "___FIELD(___ARG1,___FOREIGN_PTR) = ___CAST(___WORD,NULL);") x)
    x))

(define scheme-fpc-state-table #f)

(define (get-scheme-fpc-state!)
  (let ((thread (current-thread)))
    (or (table-ref scheme-fpc-state-table thread #f)
        (let ((scheme-fpc-state (make-scheme-fpc-state thread)))
          (table-set! scheme-fpc-state-table thread scheme-fpc-state)
          scheme-fpc-state))))

(define (make-scheme-fpc-state thread)
  (let ((scheme-fpc-state
         (let ((mut (make-mutex)))
           (mutex-lock! mut) ;; must be locked, to block at next mutex-lock!
           (vector '()
                   (lambda (self)
                     (let ((mut (vector-ref self 2)))
                       (mutex-unlock! mut)))
                   mut
                   (make-null-fpc_state*)))))
    (table-set! scheme-fpc-state-table thread scheme-fpc-state)
    ((c-lambda (scheme-object) void "setup_python_fpc_state")
     scheme-fpc-state)
    scheme-fpc-state))

(define (##start-buddy scheme-fpc-state)
  (declare (not interrupts-enabled))
  (let ((mut (make-mutex)))
    (mutex-lock! mut) ;; must be locked, to block at next mutex-lock!
    (vector-set! scheme-fpc-state
                 1
                 (lambda (self)
                   (let ((mut (vector-ref self 2)))
                     (mutex-unlock! mut))))
    (vector-set! scheme-fpc-state
                 2
                 mut)
    (let ((thread
           (##make-root-thread
            (lambda ()
              (with-exception-catcher
               (lambda (e)
;;                 (pp (list 'start-buddy-got-exception e))
;;                 (print "e=") (display-exception e)
                 #f)
               (lambda ()
;;                 (pp '-----------)
                 (sfpc-send scheme-fpc-state (vector op-return (void))) ;; signal thread is started
                 (sfpc-loop scheme-fpc-state))))
            'buddy)))
      (table-set! scheme-fpc-state-table thread scheme-fpc-state)
      (thread-start! thread))))
  
(define (cleanup-scheme-fpc-state thread)
  (let ((scheme-fpc-state (table-ref scheme-fpc-state-table thread #f)))
    (and scheme-fpc-state
         (begin
           (sfpc-send scheme-fpc-state (vector op-terminate))
           ((c-lambda (scheme-object) void "cleanup_python_fpc_state")
            scheme-fpc-state)))))

(define (sfpc-send scheme-fpc-state message)
;;  (pp (list 'sfpc-send scheme-fpc-state message))
  (let ((python-message (object->PyObject* message)))
    ((c-lambda (scheme-object PyObject*) void "sfpc_send")
     scheme-fpc-state
     python-message)
    ))

(define (sfpc-recv scheme-fpc-state)
;;  (pp (list 'sfpc-recv scheme-fpc-state))
  (mutex-lock! (vector-ref scheme-fpc-state 2))
  ((c-lambda (scheme-object) PyObject*!own "sfpc_recv")
   scheme-fpc-state))
#;
(begin
  (define op-return "return")
  (define op-call   "call")
  (define op-raise  "raise")
  (define op-error  "error")
  (define op-get-eval "get-eval")
  (define op-get-exec "get-exec")
  (define op-terminate "terminate"))

(begin
  (define op-return 0)
  (define op-call   1)
  (define op-raise  2)
  (define op-error  3)
  (define op-get-eval 4)
  (define op-get-exec 5)
  (define op-terminate 6))

(define (sfpc-loop scheme-fpc-state)
;;  (pp (list 'sfpc-loop scheme-fpc-state))
  (let loop ()
    (let* ((python-message (sfpc-recv scheme-fpc-state))
           (message (PyObject*->object python-message))
           (op (vector-ref message 0)))
      (cond ((equal? op op-return)
             (vector-ref message 1))
            ((equal? op op-call)
;;             (pp (list 'sfpc-loop-message= message))
             (sfpc-send
              scheme-fpc-state
              (with-exception-catcher
               (lambda (e)
;;                 (pp (list 'sfpc-loop-got-exception e))
;;                 (print "e=") (display-exception e)
                 (vector op-raise e))
               (lambda ()
                 (let* ((fn (vector-ref message 1))
                        (args (vector-ref message 2))
                        (kw-keys (vector-ref message 3))
                        (kw-vals (vector-ref message 4))
                        (result (apply fn (append (vector->list args) (kwargs->keywords kw-keys kw-vals)))))
                   (vector op-return result)))))
             (loop))
            ((equal? op op-raise)
             (raise (cons (vector-ref message 1) (vector-ref message 2))))
            ((equal? op op-error)
             (error "_pfpc_loop got an unknown message" (vector-ref message 1)))
            (else
             (error "sfpc-loop got an unknown message" message))))))

(define (sfpc-send-recv scheme-fpc-state msg)
  (sfpc-send scheme-fpc-state msg)
  (sfpc-loop scheme-fpc-state))

(define (sfpc-call-with-kw fn args kw-keys kw-vals)
  (let ((scheme-fpc-state (get-scheme-fpc-state!)))
    (sfpc-send-recv scheme-fpc-state (vector op-call fn args kw-keys kw-vals))))

(define (sfpc-call fn args)
  (let ((scheme-fpc-state (get-scheme-fpc-state!)))
    (sfpc-send-recv scheme-fpc-state (vector op-call fn args))))

(define (setup-fpc)

  ;; start dummy thread to prevent deadlock detection (TODO: find a fix)
  (thread-start! (make-thread (lambda () (thread-sleep! +inf.0))))

  (set! scheme-fpc-state-table (make-table test: eq? weak-keys: #t))

  (initialize))

(define (cleanup-fpc)
  (##tty-mode-reset)
  (##c-code "exit(0);") ;; TODO: why does the below cause a segfault?
  (for-each
   cleanup-scheme-fpc-state
   (map car (table->list scheme-fpc-state-table)))
  (finalize))

;;;----------------------------------------------------------------------------

;; Misc

;; (define (PyObject*-register-foreign-write-handler t)
;;   (##readtable-foreign-write-handler-register!
;;    ##main-readtable
;;    t
;;    (lambda (we obj)
;;      (##wr-sn* we obj t PyObject*-wr-str))))

;; (define (PyObject*-wr-str we obj)
;;   (let* ((repr (PyObject_Repr obj))
;;          (s (PyObject*/str->string repr)))
;;     (##wr-str we (string-append " " s))))

;; (define (register-foreign-write-handlers)
;;   (define python-subtypes
;;     '(PyObject*
;;       PyObject*/None
;;       PyObject*/bool
;;       PyObject*/int
;;       PyObject*/float
;;       PyObject*/complex
;;       PyObject*/Fraction
;;       PyObject*/bytes
;;       PyObject*/bytearray
;;       PyObject*/str
;;       PyObject*/list
;;       PyObject*/dict
;;       PyObject*/frozenset
;;       PyObject*/set
;;       PyObject*/tuple
;;       PyObject*/module
;;       PyObject*/type
;;       PyObject*/function
;;       PyObject*/builtin_function_or_method
;;       PyObject*/method
;;       PyObject*/method_descriptor
;;       PyObject*/cell
;;       PyObject*/SchemeObject
;;       ))
;;   (for-each PyObject*-register-foreign-write-handler python-subtypes))

(define (pip-install module)
  (shell-command (string-append VENV-PATH "/bin/pip install " module)))

(define (pip-uninstall module)
  (shell-command (string-append VENV-PATH "/bin/pip uninstall " module)))

;;;----------------------------------------------------------------------------

;; Setup

(setup-fpc)

(define python-eval (sfpc-send-recv (get-scheme-fpc-state!) (vector op-get-eval)))
(define python-exec (sfpc-send-recv (get-scheme-fpc-state!) (vector op-get-exec)))
(define python-SchemeProcedure (python-eval "_SchemeProcedure"))

(python-exec
 (string-append "__import__('sys').path.append('"
                (path-expand (string-append VENV-PATH "/lib/python" PYVER "/site-packages"))
                "')"))

;; (register-foreign-write-handlers)

;; TODO:
;; ratnums
;; foreign-write-handlers

;;;----------------------------------------------------------------------------

;; test

;; (println "(setup-fpc)")


;; (python-exec "def pyfunc(x,y):\n for i in range(y*1000000):\n  pass\n return [x]*y\n")

;; (python-exec "def pymap(f):\n return list(map(f, range(10)))\n")

;; (python-exec "def pytest(f):\n return f(42)\n")

;; (define pyfunc (python-eval "pyfunc"))
;; (define pymap (python-eval "pymap"))
;; (define pytest (python-eval "pytest"))
;; (define python-print (python-eval "print"))

;; (begin

;; (define threads
;;   (map (lambda (i)
;;          (thread-start!
;;           (make-thread
;;            (lambda ()
;;              (pp (pyfunc i i))))))
;;        (iota 10)))
;; (for-each thread-join! threads)
;; )


;; ;;(pp (pytest square)) ;;TODO: debug Python to Scheme calls


;; (println "(cleanup-fpc)")
;; (cleanup-fpc)

;; #|

;; Output when executed:

;; (setup-fpc)
;; ()
;; (1)
;; (2 2)
;; (3 3 3)
;; (5 5 5 5 5)
;; (4 4 4 4)
;; (6 6 6 6 6 6)
;; (8 8 8 8 8 8 8 8)
;; (7 7 7 7 7 7 7)
;; (9 9 9 9 9 9 9 9 9)
;; (cleanup-fpc)

;; |#
