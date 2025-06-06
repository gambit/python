;;;============================================================================

;;; File: "python#.scm"

;;; Copyright (c) 2020-2025 by Marc Feeley, All Rights Reserved.
;;; Copyright (c) 2020-2022 by Marc-André Bélanger, All Rights Reserved.

;;;============================================================================

(##namespace ("github.com/gambit/python#"

;; Constants
Py_eval_input
Py_file_input
Py_single_input

;; Initialization, Finalization, and Threads
Py_Initialize
Py_Finalize

;; These are no longer available:
;;   Py_SetPath
;;   Py_SetProgramName
;;   Py_SetPythonHome
;;   PySys_SetArgv
;;   PySys_SetArgvEx

;; PyRun_*
PyRun_SimpleString
PyRun_String

;; PyImport_*
PyImport_AddModuleObject
PyImport_AddModule
PyImport_ImportModule
PyImport_ImportModuleEx

;; PyModule_*
PyModule_GetDict

;; PyDict_*
PyDict_New
PyDict_Size
PyDict_Items
PyDict_Keys
PyDict_Values
PyDict_GetItem
PyDict_SetItem
PyDict_GetItemString
PyDict_SetItemString

;; PyList_*
PyList_New

;; PyTuple_*
PyTuple_GetItem

;; PyBool_*
PyBool_FromLong

;; PyLong_*
PyLong_FromUnicodeObject

;; PyUnicode_*
PyUnicode_FromString

;; PyObject_*
PyObject_CallMethod
PyObject_GetAttrString
PyObject_Length
PyObject_Repr
PyObject*-type
PyObject*-type-name

;; Call Python callables
PyObject_CallObject
PyObject_CallFunctionObjArgs
PyObject_CallFunctionObjArgs*
PyObject_CallFunctionObjArgs0
PyObject_CallFunctionObjArgs1
PyObject_CallFunctionObjArgs2
PyObject_CallFunctionObjArgs3
PyObject_CallFunctionObjArgs4

;; Converters
PyObject*/None->void
void->PyObject*/None
PyObject*/bool->boolean
boolean->PyObject*/bool
PyObject*/int->exact-integer
exact-integer->PyObject*/int
PyObject*/float->flonum
flonum->PyObject*/float
PyObject*/complex->cpxnum
flonums->PyObject*/complex
PyObject*/Fraction->ratnum
ints->PyObject*/Fraction
PyObject*/str->string
string->PyObject*/str
PyObject*/bytes->u8vector
u8vector->PyObject*/bytes
s8vector->PyObject*/bytes
PyObject*/bytearray->u8vector
u8vector->PyObject*/bytearray
PyObject*/list->vector
vector->PyObject*/list
PyObject*/list->list
list->PyObject*/list
PyObject*/tuple->vector
vector->PyObject*/tuple
PyObject*/tuple->list
list->PyObject*/tuple
PyObject*->object
object->PyObject*
procedure->PyObject*/function
SchemeObject->object
object->SchemeObject

;; Misc
pip-install
pip-uninstall
python-eval
python-exec
cleanup-fpc

scheme

))

;;;============================================================================
