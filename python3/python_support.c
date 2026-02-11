/*----------------------------------------------------------------------------
--                          G N A T C O L L                                 --
--                                                                          --
--                     Copyright (C) 2003-2022, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

/* Force a value for the macro. It will only work for gcc, but otherwise
 * we cannot use the mingwin python with gcc on Windows*/
#define PY_LONG_LONG long long
#include <Python.h>
#include <object.h>
#include <compile.h>  /* PyCodeObject definition in older versions*/
#include <frameobject.h> /* PyFrameObject definition */
#include <string.h>

/* On Windows and if we have HAVE_DECLSPEC_DLL defined remove the
   __declspec(dllexport) attribute from PyMODINIT_FUNC. Having such attribute
   to flag symbols to export from a DLL means that *only* those symbols
   are exported. */
#if _WIN32
#ifdef HAVE_DECLSPEC_DLL
#undef PyMODINIT_FUNC
#define PyMODINIT_FUNC PyObject*
#endif
#endif

#undef DEBUG
/* #define DEBUG */

#ifndef PyDescr_TYPE
#define PyDescr_TYPE(x) (((PyDescrObject *)(x))->d_type)
#define PyDescr_NAME(x) (((PyDescrObject *)(x))->d_name)
#endif

/*****************************************************************************
 * Modules
 *****************************************************************************/

PyMODINIT_FUNC
ada_Py_InitModule4
  (char *name, PyMethodDef *methods,
   char *doc, PyObject *self)
{
   struct PyModuleDef def = {
     PyModuleDef_HEAD_INIT,
     name,                /* m_name */
     doc,                 /* m_doc */
     -1,                  /* m_size */
     methods,             /* m_methods */
     NULL,                /* m_reload */
     NULL,                /* m_traverse */
     NULL,                /* m_clear */
     NULL};               /* m_free */
   struct PyModuleDef* module = (struct PyModuleDef*)
         malloc(sizeof(struct PyModuleDef));
   PyObject* mod;

   memcpy(module, &def, sizeof(struct PyModuleDef));
   mod = PyModule_Create(module);

   return mod;
}

// The definition of the module the user is creating via GNATCOLL.
// There is a single such module, so it is simpler to declare the
// variable as static rather than use calls to malloc().
static PyMethodDef user_methods[] = {
   {NULL, NULL}  /* Sentinel */
};
static struct PyModuleDef user_module = {
     PyModuleDef_HEAD_INIT,
     NULL,                /* m_name */
     NULL,                /* m_doc */
     -1,                  /* m_size */
     user_methods,        /* m_methods */
     NULL,                /* m_reload */
     NULL,                /* m_traverse */
     NULL,                /* m_clear */
     NULL                 /* m_free */
};

static char* user_module_name;

PyMODINIT_FUNC
init_user_module(void) {
   //struct PyModuleDef* module = (struct PyModuleDef*)malloc(sizeof(def));
   //memcpy(module, &def, sizeof(struct PyModuleDef));
   return PyModule_Create(&user_module);
};

//  To hide the output, we also need to rewrite displayhook.
//  Otherwise, calling a python function from Ada will print its
//  output to stdout (even though we have redirected sys.stdout ?)
//  So we make sure that nothing is ever printed. We cannot do this
//  systematically though, since in interactive mode (consoles...)
//  we still want the usual python behavior.

PyObject*
ada_py_initialize_and_module(char* program_name, char* name) {
   PyObject* module;
   PyObject* imported;

   user_module_name = strdup(name);

   user_module.m_name = user_module_name;
   Py_SetProgramName (Py_DecodeLocale (program_name, NULL));

   PyStatus status;
   PyPreConfig preconfig;

   PyPreConfig_InitPythonConfig(&preconfig);

   preconfig.utf8_mode = 1;

   status = Py_PreInitialize(&preconfig);
   if (PyStatus_Exception(status)) {
     Py_ExitStatusException(status);
   }

   PyImport_AppendInittab(user_module_name, init_user_module);
   Py_InitializeEx(0);

   // Initialize the prompt if needed

   PyObject* prompt = PySys_GetObject ("ps1");
   if (prompt == NULL) {
      prompt = PyUnicode_FromString (">>> ");
      PySys_SetObject ("ps1", prompt);
      Py_DECREF (prompt);
   }

   prompt = PySys_GetObject ("ps2");
   if (prompt == NULL) {
      prompt = PyUnicode_FromString ("... ");
      PySys_SetObject ("ps2", prompt);
      Py_DECREF (prompt);
   }

   // Make the user's module visible to scripts. We cannot use
   // PyImport_ImportModule, which imports the module but doesn't add
   // it to the global dictionary and as such it is not visible to
   // user scripts.

   imported = PyImport_ImportModule(name);
   if (imported == NULL) {
      printf ("Could not import module %s", name);
      return NULL;
   }

   // Import 'sys', which is needed for instance in Set_Default_Console
   // to get access to the default value
   PyRun_SimpleString("import sys\n");

   char* command = (char*)malloc(9 + strlen(name));
   strcpy (command, "import ");
   strcat (command, name);
   strcat (command, "\n");
   PyRun_SimpleString(command);
   free (command);

   return imported;
};

/************************************************************************
 * Methods
 * To implement methods, we have the following requirements:
 *    - we need to support the notion of bound methods in python (where self
 *      is set automatically by python to the instance that calls the method).
 *    - we need to pass data back to Ada, that was set when the method was
 *      declared. This data describes how the method is implemented in Ada.
 * The implementation is based on Python descriptors. However, none of the
 * predefined descriptors provides support for passing data back to Ada.
 * So we define our own descriptor, heavily based on the predefined one.
 *
 * From python, when you do a.foo(), the following occurs behind the scene:
 *   - retrieves "A.foo", as a PyAdaMethodDescrObject
 *   - since this is a descriptor, calls  .__get__() to get the function
 *     to execute. In practice, this calls adamethod_descr_get which
 *     creates a bound method through PyMethod_New (bound to 'a')
 *   - call that object. The implementation of classobject.c::method_call
 *     adds self, ie 'a', as the first argument in the tuple of arguments,
 *     then executes the wrapped function. Here, the wrapped function is
 *     a PyCFunction that was created when the method was registered
 *     initially, and that always calls back Ada but always passes the
 *     same 'self' argument (the data Ada itself provided).
 ************************************************************************/

typedef struct {
  PyDescr_COMMON;
  PyObject *cfunc; // An instance of PyCFunction, bound with the
                   // data that Ada needs, in the form of a PyCapsule
  PyMethodDef *def;
} PyAdaMethodDescrObject;

static PyObject *adamethod_descr_call(PyAdaMethodDescrObject *descr,
                                      PyObject *arg, PyObject *kw) {
    return PyObject_Call((PyObject *)descr->cfunc, arg, kw);;
}

// Implementation of the __get__ descriptor method. The code is heavily
// copied from descrobject.c::method_get.

static PyObject * adamethod_descr_get
   (PyAdaMethodDescrObject *descr, PyObject *obj, PyObject *type)
{
  PyObject *res;
  if (obj == NULL) {
    Py_INCREF(descr);
    return (PyObject*) descr;
  }
  if (!PyObject_TypeCheck(obj, PyDescr_TYPE(descr))) {
    PyErr_Format(PyExc_TypeError,
                 "descriptor '%V' for '%s' objects "
                 "doesn't apply to '%s' object",
                 PyDescr_NAME(descr), "?",
                 PyDescr_TYPE(descr)->tp_name,
                 Py_TYPE(obj)->tp_name);
    return NULL;
  }
  return PyMethod_New (descr->cfunc, obj);
}

// Implementation of the __getattro__ descriptor method. Manually compute
// attributes like __name__ and __qualname__ because cfunc/def are hidden
// inside PyAdaMethodDescrObject.

static PyObject *
ada_descr_getattro(PyObject *self, PyObject *name)
{
  PyAdaMethodDescrObject *descr = (PyAdaMethodDescrObject *)self;
  PyObject *descr_type = (PyObject *)PyDescr_TYPE(descr);

    if (PyUnicode_Check(name)) {

        if (PyUnicode_CompareWithASCIIString(name, "__name__") == 0) {
            return PyUnicode_FromString(descr->def->ml_name);
        }

        if (PyUnicode_CompareWithASCIIString(name, "__qualname__") == 0) {
            PyObject *type_qn =
                PyObject_GetAttrString(
                    descr_type,
                    "__qualname__");

            if (!type_qn)
                return NULL;

            PyObject *res = PyUnicode_FromFormat(
                "%U.%s", type_qn, descr->def->ml_name);

            Py_DECREF(type_qn);
            return res;
        }

        if (PyUnicode_CompareWithASCIIString(name, "__objclass__") == 0) {
            Py_INCREF(descr_type);
            return descr_type;
          }
    }

    return PyObject_GenericGetAttr(self, name);
}

static PyTypeObject PyAdaMethodDescr_Type = {
  .ob_base = PyVarObject_HEAD_INIT (NULL, 0).tp_name = "ada_method_descriptor",
  .tp_basicsize = sizeof (PyAdaMethodDescrObject),
  .tp_descr_get = (descrgetfunc)adamethod_descr_get,
  .tp_getattro = ada_descr_getattro,
  .tp_call = (ternaryfunc)adamethod_descr_call,
  .tp_flags = Py_TPFLAGS_DEFAULT && !(Py_TPFLAGS_HAVE_VECTORCALL),
  .tp_vectorcall = NULL,
  .tp_new = PyType_GenericNew,
};

// Creates a new AdaMethod instance. 'method' is the description of the Ada
// function to call, and 'data' is a PyCapsule that is passed to Ada as 'self'.

PyObject *
PyDescr_NewAdaMethod (PyTypeObject *type, PyMethodDef *def, PyObject *cfunc,
                      const char *name)
{
  PyAdaMethodDescrObject *descr = (PyAdaMethodDescrObject*) PyType_GenericAlloc
    (&PyAdaMethodDescr_Type, 0);

  if (descr != NULL) {
    Py_XINCREF(type);
    PyDescr_TYPE (descr) = type;

    PyDescr_NAME(descr) = PyUnicode_InternFromString(name);
    if (PyDescr_NAME(descr) == NULL) {
      Py_DECREF(descr);
      descr = NULL;
    }
  }

  if (descr != NULL) {
      descr->cfunc = cfunc;
      descr->def = def;
  }

  return (PyObject *)descr;
}

// Adds a new method to the class 'class'.
// 'module' is the module to which the class belongs, and is used to set
//    the __module__ attribute of the new method.
// 'def' described the C function that will be called when the function is
//    executed in python.
// 'data' is data to pass from C->Python->C (generally wrapped in a PyCapsule).
//    It will be pass as the "Self" argument to First_Level.

void ada_py_add_method
   (PyMethodDef* def, PyObject* data, PyObject* class, PyObject* module)
{
  PyObject* cfunc = PyCFunction_NewEx
    (def, data, PyUnicode_FromString (PyModule_GetName (module)));

  PyObject* method = PyDescr_NewAdaMethod
    ((PyTypeObject*)class, def, cfunc, def->ml_name);

  PyObject_SetAttrString (class, def->ml_name, method);
  Py_DECREF (method);
};

/*****************************************************************************/

int
ada_pyget_refcount (PyObject* obj)
{
   return Py_REFCNT(obj);
}

char*
ada_py_refcount_msg (PyObject* obj)
{
   static char msg[200];
   if (obj) {
      snprintf (msg, 199, "%p (%s, rc=%ld)",
                obj, Py_TYPE(obj)->tp_name, Py_REFCNT(obj));
   } else {
      msg[0] = '\0';
   }
   return msg;
}

void ada_py_print_refcount(PyObject* obj, char* msg) {
  if (obj)
    printf ("DEBUG %s %s\n", msg, ada_py_refcount_msg (obj));
}

void
ada_py_incref (PyObject* obj)
{
  Py_INCREF (obj);
#ifdef DEBUG
  ada_py_print_refcount (obj, "after incref");
#endif
}

void
ada_py_decref (PyObject* obj)
{
#ifdef DEBUG
  ada_py_print_refcount (obj, "before decref");
#endif
  Py_DECREF (obj);
}

void
ada_py_xincref (PyObject* obj)
{
  Py_XINCREF (obj);
#ifdef DEBUG
  ada_py_print_refcount (obj, "after xincref");
#endif
}

void
ada_py_xdecref (PyObject* obj)
{
#ifdef DEBUG
  ada_py_print_refcount (obj, "before xdecref");
#endif
  Py_XDECREF (obj);
}

PyTypeObject*
__gnatcoll_py_type(PyObject *obj)
{
  return (PyTypeObject*) (Py_TYPE (obj));
}

int
ada_pybasestring_check (PyObject* obj)
{
  return PyUnicode_Check (obj);
}

int
ada_pystring_check (PyObject* obj)
{
  return PyUnicode_Check (obj);
}

PyObject* ada_PyUnicode_AsEncodedString
  (PyObject *unicode, const char *encoding, const char *errors)
{
  //  A macro in python2.
  return PyUnicode_AsEncodedString (unicode, encoding, errors);
}

PyObject* ada_PyUnicode_FromString (const char *u)
{
  //  A macro in python2.
  return PyUnicode_FromString (u);
}

int
ada_pyunicode_check (PyObject* obj)
{
  return PyUnicode_Check (obj);
}

int
ada_pyint_check (PyObject* obj)
{
  //  Not available anymore.
  return PyLong_Check (obj);
}

//  May be a macro.
PyAPI_FUNC(int) ada_pylong_check (PyObject* obj) {
  return PyLong_Check (obj);
}

int
ada_pyfloat_check (PyObject* obj)
{
  return PyFloat_Check (obj);
}

int
ada_pybool_check (PyObject* obj)
{
#ifdef PyBool_Check
  return PyBool_Check (obj);
#else
  return 0;
#endif
}

int
ada_pybool_is_true (PyObject* obj)
{
  return PyObject_IsTrue (obj);
}

int
ada_pydict_check (PyObject* obj)
{
  return PyDict_Check (obj);
}

int
ada_pyanyset_check (PyObject* obj)
{
  return PyAnySet_Check (obj);
}

int
ada_pyfunction_check (PyObject* obj)
{
  return PyFunction_Check (obj);
}

PyObject*
ada_pyfunction_get_globals (PyObject* obj)
{
  return PyFunction_GET_GLOBALS (obj);
}

PyObject*
ada_pyfunction_get_code (PyObject* obj)
{
  return PyFunction_GET_CODE (obj);
}

PyObject*
ada_pyfunction_get_closure (PyObject* obj)
{
  return PyFunction_GET_CLOSURE (obj);
}

PyObject*
ada_pyfunction_get_defaults (PyObject* obj)
{
  return PyFunction_GET_DEFAULTS (obj);
}

PyObject* ada_PyEval_EvalCodeEx
  (PyCodeObject *co,
   PyObject *globals,
   PyObject *locals,
   PyObject *args,
   PyObject *kwds,
   PyObject *defs,
   PyObject *closure)
{
   /* Code copied from funcobject.c::function_call() */

  PyObject **k, **d;
  PyObject* result;
  PyObject* kwtuple;
  int nk, nd;

  if (defs != NULL && PyTuple_Check(defs)) {
     d = &PyTuple_GET_ITEM((PyTupleObject *)defs, 0);
     nd = PyTuple_Size(defs);
  } else {
     d = NULL;
     nd = 0;
  }

  if (kwds != NULL && PyDict_Check(kwds)) {
     int i = 0;
     Py_ssize_t pos = 0;

     nk = PyDict_Size(kwds);
     kwtuple = PyTuple_New(2*nk);
     if (kwtuple == NULL)
       return NULL;
     k = &PyTuple_GET_ITEM(kwtuple, 0);
     pos = i = 0;
     while (PyDict_Next(kwds, &pos, &k[i], &k[i+1])) {
       Py_INCREF(k[i]);
       Py_INCREF(k[i+1]);
       i += 2;
     }
     nk = i/2;
  } else {
     k = NULL;
     nk = 0;
  }

  result = (PyObject*) PyEval_EvalCodeEx
    ((PyObject*) co,
     globals, locals,
     &PyTuple_GET_ITEM (args, 0) /* args */, PyTuple_Size (args) /* argc*/,
     k /* kwds */, nk /* kwdc */,
     d /* defs */, nd /* defcount */,
     NULL, /* kwdefs */
     closure /* closure */);

  Py_XDECREF (kwtuple);
  return result;
}

int
ada_pycobject_check (PyObject* obj)
{
  return PyCapsule_CheckExact (obj);
}

int
ada_pytuple_check (PyObject* obj)
{
  return PyTuple_Check (obj);
}

int
ada_pylist_check (PyObject* obj)
{
  return PyList_Check (obj);
}

int
ada_pyiter_check (PyObject* obj)
{
  return PyIter_Check (obj);
}

int
ada_pymethod_check (PyObject* obj)
{
  return PyMethod_Check (obj);
}

char*
ada_tp_name (PyTypeObject* obj)
{
  return (char *)obj->tp_name;
}

PyObject* ada_py_none ()
{
  return Py_None;
}

PyObject* ada_py_false()
{
  return Py_False;
}

PyObject*
ada_py_true()
{
  return Py_True;
}

PyObject *
ada_py_object_callmethod (PyObject *o, char *m)
{
  return PyObject_CallMethod (o, m, "");
}

PyObject *
ada_py_object_callmethod_obj (PyObject *o, char *m, PyObject *arg)
{
  return PyObject_CallMethod (o, m, "(O)", arg);
}

PyObject *
ada_py_object_callmethod_int (PyObject *o, char *m, int arg)
{
  return PyObject_CallMethod (o, m, "(i)", arg);
}

int
ada_py_arg_parsetuple_ptr (PyObject *o, char *fmt, void *arg1)
{
  return PyArg_ParseTuple (o, fmt, arg1);
}

int
ada_py_arg_parsetuple_ptr2 (PyObject *o, char *fmt, void *arg1, void *arg2)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2);
}

int
ada_py_arg_parsetuple_ptr3
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2, arg3);
}

int
ada_py_arg_parsetuple_ptr4
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3, void *arg4)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4);
}

int
ada_py_arg_parsetuple_ptr5
  (PyObject *o, char *fmt,
   void *arg1, void * arg2, void *arg3, void *arg4, void *arg5)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4, arg5);
}

extern int gnat_argc;
extern char **gnat_argv;

int
__gnatcoll_py_main ()
{
  wchar_t *w_gnat_argv[gnat_argc];
  int result;

  for (int i=0; i<gnat_argc; i++) {
    w_gnat_argv[i] = Py_DecodeLocale(gnat_argv[i], NULL);
  }

  result = Py_Main (gnat_argc, w_gnat_argv);

  for (int i=0; i<gnat_argc; i++) {
    PyMem_RawFree((void *) w_gnat_argv[i]);
  }

  return result;
}

PyObject*
ada_type_new (PyTypeObject* meta, char* name, PyObject* bases, PyObject* dict)
{
  PyTypeObject* m = meta;
  PyObject *args, *kwargs, *b=NULL;
  PyObject* result;
  PyObject* str;

  if (dict == NULL) {
    printf ("ada_type_new requires a non-null dict\n");
    return NULL;
  }

  if (meta == NULL) {
    m = &PyType_Type;
  }

  /* Construct the parameter list. Do not use keyword arguments, since the
     __init__ of the builtin types do not accept them, and tp_new will try to
     call __init__, resulting in an error
   */

  args   = PyTuple_New (3);
  kwargs = PyDict_New ();

  str = PyUnicode_FromString (name);

  PyTuple_SET_ITEM (args, 0, str);    /* steal reference to str */

  if (bases == NULL) {
    b = PyTuple_New (0);
    PyTuple_SET_ITEM (args, 1, b);  /* steal ref to b */
  } else {
    PyTuple_SetItem (args, 1, bases);  /* increase refcount for bases */
  }

  PyTuple_SetItem (args, 2, dict); /* increase refcount for dict */

  result = PyType_Type.tp_new (m, args, kwargs);

  Py_XDECREF (args);
  Py_XDECREF (kwargs);

  return result;
}

int
ada_pydescr_newGetSet (PyTypeObject* type,
		       char*         name,
		       setter        set,
		       getter        get,
		       char*         doc,
		       void*         closure)
{
  struct PyGetSetDef *descr =
     (struct PyGetSetDef*)malloc (sizeof (struct PyGetSetDef));
  PyObject* prop;

  descr->name    = name;
  descr->get     = get;
  descr->set     = set;
  descr->doc     = doc;
  descr->closure = closure;

  prop = PyDescr_NewGetSet (type, descr);
  if (prop == NULL) {
    return 0;
  } else {
    PyDict_SetItemString (type->tp_dict, name, prop);
    Py_DECREF (prop);
    return 1;
  }
}

PyThreadState* ada_PyGILState_GetThisThreadState() {
#ifdef WITH_THREAD
   return PyGILState_GetThisThreadState();
#else
   return NULL;
#endif
}

int ada_PyGILState_Ensure() {
  if (Py_IsInitialized ()) {
#ifdef WITH_THREAD
    return (int)PyGILState_Ensure();
#else
    return 0;
#endif
  }
  return 0;
}

void ada_PyGILState_Release(PyGILState_STATE state) {
#ifdef WITH_THREAD
  if (Py_IsInitialized ()) {
    PyGILState_Release((PyGILState_STATE)state);
  }
#endif
}

int
ada_is_subclass (PyObject* class, PyObject* base)
{
  if (!class || !base) {
    return -1;
  } else {
    return PyObject_IsSubclass (class, base);
  }
}

const char* ada_py_builtin() {
   return "builtins";
}

const char* ada_py_builtins() {
   return "__builtins__";
}

/* Result value must be freed */

PyAPI_FUNC(const char *) ada_PyString_AsString(PyObject * val) {

   PyObject* utf8 = PyUnicode_AsUTF8String(val);
   char* tmp = PyBytes_AsString (utf8);
   char* str = strdup (tmp);
   Py_XDECREF(utf8);
   return str;

};

PyAPI_FUNC(PyObject *) PyInt_FromLong(long val) {
   return PyLong_FromLong(val);
};

PyAPI_FUNC(PyObject *) PyInt_FromSize_t(size_t val) {
   return PyLong_FromSize_t(val);
};

PyAPI_FUNC(long) PyInt_AsLong(PyObject * val) {
   return PyLong_AsLong(val);
};

PyAPI_FUNC(PyObject *) PyString_FromStringAndSize(
      const char *val, Py_ssize_t s)
{
   return PyUnicode_FromStringAndSize(val, s);
};

PyAPI_FUNC(PyObject *) PyFile_FromString
  (const char *file_name, const char *mode)
{
  PyObject * io = PyImport_ImportModule ("io");
  if (io == NULL) {
    return NULL;
  }
  return PyObject_CallMethod (io, "open", "ss", file_name, mode);
}

PyCodeObject*
ada_pyframe_get_code (PyFrameObject* obj)
{
#if PY_MAJOR_VERSION > 3 || (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION > 8)
   return PyFrame_GetCode(obj);
#else
   return obj->f_code;
#endif
}

PyFrameObject*
ada_pyframe_get_back (PyFrameObject* obj)
{
#if PY_MAJOR_VERSION > 3 || (PY_MAJOR_VERSION == 3 && PY_MINOR_VERSION > 8)
   return PyFrame_GetBack(obj);
#else
   return obj->f_back;
#endif
}

PyObject*
ada_pycode_get_filename (PyCodeObject* obj)
{
   return obj->co_filename;
}

PyObject*
ada_pycode_get_name (PyCodeObject* obj)
{
   return obj->co_name;
}
