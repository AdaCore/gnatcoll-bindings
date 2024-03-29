------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2021, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Standard interface to the python interpreter.
--  This requires at least python 3.7 to be installed on your system.

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;

package GNATCOLL.Python is

   -------------
   -- Objects --
   -------------

   type PyObject_Opaque is limited private;
   type PyObject is access all PyObject_Opaque;
   pragma Convention (C, PyObject);

   type PyObject_Array is array (Natural range <>) of PyObject;

   function Py_None return PyObject;
   --  Return the python's variable Py_None, which should be returned by
   --  procedures. Generally, one need to call Py_INCREF before returning this
   --  value.

   type PyCodeObject is new PyObject;
   type PyFrameObject is new PyObject;

   procedure Py_INCREF (Obj : PyObject);
   procedure Py_DECREF (Obj : PyObject);
   --  Increment or decrement the reference count for Obj. Obj mustn't be null

   procedure Py_XINCREF (Obj : PyObject);
   procedure Py_XDECREF (Obj : PyObject);
   --  Same as above, but Obj can be null

   procedure Print_Refcount (Obj : PyObject; Msg : String);
   --  A debug procedure that prints the reference count of the object on
   --  stdout.

   function Get_Refcount (Obj : PyObject) return Integer;
   --  Return the current reference counter for Obj. Used for debug only

   function PyObject_Str (Obj : PyObject) return PyObject;
   --  Compute the string representation of Obj.  Returns the string
   --  representation on success, NULL on failure.  This is the equivalent of
   --  the Python expression "str(obj)".
   --  This is the equivalent of the python call str(obj), and is used by
   --  python in print statements.
   --  Returned value must be Py_DECREF

   function PyObject_Repr (Obj : PyObject) return PyObject;
   --  Similar to PyObject_Str, ie provides a displayable version of Obj. This
   --  is the equivalent of the python call repr(obj), and is used by python
   --  in backquotes.
   --  Returned value must be Py_DECREF

   function PyObject_CallMethod
     (Object : PyObject; Name : String) return PyObject;
   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : PyObject) return PyObject;
   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : Integer) return PyObject;
   --  A few examples of functions to call a method.
   --  In C, the profile of this method is:
   --     PyObject* PyObject_CallMethod
   --        (PyObject* object, char* name, char* format, ...);
   --  For instance, to call it with an object and a integer as a parameter,
   --  you would use:
   --    result = PyObject_CallMethod (object, "method", "(Oi)", other_obj, 1);
   --  except that due to ABI differences, you need to use a C wrapper,
   --  otherwise things will break on e.g. x86_64
   --
   --  format has the same form as in the calls to Py_BuildValue

   function PyObject_Call
     (Object : PyObject; Args : PyObject; Kw : PyObject) return PyObject;
   --  Call a callable Python object, Object, with
   --  arguments and keywords arguments.  The 'args' argument can not be
   --  NULL, but the 'kw' argument can be NULL.
   --  The returned object must be DECREF

   function PyObject_CallObject
     (Object : PyObject; Args : PyObject) return PyObject;
   pragma Import (C, PyObject_CallObject, "PyObject_CallObject");
   --  Call a callable Python object, callable_object, with
   --  arguments given by the tuple, args.  If no arguments are
   --  needed, then args may be NULL.  Returns the result of the
   --  call on success, or NULL on failure.  This is the equivalent
   --  of the Python expression: apply(o,args).

   function PyObject_SetAttrString
     (Object : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr;
      Attr   : PyObject) return Integer;
   pragma Import (C, PyObject_SetAttrString, "PyObject_SetAttrString");
   --  Set the value of the attribute named Name, for Object, to the value
   --  Attr. Returns -1 on failure. This is the equivalent of the Python
   --  statement "Object.Name = Attr".

   function PyObject_SetAttrString
     (Obj : PyObject; Attr_Name : String; Value : PyObject) return Integer;
   procedure PyObject_SetAttrString
     (Obj : PyObject; Attr_Name : String; Value : PyObject);
   --  Same as above

   function PyObject_GenericSetAttr
     (Object : PyObject;
      Name   : PyObject;
      Attr   : PyObject) return Integer;
   pragma Import (C, PyObject_GenericSetAttr, "PyObject_GenericSetAttr");
   --  Generic attribute setter that directly interface with the object's
   --  __dict__, not with its __setattr__ method.
   --  Name must be decref-ed by the caller.

   function PyObject_GenericSetAttrString
     (Object : PyObject;
      Name   : String;
      Attr   : PyObject) return Integer;
   --  Same as above, but accepts a string as parameter

   function PyObject_HasAttrString
     (Obj : PyObject; Attr_Name : String) return Boolean;
   --  Whether a specific attribute exists for the object

   function PyObject_GetAttrString
     (Object : PyObject; Name : String) return PyObject;
   --  Lookup an attribute in the object's dictionnary.
   --  The returned object *must* be DECREF.

   function PyObject_Dir (Object : PyObject) return PyObject;
   --  A list of strings for all entries in Object's dictionary..
   --  The returned object must be DECREF.

   function PyObject_IsTrue (Obj : PyObject) return Boolean;
   --  Returns True if the object, Obj, is considered to be true, False if Obj
   --  is considered to be false. This is equivalent to the Python expression:
   --  "not not obj"

   --------------
   -- Integers --
   --------------

   function PyLong_FromLong (Value : Interfaces.C.long) return PyObject;
   pragma Import (C, PyLong_FromLong, "PyLong_FromLong");
   --  Create a new integer object from its value

   function PyLong_FromSize_t (Value : Interfaces.C.size_t) return PyObject;
   pragma Import (C, PyLong_FromSize_t, "PyLong_FromSize_t");
   --  Create a new integer object from Value

   function PyLong_AsLong (Int : PyObject) return Interfaces.C.long;
   pragma Import (C, PyLong_AsLong, "PyLong_AsLong");
   --  Return the value of Int.
   --  Return -1 and set PyErr_Occurred if Int is not an integer object.

   function PyLong_Check (Obj : PyObject) return Boolean
     with Inline;
   --  Returns true if the Obj is an integer object

   --  Not bound: PyInt_FromString and PyInt_FromUnicode

   function PyInt_FromLong (Value : Interfaces.C.long) return PyObject;
   --  Create a new integer object from its value
   --  Python3 removes this function. Use PyLong_FromLong instead.

   function PyInt_FromSize_t (Value : Interfaces.C.size_t) return PyObject;
   --  Create a new integer object from Value
   --  Python3 removes this function. Use PyLong_FromSize_t instead.

   function PyInt_AsLong (Int : PyObject) return Interfaces.C.long;
   --  Return the value of Int.
   --  Return -1 and set PyErr_Occurred if Int is not an integer object.
   --  Python3 removes this function. Use PyLong_AsLong instead.

   function PyInt_GetMax return Interfaces.C.long;
   --  Return the maximum value an integer can have
   --  Python3 removes this function. It has no PyLong equivalent.

   function PyInt_Check (Obj : PyObject) return Boolean;
   --  Returns true if the Obj is an integer object
   --  Python3 removes this function. Use PyLong_Check instead.

   ------------
   -- Floats --
   ------------

   function PyFloat_AsDouble (Float : PyObject) return Interfaces.C.double;
   --  Return the value of Float

   function PyFloat_Check (Obj : PyObject) return Boolean;
   --  Returns true if the Obj is a float object

   function PyFloat_FromDouble (Value : Interfaces.C.double) return PyObject;
   pragma Import (C, PyFloat_FromDouble, "PyFloat_FromDouble");
   --  Creates a new float object

   --------------
   -- Booleans --
   --------------
   --  Support for the "bool" type. However, older versions of python do not
   --  support this type, so you should also always check for PyInt_Check at
   --  the same time

   function PyBool_Check (Obj : PyObject) return Boolean;
   --  Returns true if Obj is a boolean object

   function PyBool_Is_True (Obj : PyObject) return Boolean;
   --  Obj must return True for PyBool_Check. This function returns True if
   --  obj is True.

   function PyBool_FromBoolean (Value : Boolean) return PyObject;
   --  Create a new boolean object

   ------------
   -- Tuples --
   ------------
   --  The following subprograms are in fact simple examples of importing the C
   --  function in your C code, depending on your exact requirement. In C,
   --  these are function with unknown number of parameters
   --
   --  The C function is:
   --      int PyArg_ParseTuple(PyObject *arg, char *format, ...);

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4 : System.Address) return Boolean;
   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4, Value5 : System.Address) return Boolean;
   --  Parses Format, and stores each of the tuple element in each of the
   --  Values. The number of elements in Format must be the same as the number
   --  of Value parameter
   --  The exact description of the format should be found in the python
   --  documentation.
   --  Note: there is *no* type safety in these functions, but then neither is
   --  there in C.

   subtype PyTuple is PyObject;

   function PyTuple_New (Size : Integer) return PyObject;
   --  Create a new tuple that contains Size elements

   function PyTuple_GetItem (Tuple : PyTuple; Index : Integer) return PyObject;
   --  Get the item at a specific location in the tuple, starting at index 0.
   --  Do not decref returned value.
   --  See also PyObject_GetItem

   procedure PyTuple_SetItem
     (Tuple : PyTuple; Index : Integer; Value : PyObject);
   --  Set an item in the tuple. The reference counting of Value is not
   --  increased
   --  See also PyObject_SetItem

   function PyTuple_Size (Tuple : PyTuple) return Integer;
   pragma Obsolescent (PyTuple_Size, "See PyObject_Size instead");
   --  Return the size of the tuple

   function Create_Tuple (Objects : PyObject_Array) return PyObject;
   --  Return a new tuple made of Objects

   function PyTuple_Check (Obj : PyObject) return Boolean;
   --  Whether Object is a tuple

   -----------
   -- Lists --
   -----------

   function PyList_New (Size : Integer := 0) return PyObject;
   --  Create a new empty list, with an initialize size

   function PyList_Append (List : PyObject; Obj : PyObject) return Integer;
   --  Append Obj at the end of List, and return the index of the newly
   --  inserted item.
   --  Increased Obj's refcount

   function PyList_GetItem (List : PyObject; Index : Integer) return PyObject;
   pragma Obsolescent (PyList_GetItem, "See PyObject_GetItem instead");
   --  Get the item at a specific location in the list, starting at index 0.
   --  Do not decref the returned value.
   --  See also PyObject_GetItem.

   function PyList_Size (List : PyObject) return Integer;
   pragma Obsolescent (PyList_Size, "See PyObject_Size instead");
   --  Return the number of items in the list

   function PyList_Check (Obj : PyObject) return Boolean;
   --  True if Obj is a python list

   ---------------
   -- Iterators --
   ---------------
   --  Iterators are an extension to list and tuples, and encapsulate both, in
   --  addition to user-defined types that have a __iter__ method.

   function PyIter_Check (Obj : PyObject) return Boolean;
   --  True if object is an iterator (as returned by PyObject_GetIter)

   function PyObject_GetIter (Obj : PyObject) return PyObject;
   pragma Import (C, PyObject_GetIter, "PyObject_GetIter");
   --  This is equivalent to the Python expression iter(o). It returns a new
   --  iterator for the object argument, or the object itself if the object is
   --  already an iterator. Raises TypeError and returns NULL if the object
   --  cannot be iterated.

   function PyObject_Size (Obj : PyObject) return Integer;
   pragma Import (C, PyObject_Size, "PyObject_Size");
   --  Return the length of object o. If the object o provides either the
   --  sequence and mapping protocols, the sequence length is returned. On
   --  error, -1 is returned. This is the equivalent to the Python expression
   --  len(o).

   function PyObject_GetItem (Obj, Key : PyObject) return PyObject;
   pragma Import (C, PyObject_GetItem, "PyObject_GetItem");
   --  Returns a new reference
   --  Return element of o corresponding to the object key or NULL on failure.
   --  This is the equivalent of the Python expression o[key].

   function PyObject_GetItem (Obj : PyObject; Key : Integer) return PyObject;
   --  A special case where the key is an integer

   function PyObject_SetItem (Obj, Key, Value : PyObject) return Integer;
   pragma Import (C, PyObject_SetItem, "PyObject_SetItem");
   --  Map the object key to the value v. Returns -1 on failure. This is the
   --  equivalent of the Python statement o[key] = v.

   procedure PyObject_SetItem
     (Obj : PyObject; Key : Integer; Value : PyObject);
   --  A special case where the key is an integer

   function PyIter_Next (Obj : PyObject) return PyObject;
   pragma Import (C, PyIter_Next, "PyIter_Next");
   --  Return the next value from the iteration o. If the object is an
   --  iterator, this retrieves the next value from the iteration, and returns
   --  NULL with no exception set if there are no remaining items. If the
   --  object is not an iterator, TypeError is raised, or if there is an error
   --  in retrieving the item, returns NULL and passes along the exception.
   --
   --  To write a loop which iterates over an iterator, the code should look
   --  something like this:
   --
   --     Iterator : PyObject := PyObject_GetIter (Obj);
   --     Item     : PyObject;
   --
   --     if Iterator = null then
   --         --  propagate error
   --     else
   --         loop
   --            Item := PyIter_Next (Iterator);
   --            exit when Item = null;
   --
   --            Py_DECREF (Item);
   --         end loop;
   --
   --         Py_DECREF (Iterator);
   --     end if;

   -------------
   -- Strings --
   -------------

   function PyBaseString_Check (Obj : PyObject) return Boolean;
   --  Returns True if Obj is either a string or a unicode object

   function PyString_Check (Obj : PyObject) return Boolean;
   --  Returns true if the Obj is a string object

   function PyString_AsString (Str : PyObject) return String;
   --  Same as above, higher-level

   function PyString_FromString (Str : String) return PyObject;
   --  Return a python object representing Str

   function PyUnicode_Check (Obj : PyObject) return Boolean;

   function PyUnicode_FromString (Str : String) return PyObject;
   --  A Unicode string, from a latin-1 encoded Ada string

   function Unicode_AsString
     (Str : PyObject; Encoding : String := "utf-8") return String;
   --  Return an encoded version of Str.
   --  This is not a function from python, but a wrapper around
   --  PyUnicode_AsEncodedString and PyString_AsString.
   --  In case of encoding error, characters are replaced with '?'

   type Unicode_Error_Handling is (Strict, Ignore, Replace);
   --  How encoding errors are treated for unicode objects
   --    Strict: raise a ValueError
   --    Ignore: ignore the wrong characters, which are skipped
   --    Replace: replace illegal characters with '?'

   function PyUnicode_AsEncodedString
     (Unicode  : PyObject;   --  A unicode object
      Encoding : String;     --  The encoding
      Errors   : Unicode_Error_Handling := Strict)     --  Error handling
      return PyObject;
   --  Encodes a Unicode object and returns the result as Python string object.
   --  You can use PyString_AsString to get the corresponding Ada string.

   -------------
   -- Modules --
   -------------

   function PyImport_AddModule (Module_Name : String) return PyObject;
   --  Return the module object corresponding to a module name.  The name
   --  argument may be of the form package.module.  First check the modules
   --  dictionary if there's one there, and if not, create a new one and insert
   --  in in the modules dictionary.  Because the former action is most common,
   --  this does not return a new reference, and you do not own the returned
   --  reference.
   --
   --  Warning: this function does not load or import the module; if the module
   --  wasn't already loaded, you will get an empty module object.  Use
   --  PyImport_ImportModule() or one of its variants to import a module.
   --  Return NULL with an exception set on failure.

   function PyImport_ImportModule (Module_Name : String) return PyObject;
   --  Import a new module in the interpreter

   function PyImport_Import (Name : PyObject) return PyObject;
   pragma Import (C, PyImport_Import, "PyImport_Import");
   --  Higher-level import emulator which emulates the "import" statement
   --  more accurately -- it invokes the __import__() function from the
   --  builtins of the current globals.  This means that the import is
   --  done using whatever import hooks are installed in the current
   --  environment, e.g. by "rexec".
   --  A dummy list ["__doc__"] is passed as the 4th argument so that
   --  e.g. PyImport_Import(PyString_FromString("win32com.client.gencache"))
   --  will return <module "gencache"> instead of <module "win32com">. */

   function PyModule_GetDict (Module : PyObject) return PyObject;
   --  Return the dictionary object that implements module's namespace; this
   --  object is the same as the __dict__ attribute of the module object.  This
   --  function never fails.
   --  It is recommended that you use the other PyModule_* subprograms rather
   --  than manipulate this dictionnary directly.
   --  The returned dictionary is a borrow reference, so you shouldn't
   --  Py_DECREF it.

   function PyModule_New (Module_Name : String) return PyObject;
   --  Create a new module.
   --  Use the PyModule_GetDic function to add new objects to the module, or
   --  better use PyModule_AddObject.

   function PyModule_AddObject
     (Module : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr;
      Object : PyObject) return Integer;
   pragma Import (C, PyModule_AddObject, "PyModule_AddObject");
   --  Add a new object to the module's directory. Object can be a subprogram,
   --  integer, ... Do not Py_DECREF Object afterward, this is only a borrowed
   --  reference.
   --  Return 0 in case of success, -1 in case of error.
   --  Name can be freed immediately by the caller

   function PyModule_AddObject
     (Module : PyObject; Name : String;  Object : PyObject) return Integer;
   --  Same as above

   function PyModule_Getname (Module : PyObject) return String;
   --  Return the name of the module

   ----------------------------------
   -- Creating modules and methods --
   ----------------------------------

   type Argument_Methods is mod 2 ** Integer'Size;
   METH_VARGS    : constant Argument_Methods := 16#0001#;
   METH_KEYWORDS : constant Argument_Methods := 16#0002#;
   METH_NOARGS   : constant Argument_Methods := 16#0004#;
   METH_CLASS    : constant Argument_Methods := 16#0010#;
   METH_STATIC   : constant Argument_Methods := 16#0020#;
   --  How arguments are passed to callbacks:
   --   - METH_VARGS: only positional arguments in the form of a tuple are
   --     accepted
   --   - "METH_VARGS or METH_KEYWORDS": a function accepting keyword
   --     arguments.
   --   - METH_CLASS and METH_STATIC can only be used for class methods, not
   --     for module methods. They both indicate that a method is a class-wide
   --     method. They are callable from the class or an instance, but the
   --     instance is ignored and not passed as a parameter.

   type C_Method_Vargs is access function
     (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, C_Method_Vargs);
   --  A callback for a METH_VARGS method.
   --  The first argument is the object on which the method is applied, or null
   --  if this is a standard function.
   --  The second argument is a tuple of the parameters. They can be extracted
   --  through a call to PyArg_ParseTuple.

   type C_Method_Keywords is access function
     (Self : PyObject; Args : PyObject; Kwargs : PyObject) return PyObject;
   pragma Convention (C, C_Method_Keywords);
   --  A callback for a METH_KEYWORDS method.
   --  The first argument is the object on which the method is applied, or null
   --  if this is a standard function.
   --  The second argument is a tuple of the positional parameters.
   --  The third argument is a hash table of the named parameters.
   --  Parameters can be extracted through a call to
   --  PyArg_ParseTupleAndKeywords.

   type C_Callback_Record is private;
   type C_Callback is access C_Callback_Record;
   pragma Convention (C, C_Callback);
   --  The exact type doesn't matter, we only want to cover all possible cases
   --  of callbacks (C_Method_Vargs, C_Method_Keywords)

   function To_Callback is new Standard.Ada.Unchecked_Conversion
     (C_Method_Vargs, C_Callback);
   function To_Callback is new Standard.Ada.Unchecked_Conversion
     (C_Method_Keywords, C_Callback);

   type PyMethodDef is record
      Name  : Interfaces.C.Strings.chars_ptr;
      Func  : C_Callback;
      Flags : Argument_Methods;
      Doc   : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C, PyMethodDef);
   --  Definition for one of the methods of an object.
   --  Name is the name used in the python interpreter to reference the method
   --  (one would use the syntax   self.Name (...))
   --  Func is the callback in the Ada code that should be called when the
   --  method is invoked.
   --  Flags indicates how the arguments should be passed.
   --  Doc is the optional documentation string for the method

   No_MethodDef : constant PyMethodDef;

   type PyMethodDef_Array is array (Natural range <>) of PyMethodDef;
   pragma Convention (C, PyMethodDef_Array);
   --  The full list of methods supported by a type.
   --  You do not need to terminate this array by a null element, as is done in
   --  C. This is automatically taken care of by Ada.

   No_MethodDef_Array : constant PyMethodDef_Array;

   procedure Free (Method : in out PyMethodDef);
   procedure Free (Methods : in out PyMethodDef_Array);
   --  Free the memory occupied by Method

   function Py_InitModule
     (Module_Name : String;
      Methods     : PyMethodDef_Array := No_MethodDef_Array;
      Doc         : String := "") return PyObject;
   --  Create and initialize a new module, with a set of predefined methods.
   --  Do not free Methods while the module is in use.
   --  The module is not visible in the interpreter until you have done a
   --  "import MODULE_NAME" in the interpreter.
   --
   --  The first parameter to the methods declared in Methods will be null.

   procedure Add_Function
     (Module : PyObject; Func : PyMethodDef; Self : PyObject := null);
   --  Add a new function to Module.
   --  Do not free Func while this function is registered.
   --  The first parameter to Func will be Self (defaults to Module if Self is
   --  null).

   ------------------
   -- Dictionaries --
   ------------------
   --  Dictionaries are hash tables, used internally by python to associate
   --  functions with modules or methods with objects.
   --  See PyModule_GetDict to see how to get the dictionary from a module.

   subtype PyDictObject is PyObject;

   function PyDict_Check (Obj : PyObject) return Boolean;
   --  Return True if Obj is a dict object or an instance of a subtype of the
   --  dict type.

   function PyDict_New return PyDictObject;
   --  Create a new empty dictionary

   function PyDict_Contains
     (Dict : PyDictObject; Key : PyObject) return Boolean;
   --  Determine if dictionary contains key

   function PyDict_SetItemString
     (Dict : PyDictObject;
      Key  : Interfaces.C.Strings.chars_ptr;
      Obj  : PyObject) return Integer;
   pragma Import (C, PyDict_SetItemString, "PyDict_SetItemString");
   --  Store a new object in Dict. Obj should be Py_DECREF after the call.
   --  Return 0 if all went well, -1 otherwise
   --  Key should be deallocated.

   procedure PyDict_SetItemString
     (Dict : PyDictObject; Key : String; Obj : PyObject);
   --  Same as above. Refcounting for Obj is automatically increased, you do
   --  not need to do it yourself.

   function PyDict_SetItem
     (Dict  : PyDictObject;
      Key   : PyObject;
      Value : PyObject) return Integer;
   --  Add a new item to the dictionary.
   --  Key and Value should be Py_DECREF'ed after this call.
   --  Return 0 if all went well, -1 otherwise

   function PyDict_GetItemString
     (Dict : PyDictObject;
      Key  : Interfaces.C.Strings.chars_ptr) return PyObject;
   pragma Import (C, PyDict_GetItemString, "PyDict_GetItemString");
   --  Get an object from a dictionary. Do not decref the returned value

   function PyDict_GetItemString
     (Dict : PyDictObject; Key  : String) return PyObject;
   --  Same as above

   function PyDict_GetItem
     (Dict : PyDictObject; Key : PyObject) return PyObject;
   --  Same as above

   procedure PyDict_Next
     (Dict  : PyObject;
      Pos   : in out Integer;
      Key   : out PyObject;
      Value : out PyObject);
   --  Starting with Pos = 0, this traverses all items in Dict.
   --  When there are no more items, Pos is set to -1.
   --  It isn't safe to use this in a loop that modifies Dict.

   function PyDict_Size (Dict : PyObject) return Integer;
   --  Return the number of elements in Dict

   ----------
   -- Sets --
   ----------
   --  A set object is an unordered collection of distinct hashable objects.
   --  Common uses include membership testing, removing duplicates from a
   --  sequence, and computing mathematical operations such as intersection,
   --  union, difference, and symmetric difference.

   function PyAnySet_Check (Obj : PyObject) return Boolean;
   --  Return true if p is a set object, a frozenset object, or an instance of
   --  a subtype.

   ---------------
   -- Functions --
   ---------------

   function PyFunction_Check (Func : PyObject) return Boolean;
   --  Whether Func is a function object

   function PyFunction_Get_Code (Func : PyObject) return PyCodeObject;
   --  Return the code of the function (see PyEval_EvalCodeEx).
   --  Refcount for the code is not increased.

   function PyFunction_Get_Globals (Func : PyObject) return PyObject;
   --  Return the globals dictionary the function belongs to

   function PyFunction_Get_Defaults (Func : PyObject) return PyObject;
   --  Return a tuple of the default values for all the parameters of Func

   function PyFunction_Get_Closure (Func : PyObject) return PyObject;
   --  ???

   function PyCallable_Check (Func : PyObject) return Boolean;
   --  Determine if the object o is callable. This function always succeeds

   ------------------
   -- Object types --
   ------------------

   subtype PyTypeObject is PyObject;
   --  The internal structure that describes a Python type (and all the default
   --  primitive subprograms like __getattr__, __setattr__, ...

   function Py_TYPE (Obj : PyObject) return PyTypeObject;
   --  Return the type object of a given object.

   function Name (Obj : PyTypeObject) return String;
   --  Name of type, useful for printing, in format "<module>.<name>"

   function Type_New
     (Name     : String;
      Bases    : PyTuple;
      Dict     : PyObject;
      Metatype : PyTypeObject := null) return PyObject;
   --  Creates a so called new-style class in python.
   --  Such classes have a metaclass (ie their type) that is "type" or one of
   --  its ancestors. Their provide a number of advantages over older classes:
   --    - it is possible to extend builtin types such as "list" or "tuple"
   --    - support for the "super" function, to provide collaborative multiple
   --      inheritance
   --    - support for properties (ie fields manipulated through setters and
   --      getters)
   --    - better Method Resolution Order, more compatible with multiple
   --      inheritance.
   --  See the original paper at
   --    http://www.python.org/download/releases/2.2.3/descrintro
   --
   --  This replaces the older PyClass_New.
   --  This isn't a standard python function, but is specific to Ada. If the
   --  Metatype is not specified, it will default to "type", although depending
   --  on the list of base classes you provide, python might decide to use
   --  another metaclass.
   --  This function is similar to calling the "type()" function from within
   --  python:
   --     A = type ("Name", (list,), {});
   --  which creates a new class Name deriving from "list".
   --
   --  Dict can contain any number of things (including for instance the list
   --  of methods for the class, although you can add some later), just as if
   --  you were defining the class in Python:
   --     - "__slots__"
   --     - "__module__"  (although that is set automatically otherwise)
   --     - "__doc__"

   function PyObject_IsInstance
     (Inst : PyObject; Cls : PyObject) return Boolean;
   --  Return if the metaclass of Inst is Cls (ie Inst was created with
   --  something like "Inst = Cls (...)"

   ----------------
   -- Exceptions --
   ----------------

   procedure PyErr_Print;
   --  Print the current exception and its traceback to sys.stderr.
   --  This also clears the error indicator.
   --  Call this procedure only if the error indicator is set

   procedure PyErr_SetInterrupt;
   --  Interrupt the current command in the interpreter. This is the equivalent
   --  of Control-C in a terminal executing python.

   procedure PyErr_Fetch
     (EType      : out PyObject;
      Occurrence : out PyObject;
      Traceback  : out PyObject);
   --  Get the current exception information.
   --  Occurrence is a tuple, made of the following information:
   --    (msg, ('input_stream_name', line, column, input_text))
   --    where msg is the exception's message, and the second tuple is the
   --    location where the exception occurred.
   --  EType is the type of the exception, like "exceptions.SyntaxError".
   --
   --  This calls clears the current exception. If you want to call PyErr_Print
   --  later on, you will need to call PyErr_Restore with the same parameters
   --  to restore the current exception.

   procedure PyErr_NormalizeException
     (EType      : in out PyObject;
      Occurrence : in out PyObject;
      Traceback  : in out PyObject);
   --  Normalize a raised exception. This generally needs to be called after
   --  PyErr_Fetch.
   --  This ensure that if EType is an class, Occurrence is an instance.

   procedure PyErr_Restore
     (EType      : PyObject;
      Occurrence : PyObject;
      Traceback  : PyObject);
   --  Set the current exception

   procedure PyErr_Clear;
   --  Clear the current exception. This must be called at the end of your
   --  exception handlers, although it is called automatically by PyErr_Print

   procedure PyErr_BadArgument;
   --  Set the current exception as a "bad argument" exception. The function
   --  should also return null to its caller.

   function PyErr_NewException
     (Name : String; Base : PyObject := null; Dict : PyObject := null)
     return PyObject;
   --  Create a new exception, which can then be raised by:
   --   - calling PyErr_SetString (Except, "message");
   --   - returning null from your subprogram
   --  Name must be of the form "module.name"

   procedure PyErr_SetString (Except : PyObject; Msg : String);
   --  Raise Except, and associate it with a specific message

   ---------
   -- Sys --
   ---------

   procedure PySys_SetObject (Name : String; Object : PyObject);
   --  Set one of the predefined objects in the python interpreter. See the
   --  module "sys".
   --  Among these objects are:
   --    - "stdin", "stdout", "stderr": standard file objects
   --    - "_stdin", _stdout", "_stderr": initial values for standard files
   --    - "modules": dictionary of modules
   --    - "path": module search path
   --    - "ps1", "ps2": prompts
   --    - "displayhook":  ???
   --    - "excepthook": ???

   function PySys_GetObject (Name : String) return PyObject;
   --  Return an object from the sys module,
   --  Returned object must not be Py_DECREF by the caller.

   -----------
   -- Files --
   -----------

   function PyFile_WriteString (Text : String; File : PyObject) return Boolean;
   --  Write a string to an instance of file. You can for instance get such an
   --  instance by using
   --     PySys_GetObject ("stdout")
   --  Return False if the string couldn't be written

   function PyFile_FromString (File_Name, Mode : String) return PyObject;
   --  Create an instance of file.
   --  Python3 removes this function. Use "io.open()" instead.

   -----------------
   -- Class types --
   -----------------

   function Lookup_Object (Module : String; Name : String) return PyObject;
   function Lookup_Object (Module : PyObject; Name : String) return PyObject;
   --  Lookup an object in the module.
   --  Typical use is
   --     Obj := Lookup_Class_Object ("__builtin__", "file");
   --  null is returned if the class is not found.
   --  The second version is slightly faster and should be used when you
   --  already have a handle to the module

   procedure Add_Method
     (Class : PyObject; Func : PyMethodDef; Self : PyObject := null;
      Module : PyObject);
   --  Add a new method to the class.
   --  The method is an instance method.
   --  When the method is called from the python interpreter, its Self argument
   --  is set to the value of Self.
   --  Its first argument will always be the instance itself. Therefore the
   --  first character in the argument to PyArg_ParseTuple should be "O".

   procedure Add_Static_Method
     (Class : PyObject; Func : PyMethodDef; Self : PyObject := null;
      Module : PyObject);
   --  Return a static version of Method. This method doesn't receive an
   --  instance or the class as its first parameter. This is similar to C++ or
   --  Java's static methods.
   --  If no documentation is set for the method, it will be set to the fully
   --  qualified name of the method, since otherwise there is no way from the
   --  GPS shell to get access to the class to which the method belongs.

   procedure Add_Class_Method
     (Class : PyObject; Func : PyMethodDef; Module : PyObject);
   --  Return a class version of Method.
   --  This is a method that receives the class as implicit first argument,
   --  just like an instance method receives the instance.
   --  It can be called either on the class or an instance. If a class method
   --  is called for a derived class, the derived class object is passed as the
   --  implied first argument.
   --  If no documentation is set for the method, it will be set to the fully
   --  qualified name of the method, since otherwise there is no way from the
   --  GPS shell to get access to the class to which the method belongs.

   function Py_IsSubclass (Class : PyObject; Base : PyObject) return Boolean;
   --  True if Class is a subclass of Base (or Base itself)

   function PyMethod_Check (Obj : PyObject) return Boolean;
   --  Whether Obj is a method of a class

   function PyMethod_Self (Obj : PyObject) return PyObject;
   --  Return the instance with which the method is bound. This might be null
   --  if we have an unbound class method (Class.method), or non-null if we
   --  have a bound class method (the result of self.method)
   --  Returns a borrowed reference, no need to Py_DECREF

   function PyMethod_Function (Obj : PyObject) return PyObject;
   --  Return the function object associated with the method. That is the code
   --  that is actually executed when the method is called

   -----------------
   -- Descriptors --
   -----------------
   --  Descriptors are an advanced feature of python, used as the underlying
   --  capability for bounded methods, properties,...
   --  Basically, when a field in an instance is a descriptor, its value is
   --  read from a Getter, instead of directly. Likewise it is set through a
   --  Setter.

   type C_Getter is access function
     (Obj : PyObject; Closure : System.Address) return PyObject;
   pragma Convention (C, C_Getter);
   type C_Setter is access function
     (Obj     : PyObject;
      Prop    : PyObject;
      Closure : System.Address) return Integer;
   pragma Convention (C, C_Setter);
   --  Closure is some custom data you have specified in the call to
   --  Create_GetSetDef

   function PyDescr_NewGetSet
     (Typ     : PyObject;
      Name    : String;
      Setter  : C_Setter := null;
      Getter  : C_Getter := null;
      Doc     : String   := "";
      Closure : System.Address := System.Null_Address) return Boolean;
   --  Register a new property (accessed through setters and getters) in the
   --  specified Typ. The property is immediately added to the dictionary.
   --  False is returned if the property could not be added.
   --  The Closure will be passed as is to Setter and Getter.

   ------------------------------------
   -- Creating and declaring methods --
   ------------------------------------

   function Create_Method_Def
     (Name : String;
      Func : C_Method_Vargs;
      Doc  : String           := "")
      return PyMethodDef;
   --  Convenience function to create method definitions.
   --  See the description of the parameters in the declaration of PyMethodDef
   --  The flags are automatically set to METH_VARGS, which is the appropriate
   --  type for callbacks of this form.
   --  The returned value must be freed by the caller.

   function Create_Method_Def
     (Name : String;
      Func : C_Method_Keywords;
      Doc  : String           := "")
      return PyMethodDef;
   --  Same as above, for methods accepting keywords.
   --  The returned value must be freed by the caller

   ------------------------
   -- Executing commands --
   ------------------------

   function PyRun_SimpleString (Cmd : String) return Boolean;
   --  Executes Cmd in the __main__ module.
   --  Return True on success, False if an exception occurred (it is your
   --  responsibility to check the current exception)

   type Interpreter_State is private;
   Py_Single_Input : constant Interpreter_State;
   Py_File_Input   : constant Interpreter_State;
   Py_Eval_Input   : constant Interpreter_State;
   --  The state of the interpreter when evaluating a string.
   --    - Single_Input: evaluate any command in the interpreter. This will
   --          print the result (but return None)
   --    - Eval_Input: evaluate an expression. Evaluates an expression.
   --          Equivalent to 'eval'.
   --    - File_Input: evaluate a whole file, and return None.
   --          Equivalent to 'exec'.

   function PyRun_String
     (Str     : String;
      Start   : Interpreter_State;
      Globals : PyObject;
      Locals  : PyObject) return PyObject;
   --  Execute Python source code from str in the context specified by the
   --  dictionaries globals and locals.  The parameter start specifies the
   --  start token that should be used to parse the source code.
   --
   --  Returns NULL if an exception occurred, None otherwise.

   function Py_CompileString
     (Cmd : String; Name : String; State : Interpreter_State)
      return PyCodeObject;
   --  Compile Cmd into a code object. Null is returned if Cmd couldn't be
   --  compiled, either because of a syntax error or because Cmd is incomplete

   function PyEval_GetGlobals return PyObject;
   pragma Import (C, PyEval_GetGlobals, "PyEval_GetGlobals");
   --  Return the dictionary for global variables

   function PyEval_EvalCode
     (Code    : PyCodeObject;
      Globals : PyObject;
      Locals  : PyObject) return PyObject;
   --  Evaluate a precompiled code object

   function PyEval_EvalCodeEx
     (Code     : PyCodeObject;
      Globals  : PyObject;
      Locals   : PyObject;
      Args     : PyTuple      := null;
      Kwds     : PyDictObject := null;
      Defaults : PyTuple      := null;
      Closure  : PyObject     := null) return PyObject;
   --  Evaluate a precompiled code object. This is mostly used to execute a
   --  function (get its code with PyFunction_Get_Code), specifying some of
   --  the parameters

   --------------------------------------
   -- Evaluating and Tracing execution --
   --------------------------------------
   --  Python will periodically call two functions that you can register: a
   --  profile function, called every time a subprogram is called or returns,
   --  and a trace function called for every instruction.
   --  These can be used to trace the execution of your program, but also to
   --  interrupt a parser embedded in your application:
   --   - register a trace function, and every n calls, check for gtk events
   --     and call PyErr_SetInterrupt if necessary
   --   - a profile function would not be called for an infinite loop that
   --     never calls another subprogram, so is not appropriate for for such
   --     usage.
   --  There is still a catch: you will not be able to interrupt a long sleep()
   --  operation with this method, since the interpret itself is paused. The
   --  best solution to handle this is to have your own Control-C handler,
   --  although the user would have to type this in the terminal used to start
   --  your application.

   type Why_Trace_Func is private;
   PyTrace_Call        : constant Why_Trace_Func;
   PyTrace_Exception   : constant Why_Trace_Func;
   PyTrace_Line        : constant Why_Trace_Func;
   PyTrace_Return      : constant Why_Trace_Func;
   PyTrace_C_Call      : constant Why_Trace_Func;
   PyTrace_C_Exception : constant Why_Trace_Func;
   PyTrace_C_Return    : constant Why_Trace_Func;

   type Py_Trace_Func is access function
     (User_Arg : PyObject;
      Frame    : PyFrameObject;
      Why      : Why_Trace_Func;
      Object   : PyObject) return Integer;
   --  Return 0 in case of success, or -1 if an exception is raised.
   --  Objects's value depends on the type of callback. For PyTrace_Return,
   --  this is the returned value. For PyTrace_Exception, this is the
   --  exception.  PyTrace_Line is called for all instructions, but only for
   --  the trace function, not the profile function.

   procedure PyEval_SetProfile (Proc : Py_Trace_Func; User_Arg : PyObject);
   --  Register a new profiling function

   procedure PyEval_SetTrace (Proc : Py_Trace_Func; User_Arg : PyObject);
   --  Register a new tracing function

   function PyFrame_GetLineNumber (Frame : PyFrameObject) return Integer;
   --  Return the line number that frame is currently executing.

   function PyFrame_Get_Code (Frame : PyFrameObject) return PyCodeObject;
   --  Return code object associated with the frame.
   --  With Python prior from 3.9 return a borrowed reference (no need to
   --  Py_DECREF)
   --  Starting with Python 3.9 creates a new reference to the code object.

   function PyCode_Get_Filename (Code : PyCodeObject) return PyObject;
   --  Return file name of the code object.
   --  Returns a borrowed reference, no need to Py_DECREF

   function PyCode_Get_Name (Code : PyCodeObject) return PyObject;
   --  Return function name of the code object.
   --  Returns a borrowed reference, no need to Py_DECREF

   function PyFrame_Get_Back (Frame : PyFrameObject) return PyFrameObject;
   --  Return previous frame in stack.
   --  With Python prior from 3.9 return a borrowed reference (no need to
   --  Py_DECREF)
   --  Starting with Python 3.9 creates a new reference to the frame object.

   -------------
   -- Threads --
   -------------

   type PyThreadState_Opaque is limited private;
   type PyThreadState is access all PyThreadState_Opaque;

private

   type PyObject_Opaque is null record;
   type PyThreadState_Opaque is null record;

   type Interpreter_State is new Integer;

   Py_Single_Input : constant Interpreter_State := 256;
   Py_File_Input   : constant Interpreter_State := 257;
   Py_Eval_Input   : constant Interpreter_State := 258;
   --  Values are copied from Python.h, and must be synchronized. They will
   --  probably never change, though, so this should be safe.

   type Why_Trace_Func is new Integer;
   PyTrace_Call        : constant Why_Trace_Func := 0;
   PyTrace_Exception   : constant Why_Trace_Func := 1;
   PyTrace_Line        : constant Why_Trace_Func := 2;
   PyTrace_Return      : constant Why_Trace_Func := 3;
   PyTrace_C_Call      : constant Why_Trace_Func := 4;
   PyTrace_C_Exception : constant Why_Trace_Func := 5;
   PyTrace_C_Return    : constant Why_Trace_Func := 6;

   type C_Callback_Record is new Integer; --  whatever

   No_MethodDef : constant PyMethodDef :=
     (Interfaces.C.Strings.Null_Ptr, null, 0,
      Interfaces.C.Strings.Null_Ptr);
   No_MethodDef_Array : constant PyMethodDef_Array := (1 .. 0 => No_MethodDef);

   pragma Convention (C, Py_Trace_Func);
   pragma Import (C, PyDict_New, "PyDict_New");
   pragma Import (C, PyEval_SetProfile, "PyEval_SetProfile");
   pragma Import (C, PyEval_SetTrace, "PyEval_SetTrace");
   pragma Inline (PyImport_AddModule);
   pragma Inline (PyRun_SimpleString);
   pragma Inline (PyArg_ParseTuple);
   pragma Inline (PyString_Check);
   pragma Inline (PyUnicode_Check);
   pragma Inline (PyInt_Check);
   pragma Inline (PyFloat_Check);
   pragma Import (C, PyModule_GetDict, "PyModule_GetDict");
   pragma Import (C, Py_INCREF, "ada_py_incref");
   pragma Import (C, Py_DECREF, "ada_py_decref");
   pragma Import (C, Py_XINCREF, "ada_py_xincref");
   pragma Import (C, Py_XDECREF, "ada_py_xdecref");
   pragma Import (C, PyErr_Print, "PyErr_Print");
   pragma Import (C, PyObject_Str, "PyObject_Str");
   pragma Import (C, PyObject_Call, "PyObject_Call");
   pragma Import (C, PyEval_EvalCode, "PyEval_EvalCode");
   pragma Import (C, PyEval_EvalCodeEx, "ada_PyEval_EvalCodeEx");
   pragma Import (C, PyErr_SetInterrupt, "PyErr_SetInterrupt");
   pragma Import (C, PyTuple_New, "PyTuple_New");
   pragma Import (C, PyTuple_GetItem, "PyTuple_GetItem");
   pragma Import (C, PyTuple_SetItem, "PyTuple_SetItem");
   pragma Import (C, Py_None, "ada_py_none");
   pragma Import (C, PyErr_Clear, "PyErr_Clear");
   pragma Import (C, PyErr_Fetch, "PyErr_Fetch");
   pragma Import (C, PyTuple_Size, "PyTuple_Size");
   pragma Import (C, PyInt_FromLong, "PyInt_FromLong");
   pragma Import (C, PyInt_FromSize_t, "PyInt_FromSize_t");
   pragma Import (C, PyInt_AsLong, "PyInt_AsLong");
   pragma Import (C, PyFloat_AsDouble, "PyFloat_AsDouble");
   pragma Import (C, PyInt_GetMax, "PyInt_GetMax");
   pragma Import (C, PyList_New, "PyList_New");
   pragma Import (C, PyList_Append, "PyList_Append");
   pragma Import (C, PyErr_BadArgument, "PyErr_BadArgument");
   pragma Import (C, PyErr_NormalizeException, "PyErr_NormalizeException");
   pragma Import (C, PyObject_Dir, "PyObject_Dir");
   pragma Import (C, PyObject_Repr, "PyObject_Repr");
   pragma Import (C, PyErr_Restore, "PyErr_Restore");
   pragma Import (C, PyDict_Size, "PyDict_Size");
   pragma Import (C, PyList_GetItem, "PyList_GetItem");
   pragma Import (C, PyList_Size, "PyList_Size");
   pragma Import (C, PyDict_SetItem, "PyDict_SetItem");
   pragma Import (C, PyDict_GetItem, "PyDict_GetItem");
   pragma Import (C, Get_Refcount, "ada_pyget_refcount");
   pragma Import (C, PyFunction_Get_Code, "ada_pyfunction_get_code");
   pragma Import (C, PyFunction_Get_Globals, "ada_pyfunction_get_globals");
   pragma Import (C, PyFunction_Get_Closure, "ada_pyfunction_get_closure");
   pragma Import (C, PyFunction_Get_Defaults, "ada_pyfunction_get_defaults");
   pragma Import (C, PyMethod_Function, "PyMethod_Function");
   pragma Import (C, PyMethod_Self, "PyMethod_Self");
   pragma Import (C, PyFrame_GetLineNumber, "PyFrame_GetLineNumber");
   pragma Import (C, PyFrame_Get_Code, "ada_pyframe_get_code");
   pragma Import (C, PyFrame_Get_Back, "ada_pyframe_get_back");
   pragma Import (C, PyCode_Get_Filename, "ada_pycode_get_filename");
   pragma Import (C, PyCode_Get_Name, "ada_pycode_get_name");
   pragma Import (C, Py_TYPE, "__gnatcoll_py_type");
end GNATCOLL.Python;
