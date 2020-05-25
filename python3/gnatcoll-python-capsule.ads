------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2020, AdaCore                     --
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

--  Bindings to functions manipulating capsules

package GNATCOLL.Python.Capsule is

   PyCapsule_Error : exception;

   PyCapsule_Type_Opaque : aliased PyObject_Opaque;
   pragma Import (C, PyCapsule_Type_Opaque, "PyCapsule_Type");
   PyCapsule_Type : constant PyTypeObject := PyCapsule_Type_Opaque'Access;

   subtype PyCapsule is PyObject;
   --  This subtype of PyObject represents an opaque value, useful for C
   --  extension modules who need to pass an opaque value (as a void* pointer)
   --  through Python code to other C code. It is often used to make a C
   --  function pointer defined in one module available to other modules, so
   --  the regular import mechanism can be used to access C APIs defined in
   --  dynamically loaded modules.

   type PyCapsule_Destructor is access procedure (Capsule : PyCapsule);
   pragma Convention (C, PyCapsule_Destructor);
   --  Type for the capsule destructor callbacks

   function PyCapsule_CheckExact (Object : PyObject) return Boolean;
   pragma Inline (PyCapsule_CheckExact);
   --  Return True is the Object is a PyCapsule.

   function PyCapsule_New
      (Pointer : System.Address;
       Name : String;
       Destructor : PyCapsule_Destructor := null)
      return PyCapsule;
   --  Create a PyCapsule encapsulating the pointer. The pointer argument may
   --  not be NULL.
   --
   --  On failure, raise PyCapsuleError
   --
   --  Note that an Interfaces.C.String.chars_ptr associated with the capsule
   --  name is allocated by the function. The object can be freed when the
   --  destructor is called. Use PyCapsule_GetName  to retrieve it.
   --
   --  If the destructor argument is not NULL, it will be called with the
   --  capsule as its argument when it is destroyed.
   --
   --  If this capsule will be stored as an attribute of a module, the name
   --  should be specified as modulename.attributename. This will enable other
   --  modules to import the capsule using PyCapsule_Import().

   function PyCapsule_New
      (Pointer : System.Address;
       Destructor : PyCapsule_Destructor := null)
      return PyCapsule;
   --  Likewise for capsule with no name associated

   function PyCapsule_GetPointer
      (Capsule : PyCapsule; Name : String) return System.Address;
   --  Retrieve the pointer stored in the capsule. On failure, raise
   --  PyCapsuleError.
   --
   --  The name parameter must compare exactly to the name stored in the
   --  capsule.

   function PyCapsule_GetPointer (Capsule : PyCapsule) return System.Address;
   --  Likewise for capsule with no name associated.

   procedure PyCapsule_SetContext
      (Capsule : PyCapsule; Context : System.Address);
   --  Set the context pointer inside capsule to context.
   --  Raise PyCapsule_Error on error.

   function PyCapsule_GetContext (Capsule : PyCapsule) return System.Address;
   --  Return the current context stored in the capsule. On failure raise
   --  PyCapsule_Error.

end GNATCOLL.Python.Capsule;
