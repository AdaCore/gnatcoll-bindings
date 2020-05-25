------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Interfaces.C.Strings;
with System;
with GNATCOLL.Python.Errors;
use type System.Address;

package body GNATCOLL.Python.Capsule is

   package C renames Interfaces.C.Strings;
   package PyErr renames GNATCOLL.Python.Errors;

   --------------------------
   -- PyCapsule_CheckExact --
   --------------------------

   function PyCapsule_CheckExact (Object : PyObject) return Boolean
   is
   begin
      return Py_TYPE (Object) = PyCapsule_Type;
   end PyCapsule_CheckExact;

   --------------------------
   -- PyCapsule_GetContext --
   --------------------------

   function PyCapsule_GetContext (Capsule : PyCapsule) return System.Address
   is
      function Internal (Capsule : PyCapsule) return System.Address;
      pragma Import (C, Internal, "PyCapsule_GetContext");

      Result : System.Address;
   begin

      Result := Internal (Capsule => Capsule);
      if Result = System.Null_Address and then
         PyErr.PyErr_Occurred /= null
      then
         raise PyCapsule_Error with "cannot get capsule context";
      end if;

      return Result;
   end PyCapsule_GetContext;

   ---------------------------
   --  PyCapsule_GetPointer --
   ---------------------------

   function PyCapsule_GetPointer
      (Capsule : PyCapsule;
       Name : String)
      return System.Address
   is
      function Internal
         (Capsule : PyCapsule;
          Name : String)
         return System.Address;
      pragma Import (C, Internal, "PyCapsule_GetPointer");

      Result : System.Address;
   begin
      Result := Internal (Capsule => Capsule, Name => Name & ASCII.NUL);

      if Result = System.Null_Address then
         raise PyCapsule_Error
            with "cannot retrieve pointer from capsule " & Name;
      end if;

      return Result;
   end PyCapsule_GetPointer;

   function PyCapsule_GetPointer (Capsule : PyCapsule) return System.Address
   is
      function Internal
         (Capsule : PyCapsule;
          Name : System.Address)
         return System.Address;
      pragma Import (C, Internal, "PyCapsule_GetPointer");

      Result : System.Address;
   begin
      Result := Internal (Capsule => Capsule,
                          Name => System.Null_Address);
      if Result = System.Null_Address then
         raise PyCapsule_Error
            with "cannot retrieve pointer from unnamed capsule";
      end if;

      return Result;
   end PyCapsule_GetPointer;

   -------------------
   -- PyCapsule_New --
   -------------------

   function PyCapsule_New
      (Pointer : System.Address;
       Name : String;
       Destructor : PyCapsule_Destructor := null)
      return PyCapsule
   is
      function Internal
         (Pointer : System.Address;
          Name : C.chars_ptr;
          Destructor : PyCapsule_Destructor)
         return PyCapsule;
      pragma Import (C, Internal, "PyCapsule_New");

      Internal_Name : constant C.chars_ptr := C.New_String (Name & ASCII.NUL);
      Result : PyCapsule;
   begin
      Result := Internal (Pointer => Pointer,
                          Name    => Internal_Name,
                          Destructor => Destructor);
      if Result = null then
         raise PyCapsule_Error with "cannot create capsule " & Name;
      end if;

      return Result;
   end PyCapsule_New;

   function PyCapsule_New
      (Pointer : System.Address;
       Destructor : PyCapsule_Destructor := null)
      return PyCapsule
   is
      function Internal
         (Pointer : System.Address;
          Name : System.Address;
          Destructor : PyCapsule_Destructor)
         return PyCapsule;
      pragma Import (C, Internal, "PyCapsule_New");

      Result : PyCapsule;
   begin
      Result := Internal (Pointer => Pointer,
                          Name    => System.Null_Address,
                          Destructor => Destructor);
      if Result = null then
         raise PyCapsule_Error with "cannot create unnamed capsule";
      end if;

      return Result;
   end PyCapsule_New;

   --------------------------
   -- PyCapsule_SetContext --
   --------------------------

   procedure PyCapsule_SetContext
      (Capsule : PyCapsule; Context : System.Address)
   is
      function Internal
         (Capsule : PyCapsule; Context : System.Address)
         return Integer;
      pragma Import (C, Internal, "PyCapsule_SetContext");

      Status : Integer;
   begin
      Status := Internal (Capsule => Capsule, Context => Context);
      if Status /= 0 then
         raise PyCapsule_Error with "cannot set capsule context";
      end if;
   end PyCapsule_SetContext;

end GNATCOLL.Python.Capsule;
