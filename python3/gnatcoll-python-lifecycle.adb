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

with GNATCOLL.Python.Fileutils;
with Ada.Command_Line;

package body GNATCOLL.Python.Lifecycle is

   package Fileutils renames GNATCOLL.Python.Fileutils;

   -----------------
   -- Py_Finalize --
   -----------------

   function Py_Finalize return Boolean is
      function Internal return Integer;
      pragma Import (C, Internal, "Py_FinalizeEx");

   begin
      return Internal = 0;
   end Py_Finalize;

   -------------------
   -- Py_Initialize --
   -------------------

   procedure Py_Initialize (Initialize_Signal_Handlers : Boolean := True) is
      procedure Internal (Init_Sigs : Integer);
      pragma Import (C, Internal, "Py_InitializeEx");
   begin
      if Initialize_Signal_Handlers then
         Internal (Init_Sigs => 1);
      else
         Internal (Init_Sigs => 0);
      end if;
   end Py_Initialize;

   ----------------------
   -- Py_SetPythonHome --
   ----------------------

   procedure Py_SetPythonHome (Home : String) is
   begin
      Py_SetPythonHome (Home => Fileutils.Py_DecodeLocale (Home));
   end Py_SetPythonHome;

   ------------------------
   --  Py_SetProgramName --
   ------------------------

   procedure Py_SetProgramName (Name : String) is
   begin
      Py_SetProgramName (Name => Fileutils.Py_DecodeLocale (Name));
   end Py_SetProgramName;

   procedure Py_SetProgramName is
   begin
      Py_SetProgramName (Name => Ada.Command_Line.Command_Name);
   end Py_SetProgramName;

end GNATCOLL.Python.Lifecycle;
