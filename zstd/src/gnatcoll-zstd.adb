------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

package body GNATCOLL.ZSTD is
   use type C.unsigned;

   package CStrings renames Interfaces.C.Strings;

   -------------------------
   -- ZSTD_Get_Error_Name --
   -------------------------

   function ZSTD_Get_Error_Name (Code : C.size_t) return String is
      function Internal (Code : C.size_t) return CStrings.chars_ptr;
      pragma Import (C, Internal, "ZSTD_getErrorName");
   begin
      return CStrings.Value (Internal (Code));
   end ZSTD_Get_Error_Name;

   -------------------
   -- ZSTD_Is_Error --
   -------------------

   function ZSTD_Is_Error (Code : C.size_t) return Boolean is
      function Internal (Code : C.size_t) return C.unsigned;
      pragma Import (C, Internal, "ZSTD_isError");
   begin
      return Internal (Code) > 0;
   end ZSTD_Is_Error;

   -------------------------
   -- ZSTD_Version_String --
   -------------------------

   function ZSTD_Version_String return String is
      function Internal return CStrings.chars_ptr;
      pragma Import (C, Internal, "ZSTD_versionString");
   begin
      return CStrings.Value (Internal);
   end ZSTD_Version_String;

end GNATCOLL.ZSTD;
