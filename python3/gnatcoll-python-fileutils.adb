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

with GNATCOLL.Python.Exceptions;

package body GNATCOLL.Python.Fileutils is
   use type C.WChar_Addr;
   use type C.Size_T;

   package Exc renames GNATCOLL.Python.Exceptions;

   ---------------------
   -- Py_DecodeLocale --
   ---------------------

   function Py_DecodeLocale (Arg : String) return C.WChar_Addr
   is
      function Internal (Arg : String; Size : out C.Size_T)
      return C.WChar_Addr;
      pragma Import (C, Internal, "Py_DecodeLocale");

      Result : C.WChar_Addr;
      Size : C.Size_T;
   begin
      Result := Internal (Arg => Arg & ASCII.NUL,
                          Size => Size);
      if Result = C.Null_WChar_Addr then
         --  An error occured during decoding.
         if Size = C.Size_T'Last - 1 then
            raise Exc.MemoryError;
         else
            raise Exc.DecodingError;
         end if;
      end if;
      return Result;
   end Py_DecodeLocale;

end GNATCOLL.Python.Fileutils;
