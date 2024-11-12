------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Test_Assert;
with GNATCOLL.ZSTD;
with GNAT.IO;
with Interfaces.C;

function Test return Integer is

   package A renames Test_Assert;
   package C renames Interfaces.C;
   package Z renames GNATCOLL.ZSTD;
   package IO renames GNAT.IO;
   use all type C.unsigned;

begin

   declare
      Version_Int : constant C.unsigned := Z.ZSTD_Version_Number;
      Version_Str : constant String := Z.ZSTD_Version_String;
   begin
      IO.Put_Line (Version_Str);
      A.Assert (Version_Str'Length > 0);
      A.Assert (Version_Str'Length > 0 and then
                Version_Str (Version_Str'Last) /= ASCII.NUL);
      IO.Put_Line (Version_Int'Img);
      A.Assert (Version_Int > 1 * 100 * 100 + 4 * 100);
   end;

   return A.Report;
end Test;
