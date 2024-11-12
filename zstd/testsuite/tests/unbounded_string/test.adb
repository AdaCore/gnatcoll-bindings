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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.ZSTD;
with GNATCOLL.ZSTD.Controlled;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package Z renames GNATCOLL.ZSTD.Controlled;
   package IO renames GNAT.IO;

   Input : Unbounded_String;
   Compressed : Unbounded_String;
   Uncompressed : Unbounded_String;
   In_Ctx : Z.ZSTD_Compress_Context;
   Out_Ctx : Z.ZSTD_Decompress_Context;
begin
   Append (Input, "0123");
   for Idx in 1 .. 100 loop
      Compressed := In_Ctx.Compress (Input);
      A.Assert (Length (Compressed) > 0);

      Uncompressed := Out_Ctx.Decompress (Compressed);
      A.Assert (Uncompressed = Input);
   end loop;

   for Idx in 1 .. 100 loop
      Uncompressed := Out_Ctx.Decompress (Compressed);
      A.Assert (Uncompressed = Input);
   end loop;

   Append (Input, "0123456789");
   Compressed := In_Ctx.Compress (Input);
   IO.Put_Line (Length (Compressed)'Img);
   A.Assert (Length (Compressed) > 0);
   Uncompressed := Out_Ctx.Decompress (Compressed);
   A.Assert (Uncompressed = Input);
   return A.Report;
end Test;
