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
with Test_Measure;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.ZSTD;
with GNATCOLL.ZSTD.Controlled;
with GNATCOLL.OS.FS;

function Test return Integer is

   package A renames Test_Assert;
   package M renames Test_Measure;
   package Z renames GNATCOLL.ZSTD.Controlled;
   package FS renames GNATCOLL.OS.FS;

   Compressed : Unbounded_String;
   Uncompressed : Unbounded_String;
   In_Ctx : Z.ZSTD_Compress_Context;
   Out_Ctx : Z.ZSTD_Decompress_Context;
   FD, Src_FD, Dst_FD : FS.File_Descriptor;
begin
   In_Ctx.Set_Parameter (GNATCOLL.ZSTD.ZSTD_C_Compression_Level, 9);

   --  Create a test file
   FD := FS.Open ("./test.data", Mode => FS.Write_Mode);

   for Idx in 1 .. 10_000_000 loop
      FS.Write (FD, "0123456789");
   end loop;
   FS.Close (FD);

   FD := FS.Open ("./test.data");
   M.Start_Measure;
   Compressed := In_Ctx.Compress (FD);
   M.End_Measure (Message => "100MB compression time");
   FS.Close (FD);

   Uncompressed := Out_Ctx.Decompress (Compressed);
   A.Assert (Length (Uncompressed) = 100 * 1_000_000);

   Src_FD := FS.Open ("./test.data");
   Dst_FD := FS.Open ("./test.data.compressed", Mode => FS.Write_Mode);

   M.Start_Measure;
   In_Ctx.Compress (Src_FD, Dst_FD);
   M.End_Measure (Message => "100MB compression time (file in and out)");

   FS.Close (Src_FD);
   FS.Close (Dst_FD);

   Src_FD := FS.Open ("./test.data.compressed");
   Dst_FD := FS.Open ("./test.data.bis", Mode => FS.Write_Mode);
   M.Start_Measure;
   Out_Ctx.Decompress (Src_FD, Dst_FD);
   M.End_Measure (Message => "100MB decompression time (file in and out)");

   Src_FD := FS.Open ("./test.data.compressed");
   Uncompressed := Out_Ctx.Decompress (Src_FD);
   A.Assert (Length (Uncompressed) = 100 * 1_000_000);

   return A.Report;
end Test;
