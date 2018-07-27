------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with GNAT.OS_Lib;
with GNATCOLL.GMP.Integers.IO;

use GNATCOLL.GMP.Integers;
use GNATCOLL.GMP.Integers.IO;
use GNATCOLL.GMP;  --  for numeric types
with Test_Assert;            use Test_Assert;

procedure Test_Mod is

   R, A, B : Big_Integer;

begin
   Set (B, "5", 10);

   Set (A, "10", 10);
   Get_Mod (R, A, B);
   Assert (R = 0, "Get_Mod (10, 5) = 0");

   Set (A, "11", 10);
   Get_Mod (R, A, B);
   Assert (R = 1,"Get_Mod (11, 5) = 1");

   Set (A, "12", 10);
   Get_Mod (R, A, B);
   Assert (R = 2,"Get_Mod (12, 5) = 2");

   Set (A, "13", 10);
   Get_Mod (R, A, B);
   Assert (R = 3,"Get_Mod (13, 5) = 3");

   Set (A, "14", 10);
   Get_Mod (R, A, B);
   Assert (R = 4,"Get_Mod (14, 5) = 4");

----

   Set (B, "-5", 10);

   Set (A, "10", 10);
   Get_Mod (R, A, B);
   Assert (R = 0,"Get_Mod (10, -5) = 0");

   Set (A, "11", 10);
   Get_Mod (R, A, B);
   Assert (R = -4,"Get_Mod (11, -5) = -4");

   Set (A, "12", 10);
   Get_Mod (R, A, B);
   Assert (R = -3,"Get_Mod (12, -5) = -3");

   Set (A, "13", 10);
   Get_Mod (R, A, B);
   Assert (R = -2,"Get_Mod (13, -5) = -2");

   Set (A, "14", 10);
   Get_Mod (R, A, B);
   Assert (R = -1,"Get_Mod (14, -5) = -1");

----

   Set (B, "5", 10);

   Set (A, "10", 10);
   Set (R, A mod B);
   Assert (R = 0,"10 mod 5 = 0");

   Set (A, "11", 10);
   Set (R, A mod B);
   Assert (R = 1,"11 mod 5 = 1");

   Set (A, "12", 10);
   Set (R, A mod B);
   Assert (R = 2,"12 mod 5 = 2");

   Set (A, "13", 10);
   Set (R, A mod B);
   Assert (R = 3,"13 mod 5 = 3");

   Set (A, "14", 10);
   Set (R, A mod B);
   Assert (R = 4, "14 mod 5 = 4");

----

   Set (B, "5", 10);

   Set (A, "-10", 10);
   Set (R, A mod B);
   Assert (R = 0, "-10 mod 5 = 0");

   Set (A, "-11", 10);
   Set (R, A mod B);
   Assert (R = 4, "-11 mod 5 = 4");

   Set (A, "-12", 10);
   Set (R, A mod B);
   Assert (R = 3, "-12 mod 5 = 3");

   Set (A, "-13", 10);
   Set (R, A mod B);
   Assert (R = 2, "-13 mod 5 = 2");

   Set (A, "-14", 10);
   Set (R, A mod B);
   Assert (R = 1, "-14 mod 5 = 1");

----

   Set (B, "-5", 10);

   Set (A, "10", 10);
   Set (R, A mod B);
   Assert (R = 0, "10 mod -5 = 0");

   Set (A, "11", 10);
   Set (R, A mod B);
   Assert (R = -4, "11 mod -5 = -4");

   Set (A, "12", 10);
   Set (R, A mod B);
   Assert (R = -3, "12 mod -5 = -3");

   Set (A, "13", 10);
   Set (R, A mod B);
   Assert (R = -2, "13 mod -5 = -2");

   Set (A, "14", 10);
   Set (R, A mod B);
   Assert (R = -1, "14 mod -5 = -1");

----

   Set (B, "-5", 10);

   Set (A, "-10", 10);
   Set (R, A mod B);
   Assert (R = 0, "-10 mod -5 = 0");

   Set (A, "-11", 10);
   Set (R, A mod B);
   Assert (R = -1, "-11 mod -5 = -1");

   Set (A, "-12", 10);
   Set (R, A mod B);
   Assert (R = -2, "-12 mod -5 = -2");

   Set (A, "-13", 10);
   Set (R, A mod B);
   Assert (R = -3, "-13 mod -5 = -3");

   Set (A, "-14", 10);
   Set (R, A mod B);
   Assert (R = -4, "-14 mod -5 = -4");

end Test_Mod;
