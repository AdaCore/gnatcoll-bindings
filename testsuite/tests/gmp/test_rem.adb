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

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP, GNATCOLL.GMP.Integers;
with Test_Assert;           use Test_Assert;

procedure Test_Rem is

   R, A, B : Big_Integer;

begin
   Set (B, "5", 10);

   Set (A, "10", 10);
   Get_Rem (R, A, B);
   Assert (R = 0, "test_rem: Get_Rem 10 rem 5 = 0");


   Set (A, "11", 10);
   Get_Rem (R, A, B);
   Assert (R = 1, "test_rem: Get_Rem 11 rem 5 = 1");


   Set (A, "12", 10);
   Get_Rem (R, A, B);
   Assert (R = 2, "test_rem: Get_Rem 12 rem 5 = 2");


   Set (A, "13", 10);
   Get_Rem (R, A, B);
   Assert (R = 3, "test_rem: Get_Rem 13 rem 5 = 3");


   Set (A, "14", 10);
   Get_Rem (R, A, B);
   Assert (R = 4, "test_rem: Get_Rem 14 rem 5 = 4");


----

   Set (A, "10", 10);
   Set (R, A rem B);
   Assert (R = 0, "test_rem: 10 rem 5 = 0");


   Set (A, "11", 10);
   Set (R, A rem B);
   Assert (R = 1, "test_rem: 11 rem 5 = 1");


   Set (A, "12", 10);
   Set (R, A rem B);
   Assert (R = 2, "test_rem: 12 rem 5 = 2");


   Set (A, "13", 10);
   Set (R, A rem B);
   Assert (R = 3, "test_rem: 13 rem 5 = 3");


   Set (A, "14", 10);
   Set (R, A rem B);
   Assert (R = 4, "test_rem: 14 rem 5 = 4");


----

   Set (A, "-10", 10);
   Set (R, A rem B);
   Assert (R = 0, "test_rem: -10 rem 5 = 0");


   Set (A, "-11", 10);
   Set (R, A rem B);
   Assert (R = -1, "test_rem: -11 rem 5 = -1");


   Set (A, "-12", 10);
   Set (R, A rem B);
   Assert (R = -2, "test_rem: -12 rem 5 = -2");


   Set (A, "-13", 10);
   Set (R, A rem B);
   Assert (R = -3, "test_rem: -13 rem 5 = -3");


   Set (A, "-14", 10);
   Set (R, A rem B);
   Assert (R = -4, "test_rem: -14 rem 5 = -4");


----
   Set (B, "-5", 10);

   Set (A, "10", 10);
   Set (R, A rem B);
   Assert (R = 0, "test_rem: 10 rem -5 = 0");


   Set (A, "11", 10);
   Set (R, A rem B);
   Assert (R = 1, "test_rem: 11 rem -5 = 1");


   Set (A, "12", 10);
   Set (R, A rem B);
   Assert (R = 2, "test_rem: 12 rem -5 = 2");


   Set (A, "13", 10);
   Set (R, A rem B);
   Assert (R = 3, "test_rem: 13 rem -5 = 3");


   Set (A, "14", 10);
   Set (R, A rem B);
   Assert (R = 4, "test_rem: 14 rem -5 = 4");


----

   Set (A, "-10", 10);
   Set (R, A rem B);
   Assert (R = 0, "test_rem: -10 rem -5 = 0");


   Set (A, "-11", 10);
   Set (R, A rem B);
   Assert (R = -1, "test_rem: -11 rem -5 = -1");


   Set (A, "-12", 10);
   Set (R, A rem B);
   Assert (R = -2, "test_rem: -12 rem -5 = -2");


   Set (A, "-13", 10);
   Set (R, A rem B);
   Assert (R = -3, "test_rem: -13 rem -5 = -3");


   Set (A, "-14", 10);
   Set (R, A rem B);
   Assert (R = -4, "test_rem: -14 rem -5 = -4");

end Test_Rem;
