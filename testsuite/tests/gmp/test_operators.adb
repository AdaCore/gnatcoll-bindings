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

with GNATCOLL.GMP.Integers;  use GNATCOLL.GMP.Integers;
with Test_Assert;            use Test_Assert;

procedure Test_Operators is

   A, B, C : Big_Integer;

begin
   Set (A, "2");
   Set (B, A ** 5);

   Set (C, B - A);
   Assert (C = 30, "test_operators: 32 - 2 = 30");

   Set (A, A + 100);
   Assert (A = 102, "test_operators: A + 100 = 102");

   Set (A, B * C);
   Assert (A = 960, "test_operators: B * C = 960");

   Set (C, A / 10);
   Assert (C = 96, "test_operators: A / 10 = 96");
end Test_Operators;
