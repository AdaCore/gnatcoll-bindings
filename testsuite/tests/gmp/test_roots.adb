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

with GNATCOLL.GMP.Integers.Root_Extraction;

use GNATCOLL.GMP.Integers;
use GNATCOLL.GMP.Integers.Root_Extraction;

with Test_Assert; use Test_Assert;

procedure Test_Roots is

   A, B : Big_Integer;

   Root_Value   : constant := 99_999;
   Raised_Value : constant String := "99995000000000000000000000000000";
   Was_Exact    : Boolean;

begin
   Set (A, "144");
   Get_SQRT (A, Into => B);

   Assert (B = 12, "test_roots: sqrt of 144 = 12");

   Set (A, To => Root_Value);
   Raise_To_N (A, 5);
   Get_Nth_Root (A, N => 5, Into => B, Exact => Was_Exact);

   Assert
     (B = Root_Value,
      "test_roots: 5th root of " & Raised_Value & " = " & Root_Value'Img);

   Assert
     (Was_Exact, "test_roots: 5th root of " & Raised_Value & " is exact");

end Test_Roots;
