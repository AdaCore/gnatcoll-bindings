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

procedure Test_Div is

   Q, N, D : Big_Integer;

   Dividend : constant String :=
      "1000000000000000000000000000000000000000000000000000000000000000000000";

   Divisor : constant String := Dividend (1 .. Dividend'Length - 1);

   Quotient : constant String := Dividend
      (1 .. Dividend'Length - Divisor'Length + 1);

begin
   Set (N, Dividend);
   Set (D, Divisor);
   Divide (Q, N, D);

   Assert (Image (Q), Quotient, "test_div 1");

   Set (Q, To => N / D);

   Assert (Image (Q), Quotient, "test_div 2");
end Test_Div;
