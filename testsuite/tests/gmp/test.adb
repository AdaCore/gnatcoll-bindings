------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2015-2023, AdaCore                   --
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

with Test_Image, Test_Div, Test_Eq, Test_Pow, Test_Mod, Test_Rem, Test_Swap,
     Test_Roots, Test_GCD, Test_Operators, Test_Bitwise, Test_Rationals;
with Test_Assert;

function Test return Integer is
begin
   Test_Eq;
   Test_Image;
   Test_Swap;
   Test_Roots;
   Test_Pow;
   Test_Div;
   Test_Rem;
   Test_Mod;
   Test_GCD;
   Test_Operators;
   Test_Bitwise;
   Test_Rationals;

   return Test_Assert.Report;
end Test;
