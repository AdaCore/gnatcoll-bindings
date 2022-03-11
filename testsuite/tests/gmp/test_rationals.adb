------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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

with Ada.Exceptions; use Ada.Exceptions;

with GNATCOLL.GMP.Rational_Numbers; use GNATCOLL.GMP.Rational_Numbers;

with Test_Assert; use Test_Assert;

procedure Test_Rationals is
   procedure Test_Assignments;
   --  Test Set for all supported argument types

   ----------------------
   -- Test_Assignments --
   ----------------------

   procedure Test_Assignments is
      A, B, C, D : Rational;
   begin
      --  A is initialized to 0/1 at initialization-time

      Assert (A.Image, "0", "Image should return 0 as the denominator is 1");

      --  Reinitialize A to 1 using a non canonical form (re-using A should not
      --  generate a memory leak).

      A.Set ("2/2", Canonicalize => False);
      Assert (A.Image, "2/2");

      --  Reinitialize A to 1 using a non canonical form but let the default
      --  canonicalization happen.

      A.Set ("2/2");
      Assert (A.Image, "1");

      --  Invalid string initialization

      begin
         A.Set ("f/1");
         Assert (False, "invalid initialization string");
      exception
         when E : Failure =>
            Assert (Exception_Message (E), "cannot parse f/1 (base:  10)");
      end;

      --  Initialization in base 16

      A.Set ("f/1", Base => 16);
      Assert (A.Image, "15");

      --  Approximation of pi

      B.Set ("355/113");
      Assert (B.Image, "355/113");

      --  Rational_Numbers.Image doesn't print the leading whitespace

      C.Set (Integer'Last'Image);
      Assert (" " & C.Image, Integer'Last'Image);

      --  Cannot set a rational with 0 as denominator

      begin
         D.Set ("0/0");
         Assert (False, "0/0 is not a valid rational number");
      exception
         when E : Failure =>
            Assert (Exception_Message (E),
                    "cannot set number with 0 as denominator");
      end;

      begin
         D.Set ("123/0");
         Assert (False, "123/0 is not a valid rational number");
      exception
         when E : Failure =>
            Assert (Exception_Message (E),
                    "cannot set number with 0 as denominator");
      end;
   end Test_Assignments;

begin
   Test_Assignments;
end Test_Rationals;
