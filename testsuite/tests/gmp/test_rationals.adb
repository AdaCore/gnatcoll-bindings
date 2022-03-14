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

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNATCOLL.GMP;                  use GNATCOLL.GMP;
with GNATCOLL.GMP.Integers;         use GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Rational_Numbers; use GNATCOLL.GMP.Rational_Numbers;

with Test_Assert; use Test_Assert;

procedure Test_Rationals is
   procedure Test_Assignments;
   --  Test Set for all supported argument types

   procedure Test_Conversions;
   --  Test conversions from/to floating-point values

   procedure Test_Arithmetics;
   --  Test rational number arithmetic

   ----------------------
   -- Test_Assignments --
   ----------------------

   procedure Test_Assignments is
      A, B, C, D, R, S : Rational;
      Zero             : constant String := "0";
      One              : constant String := "2/2";
      Pi               : constant String := "355/113";
   begin
      --  A is initialized to 0/1 at initialization-time

      Assert (A.Image, Zero, "Image should return 0 as the denominator is 1");

      --  Reinitialize A to 1 using a non canonical form (re-using A should not
      --  generate a memory leak).

      A.Set (One, Canonicalize => False);
      Assert (A.Image, One);

      --  Reinitialize A to 1 using a non canonical form but let the default
      --  canonicalization happen.

      A.Set (One);
      Assert (A.Image, "1");

      --  Ensure that a reinitialized number is correctly marked as
      --  Canonicalized.

      declare
         I : Big_Integer := Make ("1");
      begin
         A.Set (One, Canonicalize => False);
         A.Set (I);
         Assert (A.Is_Canonical);
      end;

      --  Invalid string initialization

      begin
         A.Set ("f/1");
         Assert (False, "invalid initialization string");
      exception
         when E : Rational_Numbers.Failure =>
            Assert (Exception_Message (E), "cannot parse f/1 (base:  10)");
      end;

      --  Initialization in base 16

      A.Set ("f/1", Base => 16);
      Assert (A.Image, "15");

      --  Approximation of pi

      B.Set (Pi);
      Assert (B.Image, Pi);

      --  Rational_Numbers.Image doesn't print the leading whitespace

      C.Set (Integer'Last'Image);
      Assert (" " & C.Image, Integer'Last'Image);

      --  Cannot set a rational with 0 as denominator

      begin
         D.Set ("0/0");
         Assert (False, "0/0 is not a valid rational number");
      exception
         when E : Rational_Numbers.Failure =>
            Assert (Exception_Message (E),
                    "cannot set number with 0 as denominator");
      end;

      begin
         D.Set ("123/0");
         Assert (False, "123/0 is not a valid rational number");
      exception
         when E : Rational_Numbers.Failure =>
            Assert (Exception_Message (E),
                    "cannot set number with 0 as denominator");
      end;

      begin
         D.Set (1, 0);
         Assert (False, "1/0 is not a valid rational number");
      exception
         when E : Rational_Numbers.Failure =>
            Assert (Exception_Message (E),
                    "cannot set number with 0 as denominator");
      end;

      --  Check usage of bound values

      declare
         Result : constant String :=
           Long'First'Image & "/" & Trim (Unsigned_Long'Last'Image, Left);
      begin
         R.Set (Result);
         Assert (R.Image, Result);
      end;

      --  Setting from Long/Unsigned_Long

      declare
         function Image (Num, Den : String) return String
         is (Trim (Num, Left)
               & (if Den /= " 1" then "/" & Trim (Den, Left) else ""));

         LF : constant Long          := Long'First;
         LL : constant Long          := Long'Last;
         UF : constant Unsigned_Long := Unsigned_Long'First + 1;
         UL : constant Unsigned_Long := Unsigned_Long'Last;
     begin
         R.Set (LF, UF);
         Assert (R.Image, Image (LF'Image, UF'Image));
         R.Set (LL, UF);
         Assert (R.Image, Image (LL'Image, UF'Image));
         R.Set (LF, UL);
         Assert (R.Image, Image (LF'Image, UL'Image));
         R.Set (LL, UL);
         Assert (R.Image, Image (LL'Image, UL'Image));
      end;

      --  Setting from Big_Integer

      declare
         I : Big_Integer;
         L : constant Long := Long'First;
      begin
         I.Set (L);
         R.Set (I);
         Assert (R.Image, L'Image);
      end;

      --  Swap rational numbers

      R.Set ("1/2");
      S.Set ("3/4");
      R.Swap (S);
      Assert (R.Image, "3/4");
      Assert (S.Image, "1/2");
   end Test_Assignments;

   ----------------------
   -- Test_Conversions --
   ----------------------

   procedure Test_Conversions is
      R              : Rational;
      Result         : Double;
      Pos_Zero_Image : constant String := " 0.00000000000000E+00";
      Pos_Inf_Image  : constant String := "+Inf****************";
      Neg_Inf_Image  : constant String := "-Inf****************";
   begin
      --  Check conversions from/to Double

      declare
         D : constant Double := Double'Last;
      begin
         R.Set (D);
         Result := R.To_Double;
         Assert (Result = D);
      end;

      --  Same tests but from an Ada floating-point type

      declare
         E : constant Long_Float := Long_Float'Model_Small;
      begin
         R.Set (Double (E));
         Result := R.To_Double;
         Assert (Long_Float (Result) = E);
      end;

      -- Ensure that NaN, +Inf, -Inf are invalid inputs for Double conversion

      declare
         function Minus (A, B : Double) return Double is (A - B);

         Zero : constant Double := Minus (1.0, 1.0);
         NaN  : constant Double := Zero / Zero;
         Inf  : Double          := 1.0 / Zero;
      begin
         --  NaN

         begin
            Assert (NaN'Image, "NaN*****************");
            R.Set (NaN);
            Assert (False, "Should complain about NaN");
         exception
            when E : Rational_Numbers.Failure =>
               Assert (Exception_Message (E),
                       "cannot set number from a NaN");
         end;

         -- +Inf

         begin
            Assert (Inf'Image, "+Inf****************");
            R.Set (Inf);
            Assert (False, "Should complain about infinity");
         exception
            when E : Rational_Numbers.Failure =>
               Assert (Exception_Message (E),
                       "cannot set number from infinity");
         end;

         -- -Inf

         begin
            Inf := -Inf;
            Assert (Inf'Image, "-Inf****************");
            R.Set (Inf);
            Assert (False, "Should complain about infinity");
         exception
            when E : Rational_Numbers.Failure =>
               Assert (Exception_Message (E),
                       "cannot set number from infinity");
         end;
      end;

      --  0/1 is converted to 0.0 in the floating-point set

      R.Set ("0/1");
      Result := R.To_Double;
      Assert (Result'Image, Pos_Zero_Image);

      --  Underflow

      R.Set ("1/1" & (1 .. 350 => '0'));
      Result := R.To_Double;
      Assert (Result'Image, Pos_Zero_Image);

      --  Overflow

      R.Set ("1" & (1 .. 350 => '0'));
      Result := R.To_Double;
      Assert (Result'Image, Pos_Inf_Image);

   end Test_Conversions;

   ----------------------
   -- Test_Arithmetics --
   ----------------------

   procedure Test_Arithmetics is
      R, A, B, C : Rational;
   begin
      A.Set ("1/10");

      --  Init a rational number without explicit canonicalization

      B.Set ("150/15", Canonicalize => False);
      Assert (B.Image, "150/15");

      --  Canonicalize B and check that the copy C is still non-canonicalized

      C.Set (B, Canonicalize => B.Is_Canonical);
      B.Canonicalize;
      Assert (B.Image, "10");
      Assert (C.Image, "150/15");
      Assert (B.Is_Canonical);
      Assert (not C.Is_Canonical);

      --  Check some basic arithemtic

      R.Set (A + B);
      Assert (R.Image, "101/10");

      R.Set (A - B);
      Assert (R.Image, "-99/10");

      R.Set (A * B);
      Assert (R.Image, "1");

      R.Set (A / B);
      Assert (R.Image, "1/100");

      R.Set (-A);
      Assert (R.Image, "-1/10");

      R.Set (abs R);
      Assert (R.Image, "1/10");

      R.Set (A + B - A * B / (-A));
      Assert (R.Image, "201/10");

      --  Division by zero raises a Failure exception

      declare
         A, B : Rational;
      begin
         A.Set ("1/1");
         B.Set ("0/1");
         begin
            R.Set (A / B);
            Assert (False, "division by 0");
         exception
            when E : Rational_Numbers.Failure =>
               Assert (Exception_Message (E), "Division by zero");
         end;
      end;

      --  Operations raise an exception when operands are not in a canonical
      --  form.

      declare
         type Binary_Operator is access
           function (Left, Right : Rational) return Rational;
         type Unary_Operator is access
           function (Operand : Rational) return Rational;

         procedure Test_Binary
           (Op               : Binary_Operator;
            Left, Right      : Rational;
            Expected_Message : String);
         --  Test an expected failure of a binary operation

         procedure Test_Unary
           (Op               : Unary_Operator;
            Operand          : Rational;
            Expected_Message : String);
         --  Test an expected failure of an unary operation

         -----------------
         -- Test_Binary --
         -----------------

         procedure Test_Binary
           (Op               : Binary_Operator;
            Left, Right      : Rational;
            Expected_Message : String)
         is
            Result : Rational;
         begin
            begin
               Result.Set (Op (Left, Right));
               Assert (False, "Should raise a Failure exception");
            exception
               when E : Rational_Numbers.Failure =>
                  Assert (Exception_Message (E), Expected_Message);
            end;
         end Test_Binary;

         ----------------
         -- Test_Unary --
         ----------------

         procedure Test_Unary
           (Op               : Unary_Operator;
            Operand          : Rational;
            Expected_Message : String)
         is
            Result : Rational;
         begin
            begin
               Result.Set (Op (Operand));
               Assert (False, "Should raise a Failure exception");
            exception
               when E : Rational_Numbers.Failure =>
                  Assert (Exception_Message (E), Expected_Message);
            end;
         end Test_Unary;

         A, B : Rational;
      begin
         A.Set ("2/2", Canonicalize => False);
         B.Set ("1/2");

         Test_Binary ("+"'Access, A, B, "Left operand must be canonicalized");
         Test_Binary ("-"'Access, A, B, "Left operand must be canonicalized");
         Test_Binary ("*"'Access, A, B, "Left operand must be canonicalized");
         Test_Binary ("/"'Access, A, B, "Left operand must be canonicalized");

         Test_Binary ("+"'Access, B, A, "Right operand must be canonicalized");
         Test_Binary ("-"'Access, B, A, "Right operand must be canonicalized");
         Test_Binary ("*"'Access, B, A, "Right operand must be canonicalized");
         Test_Binary ("/"'Access, B, A, "Right operand must be canonicalized");

         Test_Unary ("-"'Access, A, "operand must be canonicalized");
         Test_Unary ("abs"'Access, A, "operand must be canonicalized");
      end;

end Test_Arithmetics;

begin
   Test_Assignments;
   Test_Conversions;
   Test_Arithmetics;
end Test_Rationals;
