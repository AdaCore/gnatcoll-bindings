------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

procedure Test_Bitwise is

   --  Test all logical/bitwise operators on a set of cases to make sure all
   --  functions are bound to the correct C routines.

   function Img (I : Integer) return String is (Make (I'Image).Image);

   type Binop_Test is record
      Left, Right : Integer;
      --  Left and right operands for the binary operation to test

      And_Result, Or_Result, Xor_Result : Integer;
      --  Expected result for the and/or/xor operators
   end record;

   type Binop_Test_Array is array (Positive range <>) of Binop_Test;

   Binop_Tests : constant Binop_Test_Array :=
     ((Left => 0, Right => 0,
       And_Result => 0,
       Or_Result  => 0,
       Xor_Result => 0),
      (Left => 0, Right => 1,
       And_Result => 0,
       Or_Result  => 1,
       Xor_Result => 1),
      (Left => 1, Right => 0,
       And_Result => 0,
       Or_Result  => 1,
       Xor_Result => 1),
      (Left => 1, Right => 1,
       And_Result => 1,
       Or_Result  => 1,
       Xor_Result => 0),
      (Left => 0, Right => -1,
       And_Result => 0,
       Or_Result  => -1,
       Xor_Result => -1),
      (Left => -1, Right => 0,
       And_Result => 0,
       Or_Result  => -1,
       Xor_Result => -1),
      (Left => -1, Right => -1,
       And_Result => -1,
       Or_Result  => -1,
       Xor_Result => 0));

   type Unop_Test is record
      Op : Integer;
      --  Operand for the unary operator to test

      Result : Integer;
      --  Expected result for the operator
   end record;

   type Unop_Test_Array is array (Positive range <>) of Unop_Test;

   Unop_Tests : constant Unop_Test_Array :=
     ((0, -1), (1, -2), (-1, 0));

begin
   for T of Binop_Tests loop
      declare
         L : constant Big_Integer := Make (T.Left'Image);
         R : constant Big_Integer := Make (T.Right'Image);

         And_Result : constant Big_Integer := L and R;
         Or_Result  : constant Big_Integer := L or R;
         Xor_Result : constant Big_Integer := L xor R;
      begin
         Assert (And_Result.Image,
                 Img (T.And_Result),
                 "test: " & L.Image & " and " & R.Image);
         Assert (Or_Result.Image,
                 Img (T.Or_Result),
                 "test: " & L.Image & " or " & R.Image);
         Assert (Xor_Result.Image,
                 Img (T.Xor_Result),
                 "test: " & L.Image & " xor " & R.Image);
      end;
   end loop;

   for T of Unop_Tests loop
      declare
         O      : constant Big_Integer := Make (T.Op'Image);
         Result : constant Big_Integer := not O;
      begin
         Assert (Result.Image, Img (T.Result), "test: not " & O.Image);
      end;
   end loop;
end Test_Bitwise;
