------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2023, AdaCore                     --
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

   --  Test truncate/floor/ceil division/remainder on a set of cases to make
   --  sure all functions are bound to the correct functions.

   declare
      type Test_Values is record
         N, D : Integer;
         --  Numerator and divisor

         TQ, TR : Integer;
         --  Expected quotient/remainder for the "truncate" division

         FQ, FR : Integer;
         --  Expected quotient/remainder for the "floor" division

         CQ, CR : Integer;
         --  Expected quotient/remainder for the "ceil" division
      end record;

      type Test_Values_Array is array (Positive range <>) of Test_Values;

      function Img (I : Integer) return String is (Make (I'Image).Image);

      Tests : constant Test_Values_Array :=
        ((N => 10, D => 10,
          TQ => 1, TR => 0,
          FQ => 1, FR => 0,
          CQ => 1, CR => 0),

         (N => 12, D => 10,
          TQ => 1, TR => 2,
          FQ => 1, FR => 2,
          CQ => 2, CR => -8),

         (N => 15, D => 10,
          TQ => 1, TR => 5,
          FQ => 1, FR => 5,
          CQ => 2, CR => -5),

         (N => 17, D => 10,
          TQ => 1, TR => 7,
          FQ => 1, FR => 7,
          CQ => 2, CR => -3),

         (N => 20, D => 10,
          TQ => 2, TR => 0,
          FQ => 2, FR => 0,
          CQ => 2, CR => 0),

         (N => -10, D => 10,
          TQ => -1, TR => 0,
          FQ => -1, FR => 0,
          CQ => -1, CR => 0),

         (N => -12, D => 10,
          TQ => -1, TR => -2,
          FQ => -2, FR => 8,
          CQ => -1,  CR => -2),

         (N => -15, D => 10,
          TQ => -1, TR => -5,
          FQ => -2, FR => 5,
          CQ => -1, CR => -5),

         (N => -17, D => 10,
          TQ => -1, TR => -7,
          FQ => -2, FR => 3,
          CQ => -1, CR => -7),

         (N => -20, D => 10,
          TQ => -2, TR => 0,
          FQ => -2, FR => 0,
          CQ => -2, CR => 0));
   begin
      for T of Tests loop
         declare
            N : constant Big_Integer := Make (T.N'Image);
            D : constant Big_Integer := Make (T.D'Image);

            N_Img : constant String := N.Image;
            D_Img : constant String := D.Image;

            TQ : constant Big_Integer := Truncate_Divide (N, D);
            TR : constant Big_Integer := Truncate_Remainder (N, D);

            FQ : constant Big_Integer := Floor_Divide (N, D);
            FR : constant Big_Integer := Floor_Remainder (N, D);

            CQ : constant Big_Integer := Ceil_Divide (N, D);
            CR : constant Big_Integer := Ceil_Remainder (N, D);
         begin
            Assert (TQ.Image, Img (T.TQ), "test: " & N_Img & " tdiv " & D_Img);
            Assert (TR.Image, Img (T.TR), "test: " & N_Img & " trem " & D_Img);

            Assert (FQ.Image, Img (T.FQ), "test: " & N_Img & " fdiv " & D_Img);
            Assert (FR.Image, Img (T.FR), "test: " & N_Img & " frem " & D_Img);

            Assert (CQ.Image, Img (T.CQ), "test: " & N_Img & " cdiv " & D_Img);
            Assert (CR.Image, Img (T.CR), "test: " & N_Img & " crem " & D_Img);
         end;
      end loop;
   end;
end Test_Operators;
