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

procedure Test_Image is

   N : Big_Integer;

   Input : constant String := "14000000000000000000000" &
                              "000000000000000000000001";

   Negated_Input : constant String := '-' & Input;

   Input_Base_2 : constant String :=
      "1001110011110010000001111001000111010111100010000101010100011010011" &
      "0000111101110001100000010011011111001001100000000000000000000000000" &
      "00000000000000000001";

   Input_Base_3 : constant String :=
      "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

begin
   Set (N, Input);
   Assert (Image (N), Input, "test_image 1");

   Negate (N);
   Assert (Image (N), Negated_Input, "test_image 2");

   Set (N, Input);
   Assert (Image (N, Base => 2), Input_Base_2, "test_image 3");

   for J in Input_Base_3'Range loop
      declare
         Img  : constant String := Input_Base_3 (1 .. J);
         Img0 : constant String := Input_Base_3 (1 .. J) & '0';
         Base : constant Positive := Img0'Length;
      begin
         Set (N, Img, Base => GNATCOLL.GMP.Int (Base));

         Assert
           (Img, Image (N, (if J < 36 then -Base else Base)),
            "test_image -" & J'Img);

         Set (N, Img0, Base => GNATCOLL.GMP.Int (Base));

         Assert
           (Img0, Image (N, (if J < 36 then -Base else Base)),
            "test_image: " & J'Img);
      end;
   end loop;
end Test_Image;
