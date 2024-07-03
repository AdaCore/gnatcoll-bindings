------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Text_IO;
with Interfaces.C.Strings;
with GNATCOLL.CPP.Strings;
with Test_Assert; use Test_Assert;

function Test return Integer is
   use Ada.Text_IO;
   use Interfaces.C;
   use Interfaces.C.Strings;
   use GNATCOLL.CPP.Strings;

   -------------------
   -- Output_Header --
   -------------------

   procedure Output_Header (Msg : String) is
      Dots : String (1 .. 60) := (others => '.');

   begin
      Put (Msg);
      Put (Dots (1 .. Dots'Length - Msg'Length));
   end Output_Header;

   -----------------------------------------------------
   -- Constructors, Data(), Size() and Length() Tests --
   -----------------------------------------------------

   procedure Test_01 is
      Str : CPP_String := New_CPP_String;

   begin
      Assert
        (Data (Str) = "" and then Size (Str) = 0 and then Length (Str) = 0);

      Free (Str);
   end Test_01;

   procedure Test_02 is
      Str : CPP_String := New_CPP_String ("Ada & C++");

   begin
      Assert
        (Data (Str) = "Ada & C++" and then Size (Str) = 9
         and then Length (Str) = 9);

      Free (Str);
   end Test_02;

   procedure Test_03 is
      Str_1 : CPP_String := New_CPP_String ("Ada & C++");
      Str_2 : CPP_String := New_CPP_String (Str_1);

   begin
      Assert
        (Data (Str_2) = "Ada & C++" and then Size (Str_2) = 9
         and then Length (Str_2) = 9);

      Free (Str_1);
      Free (Str_2);
   end Test_03;

   procedure Test_04 is
      C_Ptr : chars_ptr  := New_String ("Ada & C++");
      Str   : CPP_String := New_CPP_String (C_Ptr);

   begin
      Assert
        (Data (Str) = "Ada & C++" and then Size (Str) = 9
         and then Length (Str) = 9);

      Free (C_Ptr);
      Free (Str);
   end Test_04;

   procedure Test_05 is
      Str : CPP_String := New_CPP_String (10, '*');

   begin

      Assert
        (Data (Str) = "**********" and then Size (Str) = 10
         and then Length (Str) = 10);

      Free (Str);
   end Test_05;

   -------------------------
   -- Tests of Capacity() --
   -------------------------

   procedure Test_06 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Assert
        (Data (Str) = "Ada" and then Size (Str) = 3
         and then Capacity (Str) >= Size (Str)
         and then Max_Size (Str) > Capacity (Str));

      Free (Str);
   end Test_06;

   procedure Test_07 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Assert
        (Data (Str) = "Ada" and then Size (Str) = 3
         and then Capacity (Str) >= Size (Str)
         and then Max_Size (Str) > Capacity (Str));

      Free (Str);
   end Test_07;

   -----------------------
   -- Tests of Resize() --
   -----------------------

   procedure Test_08 is
      Str : CPP_String := New_CPP_String ("Ada & C++");

   begin
      Resize (Str, 3);

      Assert (Data (Str) = "Ada" and then Size (Str) = 3);

      Free (Str);
   end Test_08;

   procedure Test_09 is
      Str       : CPP_String      := New_CPP_String ("Ada");
      Init_Size : constant Size_T := Size (Str);

   begin
      Resize (Str, 2 * Init_Size);

      Assert
        (Data (Str) = "Ada" and then Size (Str) = 2 * Init_Size
         and then Length (Str) = 2 * Init_Size);

      Free (Str);
   end Test_09;

   procedure Test_10 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Resize (Str, 6, '-');

      Assert
        (Data (Str) = "Ada---" and then Size (Str) = 6
         and then Length (Str) = 6);

      Free (Str);
   end Test_10;

   ------------------------
   -- Tests of Reserve() --
   ------------------------

   procedure Test_11 is
      Str           : CPP_String      := New_CPP_String ("Ada");
      Init_Capacity : constant Size_T := Capacity (Str);

   begin
      Reserve (Str, Init_Capacity + 5);

      Assert (Capacity (Str) >= Init_Capacity + 5);

      Free (Str);
   end Test_11;

   ----------------------------------
   -- Tests of Clear() and Empty() --
   ----------------------------------

   procedure Test_12 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Clear (Str);

      Assert
        (Data (Str) = "" and then Size (Str) = 0 and then Length (Str) = 0
         and then Empty (Str));

      Free (Str);
   end Test_12;

   ------------------------
   -- Tests of Char_At() --
   ------------------------

   procedure Test_13 is
      Str : CPP_String := New_CPP_String ("abcd");

   begin
      Assert
        (Char_At (Str, 0) = 'a' and then Char_At (Str, 1) = 'b'
         and then Char_At (Str, 2) = 'c' and then Char_At (Str, 3) = 'd');

      Free (Str);
   end Test_13;

   -----------------------
   -- Tests of Append() --
   -----------------------

   procedure Test_14 is
      Str : CPP_String := New_CPP_String;

   begin
      Append (Str, "Ada");
      Append (Str, " & C++");

      Assert
        (Data (Str) = "Ada & C++" and then Size (Str) = 9
         and then Length (Str) = 9);

      Free (Str);
   end Test_14;

   procedure Test_15 is
      Str   : CPP_String := New_CPP_String;
      Str_1 : CPP_String := New_CPP_String ("Ada");
      Str_2 : CPP_String := New_CPP_String (" & C++");

   begin
      Append (Str, Str_1);
      Append (Str, Str_2);

      Assert
        (Data (Str) = "Ada & C++" and then Size (Str) = 9
         and then Length (Str) = 9);

      Free (Str_1);
      Free (Str_2);
      Free (Str);
   end Test_15;

   procedure Test_16 is
      Str     : CPP_String := New_CPP_String;
      C_Ptr_1 : chars_ptr  := New_String ("Ada");
      C_Ptr_2 : chars_ptr  := New_String (" & C++");

   begin
      Append (Str, C_Ptr_1);
      Append (Str, C_Ptr_2);

      Assert
        (Data (Str) = "Ada & C++" and then Size (Str) = 9
         and then Length (Str) = 9);

      Free (C_Ptr_1);
      Free (C_Ptr_2);
      Free (Str);
   end Test_16;

   procedure Test_17 is
      Str_1 : CPP_String := New_CPP_String ("Ada");
      Str_2 : CPP_String := New_CPP_String (" Programming");

   begin
      Append (Str_1, Str_2, Subpos => 4, Sublen => 4);

      Assert
        (Data (Str_1) = "Adagram" and then Size (Str_1) = 7
         and then Length (Str_1) = 7);

      Free (Str_1);
      Free (Str_2);
   end Test_17;

   procedure Test_18 is
      Str   : CPP_String := New_CPP_String ("Ada");
      C_Ptr : chars_ptr  := New_String (" Programming");

   begin
      Append (Str, C_Ptr, N => 8);

      Assert
        (Data (Str) = "Ada Program" and then Size (Str) = 11
         and then Length (Str) = 11);

      Free (Str);
      Free (C_Ptr);
   end Test_18;

   procedure Test_19 is
      Str : CPP_String := New_CPP_String ("Ada/C");

   begin
      Append (Str, 2, '+');

      Assert
        (Data (Str) = "Ada/C++" and then Size (Str) = 7
         and then Length (Str) = 7);

      Free (Str);
   end Test_19;

   -----------------------
   -- Tests of Assign() --
   -----------------------

   procedure Test_20 is
      Str : CPP_String := New_CPP_String ("abc");

   begin
      Assign (Str, "Ada/C++");

      Assert
        (Data (Str) = "Ada/C++" and then Size (Str) = 7
         and then Length (Str) = 7);

      Free (Str);
   end Test_20;

   procedure Test_21 is
      Str_1 : CPP_String := New_CPP_String ("Ada/C++");
      Str_2 : CPP_String := New_CPP_String;

   begin
      Assign (Str_2, Str_1);

      Assert
        (Data (Str_2) = "Ada/C++" and then Size (Str_2) = 7
         and then Length (Str_2) = 7);

      Free (Str_1);
      Free (Str_2);
   end Test_21;

   procedure Test_22 is
      C_Ptr : chars_ptr  := New_String ("Ada/C++");
      Str   : CPP_String := New_CPP_String;

   begin
      Assign (Str, C_Ptr);

      Assert
        (Data (Str) = "Ada/C++" and then Size (Str) = 7
         and then Length (Str) = 7);

      Free (C_Ptr);
      Free (Str);
   end Test_22;

   procedure Test_23 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Assign (Str, 'C');

      Assert
        (Data (Str) = "C" and then Size (Str) = 1 and then Length (Str) = 1);

      Free (Str);
   end Test_23;

   -----------------------
   -- Tests of Insert() --
   -----------------------

   procedure Test_24 is
      Str : CPP_String := New_CPP_String ("012345");

   begin
      Insert (Str, 3, "Ada");

      Assert
        (Data (Str) = "012Ada345" and then Size (Str) = 9
         and then Length (Str) = 9);

      Free (Str);
   end Test_24;

   procedure Test_25 is
      Str_1 : CPP_String := New_CPP_String ("012345");
      Str_2 : CPP_String := New_CPP_String ("Ada");

   begin
      Insert (Str_1, 3, Str_2);

      Assert
        (Data (Str_1) = "012Ada345" and then Size (Str_1) = 9
         and then Length (Str_1) = 9);

      Free (Str_1);
      Free (Str_2);
   end Test_25;

   procedure Test_26 is
      Str_1 : CPP_String := New_CPP_String ("012345");
      Str_2 : CPP_String := New_CPP_String (" Programming");

   begin
      Insert (Str_1, 3, Str_2, Subpos => 1, Sublen => 7);

      Assert
        (Data (Str_1) = "012Program345" and then Size (Str_1) = 13
         and then Length (Str_1) = 13);

      Free (Str_1);
      Free (Str_2);
   end Test_26;

   procedure Test_27 is
      Str   : CPP_String := New_CPP_String ("012345");
      C_Ptr : chars_ptr  := New_String ("Ada Programming");

   begin
      Insert (Str, 3, C_Ptr, N => 7);

      Assert
        (Data (Str) = "012Ada Pro345" and then Size (Str) = 13
         and then Length (Str) = 13);

      Free (C_Ptr);
      Free (Str);
   end Test_27;

   procedure Test_28 is
      Str : CPP_String      := New_CPP_String ("012345");
      Pos : constant size_t := 3;
      N   : constant size_t := 4;

   begin
      Insert (Str, 3, N, 'x');

      Assert
        (Data (Str) = "012xxxx345" and then Size (Str) = 10
         and then Length (Str) = 10);

      Free (Str);
   end Test_28;

   ------------------------
   -- Tests of Push_Back --
   ------------------------

   procedure Test_29 is
      Str : CPP_String := New_CPP_String ("Ada/");

   begin
      Push_Back (Str, 'C');
      Push_Back (Str, '+');
      Push_Back (Str, '+');

      Assert
        (Data (Str) = "Ada/C++" and then Size (Str) = 7
         and then Length (Str) = 7);

      Free (Str);
   end Test_29;

   -----------------------
   -- Tests of Pop_Back --
   -----------------------

   procedure Test_30 is
      Str : CPP_String := New_CPP_String ("Ada/");

   begin
      Pop_Back (Str);

      Assert (Data (Str) = "Ada");

      Free (Str);
   end Test_30;

   --------------------
   -- Tests of Erase --
   --------------------

   procedure Test_31 is
      Str : CPP_String := New_CPP_String ("012345");

   begin
      Erase (Str, 3, 2);

      Assert
        (Data (Str) = "0125" and then Size (Str) = 4
         and then Length (Str) = 4);

      Free (Str);
   end Test_31;

   procedure Test_32 is
      Str : CPP_String := New_CPP_String ("012345");

   begin
      Erase (Str);

      Assert
        (Data (Str) = "" and then Size (Str) = 0 and then Length (Str) = 0);

      Free (Str);
   end Test_32;

   ----------------------
   -- Tests of Replace --
   ----------------------

   procedure Test_33 is
      Str : CPP_String := New_CPP_String ("abcdefg");

   begin
      Replace (Str, Pos => 2, Len => 3, Text => "012345");

      Assert (Data (Str) = "ab012345fg");

      Free (Str);
   end Test_33;

   procedure Test_34 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : CPP_String := New_CPP_String ("012345");

   begin
      Replace (Str_1, Pos => 2, Len => 3, Text => Str_2);

      Assert (Data (Str_1) = "ab012345fg");

      Free (Str_1);
      Free (Str_2);
   end Test_34;

   procedure Test_35 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : CPP_String := New_CPP_String ("012345");

   begin
      Replace
        (Str_1, Pos => 2, Len => 3, Text => Str_2, Subpos => 1, Sublen => 3);

      Assert (Data (Str_1) = "ab123fg");

      Free (Str_1);
      Free (Str_2);
   end Test_35;

   procedure Test_36 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : chars_ptr  := New_String ("012345");

   begin
      Replace (Str_1, Pos => 2, Len => 3, Text => Str_2);

      Assert (Data (Str_1) = "ab012345fg");

      Free (Str_1);
      Free (Str_2);
   end Test_36;

   procedure Test_37 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : chars_ptr  := New_String ("012345");

   begin
      Replace (Str_1, Pos => 2, Len => 3, Text => Str_2, N => 3);

      Assert (Data (Str_1) = "ab012fg");

      Free (Str_1);
      Free (Str_2);
   end Test_37;

   procedure Test_38 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : chars_ptr  := New_String ("012345");

   begin
      Replace (Str_1, Pos => 2, Len => 3, N => 4, C => '.');

      Assert (Data (Str_1) = "ab....fg");

      Free (Str_1);
      Free (Str_2);
   end Test_38;

   -------------------
   -- Tests of Swap --
   -------------------

   procedure Test_39 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : CPP_String := New_CPP_String ("012345");

   begin
      Swap (Str_1, Str_2);

      Assert (Data (Str_1) = "012345" and then Data (Str_2) = "abcdefg");

      Free (Str_1);
      Free (Str_2);
   end Test_39;

   --------------------
   -- Tests of C_Str --
   --------------------

   procedure Test_40 is
      Text     : constant String := "abcdef";
      Str      : CPP_String      := New_CPP_String (Text);
      C_Ptr    : chars_ptr;
      Expected : chars_ptr       := New_String (Text);

   begin
      C_Ptr := C_Str (Str);

      declare
         Value_C_Ptr    : constant String := Value (C_Ptr);
         Value_Expected : constant String := Value (Expected);

      begin
         Assert
           (Strlen (C_Ptr) = Text'Length
            and then Strlen (C_Ptr) = Strlen (Expected)
            and then Value_C_Ptr = Value_Expected);
      end;

      Free (Str);
      Free (Expected);
   end Test_40;

   -------------------
   -- Tests of Data --
   -------------------

   procedure Test_41 is
      Text     : constant String := "Ada & C++";
      Str      : CPP_String      := New_CPP_String (Text);
      C_Ptr    : chars_ptr;
      Expected : chars_ptr       := New_String (Text);

   begin
      C_Ptr := Data (Str);

      declare
         Value_C_Ptr    : constant String := Value (C_Ptr);
         Value_Expected : constant String := Value (Expected);

      begin
         Assert
           (Strlen (C_Ptr) = Text'Length
            and then Strlen (C_Ptr) = Strlen (Expected)
            and then Value_C_Ptr = Value_Expected);
      end;

      Free (Str);
      Free (Expected);
   end Test_41;

   -------------------
   -- Tests of Copy --
   -------------------

   procedure Test_42 is
      Str       : CPP_String := New_CPP_String ("Ada & C++");
      C_Ptr     : chars_ptr  := New_String ("1234567890");
      Num_Chars : size_t;

   begin
      Copy
        (From_Str  => Str, To_Str => C_Ptr, Len => 7, Pos => 0,
         Num_Chars => Num_Chars);

      declare
         Value_C_Ptr : constant String := Value (C_Ptr);

      begin
         Assert (Num_Chars = 7 and then Value_C_Ptr = "Ada & C890");
      end;

      Free (Str);
      Free (C_Ptr);
   end Test_42;

   -------------------
   -- Tests of Find --
   -------------------

   procedure Test_43 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("Ada");
      Pos  : size_t;

   begin
      Pos := Find (Str, Text);

      Assert (Pos = 8);

      Free (Str);
      Free (Text);
   end Test_43;

   procedure Test_44 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Ada");
      Pos  : size_t;

   begin
      Pos := Find (Str, Text);

      Assert (Pos = 8);

      Free (Str);
      Free (Text);
   end Test_44;

   procedure Test_45 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Adaxxx");
      Pos  : size_t;

   begin
      Pos := Find (Str, Text, Pos => 0, N => 3);

      Assert (Pos = 8);

      Free (Str);
      Free (Text);
   end Test_45;

   procedure Test_46 is
      Str : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos : size_t;

   begin
      Pos := Find (Str, 'd');

      Assert (Pos = 9);

      Free (Str);
   end Test_46;

   ---------------------------
   -- Tests of Reverse_Find --
   ---------------------------

   procedure Test_47 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("Ada");
      Pos  : size_t;

   begin
      Pos := Reverse_Find (Str, Text);

      Assert (Pos = 8);

      Free (Str);
      Free (Text);
   end Test_47;

   procedure Test_48 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Ada");
      Pos  : size_t;

   begin
      Pos := Reverse_Find (Str, Text);

      Assert (Pos = 8);

      Free (Str);
      Free (Text);
   end Test_48;

   procedure Test_49 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Adaxxx");
      Pos  : size_t;

   begin
      Pos := Reverse_Find (Str, Text, Pos => Npos, N => 3);

      Assert (Pos = 8);

      Free (Str);
      Free (Text);
   end Test_49;

   procedure Test_50 is
      Str : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos : size_t;

   begin
      Pos := Find (Str, 'd');

      Assert (Pos = 9);

      Free (Str);
   end Test_50;

   ----------------------------
   -- Tests of Find_First_Of --
   ----------------------------

   procedure Test_51 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("dC");
      Pos  : size_t;

   begin
      Pos := Find_First_Of (Str, Text);

      Assert (Pos = 9);

      Free (Str);
      Free (Text);
   end Test_51;

   procedure Test_52 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dC");
      Pos  : size_t;

   begin
      Pos := Find_First_Of (Str, Text);

      Assert (Pos = 9);

      Free (Str);
      Free (Text);
   end Test_52;

   procedure Test_53 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dCxxx");
      Pos  : size_t;

   begin
      Pos := Find_First_Of (Str, Text, Pos => 0, N => 2);

      Assert (Pos = 9);

      Free (Str);
      Free (Text);
   end Test_53;

   procedure Test_54 is
      Str : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos : size_t;

   begin
      Pos := Find_First_Of (Str, 'd');

      Assert (Pos = 9);

      Free (Str);
   end Test_54;

   ---------------------------
   -- Tests of Find_Last_Of --
   ---------------------------

   procedure Test_55 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("dC");
      Pos  : size_t;

   begin
      Pos := Find_Last_Of (Str, Text);

      Assert (Pos = 14);

      Free (Str);
      Free (Text);
   end Test_55;

   procedure Test_56 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dC");
      Pos  : size_t;

   begin
      Pos := Find_Last_Of (Str, Text);

      Assert (Pos = 14);

      Free (Str);
      Free (Text);
   end Test_56;

   procedure Test_57 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dCxxx");
      Pos  : size_t;

   begin
      Pos := Find_Last_Of (Str, Text, Pos => Npos, N => 2);

      Assert (Pos = 14);

      Free (Str);
      Free (Text);
   end Test_57;

   procedure Test_58 is
      Str : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos : size_t;

   begin
      Pos := Find_Last_Of (Str, 'd');

      Assert (Pos = 9);

      Free (Str);
   end Test_58;

   --------------------------------
   -- Tests of Find_First_Not_Of --
   --------------------------------

   procedure Test_59 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("seTt");
      Pos  : size_t;

   begin
      Pos := Find_First_Not_Of (Str, Text);

      Assert (Pos = 4);

      Free (Str);
      Free (Text);
   end Test_59;

   procedure Test_60 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("seTt");
      Pos  : size_t;

   begin
      Pos := Find_First_Not_Of (Str, Text);

      Assert (Pos = 4);

      Free (Str);
      Free (Text);
   end Test_60;

   procedure Test_61 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("seTtxxx");
      Pos  : size_t;

   begin
      Pos := Find_First_Not_Of (Str, Text, Pos => 0, N => 4);

      Assert (Pos = 4);

      Free (Str);
      Free (Text);
   end Test_61;

   procedure Test_62 is
      Str : CPP_String := New_CPP_String ("xxxxxxxxTesting Ada & C++");
      Pos : size_t;

   begin
      Pos := Find_First_Not_Of (Str, 'x');

      Assert (Pos = 8);

      Free (Str);
   end Test_62;

   -------------------------------
   -- Tests of Find_Last_Not_Of --
   -------------------------------

   procedure Test_63 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("&+ C");
      Pos  : size_t;

   begin
      Pos := Find_Last_Not_Of (Str, Text);

      Assert (Pos = 10);

      Free (Str);
      Free (Text);
   end Test_63;

   procedure Test_64 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("&+ C");
      Pos  : size_t;

   begin
      Pos := Find_Last_Not_Of (Str, Text);

      Assert (Pos = 10);

      Free (Str);
      Free (Text);
   end Test_64;

   procedure Test_65 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("&+ Cxxx");
      Pos  : size_t;

   begin
      Pos := Find_Last_Not_Of (Str, Text, Pos => Npos, N => 4);

      Assert (Pos = 10);

      Free (Str);
      Free (Text);
   end Test_65;

   procedure Test_66 is
      Str : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos : size_t;

   begin
      Pos := Find_Last_Not_Of (Str, '+');

      Assert (Pos = 14);

      Free (Str);
   end Test_66;

   ---------------------
   -- Tests of Substr --
   ---------------------

   procedure Test_67 is
      Str    : CPP_String := New_CPP_String ("Testing Ada & C++");
      Result : CPP_String := Substr (Str, 8, 3);

   begin
      Assert (Data (Result) = "Ada");

      Free (Str);
      Free (Result);
   end Test_67;

   ----------------------
   -- Tests of Compare --
   ----------------------

   procedure Test_68 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Compare (Str_1, Str_1) = 0 and then Compare (Str_1, Str_2) > 0);

      Free (Str_1);
      Free (Str_2);
   end Test_68;

   procedure Test_69 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert
        (Compare (Str_1, 8, 3, Str_2) = 0
         and then Compare (Str_1, 0, 2, Str_2) > 0);

      Free (Str_1);
      Free (Str_2);
   end Test_69;

   procedure Test_70 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("xxAdaxx");

   begin
      Assert
        (Compare (Str_1, 8, 3, Str_2, 2, 3) = 0
         and then Compare (Str_1, 0, 2, Str_2, 2, 3) > 0);

      Free (Str_1);
      Free (Str_2);
   end Test_70;

   procedure Test_71 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Ada");

   begin
      Assert (Compare (Str_1, Str_1) = 0 and then Compare (Str_1, Str_2) > 0);

      Free (Str_1);
      Free (Str_2);
   end Test_71;

   procedure Test_72 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Ada");

   begin
      Assert
        (Compare (Str_1, 8, 3, Str_2) = 0
         and then Compare (Str_1, 0, 2, Str_2) > 0);

      Free (Str_1);
      Free (Str_2);
   end Test_72;

   procedure Test_73 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Adaxxxx");

   begin
      Assert
        (Compare (Str_1, 8, 3, Str_2, 3) = 0
         and then Compare (Str_1, 0, 2, Str_2, 2) > 0);

      Free (Str_1);
      Free (Str_2);
   end Test_73;

   ------------------
   -- Tests of "=" --
   ------------------

   procedure Test_74 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 = Str_2 and then not (Str_1 = Str_3));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_74;

   procedure Test_75 is
      Str_1 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 = Str_2 and then not (Str_1 = Str_3));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_75;

   procedure Test_76 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 = Str_2 and then not (Str_1 = Str_3));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_76;

   ------------------
   -- Tests of "<" --
   ------------------

   procedure Test_77 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_3 < Str_1 and then not (Str_1 < Str_2));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_77;

   procedure Test_78 is
      Str_1 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_3 < Str_1 and then not (Str_1 < Str_2));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_78;

   procedure Test_79 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_3 < Str_1 and then not (Str_1 < Str_2));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_79;

   -------------------
   -- Tests of "<=" --
   -------------------

   procedure Test_80 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_3 <= Str_1 and then Str_1 <= Str_2);

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_80;

   procedure Test_81 is
      Str_1 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_3 <= Str_1 and then Str_1 <= Str_2);

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_81;

   procedure Test_82 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_3 <= Str_1 and then Str_1 <= Str_2);

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_82;

   ------------------
   -- Tests of ">" --
   ------------------

   procedure Test_83 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 > Str_3 and then not (Str_1 > Str_2));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_83;

   procedure Test_84 is
      Str_1 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 > Str_3 and then not (Str_1 > Str_2));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_84;

   procedure Test_85 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 > Str_3 and then not (Str_1 > Str_2));

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_85;

   -------------------
   -- Tests of ">=" --
   -------------------

   procedure Test_86 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 >= Str_3 and then Str_1 >= Str_2);

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_86;

   procedure Test_87 is
      Str_1 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 >= Str_3 and then Str_1 >= Str_2);

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_87;

   procedure Test_88 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      Assert (Str_1 >= Str_3 and then Str_1 >= Str_2);

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end Test_88;

   --------------------
   --  Run_All_Tests --
   --------------------

   procedure Run_All_Tests is
   begin
      --  Constructors, Data(), Size() and Length() Tests -----------
      Test_01;
      Test_02;

      Test_03;

      Test_04;

      Test_05;

      -- Tests of Capacity() ----------------------------------------

      Test_06;

      Test_07;

      -- Tests of Resize() ------------------------------------------

      Test_08;

      Test_09;

      Test_10;

      -- Tests of Reserve() -----------------------------------------

      Test_11;

      -- Tests of Clear() and Empty() -------------------------------

      Test_12;

      -- Tests of Char_At() -----------------------------------------

      Test_13;

      -- Tests of Append() ------------------------------------------

      Test_14;

      Test_15;

      Test_16;

      Test_17;

      Test_18;

      Test_19;

      -- Tests of Assign() ------------------------------------------

      Test_20;

      Test_21;

      Test_22;

      Test_23;

      -- Tests of Insert() ------------------------------------------

      Test_24;

      Test_25;

      Test_26;

      Test_27;

      Test_28;

      -- Tests of Push_Back -----------------------------------------

      Test_29;

      -- Tests of Pop_Back ------------------------------------------

      Test_30;

      -- Tests of Erase ---------------------------------------------

      Test_31;

      Test_32;

      -- Tests of Replace -------------------------------------------

      Test_33;

      Test_34;

      Test_35;

      Test_36;

      Test_37;

      Test_38;

      -- Tests of Swap ----------------------------------------------

      Test_39;

      -- Tests of C_Str ---------------------------------------------

      Test_40;

      -- Tests of Data ----------------------------------------------

      Test_41;

      -- Tests of Copy ----------------------------------------------

      Test_42;

      -- Tests of Find ----------------------------------------------

      Test_43;

      Test_44;

      Test_45;

      Test_46;

      -- Tests of Reverse_Find --------------------------------------

      Test_47;

      Test_48;

      Test_49;

      Test_50;

      -- Tests of Find_First_Of -------------------------------------

      Test_51;

      Test_52;

      Test_53;

      Test_54;

      -- Tests of Find_Last_Of --------------------------------------

      Test_55;

      Test_56;

      Test_57;

      Test_58;

      -- Tests of Find_First_Not_Of ---------------------------------

      Test_59;

      Test_60;

      Test_61;

      Test_62;

      -- Tests of Find_Last_Not_Of ----------------------------------

      Test_63;

      Test_64;

      Test_65;

      Test_66;

      -- Tests of Substr --------------------------------------------

      Test_67;

      -- Tests of Compare -------------------------------------------

      Test_68;

      Test_69;

      Test_70;

      Test_71;

      Test_72;

      Test_73;

      -- Tests of "=" -----------------------------------------------

      Test_74;

      Test_75;

      Test_76;

      -- Tests of "<" -----------------------------------------------

      Test_77;

      Test_78;

      Test_79;

      -- Tests of "<=" ----------------------------------------------

      Test_80;

      Test_81;

      Test_82;

      -- Tests of ">" -----------------------------------------------

      Test_83;

      Test_84;

      Test_85;

      -- Tests of ">=" ----------------------------------------------

      Test_86;

      Test_87;

      Test_88;
   end Run_All_Tests;

begin
   Run_All_Tests;
   return Report;
end Test;
