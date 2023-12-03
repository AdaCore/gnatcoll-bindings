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

procedure Run_Tests is
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
   end;

   -----------------------------------------------------
   -- Constructors, Data(), Size() and Length() Tests --
   -----------------------------------------------------

   procedure Test_01 is
      Str : CPP_String := New_CPP_String;                  -- Test

   begin
      if Data (Str) = ""
        and then Size (Str) = 0
        and then Length (Str) = 0
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_02 is
      Str : CPP_String := New_CPP_String ("Ada & C++");    -- Test

   begin
      if Data (Str) = "Ada & C++"
        and then Size (Str) = 9
        and then Length (Str) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_03 is
      Str_1 : CPP_String := New_CPP_String ("Ada & C++");
      Str_2 : CPP_String := New_CPP_String (Str_1);        -- Test

   begin
      if Data (Str_2) = "Ada & C++"
        and then Size (Str_2) = 9
        and then Length (Str_2) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_04 is
      C_Ptr : chars_ptr  := New_String ("Ada & C++");      -- Test
      Str   : CPP_String := New_CPP_String (C_Ptr);

   begin
      if Data (Str) = "Ada & C++"
        and then Size (Str) = 9
        and then Length (Str) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (C_Ptr);
      Free (Str);
   end;

   procedure Test_05 is
      Str : CPP_String := New_CPP_String (10, '*');        -- Test

   begin

      if Data (Str) = "**********"
        and then Size (Str) = 10
        and then Length (Str) = 10
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   -------------------------
   -- Tests of Capacity() --
   -------------------------

   procedure Test_06 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      if Data (Str) = "Ada"
        and then Size (Str) = 3
        and then Capacity (Str) >= Size (Str)              -- Test
        and then Max_Size (Str) > Capacity (Str)           -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_07 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      if Data (Str) = "Ada"
        and then Size (Str) = 3
        and then Capacity (Str) >= Size (Str)              -- Test
        and then Max_Size (Str) > Capacity (Str)           -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   -----------------------
   -- Tests of Resize() --
   -----------------------

   procedure Test_08 is
      Str : CPP_String := New_CPP_String ("Ada & C++");

   begin
      Resize (Str, 3);                                     -- Test

      if Data (Str) = "Ada"
        and then Size (Str) = 3
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_09 is
      Str       : CPP_String := New_CPP_String ("Ada");
      Init_Size : constant Size_T := Size (Str);

   begin
      Resize (Str, 2 * Init_Size);                         -- Test

      if Data (Str) = "Ada"
        and then Size (Str) = 2 * Init_Size
        and then Length (Str) = 2 * Init_Size
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_10 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Resize (Str, 6, '-');                                -- Test

      if Data (Str) = "Ada---"
        and then Size (Str) = 6
        and then Length (Str) = 6
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ------------------------
   -- Tests of Reserve() --
   ------------------------

   procedure Test_11 is
      Str : CPP_String := New_CPP_String ("Ada");
      Init_Capacity : constant Size_T := Capacity (Str);

   begin
      Reserve (Str, Init_Capacity + 5);                    -- Test

      if Capacity (Str) >= Init_Capacity + 5 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ----------------------------------
   -- Tests of Clear() and Empty() --
   ----------------------------------

   procedure Test_12 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Clear (Str);                                         -- Test

      if Data (Str) = ""
        and then Size (Str) = 0
        and then Length (Str) = 0
        and then Empty (Str)                               -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ------------------------
   -- Tests of Char_At() --
   ------------------------

   procedure Test_13 is
      Str : CPP_String := New_CPP_String ("abcd");

   begin
      if Char_At (Str, 0) = 'a'                            -- Test
        and then Char_At (Str, 1) = 'b'                    -- Test
        and then Char_At (Str, 2) = 'c'                    -- Test
        and then Char_At (Str, 3) = 'd'                    -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   -----------------------
   -- Tests of Append() --
   -----------------------

   procedure Test_14 is
      Str : CPP_String := New_CPP_String;

   begin
      Append (Str, "Ada");                                 -- Test
      Append (Str, " & C++");                              -- Test

      if Data (Str) = "Ada & C++"
        and then Size (Str) = 9
        and then Length (Str) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_15 is
      Str   : CPP_String := New_CPP_String;
      Str_1 : CPP_String := New_CPP_String ("Ada");
      Str_2 : CPP_String := New_CPP_String (" & C++");

   begin
      Append (Str, Str_1);                                 -- Test
      Append (Str, Str_2);                                 -- Test

      if Data (Str) = "Ada & C++"
        and then Size (Str) = 9
        and then Length (Str) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str);
   end;

   procedure Test_16 is
      Str     : CPP_String := New_CPP_String;
      C_Ptr_1 : chars_ptr  := New_String ("Ada");
      C_Ptr_2 : chars_ptr  := New_String (" & C++");

   begin
      Append (Str, C_Ptr_1);                               -- Test
      Append (Str, C_Ptr_2);                               -- Test

      if Data (Str) = "Ada & C++"
        and then Size (Str) = 9
        and then Length (Str) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (C_Ptr_1);
      Free (C_Ptr_2);
      Free (Str);
   end;

   procedure Test_17 is
      Str_1 : CPP_String := New_CPP_String ("Ada");
      Str_2 : CPP_String := New_CPP_String (" Programming");

   begin
      Append (Str_1, Str_2, Subpos => 4, Sublen => 4);     -- Test

      if Data (Str_1) = "Adagram"
        and then Size (Str_1) = 7
        and then Length (Str_1) = 7
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_18 is
      Str   : CPP_String := New_CPP_String ("Ada");
      C_Ptr : chars_ptr  := New_String (" Programming");

   begin
      Append (Str, C_Ptr, N => 8);                         -- Test

      if Data (Str) = "Ada Program"
        and then Size (Str) = 11
        and then Length (Str) = 11
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (C_Ptr);
   end;

   procedure Test_19 is
      Str : CPP_String := New_CPP_String ("Ada/C");

   begin
      Append (Str, 2, '+');                                -- Test

      if Data (Str) = "Ada/C++"
        and then Size (Str) = 7
        and then Length (Str) = 7
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   -----------------------
   -- Tests of Assign() --
   -----------------------

   procedure Test_20 is
      Str : CPP_String := New_CPP_String ("abc");

   begin
      Assign (Str, "Ada/C++");                             -- Test

      if Data (Str) = "Ada/C++"
        and then Size (Str) = 7
        and then Length (Str) = 7
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_21 is
      Str_1 : CPP_String := New_CPP_String ("Ada/C++");
      Str_2 : CPP_String := New_CPP_String;

   begin
      Assign (Str_2, Str_1);                               -- Test

      if Data (Str_2) = "Ada/C++"
        and then Size (Str_2) = 7
        and then Length (Str_2) = 7
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_22 is
      C_Ptr : chars_ptr  := New_String ("Ada/C++");
      Str   : CPP_String := New_CPP_String;

   begin
      Assign (Str, C_Ptr);                                 -- Test

      if Data (Str) = "Ada/C++"
        and then Size (Str) = 7
        and then Length (Str) = 7
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (C_Ptr);
      Free (Str);
   end;

   procedure Test_23 is
      Str : CPP_String := New_CPP_String ("Ada");

   begin
      Assign (Str, 'C');                                   -- Test

      if Data (Str) = "C"
        and then Size (Str) = 1
        and then Length (Str) = 1
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   -----------------------
   -- Tests of Insert() --
   -----------------------

   procedure Test_24 is
      Str : CPP_String := New_CPP_String ("012345");

   begin
      Insert (Str, 3, "Ada");                            -- Test

      if Data (Str) = "012Ada345"
        and then Size (Str) = 9
        and then Length (Str) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_25 is
      Str_1 : CPP_String := New_CPP_String ("012345");
      Str_2 : CPP_String := New_CPP_String ("Ada");

   begin
      Insert (Str_1, 3, Str_2);                            -- Test

      if Data (Str_1) = "012Ada345"
        and then Size (Str_1) = 9
        and then Length (Str_1) = 9
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_26 is
      Str_1 : CPP_String := New_CPP_String ("012345");
      Str_2 : CPP_String := New_CPP_String (" Programming");

   begin
      Insert (Str_1, 3, Str_2, Subpos => 1, Sublen => 7);  -- Test

      if Data (Str_1) = "012Program345"
        and then Size (Str_1) = 13
        and then Length (Str_1) = 13
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_27 is
      Str   : CPP_String := New_CPP_String ("012345");
      C_Ptr : chars_ptr  := New_String ("Ada Programming");

   begin
      Insert (Str, 3, C_Ptr, N => 7);                    -- Test

      if Data (Str) = "012Ada Pro345"
        and then Size (Str) = 13
        and then Length (Str) = 13
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (C_Ptr);
      Free (Str);
   end;

   procedure Test_28 is
      Str : CPP_String := New_CPP_String ("012345");
      Pos : constant Size_T := 3;
      N   : constant Size_T := 4;

   begin
      Insert (Str, 3, N, 'x');                           -- Test

      if Data (Str) = "012xxxx345"
        and then Size (Str) = 10
        and then Length (Str) = 10
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ------------------------
   -- Tests of Push_Back --
   ------------------------

   procedure Test_29 is
      Str : CPP_String := New_CPP_String ("Ada/");

   begin
      Push_Back (Str, 'C');                                -- Test
      Push_Back (Str, '+');                                -- Test
      Push_Back (Str, '+');                                -- Test

      if Data (Str) = "Ada/C++"
        and then Size (Str) = 7
        and then Length (Str) = 7
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   -----------------------
   -- Tests of Pop_Back --
   -----------------------

   procedure Test_30 is
      Str : CPP_String := New_CPP_String ("Ada/");

   begin
      Pop_Back (Str);                                      -- Test

      if Data (Str) = "Ada" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   --------------------
   -- Tests of Erase --
   --------------------

   procedure Test_31 is
      Str : CPP_String := New_CPP_String ("012345");

   begin
      Erase (Str, 3, 2);                                 -- Test

      if Data (Str) = "0125"
        and then Size (Str) = 4
        and then Length (Str) = 4
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_32 is
      Str : CPP_String := New_CPP_String ("012345");

   begin
      Erase (Str);                                         -- Test

      if Data (Str) = ""
        and then Size (Str) = 0
        and then Length (Str) = 0
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ----------------------
   -- Tests of Replace --
   ----------------------

   procedure Test_33 is
      Str : CPP_String := New_CPP_String ("abcdefg");

   begin
      Replace (Str,                                        -- Test
        Pos  => 2,
        Len  => 3,
        Text => "012345");

      if Data (Str) = "ab012345fg" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   procedure Test_34 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : CPP_String := New_CPP_String ("012345");

   begin
      Replace (Str_1,                                      -- Test
        Pos  => 2,
        Len  => 3,
        Text => Str_2);

      if Data (Str_1) = "ab012345fg" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_35 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : CPP_String := New_CPP_String ("012345");

   begin
      Replace (Str_1,                                      -- Test
        Pos  => 2,
        Len  => 3,
        Text => Str_2,
        Subpos => 1,
        Sublen => 3);

      if Data (Str_1) = "ab123fg" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_36 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : chars_ptr  := New_String ("012345");

   begin
      Replace (Str_1,                                      -- Test
        Pos  => 2,
        Len  => 3,
        Text => Str_2);

      if Data (Str_1) = "ab012345fg" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_37 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : chars_ptr  := New_String ("012345");

   begin
      Replace (Str_1,                                      -- Test
        Pos  => 2,
        Len  => 3,
        Text => Str_2,
        N    => 3);

      if Data (Str_1) = "ab012fg" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_38 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : chars_ptr  := New_String ("012345");

   begin
      Replace (Str_1,                                      -- Test
        Pos  => 2,
        Len  => 3,
        N    => 4,
        C    => '.');

      if Data (Str_1) = "ab....fg" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   -------------------
   -- Tests of Swap --
   -------------------

   procedure Test_39 is
      Str_1 : CPP_String := New_CPP_String ("abcdefg");
      Str_2 : CPP_String := New_CPP_String ("012345");

   begin
      Swap (Str_1, Str_2);                                 -- Test

      if Data (Str_1) = "012345"
        and then Data (Str_2) = "abcdefg"
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   --------------------
   -- Tests of C_Str --
   --------------------

   procedure Test_40 is
      Text     : constant String := "abcdef";
      Str      : CPP_String := New_CPP_String (Text);
      C_Ptr    : chars_ptr;
      Expected : chars_ptr  := New_String (Text);

   begin
      C_Ptr := C_Str (Str);                                -- Test

      declare
         Value_C_Ptr    : constant String := Value (C_Ptr);
         Value_Expected : constant String := Value (Expected);

      begin
         if Strlen (C_Ptr) = Text'Length
           and then Strlen (C_Ptr) = Strlen (Expected)
           and then Value_C_Ptr = Value_Expected
         then
            Put_Line ("OK");
         else
            Put_Line ("Failed");
         end if;
      end;

      Free (Str);
      Free (Expected);
   end;

   -------------------
   -- Tests of Data --
   -------------------

   procedure Test_41 is
      Text     : constant String := "Ada & C++";
      Str      : CPP_String := New_CPP_String (Text);
      C_Ptr    : chars_ptr;
      Expected : chars_ptr  := New_String (Text);

   begin
      C_Ptr := Data (Str);                                 -- Test

      declare
         Value_C_Ptr    : constant String := Value (C_Ptr);
         Value_Expected : constant String := Value (Expected);

      begin
         if Strlen (C_Ptr) = Text'Length
           and then Strlen (C_Ptr) = Strlen (Expected)
           and then Value_C_Ptr = Value_Expected
         then
            Put_Line ("OK");
         else
            Put_Line ("Failed");
         end if;
      end;

      Free (Str);
      Free (Expected);
   end;

   -------------------
   -- Tests of Copy --
   -------------------

   procedure Test_42 is
      Str       : CPP_String := New_CPP_String ("Ada & C++");
      C_Ptr     : chars_ptr  := New_String ("1234567890");
      Num_Chars : Size_T;

   begin
      Copy                                                 -- Test
        (From_Str  => Str,
         To_Str    => C_Ptr,
         Len       => 7,
         Pos       => 0,
         Num_Chars => Num_Chars);

      declare
         Value_C_Ptr : constant String := Value (C_Ptr);

      begin
         if Num_Chars = 7
           and then Value_C_Ptr = "Ada & C890"
         then
            Put_Line ("OK");
         else
            Put_Line ("Failed");
         end if;
      end;

      Free (Str);
      Free (C_Ptr);
   end;

   -------------------
   -- Tests of Find --
   -------------------

   procedure Test_43 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("Ada");
      Pos  : Size_T;

   begin
      Pos := Find (Str, Text);                             -- Test

      if Pos = 8 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_44 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Ada");
      Pos  : Size_T;

   begin
      Pos := Find (Str, Text);                             -- Test

      if Pos = 8 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_45 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Adaxxx");
      Pos  : Size_T;

   begin
      Pos := Find (Str, Text, Pos => 0, N => 3);           -- Test

      if Pos = 8 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_46 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos  : Size_T;

   begin
      Pos := Find (Str, 'd');                              -- Test

      if Pos = 9 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ---------------------------
   -- Tests of Reverse_Find --
   ---------------------------

   procedure Test_47 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("Ada");
      Pos  : Size_T;

   begin
      Pos := Reverse_Find (Str, Text);                     -- Test

      if Pos = 8 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_48 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Ada");
      Pos  : Size_T;

   begin
      Pos := Reverse_Find (Str, Text);                     -- Test

      if Pos = 8 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_49 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("Adaxxx");
      Pos  : Size_T;

   begin
      Pos := Reverse_Find (Str, Text, Pos => Npos, N => 3); -- Test

      if Pos = 8 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_50 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos  : Size_T;

   begin
      Pos := Find (Str, 'd');                              -- Test

      if Pos = 9 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ----------------------------
   -- Tests of Find_First_Of --
   ----------------------------

   procedure Test_51 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("dC");
      Pos  : Size_T;

   begin
      Pos := Find_First_Of (Str, Text);                    -- Test

      if Pos = 9 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_52 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dC");
      Pos  : Size_T;

   begin
      Pos := Find_First_Of (Str, Text);                    -- Test

      if Pos = 9 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_53 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dCxxx");
      Pos  : Size_T;

   begin
      Pos := Find_First_Of (Str, Text, Pos => 0, N => 2);  -- Test

      if Pos = 9 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_54 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos  : Size_T;

   begin
      Pos := Find_First_Of (Str, 'd');                     -- Test

      if Pos = 9 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ---------------------------
   -- Tests of Find_Last_Of --
   ---------------------------

   procedure Test_55 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("dC");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Of (Str, Text);                     -- Test

      if Pos = 14 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_56 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dC");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Of (Str, Text);                     -- Test

      if Pos = 14 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_57 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("dCxxx");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Of (Str, Text, Pos => Npos, N => 2);  -- Test

      if Pos = 14 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_58 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Of (Str, 'd');                      -- Test

      if Pos = 9 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   --------------------------------
   -- Tests of Find_First_Not_Of --
   --------------------------------

   procedure Test_59 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("seTt");
      Pos  : Size_T;

   begin
      Pos := Find_First_Not_Of (Str, Text);                -- Test

      if Pos = 4 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_60 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("seTt");
      Pos  : Size_T;

   begin
      Pos := Find_First_Not_Of (Str, Text);                -- Test

      if Pos = 4 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_61 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("seTtxxx");
      Pos  : Size_T;

   begin
      Pos := Find_First_Not_Of (Str, Text, Pos => 0, N => 4);  -- Test

      if Pos = 4 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_62 is
      Str  : CPP_String := New_CPP_String ("xxxxxxxxTesting Ada & C++");
      Pos  : Size_T;

   begin
      Pos := Find_First_Not_Of (Str, 'x');                 -- Test

      if Pos = 8 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   -------------------------------
   -- Tests of Find_Last_Not_Of --
   -------------------------------

   procedure Test_63 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : CPP_String := New_CPP_String ("&+ C");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Not_Of (Str, Text);                     -- Test

      if Pos = 10 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_64 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("&+ C");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Not_Of (Str, Text);                     -- Test

      if Pos = 10 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_65 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Text : chars_ptr  := New_String ("&+ Cxxx");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Not_Of (Str, Text, Pos => Npos, N => 4);  -- Test

      if Pos = 10 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Text);
   end;

   procedure Test_66 is
      Str  : CPP_String := New_CPP_String ("Testing Ada & C++");
      Pos  : Size_T;

   begin
      Pos := Find_Last_Not_Of (Str, '+');                  -- Test

      if Pos = 14 then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
   end;

   ---------------------
   -- Tests of Substr --
   ---------------------

   procedure Test_67 is
      Str    : CPP_String := New_CPP_String ("Testing Ada & C++");
      Result : CPP_String := Substr (Str, 8, 3);           -- Test

   begin
      if Data (Result) = "Ada" then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str);
      Free (Result);
   end;

   ----------------------
   -- Tests of Compare --
   ----------------------

   procedure Test_68 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Ada");

   begin
      if Compare (Str_1, Str_1) = 0                        -- Test
        and then Compare (Str_1, Str_2) > 0                -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_69 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Ada");

   begin
      if Compare (Str_1, 8, 3, Str_2) = 0                  -- Test
       and then Compare (Str_1, 0, 2, Str_2) > 0           -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_70 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("xxAdaxx");

   begin
      if Compare (Str_1, 8, 3, Str_2, 2, 3) = 0            -- Test
       and then Compare (Str_1, 0, 2, Str_2, 2, 3) > 0     -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_71 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Ada");

   begin
      if Compare (Str_1, Str_1) = 0                        -- Test
        and then Compare (Str_1, Str_2) > 0                -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_72 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Ada");

   begin
      if Compare (Str_1, 8, 3, Str_2) = 0                  -- Test
       and then Compare (Str_1, 0, 2, Str_2) > 0           -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   procedure Test_73 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String ("Adaxxxx");

   begin
      if Compare (Str_1, 8, 3, Str_2, 3) = 0               -- Test
       and then Compare (Str_1, 0, 2, Str_2, 2) > 0        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
   end;

   ------------------
   -- Tests of "=" --
   ------------------

   procedure Test_74 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 = Str_2                                     -- Test
       and then not (Str_1 = Str_3)                        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_75 is
      Str_1 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 = Str_2                                     -- Test
        and then not (Str_1 = Str_3)                       -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_76 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 = Str_2                                     -- Test
        and then not (Str_1 = Str_3)                       -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   ------------------
   -- Tests of "<" --
   ------------------

   procedure Test_77 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_3 < Str_1                                     -- Test
       and then not (Str_1 < Str_2)                        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_78 is
      Str_1 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_3 < Str_1                                     -- Test
       and then not (Str_1 < Str_2)                        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_79 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_3 < Str_1                                     -- Test
       and then not (Str_1 < Str_2)                        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   -------------------
   -- Tests of "<=" --
   -------------------

   procedure Test_80 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_3 <= Str_1                                    -- Test
       and then Str_1 <= Str_2                             -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_81 is
      Str_1 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_3 <= Str_1                                    -- Test
       and then Str_1 <= Str_2                             -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_82 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_3 <= Str_1                                    -- Test
       and then Str_1 <= Str_2                             -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   ------------------
   -- Tests of ">" --
   ------------------

   procedure Test_83 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 > Str_3                                     -- Test
       and then not (Str_1 > Str_2)                        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_84 is
      Str_1 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 > Str_3                                     -- Test
       and then not (Str_1 > Str_2)                        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_85 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 > Str_3                                     -- Test
       and then not (Str_1 > Str_2)                        -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   -------------------
   -- Tests of ">=" --
   -------------------

   procedure Test_86 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 >= Str_3                                    -- Test
       and then Str_1 >= Str_2                             -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_87 is
      Str_1 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_2 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 >= Str_3                                    -- Test
       and then Str_1 >= Str_2                             -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   procedure Test_88 is
      Str_1 : CPP_String := New_CPP_String ("Testing Ada & C++");
      Str_2 : chars_ptr  := New_String     ("Testing Ada & C++");
      Str_3 : CPP_String := New_CPP_String ("Ada");

   begin
      if Str_1 >= Str_3                                    -- Test
       and then Str_1 >= Str_2                             -- Test
      then
         Put_Line ("OK");
      else
         Put_Line ("Failed");
      end if;

      Free (Str_1);
      Free (Str_2);
      Free (Str_3);
   end;

   --------------------
   --  Run_All_Tests --
   --------------------

   procedure Run_All_Tests is
   begin
      --  Constructors, Data(), Size() and Length() Tests -----------

      Output_Header ("Test_01");
      Test_01;

      Output_Header ("Test_02");
      Test_02;

      Output_Header ("Test_03");
      Test_03;

      Output_Header ("Test_04");
      Test_04;

      Output_Header ("Test_05");
      Test_05;

      -- Tests of Capacity() ----------------------------------------

      Output_Header ("Test_06");
      Test_06;

      Output_Header ("Test_07");
      Test_07;

      -- Tests of Resize() ------------------------------------------

      Output_Header ("Test_08");
      Test_08;

      Output_Header ("Test_09");
      Test_09;

      Output_Header ("Test_10");
      Test_10;

      -- Tests of Reserve() -----------------------------------------

      Output_Header ("Test_11");
      Test_11;

      -- Tests of Clear() and Empty() -------------------------------

      Output_Header ("Test_12");
      Test_12;

      -- Tests of Char_At() -----------------------------------------

      Output_Header ("Test_13");
      Test_13;

      -- Tests of Append() ------------------------------------------

      Output_Header ("Test_14");
      Test_14;

      Output_Header ("Test_15");
      Test_15;

      Output_Header ("Test_16");
      Test_16;

      Output_Header ("Test_17");
      Test_17;

      Output_Header ("Test_18");
      Test_18;

      Output_Header ("Test_19");
      Test_19;

      -- Tests of Assign() ------------------------------------------

      Output_Header ("Test_20");
      Test_20;

      Output_Header ("Test_21");
      Test_21;

      Output_Header ("Test_22");
      Test_22;

      Output_Header ("Test_23");
      Test_23;

      -- Tests of Insert() ------------------------------------------

      Output_Header ("Test_24");
      Test_24;

      Output_Header ("Test_25");
      Test_25;

      Output_Header ("Test_26");
      Test_26;

      Output_Header ("Test_27");
      Test_27;

      Output_Header ("Test_28");
      Test_28;

      -- Tests of Push_Back -----------------------------------------

      Output_Header ("Test_29");
      Test_29;

      -- Tests of Pop_Back ------------------------------------------

      Output_Header ("Test_30");
      Test_30;

      -- Tests of Erase ---------------------------------------------

      Output_Header ("Test_31");
      Test_31;

      Output_Header ("Test_32");
      Test_32;

      -- Tests of Replace -------------------------------------------

      Output_Header ("Test_33");
      Test_33;

      Output_Header ("Test_34");
      Test_34;

      Output_Header ("Test_35");
      Test_35;

      Output_Header ("Test_36");
      Test_36;

      Output_Header ("Test_37");
      Test_37;

      Output_Header ("Test_38");
      Test_38;

      -- Tests of Swap ----------------------------------------------

      Output_Header ("Test_39");
      Test_39;

      -- Tests of C_Str ---------------------------------------------

      Output_Header ("Test_40");
      Test_40;

      -- Tests of Data ----------------------------------------------

      Output_Header ("Test_41");
      Test_41;

      -- Tests of Copy ----------------------------------------------

      Output_Header ("Test_42");
      Test_42;

      -- Tests of Find ----------------------------------------------

      Output_Header ("Test_43");
      Test_43;

      Output_Header ("Test_44");
      Test_44;

      Output_Header ("Test_45");
      Test_45;

      Output_Header ("Test_46");
      Test_46;

      -- Tests of Reverse_Find --------------------------------------

      Output_Header ("Test_47");
      Test_47;

      Output_Header ("Test_48");
      Test_48;

      Output_Header ("Test_49");
      Test_49;

      Output_Header ("Test_50");
      Test_50;

      -- Tests of Find_First_Of -------------------------------------

      Output_Header ("Test_51");
      Test_51;

      Output_Header ("Test_52");
      Test_52;

      Output_Header ("Test_53");
      Test_53;

      Output_Header ("Test_54");
      Test_54;

      -- Tests of Find_Last_Of --------------------------------------

      Output_Header ("Test_55");
      Test_55;

      Output_Header ("Test_56");
      Test_56;

      Output_Header ("Test_57");
      Test_57;

      Output_Header ("Test_58");
      Test_58;

      -- Tests of Find_First_Not_Of ---------------------------------

      Output_Header ("Test_59");
      Test_59;

      Output_Header ("Test_60");
      Test_60;

      Output_Header ("Test_61");
      Test_61;

      Output_Header ("Test_62");
      Test_62;

      -- Tests of Find_Last_Not_Of ----------------------------------

      Output_Header ("Test_63");
      Test_63;

      Output_Header ("Test_64");
      Test_64;

      Output_Header ("Test_65");
      Test_65;

      Output_Header ("Test_66");
      Test_66;

      -- Tests of Substr --------------------------------------------

      Output_Header ("Test_67");
      Test_67;

      -- Tests of Compare -------------------------------------------

      Output_Header ("Test_68");
      Test_68;

      Output_Header ("Test_69");
      Test_69;

      Output_Header ("Test_70");
      Test_70;

      Output_Header ("Test_71");
      Test_71;

      Output_Header ("Test_72");
      Test_72;

      Output_Header ("Test_73");
      Test_73;

      -- Tests of "=" -----------------------------------------------

      Output_Header ("Test_74");
      Test_74;

      Output_Header ("Test_75");
      Test_75;

      Output_Header ("Test_76");
      Test_76;

      -- Tests of "<" -----------------------------------------------

      Output_Header ("Test_77");
      Test_77;

      Output_Header ("Test_78");
      Test_78;

      Output_Header ("Test_79");
      Test_79;

      -- Tests of "<=" ----------------------------------------------

      Output_Header ("Test_80");
      Test_80;

      Output_Header ("Test_81");
      Test_81;

      Output_Header ("Test_82");
      Test_82;

      -- Tests of ">" -----------------------------------------------

      Output_Header ("Test_83");
      Test_83;

      Output_Header ("Test_84");
      Test_84;

      Output_Header ("Test_85");
      Test_85;

      -- Tests of ">=" ----------------------------------------------

      Output_Header ("Test_86");
      Test_86;

      Output_Header ("Test_87");
      Test_87;

      Output_Header ("Test_88");
      Test_88;
   end;

begin
   Run_All_Tests;
end;
