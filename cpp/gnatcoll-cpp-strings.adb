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

with Interfaces.C.Extensions;

package body GNATCOLL.CPP.Strings is
   use Interfaces.C;
   use Interfaces.C.Extensions;
   use Interfaces.C.Strings;
   use System;

   package External is

      procedure Append_String
        (Str  : in out CPP_String;
         Text : CPP_String);
      pragma Import (C, Append_String,
        "gnatcoll_cpp_append_string");

      procedure Append_Substring
        (Str    : in out CPP_String;
         Text   : CPP_String;
         Subpos : size_t;
         Sublen : size_t);
      pragma Import (C, Append_Substring,
        "gnatcoll_cpp_append_substring");

      procedure Append_Text
        (Str  : in out CPP_String;
         Text : chars_ptr);
      pragma Import (C, Append_Text,
        "gnatcoll_cpp_append_text");

      procedure Append_Buffer
        (Str  : in out CPP_String;
         Text : chars_ptr;
         N    : size_t);
      pragma Import (C, Append_Buffer,
        "gnatcoll_cpp_append_buffer");

      procedure Append_Fill
        (Str  : in out CPP_String;
         N    : size_t;
         C    : Character);
      pragma Import (C, Append_Fill,
        "gnatcoll_cpp_append_fill");

      procedure Assign_String
        (Str  : in out CPP_String;
         Text : CPP_String);
      pragma Import (C, Assign_String,
        "gnatcoll_cpp_assign_string");

      procedure Assign_Text
        (Str  : in out CPP_String;
         Text : chars_ptr);
      pragma Import (C, Assign_Text,
        "gnatcoll_cpp_assign_text");

      procedure Assign_Char
        (Str  : in out CPP_String;
         C    : Character);
      pragma Import (C, Assign_Char,
        "gnatcoll_cpp_assign_char");

      function Capacity
        (Str  : CPP_String) return size_t;
      pragma Import (C, Capacity,
        "gnatcoll_cpp_capacity");

      function Char_At
        (Str : CPP_String; Pos : size_t) return Character;
      pragma Import (C, Char_At,
        "gnatcoll_cpp_char_at");

      procedure Clear
        (Str : in out CPP_String);
      pragma Import (C, Clear,
        "gnatcoll_cpp_clear");

      function Compare
        (Left  : CPP_String;
         Right : CPP_String) return Integer;
      pragma Import (C, Compare,
        "gnatcoll_cpp_compare");

      function Compare_With_Substring
        (Left  : CPP_String;
         Pos   : size_t;
         Len   : size_t;
         Right : CPP_String) return Integer;
      pragma Import (C, Compare_With_Substring,
        "gnatcoll_cpp_compare_with_substring");

      function Compare_Substrings
        (Left   : CPP_String;
         Pos    : size_t;
         Len    : size_t;
         Right  : CPP_String;
         Subpos : size_t;
         Sublen : size_t) return Integer;
      pragma Import (C, Compare_Substrings,
        "gnatcoll_cpp_compare_substrings");

      function Compare_With_Text
        (Left  : CPP_String;
         Right : chars_ptr) return Integer;
      pragma Import (C, Compare_With_Text,
        "gnatcoll_cpp_compare_with_text");

      function Compare_Substring_With_Text
        (Left  : CPP_String;
         Pos   : size_t;
         Len   : size_t;
         Right : chars_ptr) return Integer;
      pragma Import (C, Compare_Substring_With_Text,
        "gnatcoll_cpp_compare_substring_with_text");

      function Compare_Substring_With_Buffer
        (Left  : CPP_String;
         Pos   : size_t;
         Len   : size_t;
         Right : chars_ptr;
         N     : size_t) return Integer;
      pragma Import (C, Compare_Substring_With_Buffer,
        "gnatcoll_cpp_compare_substring_with_buffer");

      procedure Copy
        (From_Str  : CPP_String;
         To_Str    : chars_ptr;
         Len       : size_t;
         Pos       : size_t;
         Num_Chars : access size_t);
      pragma Import (C, Copy,
        "gnatcoll_cpp_copy");

      function C_Str
        (Str : CPP_String) return chars_ptr;
      pragma Import (C, C_Str,
        "gnatcoll_cpp_c_str");

      function Data
        (Str  : CPP_String) return chars_ptr;
      pragma Import (C, Data,
        "gnatcoll_cpp_data");

      procedure Destroy
        (Str  : in out CPP_String);
      pragma Import (C, Destroy,
        "gnatcoll_cpp_destroy");

      function Empty
        (Str : CPP_String) return bool;
      pragma Import (C, Empty,
        "gnatcoll_cpp_empty");

      procedure Erase_Sequence
        (Str : in out CPP_String;
         Pos : size_t;
         Len : size_t);
      pragma Import (C, Erase_Sequence,
        "gnatcoll_cpp_erase_sequence");

      function Find_String
        (Str  : CPP_String;
         Text : CPP_String;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_String,
        "gnatcoll_cpp_find_string");

      function Find_Text
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Text,
        "gnatcoll_cpp_find_text");

      function Find_Buffer
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t;
         N    : size_t) return size_t;
      pragma Import (C, Find_Buffer,
        "gnatcoll_cpp_find_buffer");

      function Find_Char
        (Str  : CPP_String;
         C    : Character;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Char,
        "gnatcoll_cpp_find_char");

      function Find_First_Not_Of_String
        (Str  : CPP_String;
         Text : CPP_String;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_First_Not_Of_String,
        "gnatcoll_cpp_find_first_not_of_string");

      function Find_First_Not_Of_Text
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_First_Not_Of_Text,
        "gnatcoll_cpp_find_first_not_of_text");

      function Find_First_Not_Of_Buffer
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t;
         N    : size_t) return size_t;
      pragma Import (C, Find_First_Not_Of_Buffer,
        "gnatcoll_cpp_find_first_not_of_buffer");

      function Find_First_Not_Of_Char
        (Str  : CPP_String;
         C    : Character;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_First_Not_Of_Char,
        "gnatcoll_cpp_find_first_not_of_char");

      function Find_First_Of_String
        (Str  : CPP_String;
         Text : CPP_String;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_First_Of_String,
        "gnatcoll_cpp_find_first_of_string");

      function Find_First_Of_Text
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_First_Of_Text,
        "gnatcoll_cpp_find_first_of_text");

      function Find_First_Of_Buffer
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t;
         N    : size_t) return size_t;
      pragma Import (C, Find_First_Of_Buffer,
        "gnatcoll_cpp_find_first_of_buffer");

      function Find_First_Of_Char
        (Str  : CPP_String;
         C    : Character;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_First_Of_Char,
        "gnatcoll_cpp_find_first_of_char");

      function Find_Last_Not_Of_String
        (Str  : CPP_String;
         Text : CPP_String;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Last_Not_Of_String,
        "gnatcoll_cpp_find_last_not_of_string");

      function Find_Last_Not_Of_Text
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Last_Not_Of_Text,
        "gnatcoll_cpp_find_last_not_of_text");

      function Find_Last_Not_Of_Buffer
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t;
         N    : size_t) return size_t;
      pragma Import (C, Find_Last_Not_Of_Buffer,
        "gnatcoll_cpp_find_last_not_of_buffer");

      function Find_Last_Not_Of_Char
        (Str  : CPP_String;
         C    : Character;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Last_Not_Of_Char,
        "gnatcoll_cpp_find_last_not_of_char");

      function Find_Last_Of_String
        (Str  : CPP_String;
         Text : CPP_String;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Last_Of_String,
        "gnatcoll_cpp_find_last_of_string");

      function Find_Last_Of_Text
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Last_Of_Text,
        "gnatcoll_cpp_find_last_of_text");

      function Find_Last_Of_Buffer
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t;
         N    : size_t) return size_t;
      pragma Import (C, Find_Last_Of_Buffer,
        "gnatcoll_cpp_find_last_of_buffer");

      function Find_Last_Of_Char
        (Str  : CPP_String;
         C    : Character;
         Pos  : size_t) return size_t;
      pragma Import (C, Find_Last_Of_Char,
        "gnatcoll_cpp_find_last_of_char");

      procedure Init
        (Str  : in out CPP_String);
      pragma Import (C, Init,
        "gnatcoll_cpp_init");

      procedure Init_With_Text
        (Str  : in out CPP_String;
         Text : chars_ptr);
      pragma Import (C, Init_With_Text,
        "gnatcoll_cpp_init_with_text");

      procedure Init_With_Fill
        (Str  : in out CPP_String;
         N    : size_t;
         C    : Character);
      pragma Import (C, Init_With_Fill,
        "gnatcoll_cpp_init_with_fill");

      procedure Insert_String
        (Str  : in out CPP_String;
         Pos  : size_t;
         Text : CPP_String);
      pragma Import (C, Insert_String,
        "gnatcoll_cpp_insert_string");

      procedure Insert_Substring
        (Str    : in out CPP_String;
         Pos    : size_t;
         Text   : CPP_String;
         Subpos : size_t;
         Sublen : size_t);
      pragma Import (C, Insert_Substring,
        "gnatcoll_cpp_insert_substring");

      procedure Insert_Text
        (Str  : in out CPP_String;
         Pos  : size_t;
         Text : chars_ptr);
      pragma Import (C, Insert_Text,
        "gnatcoll_cpp_insert_text");

      procedure Insert_Buffer
        (Str  : in out CPP_String;
         Pos  : size_t;
         Text : chars_ptr;
         N    : size_t);
      pragma Import (C, Insert_Buffer,
        "gnatcoll_cpp_insert_buffer");

      procedure Insert_Fill
        (Str  : in out CPP_String;
         Pos  : size_t;
         N    : size_t;
         C    : Character);
      pragma Import (C, Insert_Fill,
        "gnatcoll_cpp_insert_fill");

      function Length
        (Str  : CPP_String) return size_t;
      pragma Import (C, Length,
        "gnatcoll_cpp_length");

      function Max_Size
        (Str  : CPP_String) return size_t;
      pragma Import (C, Max_Size,
        "gnatcoll_cpp_max_size");

      function Npos return size_t;
      pragma Import (C, Npos,
        "gnatcoll_cpp_npos");

      procedure Pop_Back
        (Str  : in out CPP_String);
      pragma Import (C, Pop_Back,
        "gnatcoll_cpp_pop_back");

      procedure Push_Back
        (Str  : in out CPP_String;
         C    : Character);
      pragma Import (C, Push_Back,
        "gnatcoll_cpp_push_back");

      procedure Replace_String
        (Str  : in out CPP_String;
         Pos  : size_t;
         Len  : size_t;
         Text : CPP_String);
      pragma Import (C, Replace_String,
        "gnatcoll_cpp_replace_string");

      procedure Replace_Substring
        (Str    : in out CPP_String;
         Pos    : size_t;
         Len    : size_t;
         Text   : CPP_String;
         Subpos : size_t;
         Sublen : size_t);
      pragma Import (C, Replace_Substring,
        "gnatcoll_cpp_replace_substring");

      procedure Replace_Text
        (Str  : in out CPP_String;
         Pos  : size_t;
         Len  : size_t;
         Text : chars_ptr);
      pragma Import (C, Replace_Text,
        "gnatcoll_cpp_replace_text");

      procedure Replace_Buffer
        (Str  : in out CPP_String;
         Pos  : size_t;
         Len  : size_t;
         Text : chars_ptr;
         N    : size_t);
      pragma Import (C, Replace_Buffer,
        "gnatcoll_cpp_replace_buffer");

      procedure Replace_Fill
        (Str  : in out CPP_String;
         Pos  : size_t;
         Len  : size_t;
         N    : size_t;
         C    : Character);
      pragma Import (C, Replace_Fill,
        "gnatcoll_cpp_replace_fill");

      procedure Reserve
        (Str : in out CPP_String;
         N   : size_t := 0);
      pragma Import (C, Reserve,
        "gnatcoll_cpp_reserve");

      procedure Resize
        (Str  : in out CPP_String;
         N    : size_t);
      pragma Import (C, Resize,
        "gnatcoll_cpp_resize");

      procedure Resize_And_Fill
        (Str  : in out CPP_String;
         N    : size_t;
         C    : Character);
      pragma Import (C, Resize_And_Fill,
        "gnatcoll_cpp_resize_with_fill");

      function Reverse_Find_String
        (Str  : CPP_String;
         Text : CPP_String;
         Pos  : size_t) return size_t;
      pragma Import (C, Reverse_Find_String,
        "gnatcoll_cpp_reverse_find_string");

      function Reverse_Find_Text
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t) return size_t;
      pragma Import (C, Reverse_Find_Text,
        "gnatcoll_cpp_reverse_find_text");

      function Reverse_Find_Buffer
        (Str  : CPP_String;
         Text : chars_ptr;
         Pos  : size_t;
         N    : size_t) return size_t;
      pragma Import (C, Reverse_Find_Buffer,
        "gnatcoll_cpp_reverse_find_buffer");

      function Reverse_Find_Char
        (Str  : CPP_String;
         C    : Character;
         Pos  : size_t) return size_t;
      pragma Import (C, Reverse_Find_Char,
        "gnatcoll_cpp_reverse_find_char");

      function Size
        (Str  : CPP_String) return size_t;
      pragma Import (C, Size,
        "gnatcoll_cpp_size");

      procedure Substr
        (Result : in out CPP_String;
         Str    : CPP_String;
         Pos    : size_t := 0;
         Len    : size_t := Npos);
      pragma Import (C, Substr,
        "gnatcoll_cpp_substr");

      procedure Swap
        (Str  : in out CPP_String;
         Text : in out CPP_String);
      pragma Import (C, Swap,
        "gnatcoll_cpp_swap_strings");

      --------------------------
      -- Relational_Operators --
      --------------------------

      function Eq_String
        (Left : CPP_String; Right : CPP_String) return bool;
      pragma Import (C, Eq_String,
        "gnatcoll_cpp_eq_strings");

      function Eq_Text_String
        (Left : chars_ptr;  Right : CPP_String) return bool;
      pragma Import (C, Eq_Text_String,
        "gnatcoll_cpp_eq_text_string");

      function Eq_String_Text
        (Left : CPP_String; Right : chars_ptr) return bool;
      pragma Import (C, Eq_String_Text,
        "gnatcoll_cpp_eq_string_text");

      function Lt_String
        (Left : CPP_String; Right : CPP_String) return bool;
      pragma Import (C, Lt_String,
        "gnatcoll_cpp_lt_strings");

      function Lt_Text_String
        (Left : chars_ptr;  Right : CPP_String) return bool;
      pragma Import (C, Lt_Text_String,
        "gnatcoll_cpp_lt_text_string");

      function Lt_String_Text
        (Left : CPP_String; Right : chars_ptr) return bool;
      pragma Import (C, Lt_String_Text,
        "gnatcoll_cpp_lt_string_text");

      function Le_String
        (Left : CPP_String; Right : CPP_String) return bool;
      pragma Import (C, Le_String,
        "gnatcoll_cpp_le_strings");

      function Le_Text_String
        (Left : chars_ptr;  Right : CPP_String) return bool;
      pragma Import (C, Le_Text_String,
        "gnatcoll_cpp_le_text_string");

      function Le_String_Text
        (Left : CPP_String; Right : chars_ptr) return bool;
      pragma Import (C, Le_String_Text,
        "gnatcoll_cpp_le_string_text");

      function Gt_String
        (Left : CPP_String; Right : CPP_String) return bool;
      pragma Import (C, Gt_String,
        "gnatcoll_cpp_gt_strings");

      function Gt_Text_String
        (Left : chars_ptr;  Right : CPP_String) return bool;
      pragma Import (C, Gt_Text_String,
        "gnatcoll_cpp_gt_text_string");

      function Gt_String_Text
        (Left : CPP_String; Right : chars_ptr) return bool;
      pragma Import (C, Gt_String_Text,
        "gnatcoll_cpp_gt_string_text");

      function Ge_String
        (Left : CPP_String; Right : CPP_String) return bool;
      pragma Import (C, Ge_String,
        "gnatcoll_cpp_ge_strings");

      function Ge_Text_String
        (Left : chars_ptr;  Right : CPP_String) return bool;
      pragma Import (C, Ge_Text_String,
        "gnatcoll_cpp_ge_text_string");

      function Ge_String_Text
        (Left : CPP_String; Right : chars_ptr) return bool;
      pragma Import (C, Ge_String_Text,
        "gnatcoll_cpp_ge_string_text");
   end External;

   procedure Check_Non_Null (Str : CPP_String);
   --  Raise Constraint_Error if Str is not initialized.

   ------------
   -- Append --
   ------------

   procedure Append
     (Str  : in out CPP_String;
      Text : String)
   is
      C_Text : chars_ptr := New_String (Text);
   begin
      Check_Non_Null (Str);
      External.Append_Text (Str, C_Text);
      Free (C_Text);
   end Append;

   procedure Append
     (Str  : in out CPP_String;
      Text : CPP_String)
   is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Append_String (Str, Text);
   end Append;

   procedure Append
     (Str    : in out CPP_String;
      Text   : CPP_String;
      Subpos : size_t;
      Sublen : size_t) is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Append_Substring (Str, Text, Subpos, Sublen);
   end Append;

   procedure Append
     (Str  : in out CPP_String;
      Text : chars_ptr) is
   begin
      Check_Non_Null (Str);
      External.Append_Text (Str, Text);
   end Append;

   procedure Append
     (Str  : in out CPP_String;
      Text : chars_ptr;
      N    : size_t) is
   begin
      Check_Non_Null (Str);
      External.Append_Buffer (Str, Text, N);
   end Append;

   procedure Append
     (Str  : in out CPP_String;
      N    : size_t;
      C    : Character) is
   begin
      Check_Non_Null (Str);
      External.Append_Fill (Str, N, C);
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Str : in out CPP_String; Text : CPP_String) is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Assign_String (Str, Text);
   end Assign;

   procedure Assign
     (Str : in out CPP_String; Text : String)
   is
      C_Text : chars_ptr := New_String (Text);
   begin
      Check_Non_Null (Str);
      Assign (Str, C_Text);
      Free (C_Text);
   end Assign;

   procedure Assign
     (Str  : in out CPP_String;
      Text : chars_ptr) is
   begin
      Check_Non_Null (Str);
      External.Assign_Text (Str, Text);
   end Assign;

   procedure Assign
     (Str : in out CPP_String; C : Character) is
   begin
      Check_Non_Null (Str);
      External.Assign_Char (Str, C);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity
     (Str : CPP_String) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Capacity (Str);
   end Capacity;

   -------------
   -- Char_At --
   -------------

   function Char_At (Str : CPP_String; Pos : size_t) return Character is
   begin
      Check_Non_Null (Str);
      return External.Char_At (Str, Pos);
   end Char_At;

   --------------------
   -- Check_Non_Null --
   --------------------

   procedure Check_Non_Null (Str : CPP_String) is
   begin
      if Str.Wrapped_String_Address = Null_Address then
         raise Constraint_Error;
      end if;
   end Check_Non_Null;

   -----------
   -- Clear --
   -----------

   procedure Clear (Str : in out CPP_String) is
   begin
      Check_Non_Null (Str);
      External.Clear (Str);
   end Clear;

   -------------
   -- Compare --
   -------------

   function Compare
     (Left  : CPP_String;
      Right : CPP_String) return Integer is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Compare (Left, Right);
   end Compare;

   function Compare
     (Left  : CPP_String;
      Pos   : size_t;
      Len   : size_t;
      Right : CPP_String) return Integer is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Compare_With_Substring (Left, Pos, Len, Right);
   end Compare;

   function Compare
     (Left   : CPP_String;
      Pos    : size_t;
      Len    : size_t;
      Right  : CPP_String;
      Subpos : size_t;
      Sublen : size_t) return Integer is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Compare_Substrings
               (Left, Pos, Len, Right, Subpos, Sublen);
   end Compare;

   function Compare
     (Left  : CPP_String;
      Right : chars_ptr) return Integer is
   begin
      Check_Non_Null (Left);
      return External.Compare_With_Text (Left, Right);
   end Compare;

   function Compare
     (Left  : CPP_String;
      Pos   : size_t;
      Len   : size_t;
      Right : chars_ptr) return Integer is
   begin
      Check_Non_Null (Left);
      return External.Compare_Substring_With_Text (Left, Pos, Len, Right);
   end Compare;

   function Compare
     (Left  : CPP_String;
      Pos   : size_t;
      Len   : size_t;
      Right : chars_ptr;
      N     : size_t) return Integer is
   begin
      Check_Non_Null (Left);
      return External.Compare_Substring_With_Buffer (Left, Pos, Len, Right, N);
   end Compare;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From_Str  : CPP_String;
      To_Str    : chars_ptr;
      Len       : size_t;
      Pos       : size_t;
      Num_Chars : out size_t)
   is
      Result : aliased size_t;
   begin
      Check_Non_Null (From_Str);
      External.Copy (From_Str, To_Str, Len, Pos, Result'Access);
      Num_Chars := Result;
   end Copy;

   -----------
   -- C_Str --
   -----------

   function C_Str
     (Str : CPP_String) return chars_ptr is
   begin
      Check_Non_Null (Str);
      return External.C_Str (Str);
   end C_Str;

   ----------
   -- Data --
   ----------

   function Data
     (Str : CPP_String) return chars_ptr is
   begin
      Check_Non_Null (Str);
      return External.Data (Str);
   end Data;

   function Data
     (Str : CPP_String) return String is
   begin
      return Value (Data (Str));
   end Data;

   -----------
   -- Empty --
   -----------

   function Empty (Str : CPP_String) return Boolean is
   begin
      Check_Non_Null (Str);
      return External.Empty (Str) /= False;
   end Empty;

   -----------
   -- Erase --
   -----------

   procedure Erase
     (Str  : in out CPP_String;
      Pos  : size_t := 0;
      Len  : size_t := Npos) is
   begin
      Check_Non_Null (Str);
      External.Erase_Sequence (Str, Pos, Len);
   end Erase;

   ----------
   -- Find --
   ----------

   function Find
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      return External.Find_String (Str, Text, Pos);
   end Find;

   function Find
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Text (Str, Text, Pos);
   end Find;

   function Find
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t;
      N    : size_t) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Buffer (Str, Text, Pos, N);
   end Find;

   function Find
     (Str  : CPP_String;
      C    : Character;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Char (Str, C, Pos);
   end Find;

   -----------------------
   -- Find_First_Not_Of --
   -----------------------

   function Find_First_Not_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      return External.Find_First_Not_Of_String (Str, Text, Pos);
   end Find_First_Not_Of;

   function Find_First_Not_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_First_Not_Of_Text (Str, Text, Pos);
   end Find_First_Not_Of;

   function Find_First_Not_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t;
      N    : size_t) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_First_Not_Of_Buffer (Str, Text, Pos, N);
   end Find_First_Not_Of;

   function Find_First_Not_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_First_Not_Of_Char (Str, C, Pos);
   end Find_First_Not_Of;

   -------------------
   -- Find_First_Of --
   -------------------

   function Find_First_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      return External.Find_First_Of_String (Str, Text, Pos);
   end Find_First_Of;

   function Find_First_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_First_Of_Text (Str, Text, Pos);
   end Find_First_Of;

   function Find_First_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t;
      N    : size_t) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_First_Of_Buffer (Str, Text, Pos, N);
   end Find_First_Of;

   function Find_First_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : size_t := 0) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_First_Of_Char (Str, C, Pos);
   end Find_First_Of;

   ----------------------
   -- Find_Last_Not_Of --
   ----------------------

   function Find_Last_Not_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      return External.Find_Last_Not_Of_String (Str, Text, Pos);
   end Find_Last_Not_Of;

   function Find_Last_Not_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Last_Not_Of_Text (Str, Text, Pos);
   end Find_Last_Not_Of;

   function Find_Last_Not_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t;
      N    : size_t) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Last_Not_Of_Buffer (Str, Text, Pos, N);
   end Find_Last_Not_Of;

   function Find_Last_Not_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Last_Not_Of_Char (Str, C, Pos);
   end Find_Last_Not_Of;

   ------------------
   -- Find_Last_Of --
   ------------------

   function Find_Last_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      return External.Find_Last_Of_String (Str, Text, Pos);
   end Find_Last_Of;

   function Find_Last_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Last_Of_Text (Str, Text, Pos);
   end Find_Last_Of;

   function Find_Last_Of
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t;
      N    : size_t) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Last_Of_Buffer (Str, Text, Pos, N);
   end Find_Last_Of;

   function Find_Last_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Find_Last_Of_Char (Str, C, Pos);
   end Find_Last_Of;

   ----------
   -- Free --
   ----------

   procedure Free (Str : in out CPP_String) is
   begin
      Check_Non_Null (Str);
      External.Destroy (Str);
      Str.Wrapped_String_Address := Null_Address;
   end Free;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : size_t;
      Text : String)
   is
      C_Text : chars_ptr := New_String (Text);
   begin
      Check_Non_Null (Str);
      External.Insert_Text (Str, Pos, C_Text);
      Free (C_Text);
   end Insert;

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : size_t;
      Text : CPP_String)
   is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Insert_String (Str, Pos, Text);
   end Insert;

   procedure Insert
     (Str    : in out CPP_String;
      Pos    : size_t;
      Text   : CPP_String;
      Subpos : size_t;
      Sublen : size_t) is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Insert_Substring (Str, Pos, Text, Subpos, Sublen);
   end Insert;

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : size_t;
      Text : chars_ptr) is
   begin
      Check_Non_Null (Str);
      External.Insert_Text (Str, Pos, Text);
   end Insert;

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : size_t;
      Text : chars_ptr;
      N    : size_t) is
   begin
      Check_Non_Null (Str);
      External.Insert_Buffer (Str, Pos, Text, N);
   end Insert;

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : size_t;
      N    : size_t;
      C    : Character) is
   begin
      Check_Non_Null (Str);
      External.Insert_Fill (Str, Pos, N, C);
   end Insert;

   ------------
   -- Length --
   ------------

   function Length
     (Str : CPP_String) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Length (Str);
   end Length;

   --------------
   -- Max_Size --
   --------------

   function Max_Size
     (Str : CPP_String) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Max_Size (Str);
   end Max_Size;

   --------------------
   -- New_CPP_String --
   --------------------

   function New_CPP_String return CPP_String is
      Result : CPP_String;
   begin
      External.Init (Result);
      return Result;
   end New_CPP_String;

   function New_CPP_String
     (Text : chars_ptr) return CPP_String
   is
      Result : CPP_String;
   begin
      External.Init_With_Text (Result, Text);
      return Result;
   end New_CPP_String;

   function New_CPP_String
     (Text : String) return CPP_String
   is
      C_Text : chars_ptr := New_String (Text);
      Result : CPP_String;
   begin
      Result := New_CPP_String (C_Text);
      Free (C_Text);

      return Result;
   end New_CPP_String;

   function New_CPP_String
     (Text : CPP_String) return CPP_String
   is
      Contents : constant String := Data (Text);
   begin
      return New_CPP_String (Contents);
   end New_CPP_String;

   function New_CPP_String
     (N : size_t; C : Character) return CPP_String
   is
      Result : CPP_String;
   begin
      External.Init_With_Fill (Result, N, C);
      return Result;
   end New_CPP_String;

   ----------
   -- Npos --
   ----------

   function Npos return size_t is
   begin
      return External.Npos;
   end Npos;

   --------------
   -- Pop_Back --
   --------------

   procedure Pop_Back
     (Str : in out CPP_String) is
   begin
      Check_Non_Null (Str);
      External.Pop_Back (Str);
   end Pop_Back;

   ---------------
   -- Push_Back --
   ---------------

   procedure Push_Back
     (Str  : in out CPP_String;
      C    : Character) is
   begin
      Check_Non_Null (Str);
      External.Push_Back (Str, C);
   end Push_Back;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : size_t;
      Len  : size_t;
      Text : String)
   is
      C_Text : chars_ptr := New_String (Text);
   begin
      Check_Non_Null (Str);
      External.Replace_Text (Str, Pos, Len, C_Text);
      Free (C_Text);
   end Replace;

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : size_t;
      Len  : size_t;
      Text : CPP_String) is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Replace_String (Str, Pos, Len, Text);
   end Replace;

   procedure Replace
     (Str    : in out CPP_String;
      Pos    : size_t;
      Len    : size_t;
      Text   : CPP_String;
      Subpos : size_t;
      Sublen : size_t) is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Replace_Substring (Str, Pos, Len, Text, Subpos, Sublen);
   end Replace;

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : size_t;
      Len  : size_t;
      Text : chars_ptr) is
   begin
      Check_Non_Null (Str);
      External.Replace_Text (Str, Pos, Len, Text);
   end Replace;

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : size_t;
      Len  : size_t;
      Text : chars_ptr;
      N    : size_t) is
   begin
      Check_Non_Null (Str);
      External.Replace_Buffer (Str, Pos, Len, Text, N);
   end Replace;

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : size_t;
      Len  : size_t;
      N    : size_t;
      C    : Character) is
   begin
      Check_Non_Null (Str);
      External.Replace_Fill (Str, Pos, Len, N, C);
   end Replace;

   -------------
   -- Reserve --
   -------------

   procedure Reserve (Str : in out CPP_String; N : size_t := 0) is
   begin
      Check_Non_Null (Str);
      External.Reserve (Str, N);
   end Reserve;

   ------------
   -- Resize --
   ------------

   procedure Resize (Str : in out CPP_String; N : size_t) is
   begin
      Check_Non_Null (Str);
      External.Resize (Str, N);
   end Resize;

   ------------
   -- Resize --
   ------------

   procedure Resize (Str : in out CPP_String; N : size_t; C : Character) is
   begin
      Check_Non_Null (Str);
      External.Resize_And_Fill (Str, N, C);
   end Resize;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      return External.Reverse_Find_String (Str, Text, Pos);
   end Reverse_Find;

   function Reverse_Find
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Reverse_Find_Text (Str, Text, Pos);
   end Reverse_Find;

   function Reverse_Find
     (Str  : CPP_String;
      Text : chars_ptr;
      Pos  : size_t;
      N    : size_t) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Reverse_Find_Buffer (Str, Text, Pos, N);
   end Reverse_Find;

   function Reverse_Find
     (Str  : CPP_String;
      C    : Character;
      Pos  : size_t := Npos) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Reverse_Find_Char (Str, C, Pos);
   end Reverse_Find;

   ----------
   -- Size --
   ----------

   function Size
     (Str : CPP_String) return size_t is
   begin
      Check_Non_Null (Str);
      return External.Size (Str);
   end Size;

   ------------
   -- Substr --
   ------------

   function Substr
     (Str  : CPP_String;
      Pos  : size_t := 0;
      Len  : size_t := Npos) return CPP_String is
   begin
      Check_Non_Null (Str);

      return Result : CPP_String do
         External.Substr (Result, Str, Pos, Len);
      end return;
   end Substr;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Str  : in out CPP_String;
      Text : in out CPP_String) is
   begin
      Check_Non_Null (Str);
      Check_Non_Null (Text);
      External.Swap (Str, Text);
   end Swap;

   --------------------------
   -- Relational_Operators --
   --------------------------

   function "=" (Left : CPP_String; Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Eq_String (Left, Right) /= False;
   end "=";

   function "=" (Left : chars_ptr;  Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Right);
      return External.Eq_Text_String (Left, Right) /= False;
   end "=";

   function "=" (Left : CPP_String; Right : chars_ptr)  return Boolean is
   begin
      Check_Non_Null (Left);
      return External.Eq_String_Text (Left, Right) /= False;
   end "=";

   function "<" (Left : CPP_String; Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Lt_String (Left, Right) /= False;
   end "<";

   function "<" (Left : chars_ptr;  Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Right);
      return External.Lt_Text_String (Left, Right) /= False;
   end "<";

   function "<" (Left : CPP_String; Right : chars_ptr)  return Boolean is
   begin
      Check_Non_Null (Left);
      return External.Lt_String_Text (Left, Right) /= False;
   end "<";

   function "<=" (Left : CPP_String; Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Le_String (Left, Right) /= False;
   end "<=";

   function "<=" (Left : chars_ptr;  Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Right);
      return External.Le_Text_String (Left, Right) /= False;
   end "<=";

   function "<=" (Left : CPP_String; Right : chars_ptr)  return Boolean is
   begin
      Check_Non_Null (Left);
      return External.Le_String_Text (Left, Right) /= False;
   end "<=";

   function ">" (Left : CPP_String; Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Gt_String (Left, Right) /= False;
   end ">";

   function ">" (Left : chars_ptr;  Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Right);
      return External.Gt_Text_String (Left, Right) /= False;
   end ">";

   function ">" (Left : CPP_String; Right : chars_ptr)  return Boolean is
   begin
      Check_Non_Null (Left);
      return External.Gt_String_Text (Left, Right) /= False;
   end ">";

   function ">=" (Left : CPP_String; Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Left);
      Check_Non_Null (Right);
      return External.Ge_String (Left, Right) /= False;
   end ">=";

   function ">=" (Left : chars_ptr;  Right : CPP_String) return Boolean is
   begin
      Check_Non_Null (Right);
      return External.Ge_Text_String (Left, Right) /= False;
   end ">=";

   function ">=" (Left : CPP_String; Right : chars_ptr)  return Boolean is
   begin
      Check_Non_Null (Left);
      return External.Ge_String_Text (Left, Right) /= False;
   end ">=";

end GNATCOLL.CPP.Strings;
