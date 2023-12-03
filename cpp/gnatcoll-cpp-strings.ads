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

--  This package provides a wrapper of the C++ ISO/IEC 14882:1998(E) string
--  class.

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package GNATCOLL.CPP.Strings is
   type CPP_String is limited private;
   --  Defined limited because it has a pointer to the C++ string (to
   --  forbid copying variables that reference the same string).

   function Npos return Interfaces.C.size_t;
   --  Greatest possible value for string indexes.

   procedure Append
     (Str  : in out CPP_String;
      Text : String);
   --  Append Ada-string: Append to Str the conversion of Text into a null-
   --  terminated character sequence.

   procedure Append
     (Str    : in out CPP_String;
      Text   : CPP_String);
   --  Append string: Append to Str the contents of Text.

   procedure Append
     (Str    : in out CPP_String;
      Text   : CPP_String;
      Subpos : Interfaces.C.size_t;
      Sublen : Interfaces.C.size_t);
   --  Append substring: Append to Str a copy of a substring of Text. The
   --  substring is the portion of Text that begins at the character position
   --  subpos and spans sublen characters (or until the end of Text, if either
   --  Text is too short or if sublen is Npos).

   procedure Append
     (Str  : in out CPP_String;
      Text : Interfaces.C.Strings.chars_ptr);
   --  Append C-string: Append to Str the null-terminated character sequence
   --  pointed by Text.

   procedure Append
     (Str  : in out CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      N    : Interfaces.C.size_t);
   --  Append buffer: Append to Str a copy of the first n characters of the
   --  array of characters pointed by Text.

   procedure Append
     (Str  : in out CPP_String;
      N    : Interfaces.C.size_t;
      C    : Character);
   --  Append with fill: Append N consecutive copies of character c.

   procedure Assign
     (Str  : in out CPP_String;
      Text : String);
   --  Ada String: Assign to Str the conversion of Text into a null terminated
   --  character sequence.

   procedure Assign
     (Str  : in out CPP_String;
      Text : CPP_String);
   --  C++ String: Assign to Str a copy of the C++ string Text.

   procedure Assign
     (Str  : in out CPP_String;
      Text : Interfaces.C.Strings.chars_ptr);
   --  Assign to Str a copy of the null-terminated character sequence pointed
   --  by Text.

   procedure Assign
     (Str : in out CPP_String;
      C   : Character);
   --  Assign to Str a copy of the character C, replacing its current contents.

   function Capacity (Str : CPP_String) return Interfaces.C.size_t;
   --  Returns the size of the storage space currently allocated for the string
   --  expressed in terms of bytes.

   procedure Clear (Str : in out CPP_String);
   --  Erases the contents of the string, which becomes an empty string.

   function Char_At
     (Str : CPP_String;
      Pos : Interfaces.C.size_t) return Character;
   --  Return the character located at the given Position.

   function Empty (Str : CPP_String) return Boolean;
   --  Returns whether the string is empty (i.e. whether its length is 0).

   procedure Erase
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t := 0;
      Len  : Interfaces.C.size_t := Npos);
   --  Erases the portion of the string value that begins at the character
   --  position pos and spans len characters (or until the end of the string,
   --  if either the content is too short or if len is Npos). Default values
   --  erase all characters in the string (like function Clear).

   procedure Free (Str : in out CPP_String);
   --  Destroys the C++ string object.

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Text : String);
   --  Insert Ada-string: Inserts at the given position the conversion of Text
   --  into a null-terminated character sequence.

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Text : CPP_String);
   --  Insert string: Inserts into Str a copy of Text at the given position.

   procedure Insert
     (Str    : in out CPP_String;
      Pos    : Interfaces.C.size_t;
      Text   : CPP_String;
      Subpos : Interfaces.C.size_t;
      Sublen : Interfaces.C.size_t);
   --  Insert substring: Inserts into Str a copy of a substring of Text. The
   --  substring is the portion of Text that begins at the character position
   --  subpos and spans sublen characters (or until the end of Text, if either
   --  Text is too short or if sublen is Npos).

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Text : Interfaces.C.Strings.chars_ptr);
   --  Insert C-string: Inserts a copy of the null-terminated character
   --  sequence pointed by Text.

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Text : Interfaces.C.Strings.chars_ptr;
      N    : Interfaces.C.size_t);
   --  Insert buffer: Insert into Str a copy of the first N characters of the
   --  array of characters pointed by Text.

   procedure Insert
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t;
      C    : Character);
   --  Insert with character fill: Insert N consecutive copies of character C.

   function Length (Str : CPP_String) return Interfaces.C.size_t;
   --  Return the number of actual bytes that conform the contents of the
   --  string, which is not necessarily equal to its storage capacity.

   function Max_Size (Str : CPP_String) return Interfaces.C.size_t;
   --  Return the maximum length the string can reach.

   function New_CPP_String return CPP_String;
   --  Empty string constructor: allocates and initializes an empty string,
   --  with a length of zero characters.

   function New_CPP_String
     (Text : String) return CPP_String;
   --  Ada string copy constructor: allocates a new string and initializes
   --  it copying the conversion of Text into a C null-terminated character
   --  sequence.

   function New_CPP_String
     (Text : CPP_String) return CPP_String;
   --  C++ string copy constructor: allocates a new string and initializes
   --  it copying the contents of Text.

   function New_CPP_String
     (Text : Interfaces.C.Strings.chars_ptr) return CPP_String;
   --  C-String constructor: allocates a new string and initializes it copying
   --  the contents of the null-terminated character sequence pointed by Text.

   function New_CPP_String
     (N : Interfaces.C.size_t;
      C : Character) return CPP_String;
   --  Fill constructor: allocates a new string and initializes it filling the
   --  string with N consecutive copies of character C.

   procedure Pop_Back
     (Str  : in out CPP_String);
   --  Remove last character of Str.

   procedure Push_Back
     (Str  : in out CPP_String;
      C    : Character);
   --  Append character C to the end of Str.

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Len  : Interfaces.C.size_t;
      Text : String);
   --  Replace string: Replaces the portion of the string that begins at
   --  character Pos and spans Len characters by the contents of Text.

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Len  : Interfaces.C.size_t;
      Text : CPP_String);
   --  Replace string (1): Replaces the portion of the string that begins at
   --  character pos and spans len characters by the contents of Text.

   procedure Replace
     (Str    : in out CPP_String;
      Pos    : Interfaces.C.size_t;
      Len    : Interfaces.C.size_t;
      Text   : CPP_String;
      Subpos : Interfaces.C.size_t;
      Sublen : Interfaces.C.size_t);
   --  Replace substring: Replaces the portion of the string that begins at
   --  character Pos and spans Len characters by the contents of Text that
   --  begins at character Subpos and spans Sublen characters.

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Len  : Interfaces.C.size_t;
      Text : Interfaces.C.Strings.chars_ptr);
   --  Replace c-string: Replaces the portion of the string that begins at
   --  character Pos and spans Len characters by the contents of Text.

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Len  : Interfaces.C.size_t;
      Text : Interfaces.C.Strings.chars_ptr;
      N    : Interfaces.C.size_t);
   --  Replace buffer: Replaces the portion of the string that begins at
   --  character Pos and spans Len characters by the contents of the first
   --  N characters of the array of characters pointed by Text.

   procedure Replace
     (Str  : in out CPP_String;
      Pos  : Interfaces.C.size_t;
      Len  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t;
      C    : Character);
   --  Replace with character fill: Replaces the portion of the string that
   --  begins at character Pos by N consecutive copies of character C.

   procedure Reserve
     (Str : in out CPP_String;
      N   : Interfaces.C.size_t := 0);
   --  Requests that the string capacity be adapted to a planned change in size
   --  to a length of up to N characters. If N is greater than the current
   --  string capacity, the function causes the container to increase its
   --  capacity to N characters (or greater); In all other cases, it is taken
   --  as a non-binding request to shrink the string capacity: the container
   --  implementation is free to optimize otherwise and leave the string with
   --  a capacity greater than n.

   procedure Resize
     (Str : in out CPP_String;
      N   : Interfaces.C.size_t);
   --  Resize the string to a length of N characters. If N is smaller than the
   --  current string length, the current value is shortened to its first N
   --  character, removing the characters beyond the Nth. If N is greater than
   --  the current string length, the current content is extended by inserting
   --  at the end as many null characters as needed to reach a size of N.

   procedure Resize
     (Str : in out CPP_String;
      N   : Interfaces.C.size_t;
      C   : Character);
   --  Resize the string to a length of N characters. If N is smaller than the
   --  current string length, the current value is shortened to its first N
   --  character, removing the characters beyond the Nth. If N is greater than
   --  the current string length, the current content is extended by inserting
   --  at the end as many characters C as needed to reach a size of N.

   function Size (Str : CPP_String) return Interfaces.C.size_t;
   --  Return the number of actual bytes that conform the contents of the
   --  string, which is not necessarily equal to its storage capacity.

   procedure Swap
     (Str  : in out CPP_String;
      Text : in out CPP_String);
   --  Exchange the content of the container Str by the content of Text.

   -----------------------
   -- String operations --
   -----------------------

   function C_Str (Str : CPP_String) return Interfaces.C.Strings.chars_ptr;
   --  Return a pointer to an array that contains a null-terminated sequence
   --  of characters (i.e., a C-string) representing the current value of the
   --  string object.

   function Compare
     (Left  : CPP_String;
      Right : CPP_String) return Integer;
   --  Compare the value of the string objects. Returns a signed integral
   --  indicating the relation between the strings:
   --    0: They compare equal
   --   <0: Either the value of the first character that does not match is
   --       lower in the compared string, or all compared characters match
   --       but the compared string is shorter.
   --   >0: Either the value of the first character that does not match is
   --       greater in the compared string, or all compared characters match
   --       but the compared string is longer.

   function Compare
     (Left  : CPP_String;
      Pos   : Interfaces.C.size_t;
      Len   : Interfaces.C.size_t;
      Right : CPP_String) return Integer;
   --  Compare the substring that begins at its character in position Pos and
   --  spans Len characters with the value of string Right.

   function Compare
     (Left   : CPP_String;
      Pos    : Interfaces.C.size_t;
      Len    : Interfaces.C.size_t;
      Right  : CPP_String;
      Subpos : Interfaces.C.size_t;
      Sublen : Interfaces.C.size_t) return Integer;
   --  Compare the substring that begins at its character in position Pos and
   --  spans Len characters with the value of string Right from position Subpos
   --  spanning Sublen chars.

   function Compare
     (Left  : CPP_String;
      Right : Interfaces.C.Strings.chars_ptr) return Integer;
   --  Compare string with null-terminated C string.

   function Compare
     (Left  : CPP_String;
      Pos   : Interfaces.C.size_t;
      Len   : Interfaces.C.size_t;
      Right : Interfaces.C.Strings.chars_ptr) return Integer;
   --  Compare substring with null-terminated C string.

   function Compare
     (Left  : CPP_String;
      Pos   : Interfaces.C.size_t;
      Len   : Interfaces.C.size_t;
      Right : Interfaces.C.Strings.chars_ptr;
      N     : Interfaces.C.size_t) return Integer;
   --  Compare the substring that begins at its character in position Pos and
   --  spans Len characters with the value of string Right. N is the number of
   --  characters to compare.

   procedure Copy
     (From_Str  : CPP_String;
      To_Str    : Interfaces.C.Strings.chars_ptr;
      Len       : Interfaces.C.size_t;
      Pos       : Interfaces.C.size_t;
      Num_Chars : out Interfaces.C.size_t);
   --  Copies a substring of the current value of the string object From_Str
   --  into the C array pointed by To_Str. This substring contains the Len
   --  characters that start at position Pos. The function does not append
   --  a null character at the end of the copied content.

   function Data (Str : CPP_String) return String;
   --  Return the string data.

   function Data (Str : CPP_String) return Interfaces.C.Strings.chars_ptr;
   --  Return the null-terminated character sequence contained in Str.

   function Find
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches in Str for the first occurrence of the sequence Text. Pos is
   --  the position of the first character in Str to be considered in the
   --  search.

   function Find
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches in Str for the first occurrence of the sequence Text. Pos is
   --  the position of the first character in Str to be considered in the
   --  search.

   function Find
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t) return Interfaces.C.size_t;
   --  Searches in Str for the first occurrence of the sequence Text. Pos is
   --  the position of the first character in Str to be considered in the
   --  search; N is the length of the sequence of characters to match.

   function Find
     (Str  : CPP_String;
      C    : Character;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches in Str for the first occurrence of character C. Pos is the
   --  position of the first character in Str to be considered in the search.

   function Find_First_Not_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches the string for the first character that does not match any of
   --  the characters specified in Text. When Pos is specified, the search only
   --  includes characters at or after position Pos, ignoring any possible
   --  occurrences before Pos.

   function Find_First_Not_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches the string for the first character that does not match any of
   --  the characters specified in Text. When Pos is specified, the search only
   --  includes characters at or after position Pos, ignoring any possible
   --  occurrences before Pos.

   function Find_First_Not_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t) return Interfaces.C.size_t;
   --  Searches the string for the first character that does not match any of
   --  the characters specified in Text. When Pos is specified, the search only
   --  includes characters at or after position Pos, ignoring any possible
   --  occurrences before Pos. N is the number of characters to search for.

   function Find_First_Not_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches the string for the first character that does not match C.
   --  When Pos is specified, the search only includes characters at or after
   --  position Pos, ignoring any possible occurrences before Pos.

   function Find_First_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches the string for the first character that matches any of the
   --  characters specified in Text. When Pos is specified, the search only
   --  includes characters at or after position Pos, ignoring any possible
   --  occurrences before Pos.

   function Find_First_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches the string for the first character that matches any of the
   --  characters specified in Text. When Pos is specified, the search only
   --  includes characters at or after position Pos, ignoring any possible
   --  occurrences before Pos.

   function Find_First_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t) return Interfaces.C.size_t;
   --  Searches the string for the first character that matches any of the
   --  characters specified in Text. When Pos is specified, the search only
   --  includes characters at or after position Pos, ignoring any possible
   --  occurrences before Pos. N is the number of characters to search for.

   function Find_First_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : Interfaces.C.size_t := 0) return Interfaces.C.size_t;
   --  Searches the string for the first character that matches character C.
   --  When pos is specified, the search only includes characters at or after
   --  position pos, ignoring any possible occurrences before Pos. N is the
   --  number of characters to search for.

   function Find_Last_Not_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches the string for the last character that does not match any of
   --  the characters specified in Text. When Pos is specified, the search only
   --  includes characters at or before position Pos, ignoring any possible
   --  occurrences after Pos.

   function Find_Last_Not_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches the string for the last character that does not match any of
   --  the characters specified in Text. When Pos is specified, the search only
   --  includes characters at or before position Pos, ignoring any possible
   --  occurrences after Pos.

   function Find_Last_Not_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t) return Interfaces.C.size_t;
   --  Searches the string for the last character that does not match any of
   --  the characters specified in Text. When pos is specified, the search only
   --  includes characters at or before position Pos, ignoring any possible
   --  occurrences after Pos. N is the number of characters to search for.

   function Find_Last_Not_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches the string for the last character that does not match C.
   --  When pos is specified, the search only includes characters at or before
   --  position pos, ignoring any possible occurrences after Pos. N is the
   --  number of characters to search for.

   function Find_Last_Of
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches the string for the last character that matches any of the
   --  characters specified in Text. When pos is specified, the search only
   --  includes characters at or before position pos, ignoring any possible
   --  occurrences after Pos.

   function Find_Last_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches the string for the last character that matches any of the
   --  characters specified in Text. When pos is specified, the search only
   --  includes characters at or before position pos, ignoring any possible
   --  occurrences after Pos.

   function Find_Last_Of
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t) return Interfaces.C.size_t;
   --  Searches the string for the last character that matches any of the
   --  characters specified in Text. When pos is specified, the search only
   --  includes characters at or before position pos, ignoring any possible
   --  occurrences after Pos. N is the number of characters to search for.

   function Find_Last_Of
     (Str  : CPP_String;
      C    : Character;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches the string for the last character that matches character C.
   --  When pos is specified, the search only includes characters at or before
   --  position pos, ignoring any possible occurrences after Pos. N is the
   --  number of characters to search for.

   function Reverse_Find
     (Str  : CPP_String;
      Text : CPP_String;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches in Str for the last occurrence of the sequence Text. When Pos
   --  is specified, the search only includes sequences of characters that
   --  begin at or before position Pos, ignoring any possible match beginning
   --  after Pos.

   function Reverse_Find
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches in Str for the last occurrence of the sequence Text. When Pos
   --  is specified, the search only includes sequences of characters that
   --  begin at or before position Pos, ignoring any possible match beginning
   --  after Pos.

   function Reverse_Find
     (Str  : CPP_String;
      Text : Interfaces.C.Strings.chars_ptr;
      Pos  : Interfaces.C.size_t;
      N    : Interfaces.C.size_t) return Interfaces.C.size_t;
   --  Searches in Str for the last occurrence of the sequence Text. Pos is
   --  the position of the last character in Str to be considered in the
   --  search; N is the length of the sequence of characters to match.

   function Reverse_Find
     (Str  : CPP_String;
      C    : Character;
      Pos  : Interfaces.C.size_t := Npos) return Interfaces.C.size_t;
   --  Searches in Str for the last occurrence of character C. Pos is the
   --  position of the last character in Str to be considered in the search.

   function Substr
     (Str  : CPP_String;
      Pos  : Interfaces.C.size_t := 0;
      Len  : Interfaces.C.size_t := Npos) return CPP_String;
   --  Returns a newly constructed string object with its value initialized to
   --  a copy of a substring of this object.

   --------------------------
   -- Relational Operators --
   --------------------------

   function "="
     (Left  : CPP_String;
      Right : CPP_String) return Boolean;
   function "="
     (Left  : Interfaces.C.Strings.chars_ptr;
      Right : CPP_String) return Boolean;
   function "="
     (Left  : CPP_String;
      Right : Interfaces.C.Strings.chars_ptr) return Boolean;

   function "<"
     (Left  : CPP_String;
      Right : CPP_String) return Boolean;
   function "<"
     (Left  : Interfaces.C.Strings.chars_ptr;
      Right : CPP_String) return Boolean;
   function "<"
     (Left  : CPP_String;
      Right : Interfaces.C.Strings.chars_ptr) return Boolean;

   function "<="
     (Left  : CPP_String;
      Right : CPP_String) return Boolean;
   function "<="
     (Left  : Interfaces.C.Strings.chars_ptr;
      Right : CPP_String) return Boolean;
   function "<="
     (Left : CPP_String;
      Right : Interfaces.C.Strings.chars_ptr) return Boolean;

   function ">"
     (Left  : CPP_String;
      Right : CPP_String) return Boolean;
   function ">"
     (Left  : Interfaces.C.Strings.chars_ptr;
      Right : CPP_String) return Boolean;
   function ">"
     (Left  : CPP_String;
      Right : Interfaces.C.Strings.chars_ptr) return Boolean;

   function ">="
     (Left  : CPP_String;
      Right : CPP_String) return Boolean;
   function ">="
     (Left  : Interfaces.C.Strings.chars_ptr;
      Right : CPP_String) return Boolean;
   function ">="
     (Left  : CPP_String;
      Right : Interfaces.C.Strings.chars_ptr) return Boolean;

private
   type CPP_String is record
      Wrapped_String_Address : System.Address := System.Null_Address;
   end record;
end GNATCOLL.CPP.Strings;
