------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Streams;      use Ada.Streams;
with GNATCOLL.Strings; use GNATCOLL.Strings;

package Save_Streams is

   type Stream_Type is new Root_Stream_Type with private;
   --  Stream reading the data which was wrote there before

   overriding
   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write (Stream : in out Stream_Type; Item : Stream_Element_Array);

   procedure Reset (Stream : in out Stream_Type);
   --  Reset read position to the start of data

   procedure Clear (Stream : in out Stream_Type);
   --  Clear all internal written data from stream

   function Slice
     (Stream : Stream_Type; Low : Positive; High : Natural) return String;

   procedure Remove_Last_Bytes (Stream : in out Stream_Type; Count : Natural);

private

   type Stream_Type is new Root_Stream_Type with record
      Position : Natural := 0;
      Buffer   : XString;
   end record;

end Save_Streams;
