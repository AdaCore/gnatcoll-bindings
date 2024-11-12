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

package body Save_Streams is

   ----------
   -- Read --
   ----------

   overriding
   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Length : constant Integer :=
        Natural'Min (Item'Length, Stream.Buffer.Length - Stream.Position);
      Target : String (1 .. Integer'Max (Length, 0));
      for Target'Address use Item'Address;
   begin
      if Target = "" then
         Last := Item'First - 1;
         return;
      end if;

      Target :=
        To_String
          (Stream.Buffer.Slice
             (Stream.Position + 1, Stream.Position + Length));
      Stream.Position := Stream.Position + Length;
      Last := Item'First + Stream_Element_Offset (Length) - 1;
   end Read;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Stream : in out Stream_Type; Item : Stream_Element_Array)
   is
      Source : String (1 .. Item'Length);
      for Source'Address use Item'Address;
   begin
      Stream.Buffer.Append (Source);
   end Write;

   -----------
   -- Clear --
   -----------

   procedure Clear (Stream : in out Stream_Type) is
   begin
      Stream.Buffer.Clear;
      Stream.Reset;
   end Clear;

   -----------------------
   -- Remove_Last_Bytes --
   -----------------------

   procedure Remove_Last_Bytes (Stream : in out Stream_Type; Count : Natural)
   is
   begin
      Stream.Buffer := Stream.Buffer.Head (Stream.Buffer.Length - Count);
   end Remove_Last_Bytes;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in out Stream_Type) is
   begin
      Stream.Position := 0;
   end Reset;

   -----------
   -- Slice --
   -----------

   function Slice
     (Stream : Stream_Type; Low : Positive; High : Natural) return String is
   begin
      return To_String (Stream.Buffer.Slice (Low, High));
   end Slice;

end Save_Streams;
