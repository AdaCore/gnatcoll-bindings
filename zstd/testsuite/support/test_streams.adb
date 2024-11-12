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

with Ada.Text_IO; use Ada.Text_IO;

package body Test_Streams is

   function Next_Stream_Element
     (G : in out Generator; Remain : in out XString) return Stream_Element;

   -------------------------
   -- Next_Stream_Element --
   -------------------------

   function Next_Stream_Element
     (G : in out Generator; Remain : in out XString) return Stream_Element
   is
      S : State;
      E : Stream_Element;
   begin
      if Remain.Is_Empty then
         Remain := To_XString (ASCII.LF & Float'Image (Random (G)));
         Save (G, S);
         Remain.Append (Image (S) (1 .. 100));
      end if;

      E := Character'Pos (Remain (Remain.Length));
      Remain.Slice (1, Remain.Length - 1);
      return E;
   end Next_Stream_Element;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit
     (Stream : in out Stream_Type; Limit : Stream_Element_Count) is
   begin
      Stream.Limit := Limit;
   end Set_Limit;

   ----------
   -- Read --
   ----------

   overriding
   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      if not Stream.Read_Started then
         Stream.Read_Started := True;
         Reset (Stream.Read_Generator);
         Save (Stream.Read_Generator, Stream.Init_State);
      end if;

      Last := Item'First - 1;

      while Last < Item'Last and then Stream.Limit > 0 loop
         Last := Last + 1;
         Stream.Limit := Stream.Limit - 1;

         Item (Last) :=
           Next_Stream_Element (Stream.Read_Generator, Stream.Read_Remain);
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Stream : in out Stream_Type; Item : Stream_Element_Array)
   is
   begin
      if not Stream.Write_Started then
         Stream.Write_Started := True;
         Reset (Stream.Write_Generator, Stream.Init_State);
      end if;

      for J in Item'Range loop
         if Item (J)
           /= Next_Stream_Element (Stream.Write_Generator, Stream.Write_Remain)
         then
            Put_Line ("Random initialization state to restore the bug:");
            Put_Line (Image (Stream.Init_State));
            raise Program_Error with "Data differ";
         end if;
      end loop;
   end Write;

end Test_Streams;
