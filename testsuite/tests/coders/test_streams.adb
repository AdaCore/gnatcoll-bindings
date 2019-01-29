with Ada.Text_IO;                use Ada.Text_IO;

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

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is
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

         Item (Last) := Next_Stream_Element
           (Stream.Read_Generator, Stream.Read_Remain);
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   :        Stream_Element_Array) is
   begin
      if not Stream.Write_Started then
         Stream.Write_Started := True;
         Reset (Stream.Write_Generator, Stream.Init_State);
      end if;

      for J in Item'Range loop
         if Item (J) /= Next_Stream_Element
           (Stream.Write_Generator, Stream.Write_Remain)
         then
            Put_Line ("Random initialization state to restore the bug:");
            Put_Line (Image (Stream.Init_State));
            raise Program_Error with "Data differ";
         end if;
      end loop;
   end Write;

end Test_Streams;
