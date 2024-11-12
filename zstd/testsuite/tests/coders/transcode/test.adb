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

with Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Coders.ZSTD;
use GNATCOLL.Coders;
with GNATCOLL.ZSTD; use GNATCOLL.ZSTD;
with GNATCOLL.Coders.Streams;
with GNATCOLL.Paragraph_Filling;

with Save_Streams;
with Test_Streams;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   subtype Coder_Class is Coder_Interface'Class;

   Gettysburg : constant String :=
     "Four score and seven years ago our fathers brought forth on this";

   Formatted : constant String :=
     To_String (GNATCOLL.Paragraph_Filling.Knuth_Fill (Gettysburg));

   Source : Stream_Element_Array (1 .. Formatted'Length);
   for Source'Address use Formatted'Address;

   procedure Test_Stream_Coder (Coder, Back : in out Coder_Class);
   procedure Test_Truncated_Stream
     (Coder, Back : in out Coder_Class; Remove_Tail : Natural);

   -----------------------
   -- Test_Stream_Coder --
   -----------------------

   procedure Test_Stream_Coder (Coder, Back : in out Coder_Class) is
      Test_Stream  : aliased Test_Streams.Stream_Type;
      Coder_Stream : Streams.Stream_Type;
      Test_2_Flush : Boolean := True;
   begin
      Test_Stream.Set_Limit (2_000_000);
      Coder_Stream.Initialize
        (Read_Coder  => Coder'Unchecked_Access,
         Write_Coder => Back'Unchecked_Access,
         Read_From   => Test_Stream'Unchecked_Access,
         Write_To    => Test_Stream'Unchecked_Access);

      loop
         declare
            Buffer : Stream_Element_Array (1 .. 4000);
            Last   : Stream_Element_Offset;
         begin
            Coder_Stream.Read (Buffer, Last);

            Coder_Stream.Write (Buffer (1 .. Last));

            exit when Coder_Stream.End_Of_Input;

            if Test_2_Flush and then Coder.Total_Out > 16000 then
               loop
                  Coder_Stream.Flush_Read (Buffer, Last);
                  Coder_Stream.Write (Buffer (1 .. Last));
                  exit when Last < Buffer'Last;
               end loop;

               Coder_Stream.Flush_Read (Buffer, Last);
               A.Assert (Last = Buffer'First - 1, "Flushed");

               Coder_Stream.Flush_Read (Buffer, Last);
               A.Assert (Last = Buffer'First - 1, "Flushed");

               Test_2_Flush := False;
            end if;

            --  Put_Line (Coder.Total_In'Img & Coder.Total_Out'Img);
         end;
      end loop;

      loop
         declare
            Buffer : Stream_Element_Array (1 .. 4000);
            Last   : Stream_Element_Offset;
         begin
            Coder_Stream.Flush_Read (Buffer, Last, Finish);
            Coder_Stream.Write (Buffer (1 .. Last));
            exit when Last < Buffer'Last;
         end;
      end loop;

      Coder_Stream.Flush (Finish);
   end Test_Stream_Coder;

   ---------------------------
   -- Test_Truncated_Stream --
   ---------------------------

   procedure Test_Truncated_Stream
     (Coder, Back : in out Coder_Class; Remove_Tail : Natural)
   is
      Save_Stream  : aliased Save_Streams.Stream_Type;
      Coder_Stream : Streams.Stream_Type;
      Got_Back     : Stream_Element_Array (Source'First .. Source'Last + 1);
      Last         : Stream_Element_Offset;
   begin
      Coder_Stream.Initialize
        (Write_Coder => Coder'Unchecked_Access,
         Write_To    => Save_Stream'Unchecked_Access);

      Coder_Stream.Write (Source);
      Coder_Stream.Flush (Finish);

      Coder_Stream.Initialize
        (Read_Coder => Back'Unchecked_Access,
         Read_From  => Save_Stream'Unchecked_Access);

      Put_Line ("Remove tail" & Remove_Tail'Img);
      Save_Stream.Remove_Last_Bytes (Remove_Tail);

      Coder_Stream.Read (Got_Back, Last);
      A.Assert
        (Source = Got_Back (Got_Back'First .. Last),
         "compare with data got back");
   end Test_Truncated_Stream;

   Src_Last, Dst_Last : Stream_Element_Offset;
begin

   declare
      Encoder  : aliased ZSTD.Coder_Type (Encoder => True);
      Decoder  : aliased ZSTD.Coder_Type (Encoder => False);
      Restored : Stream_Element_Array (1 .. Formatted'Length);
      Encoded  : Stream_Element_Array (1 .. Formatted'Length);
   begin
      Encoder.Transcode (Source, Src_Last, Encoded, Dst_Last, Finish);
      A.Assert (Src_Last = Source'Last, "Compressed at once");

      Decoder.Transcode
        (Encoded (Encoded'First .. Dst_Last),
         Src_Last,
         Restored,
         Dst_Last,
         Finish);
      A.Assert (Restored = Source, "Decompressed text matches original text");
   end;

   --  Decode in several pass

   declare
      Encoder             : aliased ZSTD.Coder_Type (Encoder => True);
      Decoder             : aliased ZSTD.Coder_Type (Encoder => False);
      Restored            : Stream_Element_Array (1 .. Formatted'Length);
      Encoded             : Stream_Element_Array (1 .. Formatted'Length);
      Encoded_Last_Idx    : Stream_Element_Offset;
      Block_Size          : constant := 4;
      Encoded_Current_Idx : Stream_Element_Offset := Encoded'First - 1;
   begin

      Encoder.Transcode (Source, Src_Last, Encoded, Dst_Last, Finish);
      Encoded_Last_Idx := Dst_Last;

      loop
         Decoder.Transcode
           (Encoded
              (Encoded_Current_Idx + 1
               .. Stream_Element_Offset'Min
                    (Encoded_Current_Idx + Block_Size, Encoded'Last)),
            Encoded_Current_Idx,
            Restored
              (Stream_Element_Offset (Decoder.Total_Out + 1) .. Restored'Last),
            Dst_Last,
            No_Flush);

         exit when
           Encoded_Current_Idx = Encoded_Last_Idx and then Decoder.Finished;
      end loop;

      A.Assert (Restored = Source, "Decompressed text matches original text");
   end;

   declare
      Encoder : aliased ZSTD.Coder_Type (Encoder => True);
      Decoder : aliased ZSTD.Coder_Type (Encoder => False);
   begin
      Test_Stream_Coder (Coder => Encoder, Back => Decoder);
   end;

   for J in 0 .. 2 loop
      declare
         Encoder : aliased ZSTD.Coder_Type (Encoder => True);
         Decoder : aliased ZSTD.Coder_Type (Encoder => False);
      begin
         Test_Truncated_Stream (Encoder, Decoder, J);
         A.Assert (J = 0, "Expected success on holistic data");
      exception
         when E : ZSTD_Error =>
            A.Assert
              (J > 0,
               "Expected failure on truncated data "
               & Ada.Exceptions.Exception_Message (E));
      end;
   end loop;

   return A.Report;
end Test;
