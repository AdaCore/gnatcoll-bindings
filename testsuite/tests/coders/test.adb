------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Coders.LZMA;    use GNATCOLL.Coders;
with GNATCOLL.Coders.ZLib;
with GNATCOLL.Coders.Streams;
with GNATCOLL.Paragraph_Filling;

with Save_Streams;
with Test_Streams;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   subtype Coder_Class is Coder_Interface'Class;

   Coder_X      : aliased LZMA.Coder_Type;
   Back_X       : aliased LZMA.Coder_Type;
   Coder_Z      : aliased ZLib.Coder_Type;
   Back_Z       : aliased ZLib.Coder_Type;

   Gettysburg : constant String :=
     "Four score and seven years ago our fathers brought forth on this" &
     " continent a new nation, conceived in liberty, and dedicated to the" &
     " proposition that all men are created equal. Now we are engaged in a" &
     " great civil war, testing whether that nation, or any nation so" &
     " conceived and so dedicated, can long endure. We are met on a great" &
     " battlefield of that war. We have come to dedicate a portion of that" &
     " field, as a final resting place for those who here gave their lives" &
     " that that nation might live. It is altogether fitting and proper that" &
     " we should do this. But, in a larger sense, we can not dedicate, we" &
     " can not consecrate, we can not hallow this ground. The brave men," &
     " living and dead, who struggled here, have consecrated it, far above" &
     " our poor power to add or detract. The world will little note, nor" &
     " long remember what we say here, but it can never forget what they did" &
     " here. It is for us the living, rather, to be dedicated here to the" &
     " unfinished work which they who fought here have thus far so nobly" &
     " advanced. It is rather for us to be here dedicated to the great task" &
     " devotion to that cause for which they gave the last full measure of" &
     " people, shall not perish from the earth.";

   Formatted : constant String :=
     To_String (GNATCOLL.Paragraph_Filling.Knuth_Fill (Gettysburg));

   Source : Stream_Element_Array (1 .. Formatted'Length);
   for Source'Address use Formatted'Address;

   Encoded : Stream_Element_Array (1 .. Formatted'Length);
   Last    : Stream_Element_Offset;
   O, L : Stream_Element_Offset;

   procedure Encode (Coder : in out Coder_Class);

   procedure Decode (Coder : in out Coder_Class);

   procedure Test_Stream_Coder (Coder, Back : in out Coder_Class);

   ------------
   -- Encode --
   ------------

   procedure Encode (Coder : in out Coder_Class) is
   begin
      --  Compress the whole source once

      Coder.Transcode (Source, Last, Encoded, L, Finish);
      A.Assert (Last = Source'Last, "Compressed at once");
   end Encode;

   ------------
   -- Decode --
   ------------

   procedure Decode (Coder : in out Coder_Class) is
      Restored   : Stream_Element_Array (Source'Range);
      Block_Size : constant := 4;
      P : Stream_Element_Offset := Encoded'First - 1;
      --  This makes sure that the last block contains
      --  only Adler checksum data for zlib
   begin
      --  Now we decompress the data, passing short blocks of data to Zlib
      --  (because this demonstrates the problem - the last block passed will
      --  contain checksum information and there will be no output, only a
      --  check inside Zlib that the checksum is correct).

      loop
         Coder.Transcode
           (Encoded (P + 1 .. Stream_Element_Offset'Min (P + Block_Size, L)),
            P,
            Restored
              (Stream_Element_Offset (Coder.Total_Out + 1) .. Restored'Last),
            O,
            No_Flush);

         exit when P = L;
      end loop;

      A.Assert (Restored = Source, "Decompressed text matches original text");
   end Decode;

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

   ------------------
   -- Another_Test --
   ------------------

   procedure Another_Test
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
   end Another_Test;

begin

   -----------------
   --  LZMA tests --
   -----------------

   Coder_X.Encoder;
   Encode (Coder_X);

   Coder_X.Auto_Decoder;
   Decode (Coder_X);

   for T in 1 .. 3 loop
      Coder_X.Encoder (Threads => T);
      Back_X.Auto_Decoder;

      declare
         use Ada.Calendar;
         Stamp : constant Time := Clock;
      begin
         Test_Stream_Coder (Coder_X, Back_X);
         Put_Line (T'Img & Duration'Image (Clock - Stamp));
      end;
   end loop;

   for J in 0 .. 2 loop
      Coder_X.Encoder (Threads => 2);
      Back_X.Auto_Decoder;
      begin
         Another_Test (Coder_X, Back_X, J);
         A.Assert (J = 0, "Expected success on holistic data");
      exception
         when E : LZMA.LZMA_Error =>
            A.Assert
              (J > 0,
               "Expected failure on truncated data "
               & Ada.Exceptions.Exception_Message (E));
      end;
   end loop;

   -----------------
   --  ZLib tests --
   -----------------

   Coder_Z.Deflate_Init;
   Encode (Coder_Z);

   Coder_Z.Inflate_Init;
   Decode (Coder_Z);

   Coder_Z.Deflate_Init;
   Back_Z.Inflate_Init;
   Test_Stream_Coder (Coder_Z, Back_Z);

   for J in 0 .. 2 loop
      Coder_Z.Deflate_Init;
      Back_Z.Inflate_Init;
      begin
         Another_Test (Coder_Z, Back_Z, J);
         A.Assert (J = 0, "Expected success on holistic data");
      exception
         when E : ZLib.ZLib_Error =>
            A.Assert
              (J > 0,
               "Expected failure on truncated data "
               & Ada.Exceptions.Exception_Message (E));
      end;
   end loop;

   return A.Report;
end Test;
