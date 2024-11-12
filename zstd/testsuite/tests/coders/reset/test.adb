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

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Coders.ZSTD;
use GNATCOLL.Coders;
with GNATCOLL.ZSTD; use GNATCOLL.ZSTD;
with GNATCOLL.Paragraph_Filling;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   procedure Transcode_Wrapper
     (Coder       : in out ZSTD.Coder_Type;
      Input       : Stream_Element_Array;
      Output      : in out Stream_Element_Array;
      Output_Last : out Stream_Element_Offset);
   --  Transcode until the coder has finished

   procedure Transcode_Wrapper
     (Coder       : in out ZSTD.Coder_Type;
      Input       : Stream_Element_Array;
      Output      : in out Stream_Element_Array;
      Output_Last : out Stream_Element_Offset)
   is
      Input_First  : Stream_Element_Offset := Input'First;
      Output_First : Stream_Element_Offset := Output'First;
      Input_Last   : Stream_Element_Offset;

      Iteration : Integer := 0;
   begin
      loop
         Coder.Transcode
           (Input (Input_First .. Input'Last),
            Input_Last,
            Output (Output_First .. Output'Last),
            Output_Last,
            Finish);

         Input_First := Input_Last + 1;
         Output_First := Output_Last + 1;

         Iteration := Iteration + 1;

         --  Prevent infinite loop in case of error

         exit when Coder.Finished or Iteration > 100;
      end loop;

      if Iteration > 100 then
         A.Assert
           (False,
            "Infinite loop detected, something went wrong with"
            & " the transcoding");
      end if;
   end Transcode_Wrapper;

   Gettysburg : constant String :=
     "Four score and seven years ago our fathers brought forth on this";

   Formatted : constant String :=
     To_String (GNATCOLL.Paragraph_Filling.Knuth_Fill (Gettysburg));

   Source : Stream_Element_Array (1 .. Formatted'Length);
   for Source'Address use Formatted'Address;

   Encoded  : Stream_Element_Array (1 .. Formatted'Length * 2);
   Restored : Stream_Element_Array (1 .. Source'Length);

   Encoder : aliased ZSTD.Coder_Type (Encoder => True);
   Decoder : aliased ZSTD.Coder_Type (Encoder => False);

   Src_Last, Dst_Last : Stream_Element_Offset;
begin
   Encoder.Transcode (Source, Src_Last, Encoded (1 .. 2), Dst_Last, Finish);
   A.Assert (not Encoder.Finished, "Compression should not be finished");

   Encoder.Reset (ZSTD_Reset_Session_Only);
   Encoder.Transcode (Source, Src_Last, Encoded (1 .. 2), Dst_Last, Finish);
   A.Assert (not Encoder.Finished, "Compression should not be finished");

   begin
      Encoder.Reset (ZSTD_Reset_Parameters);
      A.Assert (False, "Exception should have been raised");
   exception
      when ZSTD_Error =>
         A.Assert
           (True, "can not reset parameters during an on-going compression");
      when others =>
         A.Assert (False, "Expected a ZSTD_Error");
   end;

   --  Reset the coder to a clean state

   Encoder.Reset (ZSTD_Reset_Session_And_Parameters);

   Encoder.Transcode (Source, Src_Last, Encoded (1 .. 2), Dst_Last, Finish);
   A.Assert (Encoder.Total_In > 0, "Total IN update");
   A.Assert (Encoder.Total_Out > 0, "Total OUT update");
   A.Assert (not Encoder.Finished, "Compression should not be finished");

   Encoder.Reset (ZSTD_Reset_Session_And_Parameters);
   A.Assert
     (Encoder.Total_In = 0, "Check that total IN has been correctly reset");
   A.Assert
     (Encoder.Total_Out = 0,
      "Check that total OUT IN has been correctly reset");

   Transcode_Wrapper (Encoder, Source, Encoded, Dst_Last);
   A.Assert (Encoder.Finished, "Encoder has finished the encoding");

   Decoder.Transcode (Encoded, Src_Last, Restored (1 .. 2), Dst_Last, Finish);
   A.Assert (not Decoder.Finished, "Decompression should not be finished");

   Decoder.Reset (ZSTD_Reset_Session_Only);
   Decoder.Transcode (Encoded, Src_Last, Restored (1 .. 2), Dst_Last, Finish);
   A.Assert (not Decoder.Finished, "Decompression should not be finished");

   begin
      Decoder.Reset (ZSTD_Reset_Parameters);
      A.Assert (False, "Exception should have been raised");
   exception
      when ZSTD_Error =>
         A.Assert
           (True, "can not reset parameters during an on-going decompression");
      when others =>
         A.Assert (False, "Expected a ZSTD_Error");
   end;

  --  Explicit calls to close do not make sense. It is only used to simulate an
  --  object finalization.

   Encoder.Close;
   begin
      Encoder.Reset (ZSTD_Reset_Session_Only);
      A.Assert (False, "Exception should have been raised");
   exception
      when ZSTD_Error =>
         A.Assert
           (True, "can not reset a closed encoder");
      when others =>
         A.Assert (False, "Expected a ZSTD_Error");
   end;

   Decoder.Close;
   begin
      Decoder.Reset (ZSTD_Reset_Session_Only);
      A.Assert (False, "Exception should have been raised");
   exception
      when ZSTD_Error =>
         A.Assert
           (True, "can not reset a closed decoder");
      when others =>
         A.Assert (False, "Expected a ZSTD_Error");
   end;

   return A.Report;
end Test;
