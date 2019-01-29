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

with Ada.Unchecked_Deallocation;
with Interfaces;                use Interfaces;
with GNATCOLL.Coders.LZMA.Thin; use GNATCOLL.Coders.LZMA.Thin;

package body GNATCOLL.Coders.LZMA is

   Zero_LZMA_Stream : constant Thin.lzma_stream := (others => <>);
   --  All zero fields to cleanup durty others. No need to set zeroes here
   --  because record declared with zero defaults already.

   Check_To_C : constant array (Check_Type) of lzma_check :=
     (Check_None   => LZMA_CHECK_NONE,
      Check_CRC32  => LZMA_CHECK_CRC32,
      Check_CRC64  => LZMA_CHECK_CRC64,
      Check_SHA256 => LZMA_CHECK_SHA256);

   Flush_To_C : constant array (Flush_Mode) of lzma_action :=
     (No_Flush   => LZMA_RUN,
      Sync_Flush => LZMA_SYNC_FLUSH,
      Full_Flush => LZMA_FULL_FLUSH,
      Finish     => LZMA_FINISH);

   procedure Check_Error (Code : lzma_ret);
   --  Check return code and raise exception on error

   procedure Cleanup (Coder : in out Coder_Type);
   --  Cleanup internal coding structures to use it with new data processing

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error (Code : lzma_ret) is
   begin
      if Code /= LZMA_OK then
         raise LZMA_Error with lzma_ret'Image (Code);
      end if;
   end Check_Error;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Coder : in out Coder_Type) is
   begin
      Coder.Finished := False;

      if Coder.Stream = null then
         Coder.Stream := new lzma_stream;
      else
         lzma_end (Coder.Stream);
         Coder.Stream.all := Zero_LZMA_Stream;
      end if;
   end Cleanup;

   ------------------
   -- Easy_Encoder --
   ------------------

   procedure Encoder
     (Coder   : in out Coder_Type;
      Preset  :        Preset_Type := 6;
      Extreme :        Boolean     := False;
      Threads :        Positive    := 1;
      Timeout :        Duration    := 0.0;
      Check   :        Check_Type  := Check_CRC64)
   is
      MT_Opts : aliased lzma_mt;
   begin
      Coder.Cleanup;

      MT_Opts.check  := Check_To_C (Check);
      MT_Opts.preset := Unsigned_32 (Preset) or
        (if Extreme then LZMA_PRESET_EXTREME else 0);

      if Threads = 1 then
         Check_Error
           (lzma_easy_encoder (Coder.Stream, MT_Opts.preset, MT_Opts.check));
      else
         MT_Opts.threads := Unsigned_32 (Threads);
         MT_Opts.timeout := Unsigned_32 (Timeout * 1000);

         if MT_Opts.timeout = 0 and then Timeout > 0.0 then
            --  Because 0 timeout mean no timeout limit
            MT_Opts.timeout := 1;
         end if;

         Check_Error (lzma_stream_encoder_mt (Coder.Stream, MT_Opts'Access));
      end if;
   end Encoder;

   ------------------
   -- Auto_Decoder --
   ------------------

   procedure Auto_Decoder (Coder : in out Coder_Type) is
   begin
      Coder.Cleanup;
      Check_Error (lzma_auto_decoder (Coder.Stream, Unsigned_64'Last, 0));
   end Auto_Decoder;

   -------------
   -- Is_Open --
   -------------

   overriding function Is_Open (Coder : Coder_Type) return Boolean is
   begin
      return Coder.Stream /= null;
   end Is_Open;

   ---------------
   -- Transcode --
   ---------------

   procedure Transcode
     (Coder    : in out Coder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode := No_Flush)
   is
      Code : lzma_ret;
   begin
      Coder.Stream.next_in :=
        (if In_Data'Length = 0 then null
         else In_Data (In_Data'First)'Unrestricted_Access);
      Coder.Stream.next_out :=
        (if Out_Data'Length = 0 then null
         else Out_Data (Out_Data'First)'Unrestricted_Access);

      Coder.Stream.avail_in  := In_Data'Length;
      Coder.Stream.avail_out := Out_Data'Length;

      Code := lzma_code (Coder.Stream, Flush_To_C (Flush));

      if Code = LZMA_STREAM_END then
         Coder.Finished := True;
      else
         Check_Error (Code);
      end if;

      In_Last  := In_Data'Last - Stream_Element_Offset (Coder.Stream.avail_in);
      Out_Last := Out_Data'Last
                  - Stream_Element_Offset (Coder.Stream.avail_out);
   end Transcode;

   --------------
   -- Total_In --
   --------------

   overriding function Total_In
     (Coder : Coder_Type) return Stream_Element_Count is
   begin
      return Stream_Element_Count (Coder.Stream.total_in);
   end Total_In;

   ---------------
   -- Total_Out --
   ---------------

   overriding function Total_Out
     (Coder : Coder_Type) return Stream_Element_Count is
   begin
      return Stream_Element_Count (Coder.Stream.total_out);
   end Total_Out;

   --------------
   -- Finished --
   --------------

   function Finished (Coder : Coder_Type) return Boolean is
   begin
      return Coder.Finished;
   end Finished;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Coder : in out Coder_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Thin.lzma_stream, LZMA_Stream_Access);
   begin
      if Coder.Stream /= null then
         Thin.lzma_end (Coder.Stream);
         Unchecked_Free (Coder.Stream);
      end if;
   end Close;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Coder : in out Coder_Type) is
   begin
      Close (Coder);
   end Finalize;

end GNATCOLL.Coders.LZMA;
