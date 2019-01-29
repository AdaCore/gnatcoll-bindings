------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

--  This package provides binding to LZMA compression/decompression

with Ada.Finalization;

limited with GNATCOLL.Coders.LZMA.Thin;

package GNATCOLL.Coders.LZMA is

   type Coder_Type is
     new Ada.Finalization.Limited_Controlled and Coder_Interface
   with private;

   type Preset_Type is new Integer range 0 .. 9;

   type Check_Type is
     (Check_None,
      --  No Check is calculated.
      --  Size of the Check field: 0 bytes

      Check_CRC32,
      --  CRC32 using the polynomial from the IEEE 802.3 standard
      --  Size of the Check field: 4 bytes

      Check_CRC64,
      --  CRC64 using the polynomial from the ECMA-182 standard
      --  Size of the Check field: 8 bytes

      Check_SHA256
      --  SHA-256
      --  Size of the Check field: 32 bytes
     );
   --  Type of the integrity check (Check ID)
   --
   --  The .xz format supports multiple types of checks that are calculated
   --  from the uncompressed data. They vary in both speed and ability to
   --  detect errors.

   procedure Encoder
     (Coder   : in out Coder_Type;
      Preset  :        Preset_Type := 6;
      Extreme :        Boolean     := False;
      Threads :        Positive    := 1;
      Timeout :        Duration    := 0.0;
      Check   :        Check_Type  := Check_CRC64);
   --  Initializes compression coder.
   --  Preset is compression level from 0 to 9 roughly defining compression
   --  quality and level of memory usage. Bigger number means better
   --  compression but consumes more time and memory.
   --  Extreme - Modify the compression preset (0 .. 9) to achieve a
   --  slightly better compression ratio without increasing memory usage
   --  of the compressor or decompressor. The downside is that the compression
   --  time will increase dramatically (it can easily double).
   --  Threads is number of threads to use in compression. Faster compression
   --  can be reached with more CPU and memory usage. If computer has only one
   --  CPU then this option provides no gain.
   --  Timeout has meaning only when Threads parameter is more than 1. It is
   --  to allow Transcode to return early. Multithreading can make liblzma to
   --  consume input and produce output in a very bursty way: it may first read
   --  a lot of input to fill internal buffers, then no input or output occurs
   --  for a while. To avoid very long blocking times in Transcode, a timeout
   --  may be set here. If Transcode would block longer than this time, it will
   --  return with not taken data from In_Data and no results in Out_Data.
   --  If long blocking times are fine for you, set timeout to a special
   --  value of 0, which will disable the timeout mechanism and will make
   --  Transcode block until all the input is consumed or the output buffer has
   --  been filled. Even with a timeout, Transcode might sometimes take
   --  somewhat long time to return. No timing guarantees are made.
   --  Check is integrity check type to use.

   procedure Auto_Decoder (Coder : in out Coder_Type);

   overriding function Is_Open (Coder : Coder_Type) return Boolean;
   --  Indicates that coder is ready to transcode data, i.e either Easy_Encoder
   --  or Auto_Decoder called.

   overriding procedure Transcode
     (Coder    : in out Coder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode := No_Flush);
   --  Transcodes data from In_Data to Out_Data.
   --  In_Last is the index of last element from In_Data accepted by
   --  the Coder.
   --  Out_Last is the index of the last element written to Out_Data.
   --  To tell the Coder that incoming data is complete pass Finish as the
   --  Flush parameter and call Transcoder with empty In_Data until Stream_End
   --  routine indicates end of stream.

   overriding function Total_In
     (Coder : Coder_Type) return Stream_Element_Count;
   --  Returns the total amount of input data sent into the coder

   overriding function Total_Out
     (Coder : Coder_Type) return Stream_Element_Count;
   --  Returns the total amount of output data taken from the coder

   overriding function Finished (Coder : Coder_Type) return Boolean;
   --  Indicates that incoming data stream is complete and all internally
   --  processed data is out of coder.

   overriding procedure Close (Coder : in out Coder_Type);
   --  Frees internal coder memory allocations. Note that coder is derived from
   --  Limited_Controlled and will free all memory automatically on
   --  finalization.

   LZMA_Error : exception;

private

   type LZMA_Stream_Access is access all Thin.lzma_stream;

   type Coder_Type is
     new Ada.Finalization.Limited_Controlled and Coder_Interface
   with record
      Stream   : LZMA_Stream_Access;
      Finished : Boolean := False;
   end record;

   overriding procedure Finalize (Coder : in out Coder_Type);

end GNATCOLL.Coders.LZMA;
