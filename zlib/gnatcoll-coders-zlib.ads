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

--  This package provides binding to ZLib compression/decompression

with Ada.Finalization;
with Interfaces;

limited with GNATCOLL.Coders.ZLib.Thin;

package GNATCOLL.Coders.ZLib is

   type Coder_Type is
     new Ada.Finalization.Limited_Controlled and Coder_Interface
   with private;

   type Compression_Level is new Integer range -1 .. 9;

   type Compression_Method is private;

   type Window_Bits_Type is new Integer range 8 .. 15;

   type Memory_Level_Type is new Integer range 1 .. 9;

   type Strategy_Type is private;

   type Header_Type is (None, Auto, Default, GZip);
   --  Header type usage have a some limitation for inflate.
   --  See comment for Inflate_Init.

   Default_Memory_Level : constant Memory_Level_Type := 8;
   Default_Window_Bits  : constant Window_Bits_Type  := 15;

   ----------------------------------
   -- Compression method constants --
   ----------------------------------

   Deflated : constant Compression_Method;
   --  Only one method allowed in this ZLib version

   ---------------------------------
   -- Compression level constants --
   ---------------------------------

   No_Compression      : constant Compression_Level := 0;
   Best_Speed          : constant Compression_Level := 1;
   Best_Compression    : constant Compression_Level := 9;
   Default_Compression : constant Compression_Level := -1;

   ------------------------------------
   -- Compression strategy constants --
   ------------------------------------

   --  RLE stategy can be used only in version 1.2.0 and later

   Filtered         : constant Strategy_Type;
   Huffman_Only     : constant Strategy_Type;
   RLE              : constant Strategy_Type;
   Default_Strategy : constant Strategy_Type;

   function Version return String with Inline;
   --  Return string representation of the ZLib version

   procedure Deflate_Init
     (Coder        : in out Coder_Type;
      Level        :        Compression_Level  := Default_Compression;
      Strategy     :        Strategy_Type      := Default_Strategy;
      Method       :        Compression_Method := Deflated;
      Window_Bits  :        Window_Bits_Type   := Default_Window_Bits;
      Memory_Level :        Memory_Level_Type  := Default_Memory_Level;
      Header       :        Header_Type        := Default);
   --  Compressor initialization.
   --  When Header parameter is Auto or Default, then default zlib header
   --  would be provided for compressed data.
   --  When Header is GZip, then gzip header would be set instead of
   --  default header.
   --  When Header is None, no header would be set for compressed data.

   procedure Inflate_Init
     (Coder       : in out Coder_Type;
      Window_Bits :        Window_Bits_Type := Default_Window_Bits;
      Header      :        Header_Type      := Default);
   --  Decompressor initialization.
   --  Default header type mean that ZLib default header is expecting in the
   --  input compressed stream.
   --  Header type None mean that no header is expecting in the input stream.
   --  GZip header type mean that GZip header is expecting in the
   --  input compressed stream.
   --  Auto header type mean that header type (GZip or Native) would be
   --  detected automatically in the input stream.
   --  Note that header types parameter values None, GZip and Auto are
   --  supported for inflate routine only in ZLib versions 1.2.0.2 and later.
   --  Deflate_Init is supporting all header types.

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

   ZLib_Error   : exception;

private

   pragma Assert (Stream_Element'Size    =    8);
   pragma Assert (Stream_Element'Modulus = 2**8);

   type Compression_Method is new Integer range 8 .. 8;

   type Strategy_Type is new Integer range 0 .. 3;

   Filtered         : constant Strategy_Type := 1;
   Huffman_Only     : constant Strategy_Type := 2;
   RLE              : constant Strategy_Type := 3;
   Default_Strategy : constant Strategy_Type := 0;

   Deflated : constant Compression_Method := 8;

   type Z_Stream_Access is access all Thin.Z_Stream;

   type Coder_Type is
     new Ada.Finalization.Limited_Controlled and Coder_Interface
   with record
      Stream      : Z_Stream_Access;
      Compression : Boolean;
      Stream_End  : Boolean;
      Header      : Header_Type;
      CRC         : Interfaces.Unsigned_32;
      Offset      : Stream_Element_Offset;
      --  Offset for gzip header/footer output
   end record;

   overriding procedure Finalize (Coder : in out Coder_Type);

end GNATCOLL.Coders.ZLib;
