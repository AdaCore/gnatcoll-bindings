------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

--  Direct binding to ZSTD stream API.

package GNATCOLL.ZSTD.Streams is

   type ZSTD_In_Buffer is record
      Src  : System.Address;  --  Start of input buffer
      Size : C.size_t;        --  Size of input buffer
      Pos  : C.size_t;        --  Position where reading stopped.
   end record;

   type ZSTD_Out_Buffer is record
      Dst  : System.Address;  --  start of output buffer
      Size : C.size_t;        --  size of output buffer
      Pos  : C.size_t;        --  position where writing stopped.
   end record;

   subtype ZSTD_CStream is ZSTD_CCtx;
   --  CCtx and CStream are now effectively same object. For Create, Init,
   --  Free and Reset use the CCtx functons declared in GNATCOLL.ZSTD.

   --------------------------------------
   --  Streaming compression functions --
   --------------------------------------

   type ZSTD_End_Directive is
     (ZSTD_E_Continue, --  collect more data, encoder decides when to output
      --  compressed result, for optimal compression ratio.
      ZSTD_E_Flush,    --  flush any data provided so far
      ZSTD_E_End       --  flush any remaining data and close current frame.
     );

   for ZSTD_End_Directive use
     (ZSTD_E_Continue => 0, ZSTD_E_Flush => 1, ZSTD_E_End => 2);

   function ZSTD_Compress_Stream2
     (CCtx   : ZSTD_CCtx;
      Output : ZSTD_Out_Buffer;
      Input  : ZSTD_In_Buffer;
      End_Op : ZSTD_End_Directive) return C.size_t;
   pragma Import (C, ZSTD_Compress_Stream2, "ZSTD_compressStream2");
   --  Streaming decompression function
   --  Returns a minimum amount of data remaining to be flushed from internal
   --  buffers or an error code, which can be tested using ZSTD_isError(). if
   --  Returned value /= 0, flush is not fully completed, there is still some
   --  data left within internal buffers.

   function ZSTD_CStream_In_Size return C.size_t;
   pragma Import (C, ZSTD_CStream_In_Size, "ZSTD_CStreamInSize");
   --  Return recommended size for input buffer.

   function ZSTD_CStream_Out_Size return C.size_t;
   pragma Import (C, ZSTD_CStream_Out_Size, "ZSTD_CStreamOutSize");
   --  Return recommended size for output buffer. Guarantee to successfully
   --  flush at least one complete compressed block.

   -----------------------------
   -- Streaming Decompression --
   -----------------------------

   subtype ZSTD_DStream is ZSTD_DCtx;
   Null_ZSTD_DStream : constant ZSTD_DStream;

   --  DCtx and DStream are now effectively same object. For Create, Free and
   --  Reset use the CCtx functons declared in GNATCOLL.ZSTD.

   ---------------------------------------
   -- Streaming decompression functions --
   ---------------------------------------

   function ZSTD_Decompress_Stream
     (DStream : ZSTD_DStream; Output : ZSTD_Out_Buffer; Input : ZSTD_In_Buffer)
      return C.size_t;
   pragma Import (C, ZSTD_Decompress_Stream, "ZSTD_decompressStream");

   --  Streaming decompression function.
   --  Call repetitively to consume full input updating it as necessary.
   --  Function will update both input and output `pos` fields exposing
   --  current state via these fields.
   --  Return 0 when a frame is completely decoded and fully flushed,
   --  or an error code, which can be tested using ZSTD_isError(),
   --  or any other value > 0, which means there is some decoding or flushing
   --  to do to complete current frame.

   function ZSTD_DStream_In_Size return C.size_t;
   pragma Import (C, ZSTD_DStream_In_Size, "ZSTD_DStreamInSize");
   --  Return recommended size for input buffer.

   function ZSTD_DStream_Out_Size return C.size_t;
   pragma Import (C, ZSTD_DStream_Out_Size, "ZSTD_DStreamOutSize");
   --  Return recommended size for output buffer. Guarantee to successfully
   --  flush at least one complete block in all circumstances.

private

   Null_ZSTD_DStream : constant ZSTD_DStream := ZSTD_DStream (Null_ZSTD_DCtx);

end GNATCOLL.ZSTD.Streams;
