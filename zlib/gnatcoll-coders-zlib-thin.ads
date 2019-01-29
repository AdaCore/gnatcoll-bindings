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

--  This package provides thin binding to ZLib compression/decompression

with Interfaces.C.Strings; use Interfaces;
with System;

package GNATCOLL.Coders.ZLib.Thin is

   MAX_MEM_LEVEL : constant := 9;
   MAX_WBITS     : constant := 15;   -- 32K LZ77 window

   SEEK_SET : constant := 0; -- Seek from beginning of file
   SEEK_CUR : constant := 1; -- Seek from current position
   SEEK_END : constant := 2; -- Set file pointer to EOF plus "offset"

   type Byte  is new Interfaces.C.unsigned_char; -- 8 bits
   type UInt  is new Interfaces.C.unsigned;      -- 16 bits or more
   type Int   is new Interfaces.C.int;
   type ULong is new Interfaces.C.unsigned_long; -- 32 bits or more

   subtype Chars_Ptr is Interfaces.C.Strings.chars_ptr;

   type ULong_Access is access ULong;
   type Int_Access is access Int;

   subtype Voidp is System.Address;

   subtype Byte_Access is Voidp;

   Nul : constant Voidp := System.Null_Address;

   Z_NO_FLUSH            : constant := 0;
   Z_PARTIAL_FLUSH       : constant := 1;
   Z_SYNC_FLUSH          : constant := 2;
   Z_FULL_FLUSH          : constant := 3;
   Z_FINISH              : constant := 4;
   Z_OK                  : constant := 0;
   Z_STREAM_END          : constant := 1;
   Z_NEED_DICT           : constant := 2;
   Z_ERRNO               : constant := -1;
   Z_STREAM_ERROR        : constant := -2;
   Z_DATA_ERROR          : constant := -3;
   Z_MEM_ERROR           : constant := -4;
   Z_BUF_ERROR           : constant := -5;
   Z_VERSION_ERROR       : constant := -6;
   Z_NO_COMPRESSION      : constant := 0;
   Z_BEST_SPEED          : constant := 1;
   Z_BEST_COMPRESSION    : constant := 9;
   Z_DEFAULT_COMPRESSION : constant := -1;
   Z_FILTERED            : constant := 1;
   Z_HUFFMAN_ONLY        : constant := 2;
   Z_DEFAULT_STRATEGY    : constant := 0;
   Z_BINARY              : constant := 0;
   Z_ASCII               : constant := 1;
   Z_UNKNOWN             : constant := 2;
   Z_DEFLATED            : constant := 8;
   Z_NULL                : constant := 0;

   type gzFile is new Voidp;

   type Z_Stream is private;

   type alloc_func is access function
     (Opaque : Voidp; Items : UInt; Size : UInt) return Voidp;

   type free_func is access procedure (opaque : Voidp; address : Voidp);

   function zlibVersion return Chars_Ptr;

   function Deflate (strm : access Z_Stream; flush : Int) return Int;

   function DeflateEnd (strm : access Z_Stream) return Int;
   --  Dealocate internal data

   procedure DeflateEnd (strm : access Z_Stream);
   --  Dealocate internal data and ignore error code

   function Inflate (strm : access Z_Stream; flush : Int) return Int;

   function InflateEnd (strm : access Z_Stream) return Int;
   --  Dealocate internal data

   procedure InflateEnd (strm : access Z_Stream);
   --  Dealocate internal data and ignore error code

   function deflateSetDictionary
     (strm       : access Z_Stream;
      dictionary : Byte_Access;
      dictLength : UInt) return Int;

   function deflateCopy
     (dest : access Z_Stream; source : access Z_Stream) return Int;

   function deflateReset (strm : access Z_Stream) return Int;

   function deflateParams
     (strm     : access Z_Stream;
      level    : Int;
      strategy : Int) return Int;

   function inflateSetDictionary
     (strm       : access Z_Stream;
      dictionary : Byte_Access;
      dictLength : UInt) return Int;

   function inflateSync (strm : access Z_Stream) return Int;

   function inflateReset (strm : access Z_Stream) return Int;

   function compress
     (dest      : Byte_Access;
      destLen   : ULong_Access;
      source    : Byte_Access;
      sourceLen : ULong) return Int;

   function compress2
     (dest      : Byte_Access;
      destLen   : ULong_Access;
      source    : Byte_Access;
      sourceLen : ULong;
      level     : Int) return Int;

   function uncompress
     (dest      : Byte_Access;
      destLen   : ULong_Access;
      source    : Byte_Access;
      sourceLen : ULong) return Int;

   function gzopen (path : Chars_Ptr; mode : Chars_Ptr) return gzFile;

   function gzdopen (fd : Int; mode : Chars_Ptr) return gzFile;

   function gzsetparams
     (file     : gzFile;
      level    : Int;
      strategy : Int) return Int;

   function gzread
     (file : gzFile;
      buf  : Voidp;
      len  : UInt) return Int;

   function gzwrite
     (file : gzFile;
      buf  : Voidp;
      len  : UInt) return Int;

   function gzprintf (file : gzFile; format : Chars_Ptr) return Int;

   function gzputs (file : gzFile; s : Chars_Ptr) return Int;

   function gzgets
     (file : gzFile;
      buf  : Chars_Ptr;
      len  : Int) return Chars_Ptr;

   function gzputc (file : gzFile; char : Int) return Int;

   function gzgetc (file : gzFile) return Int;

   function gzflush (file : gzFile; flush : Int) return Int;

   function gzseek
     (file : gzFile; offset : Int; whence : Int) return Int;

   function gzrewind (file : gzFile) return Int;

   function gztell (file : gzFile) return Int;

   function gzeof (file : gzFile) return Int;

   function gzclose (file : gzFile) return Int;

   function gzerror (file : gzFile; errnum : Int_Access) return Chars_Ptr;

   function adler32
     (adler : ULong; buf : Byte_Access; len : UInt) return ULong;

   function crc32 (crc : ULong; buf : Byte_Access; len : UInt) return ULong;

   function deflateInit
     (strm        : access Z_Stream;
      level       : Int;
      version     : Chars_Ptr;
      stream_size : Int) return Int;

   function deflateInit2
     (strm        : access Z_Stream;
      level       : Int;
      method      : Int;
      windowBits  : Int;
      memLevel    : Int;
      strategy    : Int;
      version     : Chars_Ptr;
      stream_size : Int) return Int;

   function Deflate_Init
     (strm       : access Z_Stream;
      level      : Int;
      method     : Int;
      windowBits : Int;
      memLevel   : Int;
      strategy   : Int) return Int with Inline;

   function inflateInit
     (strm        : access Z_Stream;
      version     : Chars_Ptr;
      stream_size : Int) return Int;

   function inflateInit2
     (strm        : access Z_Stream;
      windowBits  : Int;
      version     : Chars_Ptr;
      stream_size : Int) return Int;

   function inflateBackInit
     (strm        : access Z_Stream;
      windowBits  : Int;
      window      : Byte_Access;
      version     : Chars_Ptr;
      stream_size : Int) return Int;
   --  Size of window have to be 2**windowBits

   function Inflate_Init (strm : access Z_Stream; windowBits : Int) return Int
     with Inline;

   function zError (err : Int) return Chars_Ptr;

   function inflateSyncPoint (z : access Z_Stream) return Int;

   function get_crc_table return ULong_Access;

   --  Interface to the available fields of the z_stream structure.
   --  The application must update next_in and avail_in when avail_in has
   --  dropped to zero. It must update next_out and avail_out when avail_out
   --  has dropped to zero. The application must initialize zalloc, zfree and
   --  opaque before calling the init function.

   procedure Set_In
     (Strm   : in out Z_Stream;
      Buffer : Voidp;
      Size   : UInt)
     with Inline;

   procedure Set_Out
     (Strm   : in out Z_Stream;
      Buffer : Voidp;
      Size   : UInt)
     with Inline;

   procedure Set_Mem_Func
     (Strm   : in out Z_Stream;
      Opaque : Voidp;
      Alloc  : alloc_func;
      Free   : free_func)
     with Inline;

   function Last_Error_Message (Strm : Z_Stream) return String with Inline;

   function Avail_Out (Strm : Z_Stream) return UInt with Inline;

   function Avail_In (Strm : Z_Stream) return UInt with Inline;

   function Total_In (Strm : Z_Stream) return ULong with Inline;

   function Total_Out (Strm : Z_Stream) return ULong with Inline;

   function inflateCopy
     (dest : access Z_Stream; Source : access Z_Stream) return Int;

   function compressBound (Source_Len : ULong) return ULong;

   function deflateBound
     (Strm : access Z_Stream; Source_Len : ULong) return ULong;

   function gzungetc (C : Int; File : gzFile) return Int;

   function zlibCompileFlags return ULong;

private

   type Z_Stream is record            -- zlib.h:68
      Next_In   : Voidp      := Nul;  -- next input byte
      Avail_In  : UInt       := 0;    -- number of bytes available at next_in
      Total_In  : ULong      := 0;    -- total nb of input bytes read so far
      Next_Out  : Voidp      := Nul;  -- next output byte should be put there
      Avail_Out : UInt       := 0;    -- remaining free space at next_out
      Total_Out : ULong      := 0;    -- total nb of bytes output so far
      msg       : Chars_Ptr;          -- last error message, NULL if no error
      state     : Voidp      := Nul;  -- not visible by applications
      zalloc    : alloc_func := null; -- used to allocate the internal state
      zfree     : free_func  := null; -- used to free the internal state
      opaque    : Voidp      := Nul;
      --  private data object passed to zalloc and zfree
      data_type : Int        := 0;
      --  best guess about the data type: ascii or binary
      adler     : ULong      := 0; -- adler32 value of the uncompressed data
      reserved  : ULong      := 0; -- reserved for future use
   end record with Convention => C;

   pragma Import (C, zlibVersion, "zlibVersion");
   pragma Import (C, Deflate, "deflate");
   pragma Import (C, DeflateEnd, "deflateEnd");
   pragma Import (C, Inflate, "inflate");
   pragma Import (C, InflateEnd, "inflateEnd");
   pragma Import (C, deflateSetDictionary, "deflateSetDictionary");
   pragma Import (C, deflateCopy, "deflateCopy");
   pragma Import (C, deflateReset, "deflateReset");
   pragma Import (C, deflateParams, "deflateParams");
   pragma Import (C, inflateSetDictionary, "inflateSetDictionary");
   pragma Import (C, inflateSync, "inflateSync");
   pragma Import (C, inflateReset, "inflateReset");
   pragma Import (C, compress, "compress");
   pragma Import (C, compress2, "compress2");
   pragma Import (C, uncompress, "uncompress");
   pragma Import (C, gzopen, "gzopen");
   pragma Import (C, gzdopen, "gzdopen");
   pragma Import (C, gzsetparams, "gzsetparams");
   pragma Import (C, gzread, "gzread");
   pragma Import (C, gzwrite, "gzwrite");
   pragma Import (C, gzprintf, "gzprintf");
   pragma Import (C, gzputs, "gzputs");
   pragma Import (C, gzgets, "gzgets");
   pragma Import (C, gzputc, "gzputc");
   pragma Import (C, gzgetc, "gzgetc");
   pragma Import (C, gzflush, "gzflush");
   pragma Import (C, gzseek, "gzseek");
   pragma Import (C, gzrewind, "gzrewind");
   pragma Import (C, gztell, "gztell");
   pragma Import (C, gzeof, "gzeof");
   pragma Import (C, gzclose, "gzclose");
   pragma Import (C, gzerror, "gzerror");
   pragma Import (C, adler32, "adler32");
   pragma Import (C, crc32, "crc32");
   pragma Import (C, deflateInit, "deflateInit_");
   pragma Import (C, inflateInit, "inflateInit_");
   pragma Import (C, deflateInit2, "deflateInit2_");
   pragma Import (C, inflateInit2, "inflateInit2_");
   pragma Import (C, zError, "zError");
   pragma Import (C, inflateSyncPoint, "inflateSyncPoint");
   pragma Import (C, get_crc_table, "get_crc_table");

   --  since zlib 1.2.0:

   pragma Import (C, inflateCopy, "inflateCopy");
   pragma Import (C, compressBound, "compressBound");
   pragma Import (C, deflateBound, "deflateBound");
   pragma Import (C, gzungetc, "gzungetc");
   pragma Import (C, zlibCompileFlags, "zlibCompileFlags");

   pragma Import (C, inflateBackInit, "inflateBackInit_");

end GNATCOLL.Coders.ZLib.Thin;
