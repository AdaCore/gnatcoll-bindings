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

--  Provides a higher level binding to ZSTD. Main features are:
--
--  - Compression/Decompression Contexts are handled as limited controlled
--    objects (this automate memory management).
--  - ZSTD_Error exception is raise in case of error.

with Ada.Finalization;
with Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.ZSTD.Streams; use GNATCOLL.ZSTD.Streams;
with GNATCOLL.OS.FS;

package GNATCOLL.ZSTD.Controlled is

   package FS renames GNATCOLL.OS.FS;
   package C renames Interfaces.C;

   ZSTD_Error : exception;

   type ZSTD_Compress_Context is tagged limited private;

   type ZSTD_Decompress_Context is tagged limited private;

   -------------------------------------
   -- Compression parameters function --
   -------------------------------------

   procedure Set_Parameter
     (Self : ZSTD_Compress_Context; Param : ZSTD_C_Parameter; Value : Integer);
   --  Function used to adjust compression parameter. See
   --  GNATCOLL.ZSTD.ZSTD_C_Parameter and GNATCOLL.ZSTD.ZSTD_CCtx_Set_Parameter
   --  function. In case of error ZSTD_Error is raised.

   ----------------------------------
   -- Stream compression functions --
   ----------------------------------

   --  The following set of functions can be used to implement a stream-like
   --  compression. The functions should be used the following way
   --
   --  Ctx.Initialize_Stream
   --
   --  loop
   --     <Read some data>
   --     Ctx.Set_Stream_Input (<data>)
   --
   --     <if no more data call Ctx.End_Input_Stream>
   --
   --     while Ctx.Can_Compress_Stream loop
   --         <write data returned by Ctx.Compress_Stream>
   --    end loop
   --
   --    <if no more data exit>
   --  end loop
   --
   --  For concrete see the one-pass compress function implementations from
   --  this unit.

   procedure Initialize_Stream (Self : in out ZSTD_Compress_Context)
   with Inline => True;
   --  Mark beginning of compression. Previous context is discarded.
   --  Parameters are maintained.
   --  In case of error ZSTD_Error is raised.

   procedure Set_Stream_Input
     (Self : in out ZSTD_Compress_Context; Input_Buffer : Unbounded_String)
   with Inline => True;
   --  Set Input_Buffer unbounded string as input
   --  In case of error ZSTD_Error is raised.

   procedure Set_Stream_Input
     (Self         : in out ZSTD_Compress_Context;
      Input_Buffer : String;
      Size         : Integer)
   with Inline => True;
   --  Set the first Size elements of Input_Buffer as input
   --  In case of error ZSTD_Error is raised.

   procedure Set_Stream_Input
     (Self : in out ZSTD_Compress_Context; Input_Buffer : String)
   with Inline => True;
   --  Set Input_Buffer as input
   --  In case of error ZSTD_Error is raised.

   function Can_Compress_Stream (Self : ZSTD_Compress_Context) return Boolean
   with Inline => True;
   --  Return False once all the input set with Set_Stream_Input has been
   --  consumed using one or several calls to Compress_Stream.

   function Compress_Stream (Self : in out ZSTD_Compress_Context) return String
   with Inline => True;
   --  Consume the input and emit some output. In order to be sure that this
   --  function can be called call Can_Compress_Stream before each call.
   --  In case of error ZSTD_Error is raised.

   procedure End_Input_Stream (Self : in out ZSTD_Compress_Context)
   with Inline => True;
   --  Call after the last call to Set_Input_Stream to indicate the end of the
   --  input.

   ------------------------------------
   -- One pass compression functions --
   ------------------------------------

   function Compress
     (Self : in out ZSTD_Compress_Context; Str : Unbounded_String)
      return Unbounded_String;
   --  Return result of Str compression as Unbounded_String
   --  Raise ZSTD_Error in case of Error

   function Compress
     (Self : in out ZSTD_Compress_Context; Src_FD : FS.File_Descriptor)
      return Unbounded_String;
   --  Returned result of Src_FD compression as Unbounded_String
   --  Raise ZSTD_Error in case of Error

   procedure Compress
     (Self   : in out ZSTD_Compress_Context;
      Src_FD : FS.File_Descriptor;
      Dst_FD : FS.File_Descriptor);
   --  Write in Dst_FD the result of Src_FD compression
   --  Raise ZSTD_Error in case of Error

   ------------------------------------
   -- Stream decompression functions --
   ------------------------------------

   --  The following set of functions can be used to implement a stream-like
   --  decompression. The functions should be used the following way
   --
   --  Ctx.Initialize_Stream
   --
   --  loop
   --     <Read some data>
   --     Ctx.Set_Stream_Input (<data>)
   --     <exit when no more data to read>
   --
   --     while Ctx.Can_Decompress_Stream loop
   --         <write data returned by Ctx.Decompress_Stream>
   --    end loop
   --  end loop
   --
   --  Ctx.End_Stream
   --
   --  For concrete see the one-pass decompress function implementations from
   --  this unit.

   procedure Initialize_Stream (Self : in out ZSTD_Decompress_Context)
   with Inline => True;
   --  Mark beginning of decompression. Previous context is discarded.
   --  Parameters are maintained.
   --  In case of error ZSTD_Error is raised.

   procedure Set_Stream_Input
     (Self : in out ZSTD_Decompress_Context; Input_Buffer : Unbounded_String)
   with Inline => True;
   --  Set Input_Buffer as input
   --  In case of error ZSTD_Error is raised.

   procedure Set_Stream_Input
     (Self         : in out ZSTD_Decompress_Context;
      Input_Buffer : String;
      Size         : Integer);
   --  Set the first Size elements of Input_Buffer as input
   --  In case of error ZSTD_Error is raised.

   procedure Set_Stream_Input
     (Self : in out ZSTD_Decompress_Context; Input_Buffer : String);
   --  Set Input_Buffer as input
   --  In case of error ZSTD_Error is raised.

   function Can_Decompress_Stream
     (Self : ZSTD_Decompress_Context) return Boolean
   with Inline => True;
   --  Return False once all the input set with Set_Stream_Input has been
   --  consumed using one or several calls to Decompress_Stream.

   function Decompress_Stream
     (Self : in out ZSTD_Decompress_Context) return String
   with Inline => True;
   --  Consume the input and emit some output. In order to be sure that this
   --  function can be called call Can_Decompress_Stream before each call.
   --  In case of error ZSTD_Error is raised.

   procedure End_Stream (Self : ZSTD_Decompress_Context)
   with Inline => True;
   --  Check that decompression is complete.
   --  Raise ZSTD_Error otherwise

   ---------------------------------------
   --  One-pass decompression functions --
   ---------------------------------------

   function Decompress
     (Self : in out ZSTD_Decompress_Context; Str : Unbounded_String)
      return Unbounded_String;
   --  Return result of Str decompression as Unbounded_String
   --  Raise ZSTD_Error in case of Error

   function Decompress
     (Self : in out ZSTD_Decompress_Context; Src_FD : FS.File_Descriptor)
      return Unbounded_String;
   --  Returned result of Src_FD decompression as Unbounded_String
   --  Raise ZSTD_Error in case of Error

   procedure Decompress
     (Self   : in out ZSTD_Decompress_Context;
      Src_FD : FS.File_Descriptor;
      Dst_FD : FS.File_Descriptor);
   --  Write in Dst_FD the result of Src_FD compression
   --  Raise ZSTD_Error in case of Error

private
   package AF renames Ada.Finalization;

   type Buffer_Access is access all String;

   type ZSTD_Compress_Context is new AF.Limited_Controlled with record
      Ctx        : ZSTD_CStream;
      Buffer     : Buffer_Access;
      In_Buffer  : ZSTD_In_Buffer;
      Out_Buffer : ZSTD_Out_Buffer;
      Status     : C.size_t;
      End_Op     : ZSTD_End_Directive;
   end record;

   procedure Initialize (Self : in out ZSTD_Compress_Context);
   procedure Finalize (Self : in out ZSTD_Compress_Context);

   type ZSTD_Decompress_Context is new AF.Limited_Controlled with record
      Ctx        : ZSTD_DStream;
      Buffer     : Buffer_Access;
      In_Buffer  : ZSTD_In_Buffer;
      Out_Buffer : ZSTD_Out_Buffer;
      Status     : C.size_t;
   end record;

   procedure Initialize (Self : in out ZSTD_Decompress_Context);
   procedure Finalize (Self : in out ZSTD_Decompress_Context);

end GNATCOLL.ZSTD.Controlled;
