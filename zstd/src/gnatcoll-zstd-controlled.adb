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

pragma Warnings (Off);
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On);
with Ada.Unchecked_Deallocation;

package body GNATCOLL.ZSTD.Controlled is

   package Unb_Aux renames Ada.Strings.Unbounded.Aux;
   use all type C.size_t;

   procedure Free is new Ada.Unchecked_Deallocation (String, Buffer_Access);

   procedure Set_Stream_Input
     (Buffer : in out ZSTD_In_Buffer; Str : Unbounded_String)
   with Inline => True;

   procedure Set_Stream_Input
     (Buffer : in out ZSTD_In_Buffer; Str : String; Size : Integer := -1)
   with Inline => True;

   procedure Set_Stream_Output
     (Buffer : in out ZSTD_Out_Buffer; Str : Buffer_Access)
   with Inline => True;

   procedure Write_To_File (FD : FS.File_Descriptor; Buffer : String);

   ---------------------------
   -- Can_Decompress_Stream --
   ---------------------------

   function Can_Compress_Stream (Self : ZSTD_Compress_Context) return Boolean
   is
   begin
      return
        (Self.End_Op = ZSTD_E_Continue
         and then Self.In_Buffer.Pos < Self.In_Buffer.Size)
        or else (Self.End_Op = ZSTD_E_End and then Self.Status /= 0);
   end Can_Compress_Stream;

   function Can_Decompress_Stream
     (Self : ZSTD_Decompress_Context) return Boolean is
   begin
      return Self.In_Buffer.Pos < Self.In_Buffer.Size;
   end Can_Decompress_Stream;

   --------------
   -- Compress --
   --------------

   procedure Compress
     (Self   : in out ZSTD_Compress_Context;
      Src_FD : FS.File_Descriptor;
      Dst_FD : FS.File_Descriptor)
   is
      FD_Buffer : String (1 .. FS.Default_Buffer_Size);
      FD_Read   : Integer;
   begin
      Self.Initialize_Stream;

      loop
         FD_Read := FS.Read (Src_FD, FD_Buffer);
         Self.Set_Stream_Input (FD_Buffer, FD_Read);

         if FD_Read = 0 then
            Self.End_Input_Stream;
         end if;

         while Self.Can_Compress_Stream loop
            Write_To_File (Dst_FD, Self.Compress_Stream);
         end loop;

         exit when FD_Read = 0;
      end loop;
   end Compress;

   function Compress
     (Self : in out ZSTD_Compress_Context; Str : Unbounded_String)
      return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      --  Initialize compression context and buffers
      Self.Initialize_Stream;
      Self.Set_Stream_Input (Str);
      Self.End_Input_Stream;

      while Self.Can_Compress_Stream loop
         Append (Result, Self.Compress_Stream);
      end loop;

      return Result;
   end Compress;

   function Compress
     (Self : in out ZSTD_Compress_Context; Src_FD : FS.File_Descriptor)
      return Unbounded_String
   is
      Result    : Unbounded_String;
      FD_Buffer : String (1 .. FS.Default_Buffer_Size);
      FD_Read   : Integer;
   begin
      Self.Initialize_Stream;
      loop
         FD_Read := FS.Read (Src_FD, FD_Buffer);
         Self.Set_Stream_Input (FD_Buffer, FD_Read);

         if FD_Read = 0 then
            Self.End_Input_Stream;
         end if;

         while Self.Can_Compress_Stream loop
            Append (Result, Self.Compress_Stream);
         end loop;

         exit when FD_Read = 0;
      end loop;
      return Result;

   end Compress;

   ---------------------
   -- Compress_Stream --
   ---------------------

   function Compress_Stream (Self : in out ZSTD_Compress_Context) return String
   is
   begin
      Self.Status :=
        ZSTD_Compress_Stream2
          (Self.Ctx,
           Output => Self.Out_Buffer,
           Input  => Self.In_Buffer,
           End_Op => Self.End_Op);
      if ZSTD_Is_Error (Self.Status) then
         raise ZSTD_Error with "invalid compression";
      end if;

      if Self.Out_Buffer.Pos > 0 then
         declare
            Last : constant Integer := Integer (Self.Out_Buffer.Pos);
         begin
            Self.Out_Buffer.Pos := 0;
            return Self.Buffer (1 .. Last);
         end;
      else
         return "";
      end if;
   end Compress_Stream;

   ----------------
   -- Decompress --
   ----------------

   function Decompress
     (Self : in out ZSTD_Decompress_Context; Str : Unbounded_String)
      return Unbounded_String
   is
      Result : Unbounded_String;

   begin
      Self.Initialize_Stream;
      Self.Set_Stream_Input (Str);

      while Self.Can_Decompress_Stream loop
         Append (Result, Self.Decompress_Stream);
      end loop;

      Self.End_Stream;

      return Result;
   end Decompress;

   function Decompress
     (Self : in out ZSTD_Decompress_Context; Src_FD : FS.File_Descriptor)
      return Unbounded_String
   is
      Result    : Unbounded_String;
      FD_Buffer : String (1 .. FS.Default_Buffer_Size);
      FD_Read   : Integer;
   begin
      Self.Initialize_Stream;

      loop
         FD_Read := FS.Read (Src_FD, FD_Buffer);
         Self.Set_Stream_Input (FD_Buffer, FD_Read);
         exit when FD_Read = 0;

         while Self.Can_Decompress_Stream loop
            Append (Result, Self.Decompress_Stream);
         end loop;
      end loop;

      Self.End_Stream;

      return Result;
   end Decompress;

   procedure Decompress
     (Self   : in out ZSTD_Decompress_Context;
      Src_FD : FS.File_Descriptor;
      Dst_FD : FS.File_Descriptor)
   is
      FD_Buffer : String (1 .. FS.Default_Buffer_Size);
      FD_Read   : Integer;
   begin
      Self.Initialize_Stream;

      loop
         FD_Read := FS.Read (Src_FD, FD_Buffer);
         Self.Set_Stream_Input (FD_Buffer, FD_Read);
         exit when FD_Read = 0;

         Self.In_Buffer.Size := C.size_t (FD_Read);
         Self.In_Buffer.Pos := 0;

         while Self.Can_Decompress_Stream loop
            FS.Write (Dst_FD, Self.Decompress_Stream);
         end loop;
      end loop;

      Self.End_Stream;
   end Decompress;

   -----------------------
   -- Decompress_Stream --
   -----------------------

   function Decompress_Stream
     (Self : in out ZSTD_Decompress_Context) return String is
   begin
      Self.Status :=
        ZSTD_Decompress_Stream
          (Self.Ctx, Output => Self.Out_Buffer, Input => Self.In_Buffer);
      if ZSTD_Is_Error (Self.Status) then
         raise ZSTD_Error with "error during stream decompression";
      end if;

      if Self.Out_Buffer.Pos > 0 then
         declare
            Last : constant Integer := Integer (Self.Out_Buffer.Pos);
         begin
            Self.Out_Buffer.Pos := 0;
            return Self.Buffer (1 .. Last);
         end;
      else
         return "";
      end if;
   end Decompress_Stream;

   ----------------------
   -- End_Input_Stream --
   ----------------------

   procedure End_Input_Stream (Self : in out ZSTD_Compress_Context) is
   begin
      Self.End_Op := ZSTD_E_End;
      --  By changing the status to 1 we ensure that at least one call to
      --  compress will be done after marking the end of the input.
      Self.Status := 1;
   end End_Input_Stream;

   ----------------
   -- End_Stream --
   ----------------

   procedure End_Stream (Self : ZSTD_Decompress_Context) is
   begin
      if Self.Status /= 0 then
         raise ZSTD_Error with "truncated compressed stream detected";
      end if;
   end End_Stream;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out ZSTD_Compress_Context) is
      Status : C.size_t;
   begin
      if Self.Ctx /= Null_ZSTD_CCtx then
         Status := ZSTD_Free_CCtx (Self.Ctx);
         if ZSTD_Is_Error (Status) then
            raise ZSTD_Error with "cannot free ZSTD compression context";
         end if;
      end if;
      Free (Self.Buffer);
   end Finalize;

   procedure Finalize (Self : in out ZSTD_Decompress_Context) is
      Status : C.size_t;
   begin
      if Self.Ctx /= Null_ZSTD_DCtx then
         Status := ZSTD_Free_DCtx (Self.Ctx);
         if ZSTD_Is_Error (Status) then
            raise ZSTD_Error with "cannot free ZSTD decompression context";
         end if;
      end if;
      Free (Self.Buffer);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out ZSTD_Compress_Context) is
   begin
      Self.Ctx := ZSTD_Create_CCtx;
      if Self.Ctx = Null_ZSTD_CCtx then
         raise ZSTD_Error with "cannot create ZSTD compression context";
      end if;
      Self.Buffer := new String (1 .. 2 ** 17);
   end Initialize;

   procedure Initialize (Self : in out ZSTD_Decompress_Context) is
   begin
      Self.Ctx := ZSTD_Create_DCtx;
      if Self.Ctx = Null_ZSTD_DCtx then
         raise ZSTD_Error with "cannot create ZSTD decompression context";
      end if;
      Self.Buffer := new String (1 .. 2 ** 17);
   end Initialize;

   -----------------------
   -- Initialize_Stream --
   -----------------------

   procedure Initialize_Stream (Self : in out ZSTD_Decompress_Context) is
      Status : C.size_t;
   begin
      --  Initialize the context
      Status := ZSTD_DCtx_Reset (Self.Ctx, ZSTD_Reset_Session_Only);
      if ZSTD_Is_Error (Status) then
         raise ZSTD_Error with "cannot initialize decompression stream";
      end if;

      --  Initialize output buffer
      Set_Stream_Output (Self.Out_Buffer, Self.Buffer);
   end Initialize_Stream;

   procedure Initialize_Stream (Self : in out ZSTD_Compress_Context) is
      Status : C.size_t;
   begin
      --  Initialize the context
      Status := ZSTD_CCtx_Reset (Self.Ctx, ZSTD_Reset_Session_Only);
      if ZSTD_Is_Error (Status) then
         raise ZSTD_Error with "cannot initialize decompression stream";
      end if;

      --  Initialize input buffer
      Set_Stream_Output (Self.Out_Buffer, Self.Buffer);

      Self.End_Op := ZSTD_E_Continue;
   end Initialize_Stream;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter
     (Self : ZSTD_Compress_Context; Param : ZSTD_C_Parameter; Value : Integer)
   is
      Status : C.size_t;
   begin
      Status :=
        ZSTD_CCtx_Set_Parameter
          (CCtx => Self.Ctx, Param => Param, Value => Value);
      if ZSTD_Is_Error (Status) then
         raise ZSTD_Error with "invalid parameter value" & Value'Img;
      end if;
   end Set_Parameter;

   ----------------------
   -- Set_Stream_Input --
   ----------------------

   procedure Set_Stream_Input
     (Self         : in out ZSTD_Compress_Context;
      Input_Buffer : String;
      Size         : Integer) is
   begin
      Set_Stream_Input (Self.In_Buffer, Input_Buffer, Size);
   end Set_Stream_Input;

   procedure Set_Stream_Input
     (Self : in out ZSTD_Compress_Context; Input_Buffer : String) is
   begin
      Set_Stream_Input (Self.In_Buffer, Input_Buffer, Input_Buffer'Length);
   end Set_Stream_Input;

   procedure Set_Stream_Input
     (Self : in out ZSTD_Compress_Context; Input_Buffer : Unbounded_String) is
   begin
      Set_Stream_Input (Self.In_Buffer, Input_Buffer);
   end Set_Stream_Input;

   procedure Set_Stream_Input
     (Self : in out ZSTD_Decompress_Context; Input_Buffer : Unbounded_String)
   is
   begin
      Set_Stream_Input (Self.In_Buffer, Input_Buffer);
   end Set_Stream_Input;

   procedure Set_Stream_Input
     (Self         : in out ZSTD_Decompress_Context;
      Input_Buffer : String;
      Size         : Integer) is
   begin
      Set_Stream_Input (Self.In_Buffer, Input_Buffer, Size);
   end Set_Stream_Input;

   procedure Set_Stream_Input
     (Self : in out ZSTD_Decompress_Context; Input_Buffer : String) is
   begin
      Set_Stream_Input (Self.In_Buffer, Input_Buffer, Input_Buffer'Length);
   end Set_Stream_Input;

   procedure Set_Stream_Input
     (Buffer : in out ZSTD_In_Buffer; Str : Unbounded_String)
   is
      In_Str_Access : Unb_Aux.Big_String_Access;
      In_Str_Length : Natural;
   begin
      Unb_Aux.Get_String (Str, In_Str_Access, In_Str_Length);

      if In_Str_Length > 0 then
         Buffer.Src := In_Str_Access (1)'Address;
      else
         Buffer.Src := System.Null_Address;
      end if;

      Buffer.Size := C.size_t (In_Str_Length);
      Buffer.Pos := 0;
   end Set_Stream_Input;

   procedure Set_Stream_Input
     (Buffer : in out ZSTD_In_Buffer; Str : String; Size : Integer := -1) is
   begin
      if Str'Length > 0 then
         Buffer.Src := Str (Str'First)'Address;
      else
         Buffer.Src := System.Null_Address;
      end if;

      if Size = -1 or else Size > Str'Length then
         Buffer.Size := C.size_t (Str'Length);
      else
         Buffer.Size := C.size_t (Size);
      end if;

      Buffer.Pos := 0;
   end Set_Stream_Input;

   -----------------------
   -- Set_Stream_Output --
   -----------------------

   procedure Set_Stream_Output
     (Buffer : in out ZSTD_Out_Buffer; Str : Buffer_Access) is
   begin
      --  By construction we are sure that the string is not empty
      Buffer.Dst := Str.all (1)'Address;
      Buffer.Size := Str.all'Length;
      Buffer.Pos := 0;
   end Set_Stream_Output;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File (FD : FS.File_Descriptor; Buffer : String) is
   begin
      if Buffer'Length > 0 then
         FS.Write (FD, Buffer);
      end if;
   end Write_To_File;

end GNATCOLL.ZSTD.Controlled;
