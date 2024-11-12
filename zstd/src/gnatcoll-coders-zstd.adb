------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                     --
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

with GNATCOLL.ZSTD.Streams; use GNATCOLL.ZSTD.Streams;
with System;

package body GNATCOLL.Coders.ZSTD is

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (Coder : in out Coder_Type) is
      use C;
   begin
      if Coder.Encoder then
         if ZSTD_Free_CCtx (Coder.CContext) /= 0 then
            raise ZSTD_Error with "Failed to free ZSTD compression context";
         end if;
         Coder.CContext := Null_ZSTD_CCtx;
      else
         if ZSTD_Free_DCtx (Coder.DContext) /= 0 then
            raise ZSTD_Error with "Failed to free ZSTD decompression context";
         end if;
         Coder.DContext := Null_ZSTD_DCtx;
      end if;
   end Close;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Coder : in out Coder_Type) is
   begin
      Close (Coder);
   end Finalize;

   --------------
   -- Finished --
   --------------

   function Finished (Coder : Coder_Type) return Boolean is
   begin
      return Coder.Finished;
   end Finished;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Coder : Coder_Type) return Boolean is
   begin
      case Coder.Encoder is
         when True =>
            return Coder.CContext /= Null_ZSTD_CCtx;

         when False =>
            return Coder.DContext /= Null_ZSTD_DStream;
      end case;
   end Is_Open;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Coder     : in out Coder_Type;
      Directive : ZSTD_Reset_Directive := ZSTD_Reset_Session_Only)
   is
      Status : C.size_t;
   begin
      if not Coder.Is_Open then
         raise ZSTD_Error with "can not reset a coder which has been closed";
      end if;

      Coder.Total_In := 0;
      Coder.Total_Out := 0;
      Coder.Finished := True;

      if Coder.Encoder then
         Status := ZSTD_CCtx_Reset (Coder.CContext, Directive);
      else
         Status := ZSTD_DCtx_Reset (Coder.DContext, Directive);
      end if;

      if ZSTD_Is_Error (Status) then
         raise ZSTD_Error with "failed to reset the coder";
      end if;
   end Reset;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter
     (Coder : in out Coder_Type; Param : ZSTD_C_Parameter; Value : Integer)
   is
      Status : C.size_t;
   begin
      if not Coder.Finished then
         raise ZSTD_Error
           with "can not set parameter during an ongoing compression";
      end if;

      if not Coder.Encoder then
         raise ZSTD_Error
           with "can not set compression parameter with a decoder";
      end if;

      Status :=
        ZSTD_CCtx_Set_Parameter
          (CCtx => Coder.CContext, Param => Param, Value => Value);

      if ZSTD_Is_Error (Status) then
         raise ZSTD_Error with "invalid parameter value" & Value'Img;
      end if;
   end Set_Parameter;

   procedure Set_Parameter
     (Coder : in out Coder_Type; Param : ZSTD_D_Parameter; Value : Integer)
   is
      Status : C.size_t;
   begin
      if not Coder.Finished then
         raise ZSTD_Error
           with "can not set parameter during an ongoing decompression";
      end if;

      if Coder.Encoder then
         raise ZSTD_Error
           with "can not set decompression parameter with an encoder";
      end if;

      Status :=
        ZSTD_DCtx_Set_Parameter
          (DCtx => Coder.DContext, Param => Param, Value => Value);

      if ZSTD_Is_Error (Status) then
         raise ZSTD_Error with "invalid parameter value" & Value'Img;
      end if;
   end Set_Parameter;

   --------------
   -- Total_In --
   --------------

   overriding
   function Total_In (Coder : Coder_Type) return Stream_Element_Count is
   begin
      return Coder.Total_In;
   end Total_In;

   ---------------
   -- Total_Out --
   ---------------

   overriding
   function Total_Out (Coder : Coder_Type) return Stream_Element_Count is
   begin
      return Coder.Total_Out;
   end Total_Out;

   ---------------
   -- Transcode --
   ---------------

   overriding
   procedure Transcode
     (Coder    : in out Coder_Type;
      In_Data  : Stream_Element_Array;
      In_Last  : out Stream_Element_Offset;
      Out_Data : out Stream_Element_Array;
      Out_Last : out Stream_Element_Offset;
      Flush    : Flush_Mode := No_Flush)
   is

      function Buffer_Addr (A : Stream_Element_Array) return System.Address;

      function Buffer_Addr (A : Stream_Element_Array) return System.Address is
      begin
         if A'Length = 0 then
            return System.Null_Address;
         else
            return A (A'First)'Address;
         end if;
      end Buffer_Addr;

      use C;

      In_Buffer            : constant ZSTD_In_Buffer :=
        (Src  => Buffer_Addr (In_Data),
         Size => C.size_t (In_Data'Length),
         Pos  => 0);
      Out_Buffer           : constant ZSTD_Out_Buffer :=
        (Dst  => Buffer_Addr (Out_Data),
         Size => C.size_t (Out_Data'Length),
         Pos  => 0);
      Remaining_Input_Data : C.size_t;
      Directive            : ZSTD_End_Directive;

   begin

      Coder.Finished := False;

      if Coder.Encoder then
         if Flush = Finish then
            Directive := ZSTD_E_End;
         elsif Flush = No_Flush then
            Directive := ZSTD_E_Continue;
         else
            Directive := ZSTD_E_Flush;
         end if;

         Remaining_Input_Data :=
           ZSTD_Compress_Stream2
             (CCtx   => Coder.CContext,
              Output => Out_Buffer,
              Input  => In_Buffer,
              End_Op => Directive);
      else
         Remaining_Input_Data :=
           ZSTD_Decompress_Stream
             (DStream => Coder.DContext,
              Output  => Out_Buffer,
              Input   => In_Buffer);
      end if;

      if ZSTD_Is_Error (Remaining_Input_Data) then
         raise ZSTD_Error with ZSTD_Get_Error_Name (Remaining_Input_Data);
      end if;

      In_Last :=
        Ada.Streams.Stream_Element_Offset (In_Buffer.Pos) + In_Data'First - 1;
      Out_Last :=
        Ada.Streams.Stream_Element_Offset (Out_Buffer.Pos) + Out_Data'First
        - 1;

      Coder.Total_In :=
        Coder.Total_In + Ada.Streams.Stream_Element_Offset (In_Buffer.Pos);
      Coder.Total_Out :=
        Coder.Total_Out + Ada.Streams.Stream_Element_Offset (Out_Buffer.Pos);

      if Remaining_Input_Data = C.size_t (0) then
         Coder.Finished := True;
      end if;
   end Transcode;

   -------------
   -- Version --
   -------------

   function Version return String is
   begin
      return ZSTD_Version_String;
   end Version;


end GNATCOLL.Coders.ZSTD;
