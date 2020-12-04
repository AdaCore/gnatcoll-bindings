------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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
with Interfaces.C.Strings;      use Interfaces;
with GNATCOLL.Coders.ZLib.Thin; use GNATCOLL.Coders.ZLib.Thin;

package body GNATCOLL.Coders.ZLib is

   type Return_Code_Enum is
      (OK,
       STREAM_END,
       NEED_DICT,
       ERRNO,
       STREAM_ERROR,
       DATA_ERROR,
       MEM_ERROR,
       BUF_ERROR,
       VERSION_ERROR);

   type Flate_Step_Function is access function
     (Strm : access Thin.Z_Stream; Flush : Thin.Int) return Thin.Int
     with Convention => C;

   type Flate_End_Procedure is access procedure
     (Ctrm : access Thin.Z_Stream) with Convention => C;

   type Flate_Type is record
      Step : Flate_Step_Function;
      Done : Flate_End_Procedure;
   end record;

   subtype Footer_Array is Stream_Element_Array (1 .. 8);

   Simple_GZip_Header : constant Stream_Element_Array (1 .. 10) :=
     (16#1f#, 16#8b#,                 -- Magic header
      16#08#,                         -- Z_DEFLATED
      16#00#,                         -- Flags
      16#00#, 16#00#, 16#00#, 16#00#, -- Time
      16#00#,                         -- XFlags
      16#03#                          -- OS code
     );
   --  The simplest gzip header is not for informational, but just for
   --  gzip format compatibility.
   --  Note that some code below is using assumption
   --  Simple_GZip_Header'Last > Footer_Array'Last, so do not make
   --  Simple_GZip_Header'Last <= Footer_Array'Last.

   Return_Code : constant array (Thin.Int range <>) of Return_Code_Enum :=
     (Z_OK            => OK,
      Z_STREAM_END    => STREAM_END,
      Z_NEED_DICT     => NEED_DICT,
      Z_ERRNO         => ERRNO,
      Z_STREAM_ERROR  => STREAM_ERROR,
      Z_DATA_ERROR    => DATA_ERROR,
      Z_MEM_ERROR     => MEM_ERROR,
      Z_BUF_ERROR     => BUF_ERROR,
      Z_VERSION_ERROR => VERSION_ERROR);

   Flate : constant array (Boolean) of Flate_Type :=
             (True  => (Step => Thin.Deflate'Access,
                        Done => Thin.DeflateEnd'Access),
              False => (Step => Thin.Inflate'Access,
                        Done => Thin.InflateEnd'Access));

   Flush_To_C : constant array (Flush_Mode) of Thin.Int :=
     (No_Flush   => Z_NO_FLUSH,
      Sync_Flush => Z_SYNC_FLUSH,
      Full_Flush => Z_FULL_FLUSH,
      Finish     => Z_FINISH);

   procedure Raise_Error (Stream : Z_Stream) with Inline;

   function CRC32
     (CRC  : Unsigned_32;
      Data : Stream_Element_Array) return Unsigned_32 with Inline;
   --  Compute CRC32, it could be necessary to make gzip format

   procedure CRC32
     (CRC : in out Unsigned_32; Data : Stream_Element_Array) with Inline;
   --  Compute CRC32, it could be necessary to make gzip format

   procedure Transcode_Auto
     (Coder    : in out Coder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode);
   --  Transcode routine without additional headers

   procedure Transcode_GZip
     (Coder    : in out Coder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode);
   --  Separate transcode routine to make gzip header

   procedure Cleanup (Coder : in out Coder_Type);
   --  Deallocate internal data from last processig if was and prepare for
   --  another compression/decompresion processing initialization.

   -----------
   -- CRC32 --
   -----------

   function CRC32
     (CRC : Unsigned_32; Data : Stream_Element_Array) return Unsigned_32 is
   begin
      return Unsigned_32 (crc32 (ULong (CRC), Data'Address, Data'Length));
   end CRC32;

   procedure CRC32 (CRC : in out Unsigned_32; Data : Stream_Element_Array) is
   begin
      CRC := CRC32 (CRC, Data);
   end CRC32;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Coder : in out Coder_Type) is
   begin
      if Coder.Stream = null then
         Coder.Stream := new Z_Stream;
      else
         Flate (Coder.Compression).Done (Coder.Stream);
      end if;

      Coder.Stream_End := False;
   end Cleanup;

   ------------------
   -- Deflate_Init --
   ------------------

   procedure Deflate_Init
     (Coder        : in out Coder_Type;
      Level        :        Compression_Level  := Default_Compression;
      Strategy     :        Strategy_Type      := Default_Strategy;
      Method       :        Compression_Method := Deflated;
      Window_Bits  :        Window_Bits_Type   := Default_Window_Bits;
      Memory_Level :        Memory_Level_Type  := Default_Memory_Level;
      Header       :        Header_Type        := Default)
   is
      Win_Bits : Thin.Int := Thin.Int (Window_Bits);
   begin
      Cleanup (Coder);

      --  We allow ZLib to make header only in case of default header type.
      --  Otherwise we would either do header by ourselfs, or do not do
      --  header at all.

      if Header = None or else Header = GZip then
         Win_Bits := -Win_Bits;
      end if;

      --  For the GZip CRC calculation and make headers

      if Header = GZip then
         Coder.CRC    := 0;
         Coder.Offset := Simple_GZip_Header'First;
      else
         Coder.Offset := Simple_GZip_Header'Last + 1;
      end if;

      Coder.Compression := True;
      Coder.Header      := Header;

      if Thin.Deflate_Init
           (Coder.Stream,
            level      => Thin.Int (Level),
            method     => Thin.Int (Method),
            windowBits => Win_Bits,
            memLevel   => Thin.Int (Memory_Level),
            strategy   => Thin.Int (Strategy)) /= Thin.Z_OK
      then
         Raise_Error (Coder.Stream.all);
      end if;
   end Deflate_Init;

   ------------------
   -- Inflate_Init --
   ------------------

   procedure Inflate_Init
     (Coder       : in out Coder_Type;
      Window_Bits :        Window_Bits_Type := Default_Window_Bits;
      Header      :       Header_Type      := Default)
   is
      Win_Bits : Thin.Int := Thin.Int (Window_Bits);

      procedure Check_Version;
      --  Check the latest header types compatibility

      -------------------
      -- Check_Version --
      -------------------

      procedure Check_Version is
      begin
         if Version <= "1.1.4" then
            raise ZLib_Error with
              "Inflate header type " & Header_Type'Image (Header)
              & " incompatible with ZLib version " & Version;
         end if;
      end Check_Version;

   begin
      Cleanup (Coder);

      case Header is
         when None =>
            Check_Version;

            --  Inflate data without headers determined
            --  by negative Win_Bits.

            Win_Bits := -Win_Bits;
         when GZip =>
            Check_Version;

            --  Inflate gzip data defined by flag 16

            Win_Bits := Win_Bits + 16;
         when Auto =>
            Check_Version;

            --  Inflate with automatic detection
            --  of gzip or native header defined by flag 32.

            Win_Bits := Win_Bits + 32;
         when Default => null;
      end case;

      Coder.Compression := False;
      Coder.Header      := Header;

      if Thin.Inflate_Init (Coder.Stream, Win_Bits) /= Thin.Z_OK  then
         Raise_Error (Coder.Stream.all);
      end if;
   end Inflate_Init;

   ---------------
   -- Translate --
   ---------------

   overriding procedure Transcode
     (Coder     : in out Coder_Type;
      In_Data   :        Stream_Element_Array;
      In_Last   :    out Stream_Element_Offset;
      Out_Data  :    out Stream_Element_Array;
      Out_Last  :    out Stream_Element_Offset;
      Flush     :        Flush_Mode := No_Flush) is
   begin
      if Coder.Header = GZip and then Coder.Compression then
         Transcode_GZip
           (Coder    => Coder,
            In_Data  => In_Data,
            In_Last  => In_Last,
            Out_Data => Out_Data,
            Out_Last => Out_Last,
            Flush    => Flush);
      else
         Transcode_Auto
           (Coder    => Coder,
            In_Data  => In_Data,
            In_Last  => In_Last,
            Out_Data => Out_Data,
            Out_Last => Out_Last,
            Flush    => Flush);
      end if;
   end Transcode;

   --------------------
   -- Transcode_Auto --
   --------------------

   procedure Transcode_Auto
     (Coder    : in out Coder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode)
   is
      Code : Thin.Int;
   begin
      if Out_Data'Length = 0 and then In_Data'Length = 0 then
         raise Constraint_Error;
      end if;

      Set_Out (Coder.Stream.all, Out_Data'Address, Out_Data'Length);
      Set_In  (Coder.Stream.all, In_Data'Address, In_Data'Length);

      Code := Flate (Coder.Compression).Step
                (Coder.Stream, Flush_To_C (Flush));

      if Code = Thin.Z_STREAM_END then
         Coder.Stream_End := True;

      elsif Code /= Z_OK
        and then
          (Code /= Z_BUF_ERROR
           or else Flush = No_Flush
           or else In_Data'Length > 0
           or else Total_In (Coder.Stream.all) = 0
           or else (In_Data'Length = 0
                    and then Flush = Finish
                    and then Avail_Out (Coder.Stream.all) = Out_Data'Length))
      then
         raise ZLib_Error with Return_Code_Enum'Image (Return_Code (Code)) &
           ": " & Last_Error_Message (Coder.Stream.all);
      end if;

      In_Last  := In_Data'Last
        - Stream_Element_Offset (Avail_In (Coder.Stream.all));
      Out_Last := Out_Data'Last
        - Stream_Element_Offset (Avail_Out (Coder.Stream.all));
   end Transcode_Auto;

   --------------------
   -- Transcode_GZip --
   --------------------

   procedure Transcode_GZip
     (Coder    : in out Coder_Type;
      In_Data  :        Stream_Element_Array;
      In_Last  :    out Stream_Element_Offset;
      Out_Data :    out Stream_Element_Array;
      Out_Last :    out Stream_Element_Offset;
      Flush    :        Flush_Mode)
   is
      Out_First : Stream_Element_Offset;

      procedure Add_Data (Data : Stream_Element_Array);
      --  Add data to stream from the Coder.Offset till necessary,
      --  used for add gzip headr/footer.

      procedure Put_32
        (Item : in out Stream_Element_Array; Data : Unsigned_32) with Inline;

      --------------
      -- Add_Data --
      --------------

      procedure Add_Data (Data : Stream_Element_Array) is
         Data_First : Stream_Element_Offset renames Coder.Offset;
         Data_Last  : Stream_Element_Offset;
         Data_Len   : Stream_Element_Offset; --  -1
         Out_Len    : Stream_Element_Offset; --  -1
      begin
         Out_First := Out_Last + 1;

         if Data_First > Data'Last then
            return;
         end if;

         Data_Len  := Data'Last     - Data_First;
         Out_Len   := Out_Data'Last - Out_First;

         if Data_Len <= Out_Len then
            Out_Last  := Out_First  + Data_Len;
            Data_Last := Data'Last;
         else
            Out_Last  := Out_Data'Last;
            Data_Last := Data_First + Out_Len;
         end if;

         Out_Data (Out_First .. Out_Last) := Data (Data_First .. Data_Last);

         Data_First := Data_Last + 1;
         Out_First  := Out_Last + 1;
      end Add_Data;

      ------------
      -- Put_32 --
      ------------

      procedure Put_32
        (Item : in out Stream_Element_Array; Data : Unsigned_32)
      is
         D : Unsigned_32 := Data;
      begin
         for J in Item'First .. Item'First + 3 loop
            Item (J) := Stream_Element (D and 16#FF#);
            D := Shift_Right (D, 8);
         end loop;
      end Put_32;

   begin
      Out_Last := Out_Data'First - 1;

      if not Coder.Stream_End then
         Add_Data (Simple_GZip_Header);

         Transcode_Auto
           (Coder    => Coder,
            In_Data  => In_Data,
            In_Last  => In_Last,
            Out_Data => Out_Data (Out_First .. Out_Data'Last),
            Out_Last => Out_Last,
            Flush    => Flush);

         CRC32 (Coder.CRC, In_Data (In_Data'First .. In_Last));
      end if;

      if Coder.Stream_End and then Out_Last <= Out_Data'Last then
         --  This detection method would work only when
         --  Simple_GZip_Header'Last > Footer_Array'Last

         if Coder.Offset = Simple_GZip_Header'Last + 1 then
            Coder.Offset := Footer_Array'First;
         end if;

         declare
            Footer : Footer_Array;
         begin
            Put_32 (Footer, Coder.CRC);
            Put_32 (Footer (Footer'First + 4 .. Footer'Last),
                    Unsigned_32'Mod (Total_In (Coder)));
            Add_Data (Footer);
         end;
      end if;
   end Transcode_GZip;

   -------------
   -- Version --
   -------------

   function Version return String is
   begin
      return Interfaces.C.Strings.Value (Thin.zlibVersion);
   end Version;

   --------------
   -- Total_In --
   --------------

   overriding function Total_In
     (Coder : Coder_Type) return Stream_Element_Count is
   begin
      return Stream_Element_Count (Thin.Total_In (Coder.Stream.all));
   end Total_In;

   ---------------
   -- Total_Out --
   ---------------

   overriding function Total_Out
     (Coder : Coder_Type) return Stream_Element_Count is
   begin
      return Stream_Element_Count (Thin.Total_Out (Coder.Stream.all));
   end Total_Out;

   --------------
   -- Finished --
   --------------

   function Finished (Coder : Coder_Type) return Boolean is
   begin
      return Coder.Stream_End;
   end Finished;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Coder : Coder_Type) return Boolean is
   begin
      return Coder.Stream /= null;
   end Is_Open;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Stream : Z_Stream) is
   begin
      raise ZLib_Error with Last_Error_Message (Stream);
   end Raise_Error;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Coder : in out Coder_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Z_Stream, Z_Stream_Access);
   begin
      if Coder.Stream /= null then
         Flate (Coder.Compression).Done (Coder.Stream);

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

end GNATCOLL.Coders.ZLib;
