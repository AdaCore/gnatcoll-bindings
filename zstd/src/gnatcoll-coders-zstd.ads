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

--  This package provides binding to ZSTD compression/decompression

with Ada.Finalization;
with GNATCOLL.ZSTD; use GNATCOLL.ZSTD;

private with GNATCOLL.ZSTD.Streams;

package GNATCOLL.Coders.ZSTD is

   type Coder_Type (Encoder : Boolean) is
     new Ada.Finalization.Limited_Controlled
     and Coder_Interface with private;

   function Version return String
   with Inline;
   --  Return string representation of the ZSTD version

   procedure Reset
     (Coder     : in out Coder_Type;
      Directive : ZSTD_Reset_Directive := ZSTD_Reset_Session_Only);
   --  Reset an open coder. There are 2 different things that can be
   --  reset, independently or jointly :
   --  - The session : will stop transcoding current frame, and make context
   --                  ready to start a new one.
   --                  Useful after an error, or to interrupt any ongoing
   --                  transcoding. Any internal data not yet flushed is
   --                  cancelled.
   --                  Resetting session never fails.
   --  - The parameters : changes all parameters back to "default".
   --                     Parameters can only be changed between 2 sessions
   --                     (i.e. no transcoding is currently ongoing).
   --                     Otherwise, ZSTD_Error is raised.
   --  - Both : similar to resetting the session, followed by resetting
   --           parameters.

   overriding
   function Is_Open (Coder : Coder_Type) return Boolean;
   --  Indicate if the coder is ready to process data

   overriding
   procedure Transcode
     (Coder    : in out Coder_Type;
      In_Data  : Stream_Element_Array;
      In_Last  : out Stream_Element_Offset;
      Out_Data : out Stream_Element_Array;
      Out_Last : out Stream_Element_Offset;
      Flush    : Flush_Mode := No_Flush);
   --  Transcode data from In_Data to Out_Data.
   --  In_Last is the index of last element from In_Data accepted by
   --  the Coder.
   --  Out_Last is the index of the last element written to Out_Data.
   --  To tell the Coder that incoming data is complete pass Finish as the
   --  Flush parameter and call Transcoder with empty In_Data until Stream_End
   --  routine indicates end of stream.

   procedure Set_Parameter
     (Coder : in out Coder_Type; Param : ZSTD_C_Parameter; Value : Integer);
   --  Set one compression parameter, selected by the
   --  provided ZSTD_C_Parameter.
   --  Raises an error if the provided value is invalid or if a compression
   --  operation is currently in progress.

   procedure Set_Parameter
     (Coder : in out Coder_Type; Param : ZSTD_D_Parameter; Value : Integer);
   --  Set one decompression parameter, selected by the
   --  provided ZSTD_D_Parameter.
   --  Raises an error if the provided value is invalid or if a decompression
   --  operation is currently in progress.

   overriding
   function Total_In (Coder : Coder_Type) return Stream_Element_Count;
   --  Return the total amount of input data sent into the coder

   overriding
   function Total_Out (Coder : Coder_Type) return Stream_Element_Count;
   --  Return the total amount of output data taken from the coder

   overriding
   function Finished (Coder : Coder_Type) return Boolean;
   --  Indicate that incoming data stream is complete and all internally
   --  processed data is out of coder.

   overriding
   procedure Close (Coder : in out Coder_Type);
   --  Free internal coder memory allocations. Note that coder is derived from
   --  Limited_Controlled and will free all memory automatically on
   --  finalization.

private

   pragma Assert (Stream_Element'Size = 8);
   pragma Assert (Stream_Element'Modulus = 2 ** 8);
   use GNATCOLL.ZSTD.Streams;

   type Coder_Type (Encoder : Boolean) is
     new Ada.Finalization.Limited_Controlled
     and Coder_Interface
   with record
      Total_In  : Stream_Element_Count := 0;
      Total_Out : Stream_Element_Count := 0;

      Finished : Boolean := True;

      case Encoder is
         when True =>
            CContext : ZSTD_CCtx := ZSTD_Create_CCtx;

         when False =>
            DContext : ZSTD_DStream := ZSTD_Create_DCtx;
      end case;
   end record;

   overriding
   procedure Finalize (Coder : in out Coder_Type);

end GNATCOLL.Coders.ZSTD;
