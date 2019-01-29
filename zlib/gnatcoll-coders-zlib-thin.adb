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

package body GNATCOLL.Coders.ZLib.Thin is

   ZLIB_VERSION  : constant Chars_Ptr := zlibVersion;

   --------------
   -- Avail_In --
   --------------

   function Avail_In (Strm : Z_Stream) return UInt is
   begin
      return Strm.Avail_In;
   end Avail_In;

   ---------------
   -- Avail_Out --
   ---------------

   function Avail_Out (Strm : Z_Stream) return UInt is
   begin
      return Strm.Avail_Out;
   end Avail_Out;

   ------------------
   -- Deflate_Init --
   ------------------

   function Deflate_Init
     (strm       : access Z_Stream;
      level      : Int;
      method     : Int;
      windowBits : Int;
      memLevel   : Int;
      strategy   : Int) return Int is
   begin
      return deflateInit2
               (strm,
                level,
                method,
                windowBits,
                memLevel,
                strategy,
                ZLIB_VERSION,
                stream_size => strm.all'Size / System.Storage_Unit);
   end Deflate_Init;

   ------------------
   -- Inflate_Init --
   ------------------

   function Inflate_Init
     (strm : access Z_Stream; windowBits : Int) return Int is
   begin
      return inflateInit2
               (strm, windowBits, ZLIB_VERSION,
                stream_size => strm.all'Size / System.Storage_Unit);
   end Inflate_Init;

   ------------------------
   -- Last_Error_Message --
   ------------------------

   function Last_Error_Message (Strm : Z_Stream) return String is
      use Interfaces.C.Strings;
   begin
      if Strm.msg = Null_Ptr then
         return "";
      else
         return Value (Strm.msg);
      end if;
   end Last_Error_Message;

   ------------
   -- Set_In --
   ------------

   procedure Set_In
     (Strm : in out Z_Stream; Buffer : Voidp; Size : UInt) is
   begin
      Strm.Next_In  := Buffer;
      Strm.Avail_In := Size;
   end Set_In;

   ------------------
   -- Set_Mem_Func --
   ------------------

   procedure Set_Mem_Func
     (Strm   : in out Z_Stream;
      Opaque :        Voidp;
      Alloc  :        alloc_func;
      Free   :        free_func) is
   begin
      Strm.opaque := Opaque;
      Strm.zalloc := Alloc;
      Strm.zfree  := Free;
   end Set_Mem_Func;

   -------------
   -- Set_Out --
   -------------

   procedure Set_Out
     (Strm : in out Z_Stream; Buffer : Voidp; Size : UInt) is
   begin
      Strm.Next_Out  := Buffer;
      Strm.Avail_Out := Size;
   end Set_Out;

   --------------
   -- Total_In --
   --------------

   function Total_In (Strm : Z_Stream) return ULong is
   begin
      return Strm.Total_In;
   end Total_In;

   ---------------
   -- Total_Out --
   ---------------

   function Total_Out (Strm : Z_Stream) return ULong is
   begin
      return Strm.Total_Out;
   end Total_Out;

end GNATCOLL.Coders.ZLib.Thin;
