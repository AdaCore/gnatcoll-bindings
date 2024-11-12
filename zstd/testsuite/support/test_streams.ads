------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with Ada.Streams;               use Ada.Streams;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with GNATCOLL.Strings;          use GNATCOLL.Strings;

package Test_Streams is

   type Stream_Type is new Root_Stream_Type with private;
   --  Stream checking that all data taken from Read have to be the same
   --  accepted by Write.

   overriding
   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write (Stream : in out Stream_Type; Item : Stream_Element_Array);

   procedure Set_Limit
     (Stream : in out Stream_Type; Limit : Stream_Element_Count);
   --  Set the data limit to get from Read routine.

private

   type Stream_Type is new Root_Stream_Type with record
      Read_Started    : Boolean := False;
      Write_Started   : Boolean := False;
      Limit           : Stream_Element_Count := Stream_Element_Count'Last;
      Read_Generator  : Generator;
      Write_Generator : Generator;
      Init_State      : State;
      Read_Remain     : XString;
      Write_Remain    : XString;
   end record;

end Test_Streams;
