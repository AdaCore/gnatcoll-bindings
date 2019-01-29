with Ada.Streams;               use Ada.Streams;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with GNATCOLL.Strings;          use GNATCOLL.Strings;

package Test_Streams is

   type Stream_Type is new Root_Stream_Type with private;
   --  Stream checking that all data taken from Read have to be the same
   --  accepted by Write.

   overriding procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Stream_Type;
      Item   :        Stream_Element_Array);

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
