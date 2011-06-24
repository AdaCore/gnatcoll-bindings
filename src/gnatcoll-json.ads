-----------------------------------------------------------------------
--                        G N A T C O L L                            --
--                                                                   --
--                  Copyright (C) 2011, AdaCore                      --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package GNATCOLL.JSON is

   type JSON_Value_Type is
     (JSON_Null_Type,
      JSON_Boolean_Type,
      JSON_Int_Type,
      JSON_Float_Type,
      JSON_String_Type,
      JSON_Array_Type,
      JSON_Object_Type);

   Invalid_JSON_Stream : exception;

   subtype UTF8_String is String;
   type UTF8_String_Access is access all UTF8_String;

   subtype JSON_Elementary_Value_Type is JSON_Value_Type range
     JSON_Null_Type .. JSON_String_Type;
   subtype JSON_Container_Value_Type is JSON_Value_Type range
     JSON_Array_Type .. JSON_Object_Type;

   type JSON_Value is tagged private;
   type JSON_Array is private;

   Empty_Array : constant JSON_Array;

   --  Array handling
   function Length (Arr : JSON_Array) return Natural;
   function Get (Arr : JSON_Array; Index : Positive) return JSON_Value;
   procedure Append (Arr : in out JSON_Array; Val : JSON_Value);

   --  Read or write JSON values into strings

   function Read (Strm, Filename :  String) return JSON_Value;
   function Write (Item : JSON_Value) return String;

   --  Creation of JSON values

   function Create return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Null_Type);
   --  Creates a 'null' JSON value

   function Create (Val : Boolean) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Boolean_Type);
   --  Creates a boolean-typed JSON value

   function Create (Val : Integer) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Int_Type);
   --  Creates an integer-typed JSON value

   function Create (Val : Float) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Float_Type);
   --  Creates a float-typed JSON value

   function Create (Val : UTF8_String) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_String_Type);
   --  Creates a string-typed JSON value

   function Create (Val : JSON_Array) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Array_Type);
   --  Creates a JSON value from the JSON array

   function Create_Object return JSON_Value;
   pragma Postcondition (Kind (Create_Object'Result) = JSON_Object_Type);
   --  Creates an empty object. Values need to be added using the below
   --  Set_Field methods

   procedure Set_Field
     (Val        : in out JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Value);
   pragma Precondition (Kind (Val) = JSON_Object_Type);
   --  Adds or modifies the named field for the specified json object, using
   --  the Field value.

   procedure Set_Field
     (Val        : in out JSON_Value;
      Field_Name : UTF8_String;
      Field      : Boolean);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : in out JSON_Value;
      Field_Name : UTF8_String;
      Field      : Integer);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : in out JSON_Value;
      Field_Name : UTF8_String;
      Field      : Float);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : in out JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_String);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : in out JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Array);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   --  Utility functions used to translate a JSON value into an ordinary object

   function Kind (Val : JSON_Value) return JSON_Value_Type;

   function Get (Val : JSON_Value) return Boolean;
   pragma Precondition (Kind (Val) = JSON_Boolean_Type);

   function Get (Val : JSON_Value) return Integer;
   pragma Precondition (Kind (Val) = JSON_Int_Type);

   function Get (Val : JSON_Value) return Float;
   pragma Precondition (Kind (Val) = JSON_Float_Type);

   function Get (Val : JSON_Value) return UTF8_String;
   pragma Precondition (Kind (Val) = JSON_String_Type);

   function Get (Val : JSON_Value) return JSON_Array;
   pragma Precondition (Kind (Val) = JSON_Array_Type);

   function Has_Field (Val : JSON_Value; Field : UTF8_String) return Boolean;
   pragma Precondition (Kind (Val) = JSON_Object_Type);
   --  Tell whether the object val contains a field named Field

   function Get (Val : JSON_Value; Field : UTF8_String) return JSON_Value;
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return Boolean;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Boolean_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return Integer;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Int_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return Float;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Float_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return UTF8_String;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_String_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return JSON_Array;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Array_Type);

   procedure Map_JSON_Object
     (Val : JSON_Value;
      CB  : access procedure (Name : UTF8_String; Value : JSON_Value));
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   generic
      type Mapped is private;
   procedure Gen_Map_JSON_Object
     (Val         : JSON_Value;
      CB          : access procedure
        (User_Object : in out Mapped;
         Name        : UTF8_String;
         Value       : JSON_Value);
      User_Object : in out Mapped);

private

   type JSON_Array_Access is access all JSON_Array;
   type JSON_Object_Internal;
   type JSON_Object_Access is access all JSON_Object_Internal;

   type Counter is access Natural;

   type JSON_Value is new Ada.Finalization.Controlled with record
      Cnt        : Counter := null;
      Kind       : JSON_Value_Type := JSON_Null_Type;
      Bool_Value : Boolean;
      Int_Value  : Integer;
      Flt_Value  : Float;
      Str_Value  : UTF8_String_Access;
      Arr_Value  : JSON_Array_Access;
      Obj_Value  : JSON_Object_Access;
   end record;

   overriding procedure Initialize (Obj : in out JSON_Value);
   overriding procedure Adjust (Obj : in out JSON_Value);
   overriding procedure Finalize (Obj : in out JSON_Value);

   package Vect_Pkg is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => JSON_Value);
   type JSON_Array is record
      Vals : Vect_Pkg.Vector;
   end record;

   Empty_Array : constant JSON_Array :=
      (Vals => Vect_Pkg.Empty_Vector);

   package Names_Pkg is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => UTF8_String);
   type JSON_Object_Internal is record
      Names : Names_Pkg.Vector;
      Vals  : Vect_Pkg.Vector;
   end record;

   procedure Free is
     new Ada.Unchecked_Deallocation (UTF8_String, UTF8_String_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Array, JSON_Array_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Object_Internal, JSON_Object_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Natural, Counter);

end GNATCOLL.JSON;
