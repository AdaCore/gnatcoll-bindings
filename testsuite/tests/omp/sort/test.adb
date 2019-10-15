with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with GNATCOLL.OMP.Generic_Array_Sort;
with Ada.Numerics.Discrete_Random;
with Test_Assert; use Test_Assert;

function Test return Integer is
   Timing : constant Boolean := False;

   type Index is range 1 .. 5_000_000;
   type My_Array is array (Index range <>) of Integer;
   procedure My_Sort is new GNATCOLL.OMP.Generic_Array_Sort
     (Index, Integer, My_Array);

   package Random is new Ada.Numerics.Discrete_Random (Integer);

   procedure Randomize (Container : in out My_Array) is
      Seed : Random.Generator;
   begin
      for J in Container'Range loop
         Container (J) := Random.Random (Seed);
      end loop;
   end Randomize;

   procedure Check_Array_Sorted (Container : My_Array) is
   begin
      for J in Container'First + 1 .. Container'Last loop
         if Container (J) < Container (J - 1) then
            Assert (False);
         end if;
      end loop;
   end Check_Array_Sorted;

   Arr   : access My_Array := new My_Array (Index);
   Start : Time;
   Time  : Duration;

begin
   Randomize (Arr.all);

   Start := Clock;
   My_Sort (Arr.all);
   Time := Clock - Start;
   Check_Array_Sorted (Arr.all);

   if Timing then
      Put_Line ("time: " & Time'Image);
   end if;

   return Report;
end Test;
