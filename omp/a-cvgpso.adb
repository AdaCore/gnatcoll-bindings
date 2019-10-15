------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with GNATCOLL.OMP.Generic_Array_Sort;

package body Ada.Containers.Vectors.Generic_Parallel_Sorting is

   --  Reuse Ada.Containers.Vectors.Generic_Sorting for Is_Sorted and Merge

   package Linear_Sorting is new Ada.Containers.Vectors.Generic_Sorting;

   ---------------
   -- Is_Sorted --
   ---------------

   function Is_Sorted (Container : Vector) return Boolean
     renames Linear_Sorting.Is_Sorted;

   -----------
   -- Merge --
   -----------

   procedure Merge (Target, Source : in out Vector)
     renames Linear_Sorting.Merge;

   ----------
   -- Sort --
   ----------

   procedure Sort (Container : in out Vector) is
      procedure Sort is
         new GNATCOLL.OMP.Generic_Array_Sort
          (Index_Type   => Index_Type,
           Element_Type => Element_Type,
           Array_Type   => Elements_Array,
           "<"          => "<");

   begin
      if Container.Last <= Index_Type'First then
         return;
      end if;

      --  The exception behavior for the vector container must match that
      --  for the list container, so we check for cursor tampering here
      --  (which will catch more things) instead of for element tampering
      --  (which will catch fewer things). It's true that the elements of
      --  this vector container could be safely moved around while (say) an
      --  iteration is taking place (iteration only increments the busy
      --  counter), and so technically all we would need here is a test for
      --  element tampering (indicated by the lock counter), that's simply
      --  an artifact of our array-based implementation. Logically Sort
      --  requires a check for cursor tampering.

      TC_Check (Container.TC);

      --  Per AI05-0022, the container implementation is required to detect
      --  element tampering by a generic actual subprogram.

      declare
         Lock : With_Lock (Container.TC'Unchecked_Access);
         pragma Unreferenced (Lock);
      begin
         Sort (Container.Elements.EA (Index_Type'First .. Container.Last));
      end;
   end Sort;

end Ada.Containers.Vectors.Generic_Parallel_Sorting;
