/****************************************************************************
 *                             G N A T C O L L                              *
 *                                                                          *
 *                        Copyright (C) 2019, AdaCore                       *
 *                                                                          *
 * This library is free software;  you can redistribute it and/or modify it *
 * under terms of the  GNU General Public License  as published by the Free *
 * Software  Foundation;  either version 3,  or (at your  option) any later *
 * version. This library is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

/* Provide helper functions to implement
   GNATCOLL.OMP.Generic_Constrained_Array_Sort via a parallel merge sort
   using C OpenMP directives.  */

const int insertion_threshold = 48;
const int serial_threshold = 1024;

typedef void (*merge_func)(void *, void *, long, long, long, long);
typedef void (*insertion_sort_func)(void *, long, long);
typedef struct {
  merge_func merge;
  insertion_sort_func insertion_sort;
} cb_struct;

static void mergesort_parallel_omp
  (void *a, void *temp, cb_struct *callbacks, long i, long j);
static void mergesort_serial
  (void * a, void *temp, cb_struct *callbacks, long i, long j);
void gnatcoll_omp_merge_sort
  (void *container, void *temp, cb_struct *callbacks, long i, long j);

/* Driver for OpenMP version of merge sort */
void
gnatcoll_omp_merge_sort
  (void *container, void *temp, cb_struct *callbacks, long i, long j)
{
  #pragma omp parallel
  #pragma omp single
  mergesort_parallel_omp (container, temp, callbacks, i, j);
}

/* Non OpenMP version of merge sort */
static void
mergesort_serial (void * a, void *temp, cb_struct *callbacks, long i, long j)
{
  if (j - i <= insertion_threshold) {
    (*callbacks->insertion_sort) (a, i, j);
    return;
  }

  long mid = (i + j) / 2;
  mergesort_serial (a, temp, callbacks, i, mid);
  mergesort_serial (a, temp, callbacks, mid + 1, j);
  (*callbacks->merge) (a, temp, i, mid, mid + 1, j);
}

/* Parallel part of merge sort using OpenMP */
static void
mergesort_parallel_omp
  (void *a, void *temp, cb_struct *callbacks, long i, long j)
{
  if (j - i <= serial_threshold) {
    mergesort_serial (a, temp, callbacks, i, j);
    return;
  }

  long mid = (i + j) / 2;

  #pragma omp task
  mergesort_parallel_omp (a, temp, callbacks, i, mid);
  mergesort_parallel_omp (a, temp, callbacks, mid + 1, j);

  #pragma omp taskwait
  //merge the two sorted sub-arrays
  (*callbacks->merge) (a, temp, i, mid, mid + 1, j);
}
