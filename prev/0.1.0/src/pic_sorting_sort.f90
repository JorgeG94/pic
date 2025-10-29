!! Licensing:
!!
!! This file is subjec† both to the Fortran Standard Library license, and
!! to additional licensing requirements as it contains translations of
!! other software.
!!
!! The Fortran Standard Library, including this file, is distributed under
!! the MIT license that should be included with the library's distribution.
!!
!!   Copyright (c) 2021 Fortran stdlib developers
!!
!!   Permission is hereby granted, free of charge, to any person obtaining a
!!   copy of this software and associated documentation files (the
!!   "Software"),  to deal in the Software without restriction, including
!!   without limitation the rights to use, copy, modify, merge, publish,
!!   distribute, sublicense, and/or sellcopies of the Software, and to permit
!!   persons to whom the Software is furnished to do so, subject to the
!!   following conditions:
!!
!!   The above copyright notice and this permission notice shall be included
!!   in all copies or substantial portions of the Software.
!!
!!   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
!!   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!!   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
!!   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
!!   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
!!   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
!!   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!!
!! The generic subroutine, `SORT`, is substantially a
!! translation to Fortran 2008, of the `introsort` of David Musser.
!! David Musser has given permission to include a variant of `introsort`
!! in the Fortran Standard Library under the MIT license provided
!! we cite:
!!
!!   Musser, D.R., “Introspective Sorting and Selection Algorithms,”
!!   Software—Practice and Experience, Vol. 27(8), 983–993 (August 1997).
!!
!! as the official source of the algorithm.

!submodule(pic_sorting) pic_sorting_sort
module pic_sorting_sort
   use pic_types, only: int32, int64, sp, dp, int_index
   use pic_optional_value, only: pic_optional
!! This submodule implements the overloaded sorting subroutine `SORT`
!! that can be used to sort four kinds of `INTEGER` arrays and three kinds
!! of `REAL` arrays. Sorting is in order of increasing value, with the worst
!! case run time performance of `O(N Ln(N))`.
!!
!! `SORT` uses the `INTROSORT` sorting algorithm of David Musser,
!! http://www.cs.rpi.edu/~musser/gp/introsort.ps. `introsort` is a hybrid
!! unstable comparison algorithm combining `quicksort`, `insertion sort`, and
!! `heap sort`. While this algorithm is always O(N Ln(N)) it is relatively
!! fast on randomly ordered data, but inconsistent in performance on partly
!! sorted data, sometimes having `merge sort` performance, sometimes having
!! better than `quicksort` performance.

   implicit none

   public :: sort
!! The generic subroutine implementing the `SORT` algorithm to return
!! an input array with its elements sorted in order of (non-)decreasing
!! value. Its use has the syntax:
!!
!!     call sort( array[, reverse] )
!!
!! with the arguments:
!!
!! * array: the rank 1 array to be sorted. It is an `intent(inout)`
!!   argument of any of the types `integer(int8)`, `integer(int16)`,
!!   `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`,
!!   `real(real128)`, `character(*)`, `type(string_type)`,
!!   `type(bitset_64)`, `type(bitset_large)`. If both the type
!!   of `array` is real and at least one of the elements is a `NaN`, then
!!   the ordering of the result is undefined. Otherwise it is defined to be the
!!   original elements in non-decreasing order.
!! * `reverse` (optional): shall be a scalar of type default logical. It
!!   is an `intent(in)` argument. If present with a value of `.true.` then
!!   `array` will be sorted in order of non-increasing values in unstable
!!   order. Otherwise index will sort `array` in order of non-decreasing
!!   values in unstable order.
!!
!!#### Example
!!
!!```fortran
!!    ...
!!    ! Read random data from a file
!!    call read_file( 'dummy_file', array )
!!    ! Sort the random data
!!    call sort( array )
!!    ! Process the sorted data
!!    call array_search( array, values )
!!    ...
!!```

   private

   interface sort
!! The generic subroutine interface implementing the `SORT` algorithm, based
!! on the `introsort` of David Musser.
!! ([Specification](../page/specs/stdlib_sorting.html#sort-sorts-an-input-array))

      pure module subroutine int32_sort(array, reverse)
!! `int32_sort( array[, reverse] )` sorts the input `ARRAY` of type `integer(int32)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
         implicit none
         integer(int32), intent(inout)         :: array(0:)
         logical, intent(in), optional :: reverse
      end subroutine int32_sort

      pure module subroutine int64_sort(array, reverse)
!! `int64_sort( array[, reverse] )` sorts the input `ARRAY` of type `integer(int64)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
         implicit none
         integer(int64), intent(inout)         :: array(0:)
         logical, intent(in), optional :: reverse
      end subroutine int64_sort

      pure module subroutine sp_sort(array, reverse)
!! `sp_sort( array[, reverse] )` sorts the input `ARRAY` of type `real(sp)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
         implicit none
         real(sp), intent(inout)         :: array(0:)
         logical, intent(in), optional :: reverse
      end subroutine sp_sort

      pure module subroutine dp_sort(array, reverse)
!! `dp_sort( array[, reverse] )` sorts the input `ARRAY` of type `real(dp)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
         implicit none
         real(dp), intent(inout)         :: array(0:)
         logical, intent(in), optional :: reverse
      end subroutine dp_sort

      pure module subroutine char_sort(array, reverse)
!! `char_sort( array[, reverse] )` sorts the input `ARRAY` of type `character(len=*)`
!! using a hybrid sort based on the `introsort` of David Musser.
!! The algorithm is of order O(N Ln(N)) for all inputs.
!! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
!! behavior is small for random data compared to other sorting algorithms.
         implicit none
         character(len=*), intent(inout)         :: array(0:)
         logical, intent(in), optional :: reverse
      end subroutine char_sort

   end interface sort

contains

   pure module subroutine int32_sort(array, reverse)
      integer(int32), intent(inout) :: array(0:)
      logical, intent(in), optional            :: reverse

      if (pic_optional(reverse, .false.)) then
         call int32_decrease_sort(array)
      else
         call int32_increase_sort(array)
      end if
   end subroutine int32_sort
   pure module subroutine int64_sort(array, reverse)
      integer(int64), intent(inout) :: array(0:)
      logical, intent(in), optional            :: reverse

      if (pic_optional(reverse, .false.)) then
         call int64_decrease_sort(array)
      else
         call int64_increase_sort(array)
      end if
   end subroutine int64_sort
   pure module subroutine sp_sort(array, reverse)
      real(sp), intent(inout) :: array(0:)
      logical, intent(in), optional            :: reverse

      if (pic_optional(reverse, .false.)) then
         call sp_decrease_sort(array)
      else
         call sp_increase_sort(array)
      end if
   end subroutine sp_sort
   pure module subroutine dp_sort(array, reverse)
      real(dp), intent(inout) :: array(0:)
      logical, intent(in), optional            :: reverse

      if (pic_optional(reverse, .false.)) then
         call dp_decrease_sort(array)
      else
         call dp_increase_sort(array)
      end if
   end subroutine dp_sort

   pure module subroutine char_sort(array, reverse)
      character(len=*), intent(inout) :: array(0:)
      logical, intent(in), optional            :: reverse

      if (pic_optional(reverse, .false.)) then
         call char_decrease_sort(array)
      else
         call char_increase_sort(array)
      end if
   end subroutine char_sort

   pure subroutine int32_increase_sort(array)
! `int32_increase_sort( array )` sorts the input `ARRAY` of type `integer(int32)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `int32_increase_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      integer(int32), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         integer(int32), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         integer(int32) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u > v) .neqv. (u > w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v < u) .neqv. (v < w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) <= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         integer(int32), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         integer(int32) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         integer(int32), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         integer(int32)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         integer(int32)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) > array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) > array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine int32_increase_sort

   pure subroutine int64_increase_sort(array)
! `int64_increase_sort( array )` sorts the input `ARRAY` of type `integer(int64)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `int64_increase_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      integer(int64), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         integer(int64), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         integer(int64) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u > v) .neqv. (u > w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v < u) .neqv. (v < w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) <= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         integer(int64), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         integer(int64) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         integer(int64), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         integer(int64)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         integer(int64)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) > array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) > array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine int64_increase_sort

   pure subroutine sp_increase_sort(array)
! `sp_increase_sort( array )` sorts the input `ARRAY` of type `real(sp)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `sp_increase_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      real(sp), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         real(sp), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         real(sp) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u > v) .neqv. (u > w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v < u) .neqv. (v < w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) <= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         real(sp), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         real(sp) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         real(sp), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         real(sp)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         real(sp)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) > array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) > array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine sp_increase_sort

   pure subroutine dp_increase_sort(array)
! `dp_increase_sort( array )` sorts the input `ARRAY` of type `real(dp)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `dp_increase_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      real(dp), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         real(dp), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         real(dp) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u > v) .neqv. (u > w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v < u) .neqv. (v < w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) <= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         real(dp), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         real(dp) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         real(dp), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         real(dp)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         real(dp)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) > array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) > array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine dp_increase_sort

   pure subroutine char_increase_sort(array)
! `char_increase_sort( array )` sorts the input `ARRAY` of type `character(len=*)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `char_increase_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      character(len=*), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         character(len=*), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         character(len=len(array)) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u > v) .neqv. (u > w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v < u) .neqv. (v < w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) <= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         character(len=*), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         character(len=len(array)) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         character(len=*), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         character(len=len(array))   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         character(len=len(array))   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) > array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) > array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine char_increase_sort

   pure subroutine int32_decrease_sort(array)
! `int32_decrease_sort( array )` sorts the input `ARRAY` of type `integer(int32)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `int32_decrease_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      integer(int32), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         integer(int32), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         integer(int32) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u < v) .neqv. (u < w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v > u) .neqv. (v > w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) >= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         integer(int32), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         integer(int32) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) >= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         integer(int32), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         integer(int32)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         integer(int32)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) < array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) < array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine int32_decrease_sort

   pure subroutine int64_decrease_sort(array)
! `int64_decrease_sort( array )` sorts the input `ARRAY` of type `integer(int64)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `int64_decrease_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      integer(int64), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         integer(int64), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         integer(int64) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u < v) .neqv. (u < w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v > u) .neqv. (v > w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) >= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         integer(int64), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         integer(int64) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) >= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         integer(int64), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         integer(int64)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         integer(int64)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) < array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) < array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine int64_decrease_sort

   pure subroutine sp_decrease_sort(array)
! `sp_decrease_sort( array )` sorts the input `ARRAY` of type `real(sp)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `sp_decrease_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      real(sp), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         real(sp), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         real(sp) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u < v) .neqv. (u < w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v > u) .neqv. (v > w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) >= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         real(sp), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         real(sp) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) >= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         real(sp), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         real(sp)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         real(sp)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) < array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) < array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine sp_decrease_sort

   pure subroutine dp_decrease_sort(array)
! `dp_decrease_sort( array )` sorts the input `ARRAY` of type `real(dp)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `dp_decrease_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      real(dp), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         real(dp), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         real(dp) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u < v) .neqv. (u < w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v > u) .neqv. (v > w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) >= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         real(dp), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         real(dp) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) >= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         real(dp), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         real(dp)   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         real(dp)   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) < array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) < array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine dp_decrease_sort

   pure subroutine char_decrease_sort(array)
! `char_decrease_sort( array )` sorts the input `ARRAY` of type `character(len=*)`
! using a hybrid sort based on the `introsort` of David Musser. As with
! `introsort`, `char_decrease_sort( array )` is an unstable hybrid comparison
! algorithm using `quicksort` for the main body of the sort tree,
! supplemented by `insertion sort` for the outer branches, but if
! `quicksort` is converging too slowly the algorithm resorts
! to `heapsort`. The algorithm is of order O(N Ln(N)) for all inputs.
! Because it relies on `quicksort`, the coefficient of the O(N Ln(N))
! behavior is typically small compared to other sorting algorithms.

      character(len=*), intent(inout) :: array(0:)

      integer(int32) :: depth_limit

      depth_limit = 2*int(floor(log(real(size(array, kind=int_index), &
                                         kind=dp))/log(2.0_dp)), &
                          kind=int32)
      call introsort(array, depth_limit)

   contains

      pure recursive subroutine introsort(array, depth_limit)
! It devolves to `insertionsort` if the remaining number of elements
! is fewer than or equal to `INSERT_SIZE`, `heapsort` if the completion
! of the `quicksort` is too slow as estimated from `DEPTH_LIMIT`,
! otherwise sorting is done by a `quicksort`.
         character(len=*), intent(inout) :: array(0:)
         integer(int32), intent(in)     :: depth_limit

         integer(int_index), parameter :: insert_size = 16_int_index
         integer(int_index)            :: index

         if (size(array, kind=int_index) <= insert_size) then
            ! May be best at the end of SORT processing the whole array
            ! See Musser, D.R., “Introspective Sorting and Selection
            ! Algorithms,” Software—Practice and Experience, Vol. 27(8),
            ! 983–993 (August 1997).

            call insertion_sort(array)
         else if (depth_limit == 0) then
            call heap_sort(array)
         else
            call partition(array, index)
            call introsort(array(0:index - 1), depth_limit - 1)
            call introsort(array(index + 1:), depth_limit - 1)
         end if

      end subroutine introsort

      pure subroutine partition(array, index)
! quicksort partition using median of three.
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(out) :: index

         character(len=len(array)) :: u, v, w, x, y
         integer(int_index) :: i, j

! Determine median of three and exchange it with the end.
         u = array(0)
         v = array(size(array, kind=int_index)/2 - 1)
         w = array(size(array, kind=int_index) - 1)
         if ((u < v) .neqv. (u < w)) then
            x = u
            y = array(0)
            array(0) = array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else if ((v > u) .neqv. (v > w)) then
            x = v
            y = array(size(array, kind=int_index)/2 - 1)
            array(size(array, kind=int_index)/2 - 1) = &
               array(size(array, kind=int_index) - 1)
            array(size(array, kind=int_index) - 1) = y
         else
            x = w
         end if
! Partition the array.
         i = -1_int_index
         do j = 0_int_index, size(array, kind=int_index) - 2
            if (array(j) >= x) then
               i = i + 1
               y = array(i)
               array(i) = array(j)
               array(j) = y
            end if
         end do
         y = array(i + 1)
         array(i + 1) = array(size(array, kind=int_index) - 1)
         array(size(array, kind=int_index) - 1) = y
         index = i + 1

      end subroutine partition

      pure subroutine insertion_sort(array)
! Bog standard insertion sort.
         character(len=*), intent(inout) :: array(0:)

         integer(int_index) :: i, j
         character(len=len(array)) :: key

         do j = 1_int_index, size(array, kind=int_index) - 1
            key = array(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) >= key) exit
               array(i + 1) = array(i)
               i = i - 1
            end do
            array(i + 1) = key
         end do

      end subroutine insertion_sort

      pure subroutine heap_sort(array)
! A bog standard heap sort
         character(len=*), intent(inout) :: array(0:)

         integer(int_index) :: i, heap_size
         character(len=len(array))   :: y

         heap_size = size(array, kind=int_index)
! Build the max heap
         do i = (heap_size - 2)/2_int_index, 0_int_index, -1_int_index
            call max_heapify(array, i, heap_size)
         end do
         do i = heap_size - 1, 1_int_index, -1_int_index
! Swap the first element with the current final element
            y = array(0)
            array(0) = array(i)
            array(i) = y
! Sift down using max_heapify
            call max_heapify(array, 0_int_index, i)
         end do

      end subroutine heap_sort

      pure recursive subroutine max_heapify(array, i, heap_size)
! Transform the array into a max heap
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: i, heap_size

         integer(int_index) :: l, r, largest
         character(len=len(array))   :: y

         largest = i
         l = 2_int_index*i + 1_int_index
         r = l + 1_int_index
         if (l < heap_size) then
            if (array(l) < array(largest)) largest = l
         end if
         if (r < heap_size) then
            if (array(r) < array(largest)) largest = r
         end if
         if (largest /= i) then
            y = array(i)
            array(i) = array(largest)
            array(largest) = y
            call max_heapify(array, largest, heap_size)
         end if

      end subroutine max_heapify

   end subroutine char_decrease_sort

end module pic_sorting_sort
!end submodule pic_sorting_sort
