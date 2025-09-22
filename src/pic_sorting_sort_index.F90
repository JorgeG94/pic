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
!! The generic subroutine, `SORT_INDEX`, is substantially a translation to
!! Fortran 2008 of the `"Rust" sort` sorting routines in
!! [`slice.rs`](https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs)
!! The `rust sort` implementation is distributed with the header:
!!
!!   Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
!!   file at the top-level directory of this distribution and at
!!   http://rust-lang.org/COPYRIGHT.
!!
!!   Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
!!   http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
!!   <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
!!   option. This file may not be copied, modified, or distributed
!!   except according to those terms.
!!
!! so the license for the original`slice.rs` code is compatible with the use
!! of modified versions of the code in the Fortran Standard Library under
!! the MIT license.

!submodule(pic_sorting) pic_sorting_sort_index
module pic_sorting_sort_index
   use pic_types, only: int32, int64, int_index, sp, dp, int_index_low
   use pic_optional_value, only: pic_optional

   implicit none
   public :: sort_index
!! The generic subroutine implementing the `SORT_INDEX` algorithm to
!! return an index array whose elements would sort the input array in the
!! desired direction. It is primarily intended to be used to sort a
!! derived type array based on the values of a component of the array.
!! Its use has the syntax:
!!
!!     call sort_index( array, index[, work, iwork, reverse ] )
!!
!! with the arguments:
!!
!! * array: the rank 1 array to be sorted. It is an `intent(inout)`
!!   argument of any of the types `integer(int8)`, `integer(int16)`,
!!   `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`,
!!   `real(real128)`, `character(*)`, `type(string_type)`,
!!   `type(bitset_64)`, `type(bitset_large)`. If both the
!!   type of `array` is real and at least one of the elements is a `NaN`,
!!   then the ordering of the `array` and `index` results is undefined.
!!   Otherwise it is defined to be as specified by reverse.
!!
!! * index: a rank 1 array of sorting indices. It is an `intent(out)`
!!   argument of the type `integer(int_index)`. Its size shall be the
!!   same as `array`. On return, if defined, its elements would
!!   sort the input `array` in the direction specified by `reverse`.
!!
!! * work (optional): shall be a rank 1 array of the same type as
!!   `array`, and shall have at least `size(array)/2` elements. It is an
!!   `intent(out)` argument to be used as "scratch" memory
!!   for internal record keeping. If associated with an array in static
!!   storage, its use can significantly reduce the stack memory requirements
!!   for the code. Its value on return is undefined.
!!
!! * iwork (optional): shall be a rank 1 integer array of kind `int_index`,
!!   and shall have at least `size(array)/2` elements. It is an
!!   `intent(out)` argument to be used as "scratch" memory
!!   for internal record keeping. If associated with an array in static
!!   storage, its use can significantly reduce the stack memory requirements
!!   for the code. Its value on return is undefined.
!!
!! * `reverse` (optional): shall be a scalar of type default logical. It
!!   is an `intent(in)` argument. If present with a value of `.true.` then
!!   `index` will sort `array` in order of non-increasing values in stable
!!   order. Otherwise index will sort `array` in order of non-decreasing
!!   values in stable order.
!!
!!#### Examples
!!
!! Sorting a related rank one array:
!!
!!```Fortran
!!    subroutine sort_related_data( a, b, work, index, iwork )
!!        ! Sort `b` in terms or its related array `a`
!!        integer, intent(inout)         :: a(:)
!!        integer(int32), intent(inout)  :: b(:) ! The same size as a
!!        integer(int32), intent(out)    :: work(:)
!!        integer(int_index), intent(out) :: index(:)
!!        integer(int_index), intent(out) :: iwork(:)
!!    ! Find the indices to sort a
!!        call sort_index(a, index(1:size(a)),&
!!            work(1:size(a)/2), iwork(1:size(a)/2))
!!    ! Sort b based on the sorting of a
!!        b(:) = b( index(1:size(a)) )
!!    end subroutine sort_related_data
!!```
!!
!! Sorting a rank 2 array based on the data in a column
!!
!!```Fortran
!!    subroutine sort_related_data( array, column, work, index, iwork )
!!    ! Sort `a_data` in terms or its component `a`
!!        integer, intent(inout)         :: a(:,:)
!!        integer(int32), intent(in)     :: column
!!        integer(int32), intent(out)    :: work(:)
!!        integer(int_index), intent(out) :: index(:)
!!        integer(int_index), intent(out) :: iwork(:)
!!        integer, allocatable           :: dummy(:)
!!        integer :: i
!!        allocate(dummy(size(a, dim=1)))
!!    ! Extract a component of `a_data`
!!        dummy(:) = a(:, column)
!!    ! Find the indices to sort the column
!!        call sort_index(dummy, index(1:size(dummy)),&
!!                        work(1:size(dummy)/2), iwork(1:size(dummy)/2))
!!    ! Sort a based on the sorting of its column
!!        do i=1, size(a, dim=2)
!!            a(:, i) = a(index(1:size(a, dim=1)), i)
!!        end do
!!    end subroutine sort_related_data
!!```
!!
!! Sorting an array of a derived type based on the dsta in one component
!!```fortran
!!    subroutine sort_a_data( a_data, a, work, index, iwork )
!!    ! Sort `a_data` in terms or its component `a`
!!        type(a_type), intent(inout)    :: a_data(:)
!!        integer(int32), intent(inout)  :: a(:)
!!        integer(int32), intent(out)    :: work(:)
!!        integer(int_index), intent(out) :: index(:)
!!        integer(int_index), intent(out) :: iwork(:)
!!    ! Extract a component of `a_data`
!!        a(1:size(a_data)) = a_data(:) % a
!!    ! Find the indices to sort the component
!!        call sort_index(a(1:size(a_data)), index(1:size(a_data)),&
!!                        work(1:size(a_data)/2), iwork(1:size(a_data)/2))
!!    ! Sort a_data based on the sorting of that component
!!        a_data(:) = a_data( index(1:size(a_data)) )
!!    end subroutine sort_a_data
!!```

   private
   integer, parameter :: &
      ! The maximum number of entries in a run stack, good for an array of
      ! 2**64 elements see
      ! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
      max_merge_stack = int(ceiling(log(2._dp**64)/ &
                                    log(1.6180339887_dp)))

   type run_type
!! Used to pass state around in a stack among helper functions for the
!! `ORD_SORT` and `SORT_INDEX` algorithms
      integer(int_index) :: base = 0
      integer(int_index) :: len = 0
   end type run_type

   interface sort_index
!! The generic subroutine interface implementing the `SORT_INDEX` algorithm,
!! based on the `"Rust" sort` algorithm found in `slice.rs`
!! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
!! but modified to return an array of indices that would provide a stable
!! sort of the rank one `ARRAY` input.
!!
!! The indices by default correspond to a
!! non-decreasing sort, but if the optional argument `REVERSE` is present
!! with a value of `.TRUE.` the indices correspond to a non-increasing sort.

      module subroutine int32_sort_index_default(array, index, work, iwork, &
                                                 reverse)
!! `int32_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int32)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         integer(int32), intent(inout)                     :: array(0:)
         integer(int_index), intent(out)                      :: index(0:)
         integer(int32), intent(out), optional             :: work(0:)
         integer(int_index), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine int32_sort_index_default

      module subroutine int64_sort_index_default(array, index, work, iwork, &
                                                 reverse)
!! `int64_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         integer(int64), intent(inout)                     :: array(0:)
         integer(int_index), intent(out)                      :: index(0:)
         integer(int64), intent(out), optional             :: work(0:)
         integer(int_index), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine int64_sort_index_default

      module subroutine sp_sort_index_default(array, index, work, iwork, &
                                              reverse)
!! `sp_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(sp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         real(sp), intent(inout)                     :: array(0:)
         integer(int_index), intent(out)                      :: index(0:)
         real(sp), intent(out), optional             :: work(0:)
         integer(int_index), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine sp_sort_index_default

      module subroutine dp_sort_index_default(array, index, work, iwork, &
                                              reverse)
!! `dp_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(dp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         real(dp), intent(inout)                     :: array(0:)
         integer(int_index), intent(out)                      :: index(0:)
         real(dp), intent(out), optional             :: work(0:)
         integer(int_index), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine dp_sort_index_default

      module subroutine char_sort_index_default(array, index, work, iwork, &
                                                reverse)
!! `char_sort_index_default( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `character(len=*)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         character(len=*), intent(inout)                     :: array(0:)
         integer(int_index), intent(out)                      :: index(0:)
         character(len=len(array)), intent(out), optional             :: work(0:)
         integer(int_index), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine char_sort_index_default

      module subroutine int32_sort_index_low(array, index, work, iwork, &
                                             reverse)
!! `int32_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int32)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         integer(int32), intent(inout)                     :: array(0:)
         integer(int_index_low), intent(out)                      :: index(0:)
         integer(int32), intent(out), optional             :: work(0:)
         integer(int_index_low), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine int32_sort_index_low

      module subroutine int64_sort_index_low(array, index, work, iwork, &
                                             reverse)
!! `int64_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `integer(int64)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         integer(int64), intent(inout)                     :: array(0:)
         integer(int_index_low), intent(out)                      :: index(0:)
         integer(int64), intent(out), optional             :: work(0:)
         integer(int_index_low), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine int64_sort_index_low

      module subroutine sp_sort_index_low(array, index, work, iwork, &
                                          reverse)
!! `sp_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(sp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         real(sp), intent(inout)                     :: array(0:)
         integer(int_index_low), intent(out)                      :: index(0:)
         real(sp), intent(out), optional             :: work(0:)
         integer(int_index_low), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine sp_sort_index_low

      module subroutine dp_sort_index_low(array, index, work, iwork, &
                                          reverse)
!! `dp_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `real(dp)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         real(dp), intent(inout)                     :: array(0:)
         integer(int_index_low), intent(out)                      :: index(0:)
         real(dp), intent(out), optional             :: work(0:)
         integer(int_index_low), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine dp_sort_index_low

      module subroutine char_sort_index_low(array, index, work, iwork, &
                                            reverse)
!! `char_sort_index_low( array, index[, work, iwork, reverse] )` sorts
!! an input `ARRAY` of type `character(len=*)`
!! using a hybrid sort based on the `"Rust" sort` algorithm found in `slice.rs`
!! and returns the sorted `ARRAY` and an array `INDEX` of indices in the
!! order that would sort the input `ARRAY` in the desired direction.
         implicit none
         character(len=*), intent(inout)                     :: array(0:)
         integer(int_index_low), intent(out)                      :: index(0:)
         character(len=len(array)), intent(out), optional             :: work(0:)
         integer(int_index_low), intent(out), optional            :: iwork(0:)
         logical, intent(in), optional             :: reverse
      end subroutine char_sort_index_low

   end interface sort_index

contains

   module subroutine int32_sort_index_default(array, index, work, iwork, reverse)
! A modification of `int32_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      integer(int32), intent(inout)         :: array(0:)
      integer(int_index), intent(out)           :: index(0:)
      integer(int32), intent(out), optional :: work(0:)
      integer(int_index), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      integer(int32), allocatable :: buf(:)
      integer(int_index), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      stat = 0
      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index) :: key_index
         integer(int32) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int32) :: tmp
         integer(int_index) :: i
         integer(int_index) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)
         integer(int32), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         integer(int32), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: index(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: itemp
         integer(int_index) :: lo, hi
         integer(int32) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine int32_sort_index_default

   module subroutine int64_sort_index_default(array, index, work, iwork, reverse)
! A modification of `int64_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      integer(int64), intent(inout)         :: array(0:)
      integer(int_index), intent(out)           :: index(0:)
      integer(int64), intent(out), optional :: work(0:)
      integer(int_index), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      integer(int64), allocatable :: buf(:)
      integer(int_index), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index) :: key_index
         integer(int64) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int64) :: tmp
         integer(int_index) :: i
         integer(int_index) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)
         integer(int64), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         integer(int64), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: index(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: itemp
         integer(int_index) :: lo, hi
         integer(int64) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine int64_sort_index_default

   module subroutine sp_sort_index_default(array, index, work, iwork, reverse)
! A modification of `sp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      real(sp), intent(inout)         :: array(0:)
      integer(int_index), intent(out)           :: index(0:)
      real(sp), intent(out), optional :: work(0:)
      integer(int_index), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      real(sp), allocatable :: buf(:)
      integer(int_index), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index) :: key_index
         real(sp) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         real(sp) :: tmp
         integer(int_index) :: i
         integer(int_index) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)
         real(sp), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         real(sp), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: index(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: itemp
         integer(int_index) :: lo, hi
         real(sp) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine sp_sort_index_default

   module subroutine dp_sort_index_default(array, index, work, iwork, reverse)
! A modification of `dp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      real(dp), intent(inout)         :: array(0:)
      integer(int_index), intent(out)           :: index(0:)
      real(dp), intent(out), optional :: work(0:)
      integer(int_index), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      real(dp), allocatable :: buf(:)
      integer(int_index), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index) :: key_index
         real(dp) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         real(dp) :: tmp
         integer(int_index) :: i
         integer(int_index) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)
         real(dp), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         real(dp), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: index(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: itemp
         integer(int_index) :: lo, hi
         real(dp) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine dp_sort_index_default

   module subroutine char_sort_index_default(array, index, work, iwork, reverse)
! A modification of `char_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      character(len=*), intent(inout)         :: array(0:)
      integer(int_index), intent(out)           :: index(0:)
      character(len=len(array)), intent(out), optional :: work(0:)
      integer(int_index), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      character(len=:), allocatable :: buf(:)
      integer(int_index), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      stat = 0_int_index
      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (character(len=len(array)) :: buf(0:array_size/2 - 1), &
                   stat=stat)
#ifdef __NVCOMPILER_LLVM__

#else
         if (stat /= 0) then
            error stop "Allocation of array failed"
         end if
#endif

         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index) :: key_index
         character(len=len(array)) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         character(len=len(array)) :: tmp
         integer(int_index) :: i
         integer(int_index) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)
         character(len=len(array)), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         character(len=len(array)), intent(inout) :: buf(0:)
         integer(int_index), intent(inout) :: index(0:)
         integer(int_index), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(inout) :: index(0:)

         integer(int_index) :: itemp
         integer(int_index) :: lo, hi
         character(len=len(array)) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine char_sort_index_default

   module subroutine int32_sort_index_low(array, index, work, iwork, reverse)
! A modification of `int32_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      integer(int32), intent(inout)         :: array(0:)
      integer(int_index_low), intent(out)           :: index(0:)
      integer(int32), intent(out), optional :: work(0:)
      integer(int_index_low), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      integer(int32), allocatable :: buf(:)
      integer(int_index_low), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index_low)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         integer(int32), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index_low) :: key_index
         integer(int32) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         integer(int32), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int32) :: tmp
         integer(int_index) :: i
         integer(int_index_low) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         integer(int32), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         integer(int32), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         integer(int32), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         integer(int32), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         integer(int32), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index_low) :: itemp
         integer(int_index) :: lo, hi
         integer(int32) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine int32_sort_index_low

   module subroutine int64_sort_index_low(array, index, work, iwork, reverse)
! A modification of `int64_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      integer(int64), intent(inout)         :: array(0:)
      integer(int_index_low), intent(out)           :: index(0:)
      integer(int64), intent(out), optional :: work(0:)
      integer(int_index_low), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      integer(int64), allocatable :: buf(:)
      integer(int_index_low), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index_low)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         integer(int64), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index_low) :: key_index
         integer(int64) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         integer(int64), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int64) :: tmp
         integer(int_index) :: i
         integer(int_index_low) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         integer(int64), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         integer(int64), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         integer(int64), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         integer(int64), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         integer(int64), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index_low) :: itemp
         integer(int_index) :: lo, hi
         integer(int64) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine int64_sort_index_low

   module subroutine sp_sort_index_low(array, index, work, iwork, reverse)
! A modification of `sp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      real(sp), intent(inout)         :: array(0:)
      integer(int_index_low), intent(out)           :: index(0:)
      real(sp), intent(out), optional :: work(0:)
      integer(int_index_low), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      real(sp), allocatable :: buf(:)
      integer(int_index_low), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index_low)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         real(sp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index_low) :: key_index
         real(sp) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         real(sp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         real(sp) :: tmp
         integer(int_index) :: i
         integer(int_index_low) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         real(sp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         real(sp), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         real(sp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         real(sp), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         real(sp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index_low) :: itemp
         integer(int_index) :: lo, hi
         real(sp) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine sp_sort_index_low

   module subroutine dp_sort_index_low(array, index, work, iwork, reverse)
! A modification of `dp_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      real(dp), intent(inout)         :: array(0:)
      integer(int_index_low), intent(out)           :: index(0:)
      real(dp), intent(out), optional :: work(0:)
      integer(int_index_low), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      real(dp), allocatable :: buf(:)
      integer(int_index_low), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index_low)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (buf(0:array_size/2 - 1), stat=stat)
         if (stat /= 0) error stop "Allocation of array buffer failed."
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         real(dp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index_low) :: key_index
         real(dp) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         real(dp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         real(dp) :: tmp
         integer(int_index) :: i
         integer(int_index_low) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         real(dp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         real(dp), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         real(dp), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         real(dp), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         real(dp), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index_low) :: itemp
         integer(int_index) :: lo, hi
         real(dp) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine dp_sort_index_low

   module subroutine char_sort_index_low(array, index, work, iwork, reverse)
! A modification of `char_ord_sort` to return an array of indices that
! would perform a stable sort of the `ARRAY` as input, and also sort `ARRAY`
! as desired. The indices by default
! correspond to a non-decreasing sort, but if the optional argument
! `REVERSE` is present with a value of `.TRUE.` the indices correspond to
! a non-increasing sort. The logic of the determination of indexing largely
! follows the `"Rust" sort` found in `slice.rs`:
! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159
! The Rust version in turn is a simplification of the Timsort algorithm
! described in
! https://svn.python.org/projects/python/trunk/Objects/listsort.txt, as
! it drops both the use of 'galloping' to identify bounds of regions to be
! sorted and the estimation of the optimal `run size`. However it remains
! a hybrid sorting algorithm combining an iterative Merge sort controlled
! by a stack of `RUNS` identified by regions of uniformly decreasing or
! non-decreasing sequences that may be expanded to a minimum run size and
! initially processed by an insertion sort.
!
! Note the Fortran implementation simplifies the logic as it only has to
! deal with Fortran arrays of intrinsic types and not the full generality
! of Rust's arrays and lists for arbitrary types. It also adds the
! estimation of the optimal `run size` as suggested in Tim Peters'
! original `listsort.txt`, and the optional `work` and `iwork` arrays to be
! used as scratch memory.

      character(len=*), intent(inout)         :: array(0:)
      integer(int_index_low), intent(out)           :: index(0:)
      character(len=len(array)), intent(out), optional :: work(0:)
      integer(int_index_low), intent(out), optional :: iwork(0:)
      logical, intent(in), optional :: reverse

      character(len=:), allocatable :: buf(:)
      integer(int_index_low), allocatable :: ibuf(:)
      integer(int_index) :: array_size, i, stat

      stat = 0_int_index
      array_size = size(array, kind=int_index)

      if (array_size > huge(index)) then
         error stop "Too many entries for the kind of index."
      end if

      if (array_size > size(index, kind=int_index)) then
         error stop "Too many entries for the size of index."
      end if

      do i = 0, array_size - 1
         index(i) = int(i + 1, kind=int_index_low)
      end do

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

! If necessary allocate buffers to serve as scratch memory.
      if (present(work)) then
         if (size(work, kind=int_index) < array_size/2) then
            error stop "work array is too small."
         end if
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, work, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, work, ibuf)
         end if
      else
         allocate (character(len=len(array)) :: buf(0:array_size/2 - 1), &
                   stat=stat)
#ifdef __NVCOMPILER_LLVM__

#else
         if (stat /= 0) then
            error stop "Allocation of array failed"
         end if
#endif
         if (present(iwork)) then
            if (size(iwork, kind=int_index) < array_size/2) then
               error stop "iwork array is too small."
            end if
            call merge_sort(array, index, buf, iwork)
         else
            allocate (ibuf(0:array_size/2 - 1), stat=stat)
            if (stat /= 0) error stop "Allocation of index buffer failed."
            call merge_sort(array, index, buf, ibuf)
         end if
      end if

      if (pic_optional(reverse, .false.)) then
         call reverse_segment(array, index)
      end if

   contains

      pure function calc_min_run(n) result(min_run)
!! Returns the minimum length of a run from 32-63 so that N/MIN_RUN is
!! less than or equal to a power of two. See
!! https://svn.python.org/projects/python/trunk/Objects/listsort.txt
         integer(int_index)             :: min_run
         integer(int_index), intent(in) :: n

         integer(int_index) :: num, r

         num = n
         r = 0_int_index

         do while (num >= 64)
            r = ior(r, iand(num, 1_int_index))
            num = ishft(num, -1_int_index)
         end do
         min_run = num + r

      end function calc_min_run

      pure subroutine insertion_sort(array, index)
! Sorts `ARRAY` using an insertion sort, while maintaining consistency in
! location of the indices in `INDEX` to the elements of `ARRAY`.
         character(len=*), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index) :: i, j
         integer(int_index_low) :: key_index
         character(len=len(array)) :: key

         do j = 1, size(array, kind=int_index) - 1
            key = array(j)
            key_index = index(j)
            i = j - 1
            do while (i >= 0)
               if (array(i) <= key) exit
               array(i + 1) = array(i)
               index(i + 1) = index(i)
               i = i - 1
            end do
            array(i + 1) = key
            index(i + 1) = key_index
         end do

      end subroutine insertion_sort

      pure function collapse(runs) result(r)
! Examine the stack of runs waiting to be merged, identifying adjacent runs
! to be merged until the stack invariants are restablished:
!
! 1. len(-3) > len(-2) + len(-1)
! 2. len(-2) > len(-1)
         integer(int_index) :: r
         type(run_type), intent(in), target :: runs(0:)

         integer(int_index) :: n
         logical :: test

         n = size(runs, kind=int_index)
         test = .false.
         if (n >= 2) then
            if (runs(n - 1)%base == 0 .or. &
                runs(n - 2)%len <= runs(n - 1)%len) then
               test = .true.
            else if (n >= 3) then  ! X exists
               if (runs(n - 3)%len <= &
                   runs(n - 2)%len + runs(n - 1)%len) then
                  test = .true.
!               |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
               else if (n >= 4) then
                  if (runs(n - 4)%len <= &
                      runs(n - 3)%len + runs(n - 2)%len) then
                     test = .true.
!               |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                  end if
               end if
            end if
         end if
         if (test) then
! By default merge Y & Z, rho2 or rho3
            if (n >= 3) then
               if (runs(n - 3)%len < runs(n - 1)%len) then
                  r = n - 3
! |X| < |Z| => merge X & Y, rho1
                  return
               end if
            end if
            r = n - 2
! |Y| <= |Z| => merge Y & Z, rho4
            return
         else
            r = -1
         end if

      end function collapse

      pure subroutine insert_head(array, index)
! Inserts `array(0)` into the pre-sorted sequence `array(1:)` so that the
! whole `array(0:)` becomes sorted, copying the first element into
! a temporary variable, iterating until the right place for it is found.
! copying every traversed element into the slot preceding it, and finally,
! copying data from the temporary variable into the resulting hole.
! Consistency of the indices in `index` with the elements of `array`
! are maintained.

         character(len=*), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         character(len=len(array)) :: tmp
         integer(int_index) :: i
         integer(int_index_low) :: tmp_index

         tmp = array(0)
         tmp_index = index(0)
         find_hole: do i = 1, size(array, kind=int_index) - 1
            if (array(i) >= tmp) exit find_hole
            array(i - 1) = array(i)
            index(i - 1) = index(i)
         end do find_hole
         array(i - 1) = tmp
         index(i - 1) = tmp_index

      end subroutine insert_head

      subroutine merge_sort(array, index, buf, ibuf)
! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
! which is described in detail at
! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
!
! The algorithm identifies strictly descending and non-descending
! subsequences, which are called natural runs. Where these runs are less
! than a minimum run size they are padded by adding additional samples
! using an insertion sort. The merge process is driven by a stack of
! pending unmerged runs. Each newly found run is pushed onto the stack,
! and then pairs of adjacentd runs are merged until these two invariants
! are satisfied:
!
! 1. for every `i` in `1..size(runs)-1`: `runs(i - 1)%len > runs(i)%len`
! 2. for every `i` in `2..size(runs)-1`: `runs(i - 2)%len >
!    runs(i - 1)%len + runs(i)%len`
!
! The invariants ensure that the total running time is `O(n log n)`
! worst-case. Consistency of the indices in `index` with the elements of
! `array` are maintained.

         character(len=*), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         character(len=len(array)), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_size, finish, min_run, r, r_count, &
                               start
         type(run_type) :: runs(0:max_merge_stack - 1), left, right

         array_size = size(array, kind=int_index)

! Very short runs are extended using insertion sort to span at least this
! many elements. Slices of up to this length are sorted using insertion sort.
         min_run = calc_min_run(array_size)

         if (array_size <= min_run) then
            if (array_size >= 2) call insertion_sort(array, index)
            return
         end if

! Following Rust sort, natural runs in `array` are identified by traversing
! it backwards. By traversing it backward, merges more often go in the
! opposite direction (forwards). According to developers of Rust sort,
! merging forwards is slightly faster than merging backwards. Therefore
! identifying runs by traversing backwards should improve performance.
         r_count = 0
         finish = array_size - 1
         do while (finish >= 0)
! Find the next natural run, and reverse it if it's strictly descending.
            start = finish
            if (start > 0) then
               start = start - 1
               if (array(start + 1) < array(start)) then
                  Descending: do while (start > 0)
                     if (array(start) >= array(start - 1)) &
                        exit Descending
                     start = start - 1
                  end do Descending
                  call reverse_segment(array(start:finish), &
                                       index(start:finish))
               else
                  Ascending: do while (start > 0)
                     if (array(start) < array(start - 1)) exit Ascending
                     start = start - 1
                  end do Ascending
               end if
            end if

! If the run is too short insert some more elements using an insertion sort.
            Insert: do while (start > 0)
               if (finish - start >= min_run - 1) exit Insert
               start = start - 1
               call insert_head(array(start:finish), index(start:finish))
            end do Insert
            if (start == 0 .and. finish == array_size - 1) return

            runs(r_count) = run_type(base=start, &
                                     len=finish - start + 1)
            finish = start - 1
            r_count = r_count + 1

! Determine whether pairs of adjacent runs need to be merged to satisfy
! the invariants, and, if so, merge them.
            Merge_loop: do
               r = collapse(runs(0:r_count - 1))
               if (r < 0 .or. r_count <= 1) exit Merge_loop
               left = runs(r + 1)
               right = runs(r)
               call merge(array(left%base: &
                                right%base + right%len - 1), &
                          left%len, buf, &
                          index(left%base: &
                                right%base + right%len - 1), ibuf)

               runs(r) = run_type(base=left%base, &
                                  len=left%len + right%len)
               if (r == r_count - 3) runs(r + 1) = runs(r + 2)
               r_count = r_count - 1

            end do Merge_loop
         end do
         if (r_count /= 1) &
            error stop "MERGE_SORT completed without RUN COUNT == 1."

      end subroutine merge_sort

      pure subroutine merge(array, mid, buf, index, ibuf)
! Merges the two non-decreasing runs `ARRAY(0:MID-1)` and `ARRAY(MID:)`
! using `BUF` as temporary storage, and stores the merged runs into
! `ARRAY(0:)`. `MID` must be > 0, and < `SIZE(ARRAY)-1`. Buffer `BUF`
! must be long enough to hold the shorter of the two runs.
         character(len=*), intent(inout) :: array(0:)
         integer(int_index), intent(in)  :: mid
         character(len=len(array)), intent(inout) :: buf(0:)
         integer(int_index_low), intent(inout) :: index(0:)
         integer(int_index_low), intent(inout) :: ibuf(0:)

         integer(int_index) :: array_len, i, j, k

         array_len = size(array, kind=int_index)

! Merge first copies the shorter run into `buf`. Then, depending on which
! run was shorter, it traces the copied run and the longer run forwards
! (or backwards), comparing their next unprocessed elements and then
! copying the lesser (or greater) one into `array`.

         if (mid <= array_len - mid) then  ! The left run is shorter.
            buf(0:mid - 1) = array(0:mid - 1)
            ibuf(0:mid - 1) = index(0:mid - 1)
            i = 0
            j = mid
            merge_lower: do k = 0, array_len - 1
               if (buf(i) <= array(j)) then
                  array(k) = buf(i)
                  index(k) = ibuf(i)
                  i = i + 1
                  if (i >= mid) exit merge_lower
               else
                  array(k) = array(j)
                  index(k) = index(j)
                  j = j + 1
                  if (j >= array_len) then
                     array(k + 1:) = buf(i:mid - 1)
                     index(k + 1:) = ibuf(i:mid - 1)
                     exit merge_lower
                  end if
               end if
            end do merge_lower
         else  ! The right run is shorter
            buf(0:array_len - mid - 1) = array(mid:array_len - 1)
            ibuf(0:array_len - mid - 1) = index(mid:array_len - 1)
            i = mid - 1
            j = array_len - mid - 1
            merge_upper: do k = array_len - 1, 0, -1
               if (buf(j) >= array(i)) then
                  array(k) = buf(j)
                  index(k) = ibuf(j)
                  j = j - 1
                  if (j < 0) exit merge_upper
               else
                  array(k) = array(i)
                  index(k) = index(i)
                  i = i - 1
                  if (i < 0) then
                     array(0:k - 1) = buf(0:j)
                     index(0:k - 1) = ibuf(0:j)
                     exit merge_upper
                  end if
               end if
            end do merge_upper
         end if
      end subroutine merge

      pure subroutine reverse_segment(array, index)
! Reverse a segment of an array in place
         character(len=*), intent(inout) :: array(0:)
         integer(int_index_low), intent(inout) :: index(0:)

         integer(int_index_low) :: itemp
         integer(int_index) :: lo, hi
         character(len=len(array)) :: temp

         lo = 0
         hi = size(array, kind=int_index) - 1
         do while (lo < hi)
            temp = array(lo)
            array(lo) = array(hi)
            array(hi) = temp
            itemp = index(lo)
            index(lo) = index(hi)
            index(hi) = itemp
            lo = lo + 1
            hi = hi - 1
         end do

      end subroutine reverse_segment

   end subroutine char_sort_index_low

!end submodule pic_sorting_sort_index
end module pic_sorting_sort_index
