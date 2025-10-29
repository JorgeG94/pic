!! Licensing:
!!
!! This file is subject both to the Fortran Standard Library license, and
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
!! Two of the generic subroutines, `ORD_SORT` and `SORT_INDEX`, are
!! substantially translations to Fortran 2008 of the `"Rust" sort` sorting
!! routines in
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
!!
!! One of the generic subroutines, `SORT`, is substantially a
!! translation to Fortran 2008, of the `introsort` of David Musser.
!! David Musser has given permission to include a variant of `introsort`
!! in the Fortran Standard Library under the MIT license provided
!! we cite:
!!
!!   Musser, D.R., “Introspective Sorting and Selection Algorithms,”
!!   Software—Practice and Experience, Vol. 27(8), 983–993 (August 1997).
!!
!! as the official source of the algorithm.

! taken from the Fortran stdlib project since the stdlib does not build with the nvidia compilers
! and I need portability
module pic_sorting
!! This module implements overloaded sorting subroutines named `ORD_SORT`,
!! `SORT_INDEX`, and `SORT`, that each can be used to sort two kinds
!! of `INTEGER` arrays, two kinds of `REAL` arrays, `character(len=*)` arrays
!!
!! By default sorting is in order of
!! increasing value, but there is an option to sort in decreasing order.
!! All the subroutines have worst case run time performance of `O(N Ln(N))`,
!! but on largely sorted data `ORD_SORT` and `SORT_INDEX` can have a run time
!! performance of `O(N)`.
!!
!! `ORD_SORT` is a translation of the `"Rust" sort` sorting algorithm in
!! `slice.rs`:
!! https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs
!! which in turn is inspired by the `timsort` algorithm of Tim Peters,
!! http://svn.python.org/projects/python/trunk/Objects/listsort.txt.
!! `ORD_SORT` is a hybrid stable comparison algorithm combining `merge sort`,
!! and `insertion sort`. It is always at worst O(N Ln(N)) in sorting random
!! data, having a performance about 25% slower than `SORT` on such
!! data, but has much better performance than `SORT` on partially
!! sorted data, having O(N) performance on uniformly non-increasing or
!! non-decreasing data.
!!
!! `SORT_INDEX` is a modification of `ORD_SORT` so that in addition to
!! sorting the input array, it returns the indices that map to a
!! stable sort of the original array. These indices are
!! intended to be used to sort data that is correlated with the input
!! array, e.g., different arrays in a database, different columns of a
!! rank 2 array, different elements of a derived type. It is less
!! efficient than `ORD_SORT` at sorting a simple array.
!!
!! `SORT` uses the `INTROSORT` sorting algorithm of David Musser,
!! http://www.cs.rpi.edu/~musser/gp/introsort.ps. `introsort` is a hybrid
!! unstable comparison algorithm combining `quicksort`, `insertion sort`, and
!! `heap sort`. While this algorithm is always O(N Ln(N)) it is relatively
!! fast on randomly ordered data, but inconsistent in performance on partly
!! sorted data, sometimes having `merge sort` performance, sometimes having
!! better than `quicksort` performance. `UNORD_SOORT` is about 25%
!! more efficient than `ORD_SORT` at sorting purely random data, but af an
!! order of `Ln(N)` less efficient at sorting partially sorted data.

   use pic_types, only: &
      int32, &
      int64, &
      sp, &
      dp

   use pic_optional_value, only: pic_optional
   use pic_sorting_ord_sort, only: ord_sort
   use pic_sorting_radix_sort, only: radix_sort
   use pic_sorting_sort, only: sort
   use pic_sorting_sort_index, only: sort_index

   implicit none
   private
   public :: ord_sort, sort_index, sort, radix_sort

end module pic_sorting
