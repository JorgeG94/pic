module test_pic_sorting
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: sp, dp, int32, int64, default_int
   use pic_sorting, only: sort, sort_index, radix_sort, ord_sort
   use pic_array, only: is_sorted, ascending, DESCENDING
   implicit none
   private
   public :: collect_pic_sorting_tests

contains

   subroutine collect_pic_sorting_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  !new_unittest("test_index_sort_char_int32", test_index_sort_char_int32), &
                  !new_unittest("test_index_sort_char_int64", test_index_sort_char_int64), &
                  new_unittest("test_index_sort_int32_int32", test_index_sort_int32_int32), &
                  new_unittest("test_index_sort_int64_int64", test_index_sort_int64_int64), &
                  new_unittest("test_index_sort_int32_int64", test_index_sort_int32_int64), &
                  new_unittest("test_index_sort_int64_int32", test_index_sort_int64_int32), &
                  new_unittest("test_index_sort_sp_int32", test_index_sort_sp_int32), &
                  new_unittest("test_index_sort_sp_int64", test_index_sort_sp_int64), &
                  new_unittest("test_index_sort_dp_int32", test_index_sort_dp_int32), &
                  new_unittest("test_index_sort_dp_int64", test_index_sort_dp_int64), &
                  new_unittest("test_sort_char", test_sort_char), &
                  new_unittest("test_sort_int32", test_sort_int32), &
                  new_unittest("test_sort_int64", test_sort_int64), &
                  new_unittest("test_sort_sp", test_sort_sp), &
                  new_unittest("test_sort_dp", test_sort_dp), &
                  new_unittest("test_ord_sort_char", test_ord_sort_char), &
                  new_unittest("test_ord_sort_int32", test_ord_sort_int32), &
                  new_unittest("test_ord_sort_int64", test_ord_sort_int64), &
                  new_unittest("test_ord_sort_sp", test_ord_sort_sp), &
                  new_unittest("test_ord_sort_dp", test_ord_sort_dp), &
                  new_unittest("test_radix_sort_int32", test_radix_sort_int32), &
                  new_unittest("test_radix_sort_int64", test_radix_sort_int64), &
                  new_unittest("test_radix_sort_sp", test_radix_sort_sp), &
                  new_unittest("test_radix_sort_dp", test_radix_sort_dp) &
                  ]

   end subroutine collect_pic_sorting_tests

   subroutine test_index_sort_char_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      integer(int32), parameter :: expected_index(5) = [2_int32, 3_int32, 4_int32, 5_int32, 1_int32]
      integer(int32), parameter :: expected_reverse_index(5) = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      integer(int32) :: index(5)

      print *, "BEFORE SORT"
      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index)
      print *, "AFTER SORT"

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_reverse_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return

   end subroutine test_index_sort_char_int32

   subroutine test_index_sort_char_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      integer(int64) :: index(5)
      integer(int64), parameter :: expected_index(5) = [2_int64, 3_int64, 4_int64, 5_int64, 1_int64]
      integer(int64), parameter :: expected_reverse_index(5) = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return
      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_reverse_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_char_int64

   subroutine test_index_sort_int32_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: array(5)
      integer(int32) :: index(5)
      integer(int32), parameter :: expected_index(5) = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return
      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return

   end subroutine test_index_sort_int32_int32

   subroutine test_index_sort_int32_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: array(5)
      integer(int64) :: index(5)
      integer(int64), parameter :: expected_index(5) = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]

      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_int32_int64

   subroutine test_index_sort_int64_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: array(5)
      integer(int32) :: index(5)
      integer(int32), parameter :: expected_index(5) = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_int64_int32

   subroutine test_index_sort_int64_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: array(5)
      integer(int64) :: index(5)
      integer(int64), parameter :: expected_index(5) = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]

      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_int64_int64

   subroutine test_index_sort_sp_int32(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: array(5)
      integer(int32) :: index(5)
      integer(int32), parameter :: expected_index(5) = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]

      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_sp_int32

   subroutine test_index_sort_sp_int64(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: array(5)
      integer(int64) :: index(5)
      integer(int64), parameter :: expected_index(5) = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]

      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_sp_int64

   subroutine test_index_sort_dp_int32(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: array(5)
      integer(int32) :: index(5)
      integer(int32), parameter :: expected_index(5) = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]

      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_dp_int32

   subroutine test_index_sort_dp_int64(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: array(5)
      integer(int64) :: index(5)
      integer(int64), parameter :: expected_index(5) = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]

      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Index is not correct!")
      if (allocated(error)) return

      call sort_index(array, index, reverse=.true.)

      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call check(error, all(index == expected_index), .true., "Reverse index is not correct!")
      if (allocated(error)) return
   end subroutine test_index_sort_dp_int64

   subroutine test_sort_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]

      call sort(array)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call sort(array, .true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_sort_char

   subroutine test_sort_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: integer_array(5)

      integer_array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call sort(integer_array, .true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_sort_int32

   subroutine test_sort_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: integer_array(5)

      integer_array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call sort(integer_array, .true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_sort_int64

   subroutine test_sort_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: real_array(5)

      real_array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call sort(real_array, .true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_sort_sp

   subroutine test_sort_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: real_array(5)
      real_array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call sort(real_array, .true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_sort_dp

   subroutine test_ord_sort_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]

      call ord_sort(array)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(array, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_ord_sort_char

   subroutine test_ord_sort_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: integer_array(5)

      integer_array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call ord_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_ord_sort_int32

   subroutine test_ord_sort_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: integer_array(5)

      integer_array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call ord_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_int64

   subroutine test_ord_sort_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: real_array(5)

      real_array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call ord_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_ord_sort_sp

   subroutine test_ord_sort_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: real_array(5)

      real_array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call ord_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_ord_sort_dp

   subroutine test_radix_sort_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: integer_array(5)

      integer_array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call radix_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_radix_sort_int32

   subroutine test_radix_sort_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: integer_array(5)

      integer_array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]

      call radix_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_radix_sort_int64

   subroutine test_radix_sort_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: real_array(5)

      real_array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call radix_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_radix_sort_sp

   subroutine test_radix_sort_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: real_array(5)

      real_array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]

      call radix_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_radix_sort_dp

end module test_pic_sorting
