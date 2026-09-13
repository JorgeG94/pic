module test_pic_sorting
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: sp, dp, int32, int64, default_int
   use pic_sorting, only: sort, sort_index, radix_sort, ord_sort
   use pic_array, only: is_sorted, ascending, DESCENDING, pic_scramble_array
   use pic_error, only: error_t, ERROR_VALIDATION, ERROR_BOUNDS
   implicit none
   private
   public :: collect_pic_sorting_tests

   ! unsorted fixture shared by the error reporting tests
   integer(int32), parameter :: SORT_ERR_SRC(8) = [8_int32, 1_int32, 7_int32, 2_int32, &
                                                   6_int32, 3_int32, 5_int32, 4_int32]

   ! mixed-sign fixture for the degenerate radix_sort sizes: the first n
   ! elements are the input and the first n of ..._ASC the expected result.
   ! n == 2 also exercises the negative-value rotation branch, which needs
   ! array(1) >= 0 and array(n) < 0 after the unsigned radix pass.
   integer(int32), parameter :: RADIX_DEG_SRC(2) = [3_int32, -1_int32]
   integer(int32), parameter :: RADIX_DEG_ASC(2) = [-1_int32, 3_int32]

contains

   subroutine collect_pic_sorting_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_index_sort_char_int32", test_index_sort_char_int32), &
                  new_unittest("test_index_sort_char_int32_large", test_index_sort_char_int32_large), &
                  new_unittest("test_index_sort_char_int64", test_index_sort_char_int64), &
                  new_unittest("test_index_sort_char_int64_large", test_index_sort_char_int64_large), &
                  new_unittest("test_index_sort_int32_int32", test_index_sort_int32_int32), &
                  new_unittest("test_index_sort_int32_int32_large", test_index_sort_int32_int32_large), &
                  new_unittest("test_index_sort_int64_int64", test_index_sort_int64_int64), &
                  new_unittest("test_index_sort_int64_int64_large", test_index_sort_int64_int64_large), &
                  new_unittest("test_index_sort_int32_int64", test_index_sort_int32_int64), &
                  new_unittest("test_index_sort_int32_int64_large", test_index_sort_int32_int64_large), &
                  new_unittest("test_index_sort_int64_int32", test_index_sort_int64_int32), &
                  new_unittest("test_index_sort_int64_int32_large", test_index_sort_int64_int32_large), &
                  new_unittest("test_index_sort_sp_int32", test_index_sort_sp_int32), &
                  new_unittest("test_index_sort_sp_int32_large", test_index_sort_sp_int32_large), &
                  new_unittest("test_index_sort_sp_int64", test_index_sort_sp_int64), &
                  new_unittest("test_index_sort_sp_int64_large", test_index_sort_sp_int64_large), &
                  new_unittest("test_index_sort_dp_int32", test_index_sort_dp_int32), &
                  new_unittest("test_index_sort_dp_int32_large", test_index_sort_dp_int32_large), &
                  new_unittest("test_index_sort_dp_int64", test_index_sort_dp_int64), &
                  new_unittest("test_index_sort_dp_int64_large", test_index_sort_dp_int64_large), &
                  new_unittest("test_sort_char", test_sort_char), &
                  new_unittest("test_sort_int32", test_sort_int32), &
                  new_unittest("test_sort_int64", test_sort_int64), &
                  new_unittest("test_sort_sp", test_sort_sp), &
                  new_unittest("test_sort_dp", test_sort_dp), &
                  new_unittest("test_ord_sort_char", test_ord_sort_char), &
                  new_unittest("test_ord_sort_char_large", test_ord_sort_char_large), &
                  new_unittest("test_ord_sort_int32", test_ord_sort_int32), &
                  new_unittest("test_ord_sort_int32_large", test_ord_sort_int32_large), &
                  new_unittest("test_ord_sort_int64", test_ord_sort_int64), &
                  new_unittest("test_ord_sort_int64_large", test_ord_sort_int64_large), &
                  new_unittest("test_ord_sort_sp", test_ord_sort_sp), &
                  new_unittest("test_ord_sort_sp_large", test_ord_sort_sp_large), &
                  new_unittest("test_ord_sort_dp", test_ord_sort_dp), &
                  new_unittest("test_ord_sort_dp_large", test_ord_sort_dp_large), &
                  new_unittest("test_radix_sort_int32", test_radix_sort_int32), &
                  new_unittest("test_radix_sort_int64", test_radix_sort_int64), &
                  new_unittest("test_radix_sort_sp", test_radix_sort_sp), &
                  new_unittest("test_radix_sort_dp", test_radix_sort_dp), &
                  new_unittest("test_index_sort_char_tiny", test_index_sort_char_tiny), &
                  new_unittest("test_index_sort_char_mapping", test_index_sort_char_mapping), &
                  new_unittest("test_index_sort_numeric_tiny", test_index_sort_numeric_tiny), &
                  new_unittest("test_sort_from_pure_procedure", test_sort_from_pure_procedure), &
                  new_unittest("test_err_ord_sort_work_too_small", test_err_ord_sort_work_too_small), &
                  new_unittest("test_err_radix_sort_work_too_small", test_err_radix_sort_work_too_small), &
                  new_unittest("test_err_sort_index_work_too_small", test_err_sort_index_work_too_small), &
                  new_unittest("test_err_sort_index_iwork_too_small", test_err_sort_index_iwork_too_small), &
                  new_unittest("test_err_sort_index_index_too_small", test_err_sort_index_index_too_small), &
                  new_unittest("test_err_absent_and_success", test_err_absent_and_success), &
                  new_unittest("test_radix_sort_degenerate_sizes", test_radix_sort_degenerate_sizes), &
                  new_unittest("test_sort_median_of_three", test_sort_median_of_three), &
                  new_unittest("test_sort_heapsort_fallback", test_sort_heapsort_fallback), &
                  new_unittest("test_ord_sort_merge_exhausts_left", test_ord_sort_merge_exhausts_left), &
                  new_unittest("test_ord_sort_run_stack_collapse", test_ord_sort_run_stack_collapse), &
                  new_unittest("test_radix_sort_mostly_negative", test_radix_sort_mostly_negative) &
                  ]

   end subroutine collect_pic_sorting_tests

   subroutine test_index_sort_char_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      character(len=10) :: work(5)
      integer(int32) :: iwork(5)
      integer(int32), parameter :: expected_index(5) = [2_int32, 3_int32, 4_int32, 5_int32, 1_int32]
      integer(int32), parameter :: expected_reverse_index(5) = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      integer(int32) :: index(0:4)

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

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index, work)
      call check(error, is_sorted(array), .true., "Array is not sorted!")

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index, work, iwork)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index, iwork=iwork)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_char_int32

   subroutine test_index_sort_char_int32_large(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10), allocatable :: array(:)
      character(len=10), allocatable :: work(:)
      integer(int32), allocatable :: index(:)
      integer(int32), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         write (array(i), '(i4.4)') i
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         write (array(i), '(i4.4)') 42_int32
      end do
      do i = 11161, n_elements
         write (array(i), '(i4.4)') int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         write (array(i), '(i4.4)') 42_int32
      end do
      do i = 11161, n_elements
         write (array(i), '(i4.4)') int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_char_int32_large

   subroutine test_index_sort_char_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      character(len=10) :: work(5)
      integer(int64) :: index(0:4)
      integer(int64) :: iwork(0:4)
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
      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index, work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index, work, iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
   end subroutine test_index_sort_char_int64

   subroutine test_index_sort_char_int64_large(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10), allocatable :: array(:)
      character(len=10), allocatable :: work(:)
      integer(int64), allocatable :: index(:)
      integer(int64), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         write (array(i), '(i4.4)') i
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         write (array(i), '(i4.4)') 42_int32
      end do
      do i = 11161, n_elements
         write (array(i), '(i4.4)') int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         write (array(i), '(i4.4)') 42_int32
      end do
      do i = 11161, n_elements
         write (array(i), '(i4.4)') int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_char_int64_large

   subroutine test_index_sort_int32_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: array(5)
      integer(int32) :: work(0:4)
      integer(int32) :: iwork(0:4)
      integer(int32) :: index(0:4)
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

      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index, work=work, reverse=.false.)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index, work=work, iwork=iwork, reverse=.false.)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_int32_int32

   subroutine test_index_sort_int32_int32_large(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: array(:)
      integer(int32), allocatable :: work(:)
      integer(int32), allocatable :: index(:)
      integer(int32), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = n_elements - i + 1
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42_int32
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42_int32
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_int32_int32_large

   subroutine test_index_sort_int32_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: array(5)
      integer(int64) :: index(0:4)
      integer(int32) :: work(0:4)
      integer(int64) :: iwork(0:4)
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

      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index, work=work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index, work=work, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_index_sort_int32_int64

   subroutine test_index_sort_int32_int64_large(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: array(:)
      integer(int64), allocatable :: index(:)
      integer(int32), allocatable :: work(:)
      integer(int64), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = n_elements - i + 1
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42_int32
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42_int32
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int32)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_int32_int64_large

   subroutine test_index_sort_int64_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: array(5)
      integer(int64) :: work(0:4)
      integer(int32) :: index(0:4)
      integer(int32) :: iwork(0:4)
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

      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index, work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index, work, iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_index_sort_int64_int32

   subroutine test_index_sort_int64_int32_large(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: array(:)
      integer(int32), allocatable :: index(:)
      integer(int64), allocatable :: work(:)
      integer(int32), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = n_elements - i + 1
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42_int64
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int64)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42_int64
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int64)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_int64_int32_large

   subroutine test_index_sort_int64_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: array(5)
      integer(int64) :: work(0:4)
      integer(int64) :: index(0:4)
      integer(int64) :: iwork(0:4)
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

      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index, work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index, work, iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_index_sort_int64_int64

   subroutine test_index_sort_int64_int64_large(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: array(:)
      integer(int64), allocatable :: index(:)
      integer(int64), allocatable :: work(:)
      integer(int64), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = n_elements - i + 1
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
      do i = 1, 11160
         array(i) = 42_int64
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int64)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42_int64
      end do
      do i = 11161, n_elements
         array(i) = int(50*i, int64)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_int64_int64_large

   subroutine test_index_sort_sp_int32(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: array(5)
      real(sp) :: work(0:4)
      integer(int32) :: index(0:4)
      integer(int32) :: iwork(0:4)
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
      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index, work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index, work, iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_index_sort_sp_int32

   subroutine test_index_sort_sp_int32_large(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: array(:)
      real(sp), allocatable :: work(:)
      integer(int32), allocatable :: index(:)
      integer(int32), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = real(n_elements - i + 1, sp)
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42.0_sp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, sp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42.0_sp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, sp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_sp_int32_large

   subroutine test_index_sort_sp_int64(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: array(5)
      real(sp) :: work(0:4)
      integer(int64) :: index(0:4)
      integer(int64) :: iwork(0:4)
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
      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index, work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index, work, iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_index_sort_sp_int64

   subroutine test_index_sort_sp_int64_large(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: array(:)
      real(sp), allocatable :: work(:)
      integer(int64), allocatable :: index(:)
      integer(int64), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = real(n_elements - i + 1, sp)
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
      do i = 1, 11160
         array(i) = 42.0_sp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, sp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42.0_sp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, sp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_sp_int64_large

   subroutine test_index_sort_dp_int32(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: array(5)
      real(dp) :: work(0:4)
      integer(int32) :: index(0:4)
      integer(int32) :: iwork(0:4)
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
      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index, work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index, work, iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_index_sort_dp_int32

   subroutine test_index_sort_dp_int32_large(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: array(:)
      real(dp), allocatable :: work(:)
      integer(int32), allocatable :: index(:)
      integer(int32), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = real(n_elements - i + 1, dp)
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42.0_dp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, dp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42.0_dp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, dp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_dp_int32_large

   subroutine test_index_sort_dp_int64(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: array(5)
      real(dp) :: work(0:4)
      integer(int64) :: index(0:4)
      integer(int64) :: iwork(0:4)
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
      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index, work)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index, work, iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call sort_index(array, index, iwork=iwork)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_index_sort_dp_int64

   subroutine test_index_sort_dp_int64_large(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: array(:)
      real(dp), allocatable :: work(:)
      integer(int64), allocatable :: index(:)
      integer(int64), allocatable :: iwork(:)
      integer(int32), parameter :: n_elements = 12000_int32
      integer(int32) :: i

      allocate (array(n_elements))
      do i = 1, n_elements
         array(i) = real(n_elements - i + 1, dp)
      end do
      allocate (work(n_elements))
      allocate (index(n_elements))
      allocate (iwork(n_elements))

      call pic_scramble_array(array)
      call sort_index(array, index)

      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return
      do i = 1, 11160
         array(i) = 42.0_dp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, dp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         array(i) = 42.0_dp
      end do
      do i = 11161, n_elements
         array(i) = real(50*i, dp)
      end do
      call pic_scramble_array(array)

      call sort_index(array, index, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_index_sort_dp_int64_large

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

      block
         character(len=3), allocatable :: large_char_array(:)
         integer(int64), parameter :: n = 80000
         integer(int32) :: i
         integer(int32) :: char1, char2, char3

         allocate (large_char_array(n))

         ! Reverse sorted - 'z' to 'a' repeated
         do i = 1, n
            char1 = mod(i - 1, 26) + iachar('a')
            char2 = mod((i - 1)/26, 26) + iachar('a')
            char3 = mod((i - 1)/(26*26), 26) + iachar('a')
            large_char_array(i) = char(char1)//char(char2)//char(char3)
         end do

         call pic_scramble_array(large_char_array)

         call sort(large_char_array)
         call check(error, is_sorted(large_char_array), .true., "Char array not sorted!")
         if (allocated(error)) return

         call pic_scramble_array(large_char_array)

         call sort(large_char_array, .true.)

         call check(error, is_sorted(large_char_array, DESCENDING), .true., "Char array not sorted!")
         if (allocated(error)) return

         call sort(large_char_array)
         call check(error, is_sorted(large_char_array), .true., "Char array not sorted!")
         if (allocated(error)) return

         ! All identical characters
         large_char_array = 'xyz'
         call sort(large_char_array)
         call check(error, is_sorted(large_char_array), .true., "Identical chars not sorted!")
         if (allocated(error)) return
      end block

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

      block
         integer(int32), allocatable :: large_integer_array(:)
         integer(int32), parameter :: n = 40000
         integer(int32) :: i

         allocate (large_integer_array(n))

         do i = n, 1, -1
            large_integer_array(i) = i
         end do

         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

         call sort(large_integer_array, .true.)
         call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
         if (allocated(error)) return

         large_integer_array = 17_int32
         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

      end block

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

      block
         integer(int64), allocatable :: large_integer_array(:)
         integer(int64), parameter :: n = 40000
         integer(int64) :: i

         allocate (large_integer_array(n))

         do i = n, 1, -1
            large_integer_array(i) = i
         end do

         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

         call sort(large_integer_array, .true.)
         call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
         if (allocated(error)) return

         large_integer_array = 17_int64
         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

      end block
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
      block
         real(sp), allocatable :: large_integer_array(:)
         integer(int64), parameter :: n = 40000
         integer(int32) :: i

         allocate (large_integer_array(n))

         do i = n, 1, -1
            large_integer_array(i) = i
         end do

         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

         call sort(large_integer_array, .true.)
         call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
         if (allocated(error)) return

         large_integer_array = 17_sp
         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

      end block

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
      block
         real(dp), allocatable :: large_integer_array(:)
         integer(int64), parameter :: n = 40000
         integer(int32) :: i

         allocate (large_integer_array(n))

         do i = n, 1, -1
            large_integer_array(i) = i
         end do

         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

         call sort(large_integer_array, .true.)
         call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
         if (allocated(error)) return

         large_integer_array = 17_dp
         call sort(large_integer_array)
         call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
         if (allocated(error)) return

      end block

   end subroutine test_sort_dp

   subroutine test_ord_sort_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      character(len=10) :: work(5)

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call ord_sort(array)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(array, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      array = ["gamma     ", "bravo     ", "charlie   ", "delta     ", "echo      "]
      call ord_sort(array, work)
      call check(error, is_sorted(array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_ord_sort_char

   subroutine test_ord_sort_char_large(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10), allocatable :: large_char_array(:)
      character(len=10), allocatable :: work(:)
      integer(int32), parameter :: n = 12000_int32
      integer(int32) :: i

      allocate (large_char_array(n))
      allocate (work(n))

      ! Reverse sorted - 'z' to 'a' repeated
      do i = 1, n
         write (large_char_array(i), '(i4.4)') i
      end do

      call pic_scramble_array(large_char_array)

      call ord_sort(large_char_array, work)
      call check(error, is_sorted(large_char_array), .true., "Char array not sorted!")
      if (allocated(error)) return

      call pic_scramble_array(large_char_array)
      call ord_sort(large_char_array, work, reverse=.true.)
      call check(error, is_sorted(large_char_array, DESCENDING), .true., "Char array not sorted!")
      if (allocated(error)) return

      ! All identical characters
      large_char_array = 'xyz'
      call ord_sort(large_char_array, work)
      call check(error, is_sorted(large_char_array), .true., "Identical chars not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         write (large_char_array(i), '(i4.4)') 42_int32
      end do
      do i = 11161, n
         write (large_char_array(i), '(i4.4)') int(50*i, int32)
      end do
      call pic_scramble_array(large_char_array)

      call ord_sort(large_char_array, work)
      call check(error, is_sorted(large_char_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         write (large_char_array(i), '(i4.4)') 42_int32
      end do
      do i = 11161, n
         write (large_char_array(i), '(i4.4)') int(50*i, int32)
      end do
      call pic_scramble_array(large_char_array)

      call ord_sort(large_char_array, work, reverse=.true.)
      call check(error, is_sorted(large_char_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_char_large

   subroutine test_ord_sort_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: integer_array(5)
      integer(int32) :: work(5)

      integer_array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call ord_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      integer_array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      call ord_sort(integer_array, work)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_ord_sort_int32

   subroutine test_ord_sort_int32_large(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: large_integer_array(:)
      integer(int32), allocatable :: work(:)
      integer(int32), parameter :: n = 12000_int32
      integer(int32) :: i

      allocate (large_integer_array(n))
      allocate (work(n))

      do i = n, 1, -1
         large_integer_array(i) = i
      end do

      call pic_scramble_array(large_integer_array)

      call ord_sort(large_integer_array, work)
      call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call pic_scramble_array(large_integer_array)
      call ord_sort(large_integer_array, work, reverse=.true.)
      call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         large_integer_array(i) = 42_int32
      end do
      do i = 11161, n
         large_integer_array(i) = int(50*i, int32)
      end do
      call pic_scramble_array(large_integer_array)

      call ord_sort(large_integer_array, work)
      call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return
      do i = 1, 11160
         large_integer_array(i) = 42_int32
      end do
      do i = 11161, n
         large_integer_array(i) = int(50*i, int32)
      end do
      call pic_scramble_array(large_integer_array)

      call ord_sort(large_integer_array, work, reverse=.true.)
      call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_ord_sort_int32_large

   subroutine test_ord_sort_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: integer_array(5)
      integer(int64) :: work(5)

      integer_array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call ord_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      integer_array = [5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
      call ord_sort(integer_array, work)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_int64

   subroutine test_ord_sort_int64_large(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: large_integer_array(:)
      integer(int64), allocatable :: work(:)
      integer(int32), parameter :: n = 12000_int32
      integer(int32) :: i

      allocate (large_integer_array(n))
      allocate (work(n))

      do i = n, 1, -1
         large_integer_array(i) = i
      end do

      call pic_scramble_array(large_integer_array)

      call ord_sort(large_integer_array, work)
      call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call pic_scramble_array(large_integer_array)
      call ord_sort(large_integer_array, work, reverse=.true.)
      call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         large_integer_array(i) = 42_int64
      end do
      do i = 11161, n
         large_integer_array(i) = int(50*i, int64)
      end do
      call pic_scramble_array(large_integer_array)

      call ord_sort(large_integer_array, work)
      call check(error, is_sorted(large_integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         large_integer_array(i) = 42_int64
      end do
      do i = 11161, n
         large_integer_array(i) = int(50*i, int64)
      end do
      call pic_scramble_array(large_integer_array)

      call ord_sort(large_integer_array, work, reverse=.true.)
      call check(error, is_sorted(large_integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_int64_large

   subroutine test_ord_sort_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: real_array(5)
      real(sp) :: work(5)

      real_array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call ord_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      real_array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, 1.0_sp]
      call ord_sort(real_array, work)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_sp

   subroutine test_ord_sort_sp_large(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: large_real_array(:)
      real(sp), allocatable :: work(:)
      integer(int32), parameter :: n = 12000_int32
      integer(int32) :: i

      allocate (large_real_array(n))
      allocate (work(n))

      do i = n, 1, -1
         large_real_array(i) = real(i, sp)
      end do

      call pic_scramble_array(large_real_array)

      call ord_sort(large_real_array, work)
      call check(error, is_sorted(large_real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call pic_scramble_array(large_real_array)
      call ord_sort(large_real_array, work, reverse=.true.)
      call check(error, is_sorted(large_real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         large_real_array(i) = 42.0_sp
      end do
      do i = 11161, n
         large_real_array(i) = real(50*i, sp)
      end do
      call pic_scramble_array(large_real_array)

      call ord_sort(large_real_array, work)
      call check(error, is_sorted(large_real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         large_real_array(i) = 42.0_sp
      end do
      do i = 11161, n
         large_real_array(i) = real(50*i, sp)
      end do
      call pic_scramble_array(large_real_array)

      call ord_sort(large_real_array, work, reverse=.true.)
      call check(error, is_sorted(large_real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_sp_large

   subroutine test_ord_sort_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: real_array(5)
      real(dp) :: work(5)

      real_array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call ord_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call ord_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      real_array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
      call ord_sort(real_array, work)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_dp

   subroutine test_ord_sort_dp_large(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: large_real_array(:)
      real(dp), allocatable :: work(:)
      integer(int32), parameter :: n = 12000_int32
      integer(int32) :: i

      allocate (large_real_array(n))
      allocate (work(n))

      do i = n, 1, -1
         large_real_array(i) = real(i, dp)
      end do

      call pic_scramble_array(large_real_array)

      call ord_sort(large_real_array, work)
      call check(error, is_sorted(large_real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call pic_scramble_array(large_real_array)
      call ord_sort(large_real_array, work, reverse=.true.)
      call check(error, is_sorted(large_real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         large_real_array(i) = 42.0_dp
      end do
      do i = 11161, n
         large_real_array(i) = real(50*i, dp)
      end do
      call pic_scramble_array(large_real_array)

      call ord_sort(large_real_array, work)
      call check(error, is_sorted(large_real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      do i = 1, 11160
         large_real_array(i) = 42.0_dp
      end do
      do i = 11161, n
         large_real_array(i) = real(50*i, dp)
      end do
      call pic_scramble_array(large_real_array)

      call ord_sort(large_real_array, work, reverse=.true.)
      call check(error, is_sorted(large_real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return
   end subroutine test_ord_sort_dp_large

   subroutine test_radix_sort_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: integer_array(5)
      integer(int32) :: work_array(5)

      integer_array = [5_int32, 4_int32, 3_int32, 2_int32, -1_int32]
      call radix_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(integer_array, work_array)
      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_radix_sort_int32

   subroutine test_radix_sort_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: integer_array(5)
      integer(int64) :: work_array(5)

      integer_array = [5_int64, 4_int64, 3_int64, 2_int64, -1_int64]

      call radix_sort(integer_array)

      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(integer_array, reverse=.true.)
      call check(error, is_sorted(integer_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(integer_array, work_array)
      call check(error, is_sorted(integer_array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_radix_sort_int64

   subroutine test_radix_sort_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: real_array(5)
      real(sp) :: work_array(5)

      real_array = [5.0_sp, 4.0_sp, 3.0_sp, 2.0_sp, -1.0_sp]
      call radix_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(real_array, work_array)
      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_radix_sort_sp

   subroutine test_radix_sort_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: real_array(5)
      real(dp) :: work_array(5)

      real_array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, -1.0_dp]

      call radix_sort(real_array)

      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(real_array, reverse=.true.)
      call check(error, is_sorted(real_array, DESCENDING), .true., "Array is not sorted!")
      if (allocated(error)) return

      call radix_sort(real_array, work_array)
      call check(error, is_sorted(real_array), .true., "Array is not sorted!")
      if (allocated(error)) return

   end subroutine test_radix_sort_dp

   subroutine check_char_index_mapping(error, original, sorted_array, idx)
      !! Verify that IDX is a permutation of 1..n, that
      !! original(idx(i)) == sorted_array(i) for every i, and that the
      !! multiset of elements is preserved exactly.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: original(:)
      character(len=*), intent(in) :: sorted_array(:)
      integer(int64), intent(in) :: idx(:)
      logical, allocatable :: seen(:)
      integer(int64) :: i, n, k

      n = size(original, kind=int64)

      call check(error, size(sorted_array, kind=int64) == n, "sorted array changed size")
      if (allocated(error)) return
      call check(error, size(idx, kind=int64) == n, "index array changed size")
      if (allocated(error)) return

      allocate (seen(n))
      seen = .false.
      do i = 1, n
         k = idx(i)
         call check(error, k >= 1_int64 .and. k <= n, "index value out of range")
         if (allocated(error)) return
         call check(error,.not. seen(k), "index value repeated: not a permutation")
         if (allocated(error)) return
         seen(k) = .true.
         call check(error, original(k) == sorted_array(i), "original(index(i)) /= sorted(i)")
         if (allocated(error)) return
      end do

      call check(error, all(seen), "index is not a permutation of 1..n")
      if (allocated(error)) return

      ! Exact multiset preservation, checked independently of the index.
      do i = 1, n
         call check(error, count(original == sorted_array(i)) == count(sorted_array == sorted_array(i)), &
                    "multiset of elements was not preserved")
         if (allocated(error)) return
      end do

   end subroutine check_char_index_mapping

   subroutine test_index_sort_char_tiny(error)
      !! Arrays of size 0 and 1 make the scratch buffer bound
      !! `array_size/2 - 1` equal to -1, i.e. a legal zero-size
      !! `allocate (character(len=...) :: buf(0:-1))`. Exercise both index
      !! kinds and the work/iwork branches so every allocation guard on the
      !! character code path sees the degenerate size.
      type(error_type), allocatable, intent(out) :: error
      character(len=6) :: empty_array(0)
      character(len=6) :: one_array(1)
      character(len=6) :: work_array(1)
      integer(int32) :: empty_index_32(0), one_index_32(1), iwork_32(1)
      integer(int64) :: empty_index_64(0), one_index_64(1), iwork_64(1)

      ! size 0, low (int32) index kind
      call sort_index(empty_array, empty_index_32)
      call check(error, size(empty_array) == 0, "size-0 char array changed size")
      if (allocated(error)) return
      call check(error, is_sorted(empty_array), "size-0 char array reported unsorted")
      if (allocated(error)) return

      call sort_index(empty_array, empty_index_32, iwork=iwork_32)
      call check(error, size(empty_array) == 0, "size-0 char array changed size with iwork")
      if (allocated(error)) return

      ! size 0, default (int64) index kind
      call sort_index(empty_array, empty_index_64)
      call check(error, size(empty_array) == 0, "size-0 char array changed size (int64 index)")
      if (allocated(error)) return

      call sort_index(empty_array, empty_index_64, work_array)
      call check(error, size(empty_array) == 0, "size-0 char array changed size with work")
      if (allocated(error)) return

      ! size 1, both index kinds
      one_array(1) = "alpha "
      call sort_index(one_array, one_index_32)
      call check(error, one_array(1) == "alpha ", "size-1 char array was modified")
      if (allocated(error)) return
      call check(error, one_index_32(1) == 1_int32, "size-1 char index is not 1")
      if (allocated(error)) return

      call sort_index(one_array, one_index_64)
      call check(error, one_index_64(1) == 1_int64, "size-1 char index is not 1 (int64 index)")
      if (allocated(error)) return

      call sort_index(one_array, one_index_64, reverse=.true.)
      call check(error, one_index_64(1) == 1_int64, "size-1 reversed char index is not 1")
      if (allocated(error)) return

      call sort_index(one_array, one_index_64, work_array, iwork_64)
      call check(error, one_index_64(1) == 1_int64, "size-1 char index is not 1 with work and iwork")
      if (allocated(error)) return

      call sort_index(one_array, one_index_64, iwork=iwork_64)
      call check(error, one_index_64(1) == 1_int64, "size-1 char index is not 1 with iwork")
      if (allocated(error)) return

      call check_char_index_mapping(error, ["alpha "], one_array, int(one_index_64, int64))

   end subroutine test_index_sort_char_tiny

   subroutine test_index_sort_char_mapping(error)
      !! Index correctness for the character specialization: degenerate
      !! sizes plus a normal sized array with duplicates, which is large
      !! enough to go through the merge sort and therefore through the
      !! scratch buffer.
      type(error_type), allocatable, intent(out) :: error
      integer(int64), parameter :: n = 97_int64
      character(len=4) :: array(n), original(n)
      character(len=4) :: two_array(2), two_original(2)
      integer(int64) :: index_64(n), two_index(2)
      integer(int64) :: i

      ! Deliberately many duplicates: 97 elements drawn from 29 values.
      do i = 1, n
         write (original(i), '(i3.3)') int(modulo(i*37_int64, 29_int64))
      end do

      array = original
      call sort_index(array, index_64)
      call check(error, is_sorted(array), "char array is not sorted")
      if (allocated(error)) return
      call check_char_index_mapping(error, original, array, index_64)
      if (allocated(error)) return

      array = original
      call sort_index(array, index_64, reverse=.true.)
      call check(error, is_sorted(array, DESCENDING), "char array is not sorted descending")
      if (allocated(error)) return
      call check_char_index_mapping(error, original, array, index_64)
      if (allocated(error)) return

      ! Size 2 is the smallest size with a non-trivial permutation, and it
      ! still allocates a single-element scratch buffer.
      two_original = ["bb  ", "aa  "]
      two_array = two_original
      call sort_index(two_array, two_index)
      call check(error, is_sorted(two_array), "size-2 char array is not sorted")
      if (allocated(error)) return
      call check(error, two_index(1) == 2_int64 .and. two_index(2) == 1_int64, "size-2 char index is wrong")
      if (allocated(error)) return
      call check_char_index_mapping(error, two_original, two_array, two_index)

   end subroutine test_index_sort_char_mapping

   subroutine test_index_sort_numeric_tiny(error)
      !! The same degenerate sizes for the numeric specializations, which
      !! allocate a zero-size index buffer `ibuf(0:-1)` as well.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: empty_int32(0), one_int32(1)
      real(dp) :: empty_dp(0), one_dp(1)
      integer(int32) :: empty_index_32(0), one_index_32(1)
      integer(int64) :: empty_index_64(0), one_index_64(1)

      call sort_index(empty_int32, empty_index_32)
      call check(error, size(empty_int32) == 0, "size-0 int32 array changed size")
      if (allocated(error)) return

      call sort_index(empty_dp, empty_index_64)
      call check(error, size(empty_dp) == 0, "size-0 dp array changed size")
      if (allocated(error)) return

      one_int32(1) = 42_int32
      call sort_index(one_int32, one_index_32)
      call check(error, one_int32(1) == 42_int32, "size-1 int32 array was modified")
      if (allocated(error)) return
      call check(error, one_index_32(1) == 1_int32, "size-1 int32 index is not 1")
      if (allocated(error)) return

      one_dp(1) = -1.5_dp
      call sort_index(one_dp, one_index_64)
      call check(error, one_dp(1) == -1.5_dp, "size-1 dp array was modified")
      if (allocated(error)) return
      call check(error, one_index_64(1) == 1_int64, "size-1 dp index is not 1")
      if (allocated(error)) return

      ! ord_sort and sort on the same degenerate sizes, for completeness of
      ! the character scratch-buffer path.
      block
         character(len=6) :: empty_char(0)
         character(len=6) :: one_char(1)

         call ord_sort(empty_char)
         call check(error, size(empty_char) == 0, "size-0 char array changed size in ord_sort")
         if (allocated(error)) return

         one_char(1) = "omega "
         call ord_sort(one_char)
         call check(error, one_char(1) == "omega ", "size-1 char array was modified by ord_sort")
         if (allocated(error)) return

         call ord_sort(one_char, reverse=.true.)
         call check(error, one_char(1) == "omega ", "size-1 char array was modified by reverse ord_sort")
         if (allocated(error)) return

         call sort(empty_char)
         call check(error, size(empty_char) == 0, "size-0 char array changed size in sort")
         if (allocated(error)) return
      end block

   end subroutine test_index_sort_numeric_tiny
   ! ------------------------------------------------------------------
   ! error_t reporting
   ! ------------------------------------------------------------------

   pure subroutine pure_sort_caller(int_array, real_array, err)
      !! Compile time guard that the sorting routines stay usable from inside a
      !! `pure` procedure, `err` argument included. If this stops compiling, the
      !! migration to `error_t` has broken purity for every downstream `pure`
      !! caller of `sort`/`radix_sort`.
      integer(int32), intent(inout) :: int_array(:)
      real(dp), intent(inout) :: real_array(:)
      type(error_t), intent(inout), optional :: err

      call sort(int_array)
      call sort(real_array)
      call radix_sort(int_array, err=err)
   end subroutine pure_sort_caller

   subroutine test_sort_from_pure_procedure(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: int_array(5)
      real(dp) :: real_array(5)
      type(error_t) :: err

      int_array = [5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
      real_array = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]

      call pure_sort_caller(int_array, real_array, err)

      call check(error, is_sorted(int_array), .true., "Pure caller did not sort the integers!")
      if (allocated(error)) return
      call check(error, is_sorted(real_array), .true., "Pure caller did not sort the reals!")
      if (allocated(error)) return
      call check(error, err%has_error(), .false., "Pure caller reported a spurious error!")
      if (allocated(error)) return
   end subroutine test_sort_from_pure_procedure

   subroutine check_raised(error, err, expected_code, label)
      !! Assert that `err` carries `expected_code` and a non-empty message.
      type(error_type), allocatable, intent(out) :: error
      type(error_t), intent(in) :: err
      integer(default_int), intent(in) :: expected_code
      character(len=*), intent(in) :: label

      call check(error, err%has_error(), .true., label//": no error was reported!")
      if (allocated(error)) return
      call check(error, err%get_code() == expected_code, .true., label//": wrong error code!")
      if (allocated(error)) return
      call check(error, len_trim(err%get_message()) > 0, .true., label//": empty error message!")
      if (allocated(error)) return
   end subroutine check_raised

   subroutine test_err_ord_sort_work_too_small(error)
      !! A caller supplied `work` shorter than size(array)/2, for every
      !! `ord_sort` specialisation and both sort directions.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: i32(8), w32(3)
      integer(int64) :: i64(8), w64(3)
      real(sp) :: rsp(8), wsp(3)
      real(dp) :: rdp(8), wdp(3)
      character(len=4) :: chr(8), wchr(3)
      type(error_t) :: err
      integer(default_int) :: i

      i32 = SORT_ERR_SRC
      call ord_sort(i32, w32, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 ord_sort work")
      if (allocated(error)) return
      call check(error, all(i32 == SORT_ERR_SRC), .true., "int32 ord_sort must leave array unchanged!")
      if (allocated(error)) return

      i64 = int(SORT_ERR_SRC, int64)
      call ord_sort(i64, w64, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int64 ord_sort work")
      if (allocated(error)) return
      call check(error, all(i64 == int(SORT_ERR_SRC, int64)), .true., "int64 ord_sort must leave array unchanged!")
      if (allocated(error)) return

      rsp = real(SORT_ERR_SRC, sp)
      call ord_sort(rsp, wsp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "sp ord_sort work")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call ord_sort(rdp, wdp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "dp ord_sort work")
      if (allocated(error)) return

      do i = 1, 8
         write (chr(i), '(i4.4)') SORT_ERR_SRC(i)
      end do
      call ord_sort(chr, wchr, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "char ord_sort work")
      if (allocated(error)) return

      ! the decreasing direction goes through a separate worker routine
      i32 = SORT_ERR_SRC
      call ord_sort(i32, w32, reverse=.true., err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 ord_sort reverse work")
      if (allocated(error)) return
      call check(error, all(i32 == SORT_ERR_SRC), .true., "int32 reverse ord_sort must leave array unchanged!")
      if (allocated(error)) return

      i64 = int(SORT_ERR_SRC, int64)
      call ord_sort(i64, w64, reverse=.true., err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int64 ord_sort reverse work")
      if (allocated(error)) return

      rsp = real(SORT_ERR_SRC, sp)
      call ord_sort(rsp, wsp, reverse=.true., err=err)
      call check_raised(error, err, ERROR_VALIDATION, "sp ord_sort reverse work")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call ord_sort(rdp, wdp, reverse=.true., err=err)
      call check_raised(error, err, ERROR_VALIDATION, "dp ord_sort reverse work")
      if (allocated(error)) return

      call ord_sort(chr, wchr, reverse=.true., err=err)
      call check_raised(error, err, ERROR_VALIDATION, "char ord_sort reverse work")
      if (allocated(error)) return
   end subroutine test_err_ord_sort_work_too_small

   subroutine test_err_radix_sort_work_too_small(error)
      !! A caller supplied `work` shorter than size(array), for every
      !! `radix_sort` specialisation.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: i32(8), w32(3)
      integer(int64) :: i64(8), w64(3)
      real(sp) :: rsp(8), wsp(3)
      real(dp) :: rdp(8), wdp(3)
      type(error_t) :: err

      i32 = SORT_ERR_SRC
      call radix_sort(i32, w32, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 radix_sort work")
      if (allocated(error)) return
      call check(error, all(i32 == SORT_ERR_SRC), .true., "int32 radix_sort must leave array unchanged!")
      if (allocated(error)) return

      i64 = int(SORT_ERR_SRC, int64)
      call radix_sort(i64, w64, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int64 radix_sort work")
      if (allocated(error)) return

      rsp = real(SORT_ERR_SRC, sp)
      call radix_sort(rsp, wsp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "sp radix_sort work")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call radix_sort(rdp, wdp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "dp radix_sort work")
      if (allocated(error)) return
      call check(error, all(rdp == real(SORT_ERR_SRC, dp)), .true., "dp radix_sort must leave array unchanged!")
      if (allocated(error)) return
   end subroutine test_err_radix_sort_work_too_small

   subroutine test_err_sort_index_work_too_small(error)
      !! `work` shorter than size(array)/2 for all ten `sort_index`
      !! specialisations (five types x two index kinds).
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: i32(8), w32(3)
      integer(int64) :: i64(8), w64(3)
      real(sp) :: rsp(8), wsp(3)
      real(dp) :: rdp(8), wdp(3)
      character(len=4) :: chr(8), wchr(3)
      integer(int32) :: idx_low(8)
      integer(int64) :: idx_def(8)
      type(error_t) :: err
      integer(default_int) :: i

      do i = 1, 8
         write (chr(i), '(i4.4)') SORT_ERR_SRC(i)
      end do

      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_def, w32, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 sort_index default work")
      if (allocated(error)) return
      call check(error, all(i32 == SORT_ERR_SRC), .true., "sort_index must leave array unchanged!")
      if (allocated(error)) return
      call check(error, all(idx_def == [1_int64, 2_int64, 3_int64, 4_int64, &
                                        5_int64, 6_int64, 7_int64, 8_int64]), .true., &
                 "sort_index leaves index as the identity permutation!")
      if (allocated(error)) return

      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_low, w32, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 sort_index low work")
      if (allocated(error)) return

      i64 = int(SORT_ERR_SRC, int64)
      call sort_index(i64, idx_def, w64, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int64 sort_index default work")
      if (allocated(error)) return
      i64 = int(SORT_ERR_SRC, int64)
      call sort_index(i64, idx_low, w64, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int64 sort_index low work")
      if (allocated(error)) return

      rsp = real(SORT_ERR_SRC, sp)
      call sort_index(rsp, idx_def, wsp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "sp sort_index default work")
      if (allocated(error)) return
      rsp = real(SORT_ERR_SRC, sp)
      call sort_index(rsp, idx_low, wsp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "sp sort_index low work")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call sort_index(rdp, idx_def, wdp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "dp sort_index default work")
      if (allocated(error)) return
      rdp = real(SORT_ERR_SRC, dp)
      call sort_index(rdp, idx_low, wdp, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "dp sort_index low work")
      if (allocated(error)) return

      call sort_index(chr, idx_def, wchr, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "char sort_index default work")
      if (allocated(error)) return
      call sort_index(chr, idx_low, wchr, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "char sort_index low work")
      if (allocated(error)) return

      ! With reverse the array has already been reversed in place by the time the
      ! failure is detected, and is left reversed. This pins that contract.
      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_def, w32, reverse=.true., err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 sort_index reverse work")
      if (allocated(error)) return
      call check(error, all(i32 == SORT_ERR_SRC(8:1:-1)), .true., &
                 "reverse sort_index leaves the array reversed on error!")
      if (allocated(error)) return
   end subroutine test_err_sort_index_work_too_small

   subroutine test_err_sort_index_iwork_too_small(error)
      !! `iwork` too small, both with and without a `work` array present: those
      !! are separate branches, each with its own check.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: i32(8), w32(8)
      integer(int64) :: i64(8), w64(8)
      real(sp) :: rsp(8), wsp(8)
      real(dp) :: rdp(8), wdp(8)
      character(len=4) :: chr(8), wchr(8)
      integer(int32) :: idx_low(8), iw_low(3)
      integer(int64) :: idx_def(8), iw_def(3)
      type(error_t) :: err
      integer(default_int) :: i

      do i = 1, 8
         write (chr(i), '(i4.4)') SORT_ERR_SRC(i)
      end do

      ! work present
      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_def, w32, iw_def, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 sort_index default iwork+work")
      if (allocated(error)) return
      call check(error, all(i32 == SORT_ERR_SRC), .true., "sort_index must leave array unchanged!")
      if (allocated(error)) return
      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_low, w32, iw_low, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 sort_index low iwork+work")
      if (allocated(error)) return

      ! work absent: an array buffer is allocated first, then iwork is checked
      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_def, iwork=iw_def, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 sort_index default iwork only")
      if (allocated(error)) return
      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_low, iwork=iw_low, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int32 sort_index low iwork only")
      if (allocated(error)) return

      i64 = int(SORT_ERR_SRC, int64)
      call sort_index(i64, idx_def, w64, iw_def, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int64 sort_index default iwork+work")
      if (allocated(error)) return
      i64 = int(SORT_ERR_SRC, int64)
      call sort_index(i64, idx_low, iwork=iw_low, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "int64 sort_index low iwork only")
      if (allocated(error)) return

      rsp = real(SORT_ERR_SRC, sp)
      call sort_index(rsp, idx_def, wsp, iw_def, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "sp sort_index default iwork+work")
      if (allocated(error)) return
      rsp = real(SORT_ERR_SRC, sp)
      call sort_index(rsp, idx_low, iwork=iw_low, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "sp sort_index low iwork only")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call sort_index(rdp, idx_def, wdp, iw_def, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "dp sort_index default iwork+work")
      if (allocated(error)) return
      rdp = real(SORT_ERR_SRC, dp)
      call sort_index(rdp, idx_low, iwork=iw_low, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "dp sort_index low iwork only")
      if (allocated(error)) return

      call sort_index(chr, idx_def, wchr, iw_def, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "char sort_index default iwork+work")
      if (allocated(error)) return
      call sort_index(chr, idx_low, iwork=iw_low, err=err)
      call check_raised(error, err, ERROR_VALIDATION, "char sort_index low iwork only")
      if (allocated(error)) return
   end subroutine test_err_sort_index_iwork_too_small

   subroutine test_err_sort_index_index_too_small(error)
      !! An `index` array that cannot hold one entry per element of `array`.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: i32(8)
      integer(int64) :: i64(8)
      real(sp) :: rsp(8)
      real(dp) :: rdp(8)
      character(len=4) :: chr(8)
      integer(int32) :: idx_low(4)
      integer(int64) :: idx_def(4)
      type(error_t) :: err
      integer(default_int) :: i

      do i = 1, 8
         write (chr(i), '(i4.4)') SORT_ERR_SRC(i)
      end do

      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_def, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "int32 sort_index default index size")
      if (allocated(error)) return
      call check(error, all(i32 == SORT_ERR_SRC), .true., "sort_index must leave array unchanged!")
      if (allocated(error)) return

      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_low, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "int32 sort_index low index size")
      if (allocated(error)) return

      i64 = int(SORT_ERR_SRC, int64)
      call sort_index(i64, idx_def, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "int64 sort_index default index size")
      if (allocated(error)) return
      i64 = int(SORT_ERR_SRC, int64)
      call sort_index(i64, idx_low, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "int64 sort_index low index size")
      if (allocated(error)) return

      rsp = real(SORT_ERR_SRC, sp)
      call sort_index(rsp, idx_def, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "sp sort_index default index size")
      if (allocated(error)) return
      rsp = real(SORT_ERR_SRC, sp)
      call sort_index(rsp, idx_low, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "sp sort_index low index size")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call sort_index(rdp, idx_def, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "dp sort_index default index size")
      if (allocated(error)) return
      rdp = real(SORT_ERR_SRC, dp)
      call sort_index(rdp, idx_low, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "dp sort_index low index size")
      if (allocated(error)) return

      call sort_index(chr, idx_def, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "char sort_index default index size")
      if (allocated(error)) return
      call sort_index(chr, idx_low, err=err)
      call check_raised(error, err, ERROR_BOUNDS, "char sort_index low index size")
      if (allocated(error)) return
   end subroutine test_err_sort_index_index_too_small

   subroutine test_err_absent_and_success(error)
      !! The optional `err` plumbing must not disturb the existing API: the same
      !! calls without `err` still sort valid input, and a successful call that
      !! does pass `err` leaves it clear.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: i32(8), w32(8), iw_low(8), idx_low(8)
      real(dp) :: rdp(8), wdp(8)
      type(error_t) :: err

      ! no err argument at all
      i32 = SORT_ERR_SRC
      call ord_sort(i32, w32)
      call check(error, is_sorted(i32), .true., "ord_sort without err did not sort!")
      if (allocated(error)) return

      i32 = SORT_ERR_SRC
      call radix_sort(i32, w32)
      call check(error, is_sorted(i32), .true., "radix_sort without err did not sort!")
      if (allocated(error)) return

      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_low, w32, iw_low)
      call check(error, is_sorted(i32), .true., "sort_index without err did not sort!")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call ord_sort(rdp, wdp)
      call check(error, is_sorted(rdp), .true., "dp ord_sort without err did not sort!")
      if (allocated(error)) return

      ! err present, nothing wrong: err must stay clear
      i32 = SORT_ERR_SRC
      call ord_sort(i32, w32, err=err)
      call check(error, err%has_error(), .false., "ord_sort reported a spurious error!")
      if (allocated(error)) return
      call check(error, is_sorted(i32), .true., "ord_sort with err did not sort!")
      if (allocated(error)) return

      i32 = SORT_ERR_SRC
      call sort_index(i32, idx_low, w32, iw_low, err=err)
      call check(error, err%has_error(), .false., "sort_index reported a spurious error!")
      if (allocated(error)) return
      call check(error, is_sorted(i32), .true., "sort_index with err did not sort!")
      if (allocated(error)) return

      i32 = SORT_ERR_SRC
      call radix_sort(i32, w32, reverse=.true., err=err)
      call check(error, err%has_error(), .false., "radix_sort reported a spurious error!")
      if (allocated(error)) return
      call check(error, is_sorted(i32, DESCENDING), .true., "radix_sort with err did not sort!")
      if (allocated(error)) return

      rdp = real(SORT_ERR_SRC, dp)
      call radix_sort(rdp, wdp, err=err)
      call check(error, err%has_error(), .false., "dp radix_sort reported a spurious error!")
      if (allocated(error)) return
      call check(error, is_sorted(rdp), .true., "dp radix_sort with err did not sort!")
      if (allocated(error)) return
   end subroutine test_err_absent_and_success

   subroutine test_radix_sort_degenerate_sizes(error)
      !! `radix_sort` on arrays of size 0, 1 and 2, for every specialisation,
      !! with and without `work`, with and without `err`, in both directions.
      !!
      !! This is the regression guard for the `associated(buffer)` check on the
      !! internal scratch buffer: a legal zero-size `allocate (buffer(0))`
      !! leaves the pointer associated with stat == 0, so the guard must not
      !! misfire at size 0. An allocation that actually fails is not reachable
      !! from a test, so the raising side of that check is unexercised.
      type(error_type), allocatable, intent(out) :: error
      integer(default_int) :: n

      do n = 0, 2
         call degenerate_i32(n, error)
         if (allocated(error)) return
         call degenerate_i64(n, error)
         if (allocated(error)) return
         call degenerate_sp(n, error)
         if (allocated(error)) return
         call degenerate_dp(n, error)
         if (allocated(error)) return
      end do
   end subroutine test_radix_sort_degenerate_sizes

   function radix_deg_expect(n) result(expect)
      !! `RADIX_DEG_SRC(1:n)` in ascending order. Only n == 2 actually needs
      !! reordering; the length 0 and 1 prefixes are already sorted.
      integer(default_int), intent(in) :: n
      integer(int32), allocatable :: expect(:)

      allocate (expect(n))
      if (n == 2) then
         expect = RADIX_DEG_ASC
      else
         expect = RADIX_DEG_SRC(1:n)
      end if
   end function radix_deg_expect

   function size_tag(prefix, n) result(text)
      !! "<prefix> (size <n>)", for the degenerate-size assertion messages.
      character(len=*), intent(in) :: prefix
      integer(default_int), intent(in) :: n
      character(len=:), allocatable :: text
      character(len=16) :: num

      write (num, '(i0)') n
      text = prefix//" (size "//trim(num)//")"
   end function size_tag

   subroutine degenerate_i32(n, error)
      integer(default_int), intent(in) :: n
      type(error_type), allocatable, intent(inout) :: error
      integer(int32), allocatable :: array(:), work(:)
      integer(int32), allocatable :: expect(:), expect_rev(:)
      integer(int32), allocatable :: asc(:)
      type(error_t) :: err
      integer(default_int) :: i

      allocate (array(n), work(n), expect(n), expect_rev(n))
      asc = radix_deg_expect(n)
      expect = asc
      do i = 1, n
         expect_rev(i) = asc(n - i + 1)
      end do

      array = RADIX_DEG_SRC(1:n)
      call radix_sort(array)
      call check(error, all(array == expect), .true., size_tag("int32 radix_sort, internal buffer", n))
      if (allocated(error)) return

      array = RADIX_DEG_SRC(1:n)
      call radix_sort(array, err=err)
      call check(error, err%has_error(), .false., size_tag("int32 radix_sort raised on internal buffer", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("int32 radix_sort, internal buffer + err", n))
      if (allocated(error)) return

      array = RADIX_DEG_SRC(1:n)
      call radix_sort(array, work)
      call check(error, all(array == expect), .true., size_tag("int32 radix_sort, work", n))
      if (allocated(error)) return

      array = RADIX_DEG_SRC(1:n)
      call radix_sort(array, work, err=err)
      call check(error, err%has_error(), .false., size_tag("int32 radix_sort raised on work", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("int32 radix_sort, work + err", n))
      if (allocated(error)) return

      array = RADIX_DEG_SRC(1:n)
      call radix_sort(array, reverse=.true.)
      call check(error, all(array == expect_rev), .true., size_tag("int32 radix_sort, reverse", n))
      if (allocated(error)) return

      array = RADIX_DEG_SRC(1:n)
      call radix_sort(array, reverse=.true., err=err)
      call check(error, err%has_error(), .false., size_tag("int32 radix_sort raised on reverse", n))
      if (allocated(error)) return
      call check(error, all(array == expect_rev), .true., size_tag("int32 radix_sort, reverse + err", n))
      if (allocated(error)) return
   end subroutine degenerate_i32

   subroutine degenerate_i64(n, error)
      integer(default_int), intent(in) :: n
      type(error_type), allocatable, intent(inout) :: error
      integer(int64), allocatable :: array(:), work(:)
      integer(int64), allocatable :: expect(:), expect_rev(:)
      integer(int32), allocatable :: asc(:)
      type(error_t) :: err
      integer(default_int) :: i

      allocate (array(n), work(n), expect(n), expect_rev(n))
      asc = radix_deg_expect(n)
      expect = int(asc, int64)
      do i = 1, n
         expect_rev(i) = int(asc(n - i + 1), int64)
      end do

      array = int(RADIX_DEG_SRC(1:n), int64)
      call radix_sort(array)
      call check(error, all(array == expect), .true., size_tag("int64 radix_sort, internal buffer", n))
      if (allocated(error)) return

      array = int(RADIX_DEG_SRC(1:n), int64)
      call radix_sort(array, err=err)
      call check(error, err%has_error(), .false., size_tag("int64 radix_sort raised on internal buffer", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("int64 radix_sort, internal buffer + err", n))
      if (allocated(error)) return

      array = int(RADIX_DEG_SRC(1:n), int64)
      call radix_sort(array, work)
      call check(error, all(array == expect), .true., size_tag("int64 radix_sort, work", n))
      if (allocated(error)) return

      array = int(RADIX_DEG_SRC(1:n), int64)
      call radix_sort(array, work, err=err)
      call check(error, err%has_error(), .false., size_tag("int64 radix_sort raised on work", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("int64 radix_sort, work + err", n))
      if (allocated(error)) return

      array = int(RADIX_DEG_SRC(1:n), int64)
      call radix_sort(array, reverse=.true.)
      call check(error, all(array == expect_rev), .true., size_tag("int64 radix_sort, reverse", n))
      if (allocated(error)) return

      array = int(RADIX_DEG_SRC(1:n), int64)
      call radix_sort(array, reverse=.true., err=err)
      call check(error, err%has_error(), .false., size_tag("int64 radix_sort raised on reverse", n))
      if (allocated(error)) return
      call check(error, all(array == expect_rev), .true., size_tag("int64 radix_sort, reverse + err", n))
      if (allocated(error)) return
   end subroutine degenerate_i64

   subroutine degenerate_sp(n, error)
      integer(default_int), intent(in) :: n
      type(error_type), allocatable, intent(inout) :: error
      real(sp), allocatable :: array(:), work(:)
      real(sp), allocatable :: expect(:), expect_rev(:)
      integer(int32), allocatable :: asc(:)
      type(error_t) :: err
      integer(default_int) :: i

      allocate (array(n), work(n), expect(n), expect_rev(n))
      asc = radix_deg_expect(n)
      expect = real(asc, sp)
      do i = 1, n
         expect_rev(i) = real(asc(n - i + 1), sp)
      end do

      array = real(RADIX_DEG_SRC(1:n), sp)
      call radix_sort(array)
      call check(error, all(array == expect), .true., size_tag("sp radix_sort, internal buffer", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), sp)
      call radix_sort(array, err=err)
      call check(error, err%has_error(), .false., size_tag("sp radix_sort raised on internal buffer", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("sp radix_sort, internal buffer + err", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), sp)
      call radix_sort(array, work)
      call check(error, all(array == expect), .true., size_tag("sp radix_sort, work", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), sp)
      call radix_sort(array, work, err=err)
      call check(error, err%has_error(), .false., size_tag("sp radix_sort raised on work", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("sp radix_sort, work + err", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), sp)
      call radix_sort(array, reverse=.true.)
      call check(error, all(array == expect_rev), .true., size_tag("sp radix_sort, reverse", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), sp)
      call radix_sort(array, reverse=.true., err=err)
      call check(error, err%has_error(), .false., size_tag("sp radix_sort raised on reverse", n))
      if (allocated(error)) return
      call check(error, all(array == expect_rev), .true., size_tag("sp radix_sort, reverse + err", n))
      if (allocated(error)) return
   end subroutine degenerate_sp

   subroutine degenerate_dp(n, error)
      integer(default_int), intent(in) :: n
      type(error_type), allocatable, intent(inout) :: error
      real(dp), allocatable :: array(:), work(:)
      real(dp), allocatable :: expect(:), expect_rev(:)
      integer(int32), allocatable :: asc(:)
      type(error_t) :: err
      integer(default_int) :: i

      allocate (array(n), work(n), expect(n), expect_rev(n))
      asc = radix_deg_expect(n)
      expect = real(asc, dp)
      do i = 1, n
         expect_rev(i) = real(asc(n - i + 1), dp)
      end do

      array = real(RADIX_DEG_SRC(1:n), dp)
      call radix_sort(array)
      call check(error, all(array == expect), .true., size_tag("dp radix_sort, internal buffer", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), dp)
      call radix_sort(array, err=err)
      call check(error, err%has_error(), .false., size_tag("dp radix_sort raised on internal buffer", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("dp radix_sort, internal buffer + err", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), dp)
      call radix_sort(array, work)
      call check(error, all(array == expect), .true., size_tag("dp radix_sort, work", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), dp)
      call radix_sort(array, work, err=err)
      call check(error, err%has_error(), .false., size_tag("dp radix_sort raised on work", n))
      if (allocated(error)) return
      call check(error, all(array == expect), .true., size_tag("dp radix_sort, work + err", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), dp)
      call radix_sort(array, reverse=.true.)
      call check(error, all(array == expect_rev), .true., size_tag("dp radix_sort, reverse", n))
      if (allocated(error)) return

      array = real(RADIX_DEG_SRC(1:n), dp)
      call radix_sort(array, reverse=.true., err=err)
      call check(error, err%has_error(), .false., size_tag("dp radix_sort raised on reverse", n))
      if (allocated(error)) return
      call check(error, all(array == expect_rev), .true., size_tag("dp radix_sort, reverse + err", n))
      if (allocated(error)) return
   end subroutine degenerate_dp

   ! ------------------------------------------------------------------
   ! Helpers shared by the structured-input sorting tests below.
   !
   ! Every scenario is described as an array of non-negative integer codes.
   ! The codes are mapped onto each supported element type (including an
   ! order preserving three letter encoding for characters) so that one
   ! carefully shaped input can be pushed through every specialisation.
   ! ------------------------------------------------------------------

   pure function code_to_word(code) result(word)
      !! Order preserving encoding of a code in [0, 17575] as three letters
      integer(default_int), intent(in) :: code
      character(len=3) :: word

      word(1:1) = achar(iachar("a") + code/676_default_int)
      word(2:2) = achar(iachar("a") + mod(code/26_default_int, 26_default_int))
      word(3:3) = achar(iachar("a") + mod(code, 26_default_int))
   end function code_to_word

   pure function word_to_code(word) result(code)
      !! Inverse of code_to_word
      character(len=*), intent(in) :: word
      integer(default_int) :: code

      code = 676_default_int*(iachar(word(1:1)) - iachar("a")) &
             + 26_default_int*(iachar(word(2:2)) - iachar("a")) &
             + (iachar(word(3:3)) - iachar("a"))
   end function word_to_code

   pure function codes_to_words(codes) result(words)
      integer(default_int), intent(in) :: codes(:)
      character(len=3) :: words(size(codes))
      integer(default_int) :: i

      do i = 1, size(codes, kind=default_int)
         words(i) = code_to_word(codes(i))
      end do
   end function codes_to_words

   pure function words_to_codes(words) result(codes)
      character(len=*), intent(in) :: words(:)
      integer(default_int) :: codes(size(words))
      integer(default_int) :: i

      do i = 1, size(words, kind=default_int)
         codes(i) = word_to_code(words(i))
      end do
   end function words_to_codes

   function same_multiset(a, b) result(ok)
      !! .true. when a and b hold the same values with the same multiplicities
      integer(default_int), intent(in) :: a(:), b(:)
      logical :: ok
      integer(default_int), allocatable :: hist_a(:), hist_b(:)
      integer(default_int) :: lo, hi, i

      ok = .false.
      if (size(a, kind=default_int) /= size(b, kind=default_int)) return

      lo = min(minval(a), minval(b))
      hi = max(maxval(a), maxval(b))
      allocate (hist_a(lo:hi), hist_b(lo:hi))
      hist_a = 0_default_int
      hist_b = 0_default_int
      do i = 1, size(a, kind=default_int)
         hist_a(a(i)) = hist_a(a(i)) + 1_default_int
         hist_b(b(i)) = hist_b(b(i)) + 1_default_int
      end do

      ok = all(hist_a == hist_b)
   end function same_multiset

   subroutine check_sort_outcome(error, ordered, permuted, label)
      !! Both properties a sort must have: the right order, and the same
      !! elements it was handed
      type(error_type), allocatable, intent(out) :: error
      logical, intent(in) :: ordered, permuted
      character(len=*), intent(in) :: label

      call check(error, ordered, label//": result is not in the requested order")
      if (allocated(error)) return

      call check(error, permuted, label//": result is not a permutation of the input")
   end subroutine check_sort_outcome

   function mirror_codes(codes) result(mirrored)
      !! Reflect the codes so that an input crafted for an increasing sort
      !! exercises exactly the same path in the decreasing specialisation
      integer(default_int), intent(in) :: codes(:)
      integer(default_int) :: mirrored(size(codes))

      mirrored = maxval(codes) - codes
   end function mirror_codes

   subroutine run_sort_checks(error, codes, label)
      !! Push one scenario through sort() for every element type, ascending
      !! and descending
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), intent(in) :: codes(:)
      character(len=*), intent(in) :: label
      integer(default_int) :: n
      integer(default_int), allocatable :: mirrored(:)
      integer(int32), allocatable :: a32(:)
      integer(int64), allocatable :: a64(:)
      real(sp), allocatable :: asp(:)
      real(dp), allocatable :: adp(:)
      character(len=3), allocatable :: ach(:)

      n = size(codes, kind=default_int)
      allocate (a32(n), a64(n), asp(n), adp(n), ach(n))
      allocate (mirrored(n))
      mirrored = mirror_codes(codes)

      a32 = int(codes, int32)
      call sort(a32)
      call check_sort_outcome(error, is_sorted(a32), &
                              same_multiset(int(a32, default_int), codes), label//" sort int32 up")
      if (allocated(error)) return

      a64 = int(codes, int64)
      call sort(a64)
      call check_sort_outcome(error, is_sorted(a64), &
                              same_multiset(int(a64, default_int), codes), label//" sort int64 up")
      if (allocated(error)) return

      asp = real(codes, sp)
      call sort(asp)
      call check_sort_outcome(error, is_sorted(asp), &
                              same_multiset(nint(asp, default_int), codes), label//" sort sp up")
      if (allocated(error)) return

      adp = real(codes, dp)
      call sort(adp)
      call check_sort_outcome(error, is_sorted(adp), &
                              same_multiset(nint(adp, default_int), codes), label//" sort dp up")
      if (allocated(error)) return

      ach = codes_to_words(codes)
      call sort(ach)
      call check_sort_outcome(error, is_sorted(ach), &
                              same_multiset(words_to_codes(ach), codes), label//" sort char up")
      if (allocated(error)) return

      a32 = int(mirrored, int32)
      call sort(a32, .true.)
      call check_sort_outcome(error, is_sorted(a32, DESCENDING), &
                              same_multiset(int(a32, default_int), mirrored), label//" sort int32 down")
      if (allocated(error)) return

      a64 = int(mirrored, int64)
      call sort(a64, .true.)
      call check_sort_outcome(error, is_sorted(a64, DESCENDING), &
                              same_multiset(int(a64, default_int), mirrored), label//" sort int64 down")
      if (allocated(error)) return

      asp = real(mirrored, sp)
      call sort(asp, .true.)
      call check_sort_outcome(error, is_sorted(asp, DESCENDING), &
                              same_multiset(nint(asp, default_int), mirrored), label//" sort sp down")
      if (allocated(error)) return

      adp = real(mirrored, dp)
      call sort(adp, .true.)
      call check_sort_outcome(error, is_sorted(adp, DESCENDING), &
                              same_multiset(nint(adp, default_int), mirrored), label//" sort dp down")
      if (allocated(error)) return

      ach = codes_to_words(mirrored)
      call sort(ach, .true.)
      call check_sort_outcome(error, is_sorted(ach, DESCENDING), &
                              same_multiset(words_to_codes(ach), mirrored), label//" sort char down")
      if (allocated(error)) return
   end subroutine run_sort_checks

   subroutine run_ord_sort_checks(error, codes, label)
      !! Push one scenario through ord_sort() for every element type,
      !! ascending and descending
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), intent(in) :: codes(:)
      character(len=*), intent(in) :: label
      integer(default_int) :: n
      integer(default_int), allocatable :: mirrored(:)
      integer(int32), allocatable :: a32(:)
      integer(int64), allocatable :: a64(:)
      real(sp), allocatable :: asp(:)
      real(dp), allocatable :: adp(:)
      character(len=3), allocatable :: ach(:)

      n = size(codes, kind=default_int)
      allocate (a32(n), a64(n), asp(n), adp(n), ach(n))
      allocate (mirrored(n))
      mirrored = mirror_codes(codes)

      a32 = int(codes, int32)
      call ord_sort(a32)
      call check_sort_outcome(error, is_sorted(a32), &
                              same_multiset(int(a32, default_int), codes), label//" ord_sort int32 up")
      if (allocated(error)) return

      a64 = int(codes, int64)
      call ord_sort(a64)
      call check_sort_outcome(error, is_sorted(a64), &
                              same_multiset(int(a64, default_int), codes), label//" ord_sort int64 up")
      if (allocated(error)) return

      asp = real(codes, sp)
      call ord_sort(asp)
      call check_sort_outcome(error, is_sorted(asp), &
                              same_multiset(nint(asp, default_int), codes), label//" ord_sort sp up")
      if (allocated(error)) return

      adp = real(codes, dp)
      call ord_sort(adp)
      call check_sort_outcome(error, is_sorted(adp), &
                              same_multiset(nint(adp, default_int), codes), label//" ord_sort dp up")
      if (allocated(error)) return

      ach = codes_to_words(codes)
      call ord_sort(ach)
      call check_sort_outcome(error, is_sorted(ach), &
                              same_multiset(words_to_codes(ach), codes), label//" ord_sort char up")
      if (allocated(error)) return

      a32 = int(mirrored, int32)
      call ord_sort(a32, reverse=.true.)
      call check_sort_outcome(error, is_sorted(a32, DESCENDING), &
                              same_multiset(int(a32, default_int), mirrored), label//" ord_sort int32 down")
      if (allocated(error)) return

      a64 = int(mirrored, int64)
      call ord_sort(a64, reverse=.true.)
      call check_sort_outcome(error, is_sorted(a64, DESCENDING), &
                              same_multiset(int(a64, default_int), mirrored), label//" ord_sort int64 down")
      if (allocated(error)) return

      asp = real(mirrored, sp)
      call ord_sort(asp, reverse=.true.)
      call check_sort_outcome(error, is_sorted(asp, DESCENDING), &
                              same_multiset(nint(asp, default_int), mirrored), label//" ord_sort sp down")
      if (allocated(error)) return

      adp = real(mirrored, dp)
      call ord_sort(adp, reverse=.true.)
      call check_sort_outcome(error, is_sorted(adp, DESCENDING), &
                              same_multiset(nint(adp, default_int), mirrored), label//" ord_sort dp down")
      if (allocated(error)) return

      ach = codes_to_words(mirrored)
      call ord_sort(ach, reverse=.true.)
      call check_sort_outcome(error, is_sorted(ach, DESCENDING), &
                              same_multiset(words_to_codes(ach), mirrored), label//" ord_sort char down")
      if (allocated(error)) return
   end subroutine run_ord_sort_checks

   subroutine run_sort_index_checks(error, codes, label)
      !! Push one scenario through sort_index() for every element type and
      !! both index kinds, checking that the index really maps the original
      !! positions onto the sorted order
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), intent(in) :: codes(:)
      character(len=*), intent(in) :: label
      integer(default_int) :: n
      integer(int32), allocatable :: a32(:), o32(:)
      integer(int64), allocatable :: a64(:), o64(:)
      real(sp), allocatable :: asp(:), osp(:)
      real(dp), allocatable :: adp(:), odp(:)
      character(len=3), allocatable :: ach(:), och(:)
      integer(int32), allocatable :: idx_low(:)
      integer(int64), allocatable :: idx_default(:)

      n = size(codes, kind=default_int)
      allocate (a32(n), o32(n), a64(n), o64(n), asp(n), osp(n), adp(n), odp(n))
      allocate (ach(n), och(n), idx_low(n), idx_default(n))

      o32 = int(codes, int32)
      o64 = int(codes, int64)
      osp = real(codes, sp)
      odp = real(codes, dp)
      och = codes_to_words(codes)

      a32 = o32
      call sort_index(a32, idx_default)
      call check_sort_outcome(error, is_sorted(a32), all(o32(idx_default) == a32), &
                              label//" sort_index int32/int64")
      if (allocated(error)) return

      a32 = o32
      call sort_index(a32, idx_low)
      call check_sort_outcome(error, is_sorted(a32), all(o32(idx_low) == a32), &
                              label//" sort_index int32/int32")
      if (allocated(error)) return

      a64 = o64
      call sort_index(a64, idx_default)
      call check_sort_outcome(error, is_sorted(a64), all(o64(idx_default) == a64), &
                              label//" sort_index int64/int64")
      if (allocated(error)) return

      a64 = o64
      call sort_index(a64, idx_low)
      call check_sort_outcome(error, is_sorted(a64), all(o64(idx_low) == a64), &
                              label//" sort_index int64/int32")
      if (allocated(error)) return

      asp = osp
      call sort_index(asp, idx_default)
      call check_sort_outcome(error, is_sorted(asp), all(abs(osp(idx_default) - asp) <= 0.0_sp), &
                              label//" sort_index sp/int64")
      if (allocated(error)) return

      asp = osp
      call sort_index(asp, idx_low)
      call check_sort_outcome(error, is_sorted(asp), all(abs(osp(idx_low) - asp) <= 0.0_sp), &
                              label//" sort_index sp/int32")
      if (allocated(error)) return

      adp = odp
      call sort_index(adp, idx_default)
      call check_sort_outcome(error, is_sorted(adp), all(abs(odp(idx_default) - adp) <= 0.0_dp), &
                              label//" sort_index dp/int64")
      if (allocated(error)) return

      adp = odp
      call sort_index(adp, idx_low)
      call check_sort_outcome(error, is_sorted(adp), all(abs(odp(idx_low) - adp) <= 0.0_dp), &
                              label//" sort_index dp/int32")
      if (allocated(error)) return

      ach = och
      call sort_index(ach, idx_default)
      call check_sort_outcome(error, is_sorted(ach), all(och(idx_default) == ach), &
                              label//" sort_index char/int64")
      if (allocated(error)) return

      ach = och
      call sort_index(ach, idx_low)
      call check_sort_outcome(error, is_sorted(ach), all(och(idx_low) == ach), &
                              label//" sort_index char/int32")
      if (allocated(error)) return
   end subroutine run_sort_index_checks

   function median_of_three_codes() result(codes)
      !! Introsort samples the first, middle and last element to pick its
      !! pivot. Here the FIRST element is the median of the three, which is
      !! the branch a plain ascending or random input never reaches.
      integer(default_int) :: codes(64)
      integer(default_int) :: i

      do i = 1, 64_default_int
         codes(i) = i
      end do
      codes(1) = 500_default_int
      codes(64) = 900_default_int
   end function median_of_three_codes

   function quicksort_killer_codes() result(codes)
      !! Nearly every element equal, with a handful of smaller and a handful
      !! of larger outliers. Median-of-three keeps picking the repeated value,
      !! so each partition peels off a single element, the recursion depth
      !! limit is exhausted and introsort has to fall back to its heap sort -
      !! with distinct elements still present, so the sift-down really swaps.
      integer(default_int) :: codes(4096)
      integer(default_int) :: i

      codes = 500_default_int
      do i = 1, 8_default_int
         codes(i) = i
         codes(8_default_int + i) = 990_default_int + i
      end do
   end function quicksort_killer_codes

   function block_pair_codes() result(codes)
      !! Two natural runs: a long one holding only large values followed by a
      !! shorter one holding only small values. The merge then copies the
      !! short run into its buffer and runs out of the long run first, which
      !! is the "left run exhausted" tail of the backwards merge.
      integer(default_int) :: codes(128)
      integer(default_int) :: i

      do i = 1, 80_default_int
         codes(i) = 1000_default_int + i
      end do
      do i = 1, 48_default_int
         codes(80_default_int + i) = i
      end do
   end function block_pair_codes

   function run_stack_codes() result(codes)
      !! Seven natural runs whose lengths make the merge-sort run stack
      !! violate its invariant in the one way random data does not: the run
      !! two below the top is shorter than the top, so the collapse has to
      !! merge the lower pair instead of the top pair.
      integer(default_int) :: codes(512)
      integer(default_int), parameter :: lengths(7) = &
                                         [60_default_int, 80_default_int, 100_default_int, 128_default_int, &
                                          48_default_int, 48_default_int, 48_default_int]
      integer(default_int) :: b, i, pos

      pos = 0_default_int
      do b = 1, 7_default_int
         do i = 1, lengths(b)
            pos = pos + 1_default_int
            codes(pos) = i
         end do
      end do
   end function run_stack_codes

   subroutine test_sort_median_of_three(error)
      type(error_type), allocatable, intent(out) :: error

      call run_sort_checks(error, median_of_three_codes(), "median-of-three")
   end subroutine test_sort_median_of_three

   subroutine test_sort_heapsort_fallback(error)
      type(error_type), allocatable, intent(out) :: error

      call run_sort_checks(error, quicksort_killer_codes(), "quicksort-killer")
   end subroutine test_sort_heapsort_fallback

   subroutine test_ord_sort_merge_exhausts_left(error)
      type(error_type), allocatable, intent(out) :: error

      call run_ord_sort_checks(error, block_pair_codes(), "block-pair")
      if (allocated(error)) return

      call run_sort_index_checks(error, block_pair_codes(), "block-pair")
   end subroutine test_ord_sort_merge_exhausts_left

   subroutine test_ord_sort_run_stack_collapse(error)
      type(error_type), allocatable, intent(out) :: error

      call run_ord_sort_checks(error, run_stack_codes(), "run-stack")
      if (allocated(error)) return

      call run_sort_index_checks(error, run_stack_codes(), "run-stack")
   end subroutine test_ord_sort_run_stack_collapse

   subroutine test_radix_sort_mostly_negative(error)
      !! With more negatives than non-negatives the rotation that moves the
      !! negative half back to the front has to search downwards
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: a32(5)
      integer(int64) :: a64(5)
      integer(int32), parameter :: expected32(5) = [-3_int32, -2_int32, -1_int32, 1_int32, 2_int32]
      integer(int64), parameter :: expected64(5) = [-3_int64, -2_int64, -1_int64, 1_int64, 2_int64]

      a32 = [-1_int32, 2_int32, -3_int32, 1_int32, -2_int32]
      call radix_sort(a32)
      call check(error, all(a32 == expected32), "radix_sort(int32) with three negatives")
      if (allocated(error)) return

      a64 = [-1_int64, 2_int64, -3_int64, 1_int64, -2_int64]
      call radix_sort(a64)
      call check(error, all(a64 == expected64), "radix_sort(int64) with three negatives")
      if (allocated(error)) return

      a32 = [-1_int32, 2_int32, -3_int32, 1_int32, -2_int32]
      call radix_sort(a32, reverse=.true.)
      call check(error, all(a32 == expected32(5:1:-1)), "reversed radix_sort(int32) with three negatives")
      if (allocated(error)) return

      a64 = [-1_int64, 2_int64, -3_int64, 1_int64, -2_int64]
      call radix_sort(a64, reverse=.true.)
      call check(error, all(a64 == expected64(5:1:-1)), "reversed radix_sort(int64) with three negatives")
      if (allocated(error)) return
   end subroutine test_radix_sort_mostly_negative

end module test_pic_sorting
