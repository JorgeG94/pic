module test_pic_sorting
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: sp, dp, int32, int64, default_int
   use pic_sorting, only: sort, sort_index, radix_sort, ord_sort
   use pic_array, only: is_sorted, ascending, DESCENDING, pic_scramble_array
   implicit none
   private
   public :: collect_pic_sorting_tests

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
                  new_unittest("test_index_sort_numeric_tiny", test_index_sort_numeric_tiny) &
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

end module test_pic_sorting
