module test_pic_string_type
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_string_mod, only: pic_string_type
   use pic_types, only: int64
   implicit none
   private

   public :: collect_pic_string_type_tests

contains

   subroutine collect_pic_string_type_tests(testsuite2)
      type(unittest_type), allocatable, intent(out) :: testsuite2(:)
      testsuite2 = [ &
                   new_unittest("test_pic_string_type_basic", test_pic_string_type_basic), &
                   new_unittest("test_pic_string_type_empty", test_pic_string_type_empty), &
                   new_unittest("test_pic_string_type_assign", test_pic_string_type_assign), &
                   new_unittest("test_pic_string_type_append_to_append", test_pic_string_type_append_to_append), &
                   new_unittest("test_pic_string_type_basic_ops_assign", test_pic_string_type_basic_ops_assign), &
                   new_unittest("test_pic_string_type_push_back_chars", test_pic_string_type_push_back_chars), &
                   new_unittest("test_pic_string_type_reserve_growth", test_pic_string_type_reserve_growth), &
                   new_unittest("test_pic_string_type_clear_keeps_cap", test_pic_string_type_clear_keeps_cap), &
                   new_unittest("test_pic_string_type_to_char_roundtrip", test_pic_string_type_to_char_roundtrip) &
                   ]
   end subroutine collect_pic_string_type_tests

   subroutine test_pic_string_type_basic(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s

      call check(error, s%size() == 0_int64, "Initial size should be 0")
      if (allocated(error)) return

      call check(error, s%capacity() == 0_int64, "Initial capacity should be 0")
      if (allocated(error)) return

   end subroutine test_pic_string_type_basic

   subroutine test_pic_string_type_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

   end subroutine test_pic_string_type_empty

   subroutine test_pic_string_type_assign(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

      call s%assign("Hello")
      call check(error,.not. s%empty(), "String should not be empty after assign")
      if (allocated(error)) return

   end subroutine test_pic_string_type_assign

   subroutine test_pic_string_type_basic_ops_assign(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      character(len=:), allocatable :: c

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

      call s%assign("Hello")
      call check(error,.not. s%empty(), "String should not be empty after assign")
      if (allocated(error)) return

      call check(error, s%size() == 5_int64, "Size should be 5 after assign")
      if (allocated(error)) return

   end subroutine test_pic_string_type_basic_ops_assign

   subroutine test_pic_string_type_append_to_append(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      character(len=:), allocatable :: c

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

      call s%assign("Hello")
      call check(error,.not. s%empty(), "String should not be empty after assign")
      if (allocated(error)) return

      call check(error, s%size() == 5_int64, "Size should be 5 after assign")
      if (allocated(error)) return

      call s%append(", World!")
      call check(error, s%size() == 13_int64, "Size should be 13 after append")
      if (allocated(error)) return

   end subroutine test_pic_string_type_append_to_append

   subroutine test_pic_string_type_push_back_chars(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      call s%push_back('A')
      call s%push_back('B')
      call s%push_back('C')
      call check(error, s%size() == 3, "size after three push_back")
      if (allocated(error)) return
      call check(error, s%to_char() == "ABC", "content after three push_back")
   end subroutine test_pic_string_type_push_back_chars

   subroutine test_pic_string_type_reserve_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      integer :: cap_before, cap_after
      call s%reserve(10_int64)
      cap_before = s%capacity()
      call check(error, cap_before >= 10_int64, "reserve(10) should give >=10 capacity")
      if (allocated(error)) return
      call s%append("0123456789")  ! length 10 exactly
      call check(error, s%size() == 10_int64, "size should be 10 after append")
      if (allocated(error)) return
      cap_after = s%capacity()
      call check(error, cap_after >= cap_before, "capacity should not shrink during append")
      if (allocated(error)) return
      call s%append("X")  ! trigger growth if cap == 10
      call check(error, s%size() == 11, "size should be 11 after one more char")
      if (allocated(error)) return
      call check(error, s%to_char() == "0123456789X", "content after growth-append")
   end subroutine test_pic_string_type_reserve_growth

   subroutine test_pic_string_type_clear_keeps_cap(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      integer :: cap_before
      call s%assign("some text")
      cap_before = s%capacity()
      call s%clear()
      call check(error, s%size() == 0, "clear sets size to 0")
      if (allocated(error)) return
      call check(error, s%capacity() == cap_before, "clear should keep capacity")
      if (allocated(error)) return
      call s%append("x")
      call check(error, s%to_char() == "x", "content valid after clear+append")
   end subroutine test_pic_string_type_clear_keeps_cap

   subroutine test_pic_string_type_to_char_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      character(:), allocatable :: c
      call s%assign("roundtrip")
      c = s%to_char()
      call check(error, c == "roundtrip", "to_char returns identical contents")
   end subroutine test_pic_string_type_to_char_roundtrip

end module test_pic_string_type
