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
                   new_unittest("test_pic_string_type_basic_ops_assign", test_pic_string_type_basic_ops_assign) &
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

end module test_pic_string_type
