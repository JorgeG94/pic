module test_pic_string_utils
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_string_utils, only: to_string
   use pic_types, only: int32, int64, dp
   implicit none
   private

   public :: collect_pic_string_utils_tests

contains

   subroutine collect_pic_string_utils_tests(testsuite2)
      type(unittest_type), allocatable, intent(out) :: testsuite2(:)
      integer, parameter :: ntests = 5
      allocate (testsuite2(ntests))
      testsuite2(1) = new_unittest("test_to_string_int32", test_to_string_int32)
      testsuite2(2) = new_unittest("test_to_string_int64", test_to_string_int64)
      testsuite2(3) = new_unittest("test_to_string_dp", test_to_string_dp)
      testsuite2(4) = new_unittest("test_to_string_char", test_to_string_char)
      testsuite2(5) = new_unittest("test_to_string_logical", test_to_string_logical)

   end subroutine collect_pic_string_utils_tests

   subroutine test_to_string_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_string(int(123, kind=int32))
      call check(error, result == "123")
   end subroutine test_to_string_int32

   subroutine test_to_string_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_string(int(12345678, kind=int64))
      call check(error, result == "12345678")
   end subroutine test_to_string_int64

   subroutine test_to_string_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_string(123.456_dp)
      call check(error, result == "123.456000000000")
   end subroutine test_to_string_dp

   subroutine test_to_string_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result

      result = to_string("Hello, world!")
      call check(error, result == "Hello, world!")
   end subroutine test_to_string_char

   subroutine test_to_string_logical(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=5) :: result

      result = to_string(.true.)
      call check(error, result == "TRUE")
      if (allocated(error)) return

      result = to_string(.false.)
      call check(error, result == "FALSE")
   end subroutine test_to_string_logical

end module test_pic_string_utils
