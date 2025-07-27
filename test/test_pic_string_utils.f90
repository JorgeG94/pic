module test_pic_string_utils
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_string_utils, only: to_string, set_precision, get_precision, pad, &
                               to_upper
   use pic_types, only: int32, int64, dp, sp, default_int
   implicit none
   private

   public :: collect_pic_string_utils_tests

contains

   subroutine collect_pic_string_utils_tests(testsuite2)
      type(unittest_type), allocatable, intent(out) :: testsuite2(:)
      integer(default_int), parameter :: ntests = 11
      allocate (testsuite2(ntests))
      testsuite2(1) = new_unittest("test_to_string_int32", test_to_string_int32)
      testsuite2(2) = new_unittest("test_to_string_int64", test_to_string_int64)
      testsuite2(3) = new_unittest("test_to_string_dp", test_to_string_dp)
      testsuite2(4) = new_unittest("test_to_string_char", test_to_string_char)
      testsuite2(5) = new_unittest("test_to_string_logical", test_to_string_logical)
      testsuite2(6) = new_unittest("test_to_string_sp", test_to_string_sp)
      testsuite2(7) = new_unittest("test_set_get_precision", test_set_get_precision)
      testsuite2(8) = new_unittest("test_write_with_precision_sp", test_write_with_precision_sp)
      testsuite2(9) = new_unittest("test_write_with_precision_dp", test_write_with_precision_dp)
      testsuite2(10) = new_unittest("test_padding", test_padding)
      testsuite2(11) = new_unittest("test_to_upper", test_to_upper)

   end subroutine collect_pic_string_utils_tests

   subroutine test_to_string_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_string(int(123, kind=int32))
      call check(error, result == "123")
      if (allocated(error)) return
   end subroutine test_to_string_int32

   subroutine test_to_string_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_string(int(12345678, kind=int64))
      call check(error, result == "12345678")
      if (allocated(error)) return
   end subroutine test_to_string_int64

   subroutine test_to_string_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_string(123.456_dp)
      call check(error, result == "123.456000000000")
      if (allocated(error)) return
   end subroutine test_to_string_dp

   subroutine test_to_string_sp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result

      result = to_string(123.456_sp)
      call check(error, result == "123.456001281738")
      if (allocated(error)) return
   end subroutine test_to_string_sp

   subroutine test_to_string_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result

      result = to_string("Hello, world!")
      call check(error, result == "Hello, world!")
      if (allocated(error)) return
   end subroutine test_to_string_char

   subroutine test_to_string_logical(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=5) :: result

      result = to_string(.true.)
      call check(error, result == "TRUE")
      if (allocated(error)) return

      result = to_string(.false.)
      call check(error, result == "FALSE")
      if (allocated(error)) return
   end subroutine test_to_string_logical

   subroutine test_set_get_precision(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int) :: old_precision, new_precision
      integer(default_int), parameter :: ten = 10_default_int

      old_precision = get_precision()
      call check(error, old_precision > 0, "Initial precision should be positive")
      new_precision = default_int
      call set_precision(new_precision)
      call check(error, get_precision() == new_precision)
      if (allocated(error)) return

      call set_precision(old_precision)  ! Restore original precision
   end subroutine test_set_get_precision

   subroutine test_write_with_precision_sp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result
      real(kind=sp), parameter :: value = 123.456_sp
      integer(default_int) :: expected_len
      integer(default_int), parameter :: six = 6_default_int

      call set_precision(six)  ! Set precision for testing
      result = to_string(value)
      expected_len = len_trim(result)
      ! 6 decimal places + 1 for the decimal point + 3 for the number to the right of the decimal point
      call check(error, expected_len == 10, "Expected length of string representation is 10")

      if (allocated(error)) return
   end subroutine test_write_with_precision_sp

   subroutine test_write_with_precision_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=50) :: result
      real(kind=dp), parameter :: value = 123.456_dp
      integer(default_int) :: expected_len
      integer(default_int), parameter :: six = 6_default_int

      call set_precision(six)  ! Set precision for testing
      result = to_string(value)
      expected_len = len_trim(result)
      print *, "Result: ", expected_len, result
      ! 6 decimal places + 1 for the decimal point + 3 for the number to the right of the decimal point
      call check(error, expected_len == 10, "Expected length of string representation is 10")

      if (allocated(error)) return
   end subroutine test_write_with_precision_dp

   subroutine test_padding(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: result
      real(kind=dp), parameter :: value = 123.456_dp
      integer(default_int) :: expected_len
      integer(default_int), parameter :: three = 3_default_int
      integer(default_int), parameter :: fifteen = 15_default_int

      call set_precision(three)
      result = to_string(value)
      result = pad(result, fifteen)
      expected_len = len_trim(result)
      print *, "Result: ", to_string(expected_len)//to_string(result)
      call check(error, expected_len == fifteen, "expected length of string is 15")
      if (allocated(error)) return
   end subroutine test_padding

   subroutine test_to_upper(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=500) :: result

      result = to_upper("Hello, world!")
      call check(error, result == "HELLO, WORLD!")
      if (allocated(error)) return

   end subroutine test_to_upper

end module test_pic_string_utils
