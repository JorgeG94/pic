module test_pic_optional
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_optional_value, only: pic_optional
   use pic_types, only: int32, int64, dp, sp, default_int
   use pic_test_helpers, only: is_equal
   implicit none
   private

   public :: collect_pic_optional_tests

contains

   subroutine collect_pic_optional_tests(testsuite2)
      type(unittest_type), allocatable, intent(out) :: testsuite2(:)
      testsuite2 = [ &
                   new_unittest("test_optional_int32", test_optional_int32), &
                   new_unittest("test_optional_int64", test_optional_int64), &
                   new_unittest("test_optional_sp", test_optional_sp), &
                   new_unittest("test_optional_dp", test_optional_dp), &
                   new_unittest("test_optional_char", test_optional_char), &
                   new_unittest("test_optional_int32_array", test_optional_int32_array), &
                   new_unittest("test_optional_int64_array", test_optional_int64_array), &
                   new_unittest("test_optional_sp_array", test_optional_sp_array), &
                   new_unittest("test_optional_dp_array", test_optional_dp_array) &
                   ]

   end subroutine collect_pic_optional_tests

   function test_int32(x) result(y)
      integer(int32), intent(in), optional :: x
      integer(int32) :: y
      y = pic_optional(x, 2_int32)
   end function test_int32
   function test_int32_array(x) result(y)
      integer(int32), dimension(2), intent(in), optional :: x
      integer(int32), dimension(2) :: y
      y = pic_optional(x, [2_int32, 2_int32])
   end function test_int32_array

   function test_int64(x) result(y)
      integer(int64), intent(in), optional :: x
      integer(int64) :: y
      y = pic_optional(x, 2_int64)
   end function test_int64

   function test_int64_array(x) result(y)
      integer(int64), dimension(2), intent(in), optional :: x
      integer(int64), dimension(2) :: y
      y = pic_optional(x, [2_int64, 2_int64])
   end function test_int64_array

   function test_sp(x) result(y)
      real(sp), intent(in), optional :: x
      real(sp) :: y
      y = pic_optional(x, 2.0_sp)
   end function test_sp

   function test_sp_array(x) result(y)
      real(sp), dimension(2), intent(in), optional :: x
      real(sp), dimension(2) :: y
      y = pic_optional(x, [2.0_sp, 2.0_sp])
   end function test_sp_array

   function test_dp(x) result(y)
      real(dp), intent(in), optional :: x
      real(dp) :: y
      y = pic_optional(x, 2.0_dp)
   end function test_dp

   function test_dp_array(x) result(y)
      real(dp), dimension(2), intent(in), optional :: x
      real(dp), dimension(2) :: y
      y = pic_optional(x, [2.0_dp, 2.0_dp])
   end function test_dp_array

   function test_char(x) result(y)
      character(len=*), intent(in), optional :: x
      character(len=:), allocatable :: y

      y = pic_optional(x, "z")
   end function test_char

   subroutine test_optional_int32(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, test_int32(1_int32) == 1_int32)
      if (allocated(error)) return
      call check(error, test_int32() == 2_int32)
      if (allocated(error)) return

   end subroutine test_optional_int32

   subroutine test_optional_int64(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, test_int64(1_int64) == 1_int64)
      if (allocated(error)) return
      call check(error, test_int64() == 2_int64)
      if (allocated(error)) return

   end subroutine test_optional_int64

   subroutine test_optional_sp(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, is_equal(test_sp(1.0_sp), 1.0_sp))
      if (allocated(error)) return
      call check(error, is_equal(test_sp(), 2.0_sp))
      if (allocated(error)) return

   end subroutine test_optional_sp

   subroutine test_optional_dp(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, is_equal(test_dp(1.0_dp), 1.0_dp))
      if (allocated(error)) return
      call check(error, is_equal(test_dp(), 2.0_dp))
      if (allocated(error)) return

   end subroutine test_optional_dp

   subroutine test_optional_char(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, test_char("h") == "h")
      if (allocated(error)) return
      call check(error, test_char() == "z")
      if (allocated(error)) return

   end subroutine test_optional_char

   subroutine test_optional_int32_array(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, all(test_int32_array([1_int32, 2_int32]) == [1_int32, 2_int32]))
      if (allocated(error)) return
      call check(error, all(test_int32_array() == [2_int32, 2_int32]))
      if (allocated(error)) return

   end subroutine test_optional_int32_array

   subroutine test_optional_int64_array(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, all(test_int64_array([1_int64, 2_int64]) == [1_int64, 2_int64]))
      if (allocated(error)) return
      call check(error, all(test_int64_array() == [2_int64, 2_int64]))
      if (allocated(error)) return

   end subroutine test_optional_int64_array

   subroutine test_optional_sp_array(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, all(is_equal(test_sp_array([1.0_sp, 2.0_sp]), [1.0_sp, 2.0_sp])))
      if (allocated(error)) return
      call check(error, all(is_equal(test_sp_array(), [2.0_sp, 2.0_sp])))
      if (allocated(error)) return

   end subroutine test_optional_sp_array

   subroutine test_optional_dp_array(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, all(is_equal(test_dp_array([1.0_dp, 2.0_dp]), [1.0_dp, 2.0_dp])))
      if (allocated(error)) return
      call check(error, all(is_equal(test_dp_array(), [2.0_dp, 2.0_dp])))
      if (allocated(error)) return

   end subroutine test_optional_dp_array

end module test_pic_optional
