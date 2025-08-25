module test_suite1
   use pic_types, only: default_int
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none
   private

   public :: collect_suite1

contains

!> Collect all exported unit tests
   subroutine collect_suite1(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("valid", test_valid), &
                  new_unittest("invalid", test_invalid, should_fail=.true.) &
                  ]

   end subroutine collect_suite1

   subroutine test_valid(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, 1 + 2 == 3)
      if (allocated(error)) return

   end subroutine test_valid

   subroutine test_invalid(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int) :: i
      ! ...
      i = 1

      if (i == 1) then
         call test_failed(error, "Custom check failed")
         return
      end if
   end subroutine test_invalid

end module test_suite1
