! SPDX-Identifer: MIT
module pic_test_string_operator
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_string_type, only: string_type, assignment(=), slen, &
                              operator(>), operator(<), operator(>=), operator(<=), &
                              operator(/=), operator(==), operator(//)
   implicit none
   private
   public :: collect_string_operator_tests

contains

   !> Collect all exported unit tests
   subroutine collect_string_operator_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("gt", test_gt), &
                  new_unittest("lt", test_lt), &
                  new_unittest("ge", test_ge), &
                  new_unittest("le", test_le), &
                  new_unittest("eq", test_eq), &
                  new_unittest("ne", test_ne), &
                  new_unittest("concat", test_concat) &
                  ]
   end subroutine collect_string_operator_tests

   subroutine test_gt(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      logical :: res

      string = "bcd"
      res = string > "abc"
      call check(error, res .eqv. .true.)
      if (allocated(error)) return

      res = string > "bcd"
      call check(error, res .eqv. .false.)
      if (allocated(error)) return

      res = string > "cde"
      call check(error, res .eqv. .false.)
      block
         type(string_type) :: string_2
         string_2 = "hamster"
         res = string > string_2
         call check(error, res .eqv. .false.)
      end block
   end subroutine test_gt

   subroutine test_lt(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      logical :: res

      string = "bcd"
      res = string < "abc"
      call check(error, res .eqv. .false.)
      if (allocated(error)) return

      res = string < "bcd"
      call check(error, res .eqv. .false.)
      if (allocated(error)) return

      res = string < "cde"
      call check(error, res .eqv. .true.)

      res = "cde" < string
      call check(error, res .eqv. .false.)

      block
         type(string_type) :: string_2
         string_2 = "hamster"
         res = string < string_2
         call check(error, res .eqv. .true.)
      end block
   end subroutine test_lt

   subroutine test_ge(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      logical :: res

      string = "bcd"
      res = string >= "abc"
      call check(error, res .eqv. .true.)
      if (allocated(error)) return

      res = string >= "bcd"
      call check(error, res .eqv. .true.)
      if (allocated(error)) return

      res = string >= "cde"
      call check(error, res .eqv. .false.)

      res = "cdee" >= string
      call check(error, res .eqv. .true.)

      block
         type(string_type) :: string_2
         string_2 = "hamster"
         res = string_2 >= string
         call check(error, res .eqv. .true.)
      end block
   end subroutine test_ge

   subroutine test_le(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      logical :: res

      string = "bcd"
      res = string <= "abc"
      call check(error, res .eqv. .false.)
      if (allocated(error)) return

      res = string <= "bcd"
      call check(error, res .eqv. .true.)
      if (allocated(error)) return

      res = string <= "cde"
      call check(error, res .eqv. .true.)

      block
         type(string_type) :: string_2
         string_2 = "hamster"
         res = string_2 <= string
         call check(error, res .eqv. .false.)
      end block
   end subroutine test_le

   subroutine test_eq(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      logical :: res

      string = "bcd"
      res = string == "abc"
      call check(error, res .eqv. .false.)
      if (allocated(error)) return

      res = string == "bcd"
      call check(error, res .eqv. .true.)
      if (allocated(error)) return

      res = string == "cde"
      call check(error, res .eqv. .false.)

      res = "cde" == string
      call check(error, res .eqv. .false.)

   end subroutine test_eq

   subroutine test_ne(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      type(string_type) :: string_2
      logical :: res

      string = "bcd"
      string_2 = "shasta"
      res = string /= "abc"
      call check(error, res .eqv. .true.)
      if (allocated(error)) return

      res = string /= "bcd"
      call check(error, res .eqv. .false.)
      if (allocated(error)) return

      res = string /= "cde"
      call check(error, res .eqv. .true.)

      res = "cde" /= string
      call check(error, res .eqv. .true.)

      res = string /= string_2
      call check(error, res .eqv. .true.)
   end subroutine test_ne

   subroutine test_concat(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string

      string = "Hello, "
      string = string//"World!"
      call check(error, slen(string) == 13)
      block
         type(string_type) :: string_2
         string_2 = "hamster, "
         string = string//string_2
         print *, slen(string)
         call check(error, slen(string) == 22)
      end block

      string = "HII, "
      string = "THERE"//string
      call check(error, slen(string) == 10)

   end subroutine test_concat

end module pic_test_string_operator
