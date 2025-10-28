! SPDX-Identifier: MIT
module pic_test_string_assignment
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: int32, int64, fbool
   use pic_strings, only: to_string
   use pic_string_type, only: string_type, assignment(=), operator(==), slen
   implicit none

   private
   public :: collect_string_assignment

contains

   !> Collect all exported unit tests
   subroutine collect_string_assignment(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("assignment", test_assignment), &
                  new_unittest("constructor", test_constructor) &
                  ]
   end subroutine collect_string_assignment

   subroutine test_assignment(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      type(string_type) :: string

      call check(error, slen(string) == 0)
      if (allocated(error)) return

      string = "Sequence"
      call check(error, slen(string) == 8)
   end subroutine test_assignment

   subroutine test_constructor(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      character(len=128) :: flc

      write (flc, '(g0)') - 1026191
      call check(error, string_type(-1026191) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') 124787
      call check(error, string_type(124787) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') - 8924889_int32
      call check(error, string_type(-8924889_int32) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') 2378405_int32
      call check(error, string_type(2378405_int32) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') 921092378411_int64
      call check(error, string_type(921092378411_int64) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') - 1272835761_int64
      call check(error, string_type(-1272835761_int64) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') .true.
      call check(error, string_type(.true.) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') .false.
      call check(error, string_type(.false.) == trim(flc))
      if (allocated(error)) return

      write (flc, '(g0)') .true._fbool
      call check(error, string_type(.true._fbool) == trim(flc))
   end subroutine test_constructor

end module pic_test_string_assignment
