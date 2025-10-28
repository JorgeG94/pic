! SPDX-Identifier: MIT
module pic_test_string_match
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_ascii, only: reverse
   use pic_strings, only: starts_with, ends_with, join
   use pic_string_type, only: string_type
   implicit none
   private
   public :: collect_string_match_tests

contains

   !> Collect all exported unit tests
   subroutine collect_string_match_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("starts_with", test_starts_with), &
                  new_unittest("ends_with", test_ends_with), &
                  new_unittest("join", test_join) &
                  ]
   end subroutine collect_string_match_tests

   subroutine check_starts_with(error, string, substring)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: substring
      logical :: match
      character(len=:), allocatable :: message

      match = index(string, substring) == 1
      if (match) then
         message = "Failed to recognize that '"//string//"' starts with '"//substring//"'"
      else
         message = "Incorrectly found that '"//string//"' starts with '"//substring//"'"
      end if

      call check(error, starts_with(string, substring) .eqv. match, message)
      if (allocated(error)) return
      call check(error, starts_with(string_type(string), substring) .eqv. match, message)
      if (allocated(error)) return
      call check(error, starts_with(string, string_type(substring)) .eqv. match, message)
      if (allocated(error)) return
      call check(error, starts_with(string_type(string), string_type(substring)) .eqv. match, message)
   end subroutine check_starts_with

   subroutine test_starts_with(error)
      type(error_type), allocatable, intent(out) :: error
      call check_starts_with(error, "pattern", "pat")
      if (allocated(error)) return
      call check_starts_with(error, "pat", "pattern")
      if (allocated(error)) return
      call check_starts_with(error, "pattern", "ern")
      if (allocated(error)) return
      call check_starts_with(error, "ern", "pattern")
   end subroutine test_starts_with

   subroutine check_ends_with(error, string, substring)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: string
      character(len=*), intent(in) :: substring
      logical :: match
      character(len=:), allocatable :: message

      match = index(reverse(string), reverse(substring)) == 1
      if (match) then
         message = "Failed to recognize that '"//string//"' ends with '"//substring//"'"
      else
         message = "Incorrectly found that '"//string//"' ends with '"//substring//"'"
      end if

      call check(error, ends_with(string, substring) .eqv. match, message)
      if (allocated(error)) return
      call check(error, ends_with(string_type(string), substring) .eqv. match, message)
      if (allocated(error)) return
      call check(error, ends_with(string, string_type(substring)) .eqv. match, message)
      if (allocated(error)) return
      call check(error, ends_with(string_type(string), string_type(substring)) .eqv. match, message)
   end subroutine check_ends_with

   subroutine test_join(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=5) :: test_strings(3)

      test_strings = [character(5) :: "one", "two", "three"]
      call check_join(error, test_strings, " ", "one two three")
      if (allocated(error)) return
      call check_join(error, test_strings, ",", "one,two,three")
      if (allocated(error)) return
      call check_join(error, test_strings, "-", "one-two-three")
   end subroutine test_join

   subroutine check_join(error, strings, separator, expected)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: strings(:)
      character(len=*), intent(in) :: separator
      character(len=*), intent(in) :: expected
      character(len=:), allocatable :: joined
      character(len=:), allocatable :: message

      joined = join(strings, separator)
      message = "'join' error: Expected '"//expected//"' but got '"//joined//"'"
      call check(error, joined == expected, message)

   end subroutine check_join

   subroutine test_ends_with(error)
      type(error_type), allocatable, intent(out) :: error
      call check_ends_with(error, "pattern", "pat")
      if (allocated(error)) return
      call check_ends_with(error, "pat", "pattern")
      if (allocated(error)) return
      call check_ends_with(error, "pattern", "ern")
      if (allocated(error)) return
      call check_ends_with(error, "ern", "pattern")
   end subroutine test_ends_with

end module pic_test_string_match
