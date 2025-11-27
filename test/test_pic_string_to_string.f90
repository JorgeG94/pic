! SPDX-Identifier: MIT
module pic_test_string_to_string

   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_strings, only: to_string, to_c_char, starts_with
   use pic_optional_value, only: pic_optional
   use pic_types, only: sp, dp, int64

   implicit none
   private
   public :: collect_string_to_string_tests

contains

   !> Collect all exported unit tests
   subroutine collect_string_to_string_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("to_string-integer", test_to_string_integer), &
                  new_unittest("to_string-logical", test_to_string_logical), &
                  new_unittest("to_string-real", test_to_string_real), &
                  new_unittest("to_string-limit-i4", test_string_i4), &
                  new_unittest("to_string-limit-i8", test_string_i8) &
                  ]
   end subroutine collect_string_to_string_tests

   subroutine check_formatter(error, actual, expected, description, partial)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: actual, expected, description
      logical, intent(in), optional :: partial
      logical :: stat
      character(len=:), allocatable :: msg

      if (pic_optional(partial, .false.)) then
         stat = starts_with(actual, expected)
      else
         stat = actual == expected
      end if

      if (.not. stat) then
         msg = description//new_line("a")// &
             & "Expected: '"//expected//"' but got '"//actual//"'"
      else
         print '(" - ", a, /, "   Result: ''", a, "''")', description, actual
      end if

      call check(error, stat, msg)

   end subroutine check_formatter

   subroutine test_to_string_integer(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check_formatter(error, to_string(100_int64), "100", &
          & "Default formatter for integer number")
      if (allocated(error)) return
      call check_formatter(error, to_string(100_int64, 'I6'), "   100", &
          & "Formatter for integer number")
      if (allocated(error)) return
      call check_formatter(error, to_string(100), "100", &
          & "Default formatter for integer number")
      if (allocated(error)) return
      call check_formatter(error, to_string(100, 'I6'), "   100", &
          & "Formatter for integer number")
      if (allocated(error)) return
      call check_formatter(error, to_string(100, 'I0.6'), "000100", &
          & "Formatter with zero padding for integer number")
      if (allocated(error)) return
      call check_formatter(error, to_string(100, 'I6')//to_string(1000, '(I7)'), &
          & "   100   1000", "Multiple formatters for integers")
      if (allocated(error)) return
      call check_formatter(error, to_string(34, 'B8'), "  100010", &
          & "Binary formatter for integer number")
      if (allocated(error)) return
      call check_formatter(error, to_string(34, 'O0.3'), "042", &
          & "Octal formatter with zero padding for integer number")
      if (allocated(error)) return
      call check_formatter(error, to_string(34, 'Z3'), " 22", &
          & "Hexadecimal formatter for integer number")

   end subroutine test_to_string_integer

   subroutine test_to_string_real(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check_formatter(error, to_string(100.0_sp), "100.", &
          & "Default formatter for real number", partial=.true.)
      if (allocated(error)) return
      call check_formatter(error, to_string(100._sp, 'F6.2'), "100.00", &
          & "Formatter for real number")
      if (allocated(error)) return

      call check_formatter(error, to_string(100.0_dp), "100.", &
          & "Default formatter for real number", partial=.true.)
      if (allocated(error)) return
      call check_formatter(error, to_string(100._dp, 'F6.2'), "100.00", &
          & "Formatter for real number")
      if (allocated(error)) return
      call check_formatter(error, to_string(289._dp, 'E7.2'), ".29E+03", &
          & "Exponential formatter with rounding for real number")
      if (allocated(error)) return
      call check_formatter(error, to_string(128._dp, 'ES8.2'), "1.28E+02", &
          & "Exponential formatter for real number")
      if (allocated(error)) return

      ! Wrong demonstration
      call check_formatter(error, to_string(-100._dp, 'F6.2'), "*", &
          & "Too narrow formatter for signed real number", partial=.true.)
      if (allocated(error)) return
      call check_formatter(error, to_string(1000._dp, 'F6.3'), "*", &
          & "Too narrow formatter for real number", partial=.true.)
      if (allocated(error)) return
      call check_formatter(error, to_string(1000._dp, '7.3'), "[*]", &
          & "Invalid formatter for real number", partial=.true.)
      if (allocated(error)) return

   end subroutine test_to_string_real

   subroutine test_to_string_logical(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check_formatter(error, to_string(.true.), "T", &
          & "Default formatter for logcal value")
      if (allocated(error)) return
      call check_formatter(error, to_string(.true., 'L2'), " T", &
          & "Formatter for logical value")
      if (allocated(error)) return
      call check_formatter(error, to_string(.false., 'L2')//to_string(.true., '(L5)'), &
          & " F    T", "Multiple formatters for logical values")
      if (allocated(error)) return

      ! Wrong demonstration
      call check_formatter(error, to_string(.false., '1x'), "[*]", &
          & "Invalid formatter for logical value", partial=.true.)

   end subroutine test_to_string_logical

   subroutine test_string_i4(error)
      use pic_types, only: i4 => int32

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string(-huge(1_i4) - 1_i4), "-2147483648")
   end subroutine test_string_i4

   subroutine test_string_i8(error)
      use pic_types, only: i8 => int64

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string(-huge(1_i8) - 1_i8), "-9223372036854775808")
   end subroutine test_string_i8

end module pic_test_string_to_string


program tester_pic_string_to_string
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type
   use pic_test_string_to_string, only: collect_string_to_string_tests
   implicit none
   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
      new_testsuite("pic_string_to_string", collect_string_to_string_tests) &
      ]

   do is = 1, size(testsuites)
      write(error_unit, fmt) "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if
end program tester_pic_string_to_string
