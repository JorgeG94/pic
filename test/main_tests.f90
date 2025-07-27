program pic_tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
   & select_suite, run_selected, get_argument
   use pic_types, only: default_int, int32
   use test_suite1, only: collect_suite1
   use test_pic_string_utils, only: collect_pic_string_utils_tests
   use test_pic_timers, only: collect_pic_timers_tests
   use test_pic_logger, only: collect_pic_logger_tests
   use test_pic_flop_recorder, only: collect_pic_flop_recorder_tests
   use test_pic_flop_rate, only: collect_flop_rate_tests
   use test_pic_matrix_printer, only: collect_pic_matrix_printer_tests
   use test_pic_matrix_printer_v2, only: collect_pic_matrix_printer_v2_tests
   use test_pic_array, only: collect_pic_array_tests
   ! add here the module you want to test
   implicit none
   integer(int32) :: stat, is
   integer(default_int), parameter :: ntest_suites = 9
    !! number of tests, this number needs to be modified and equal to the number of files we have with unit tests
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0_int32
   allocate (testsuites(ntest_suites))
   ! here you add another test suite to the array
   testsuites(1) = new_testsuite("base_utils", collect_suite1)
   testsuites(2) = new_testsuite("pic_string_utils", collect_pic_string_utils_tests)
   testsuites(3) = new_testsuite("pic_timers", collect_pic_timers_tests)
   testsuites(4) = new_testsuite("pic_logger", collect_pic_logger_tests)
   testsuites(5) = new_testsuite("pic_flop_recorder", collect_pic_flop_recorder_tests)
   testsuites(6) = new_testsuite("pic_flop_rate", collect_flop_rate_tests)
   testsuites(7) = new_testsuite("pic_matrix_printer", collect_pic_matrix_printer_tests)
   testsuites(8) = new_testsuite("pic_matrix_printer_v2", collect_pic_matrix_printer_v2_tests)
   testsuites(9) = new_testsuite("pic_array", collect_pic_array_tests)

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (error_unit, fmt) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
            if (stat < 0) then
               error stop 1
            end if
         else
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write (error_unit, fmt) "Available testsuites"
         do is = 1, size(testsuites)
            write (error_unit, fmt) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (error_unit, fmt) "Testing all:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   if (stat > 0) then
      write (error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
      error stop 1
   end if

end program pic_tester
