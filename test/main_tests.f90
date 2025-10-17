program pic_tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
   & select_suite, run_selected, get_argument
   use pic_types, only: int32
   use test_suite1, only: collect_suite1
   use test_pic_string, only: collect_pic_string_tests
   use test_pic_string_type, only: collect_pic_string_type_tests
   use test_pic_timer, only: collect_pic_timers_tests
   use test_pic_logger, only: collect_pic_logger_tests
   use test_pic_flop_recorder, only: collect_pic_flop_recorder_tests
   use test_pic_flop_rate, only: collect_flop_rate_tests
   use test_pic_matrix_printer_v2, only: collect_pic_matrix_printer_v2_tests
   use test_pic_array, only: collect_pic_array_tests
   use test_pic_optional, only: collect_pic_optional_tests
   use test_pic_sorting, only: collect_pic_sorting_tests
   use pic_knowledge, only: get_knowledge
   ! add here the module you want to test
   implicit none
   integer(int32) :: stat, is
   !integer(default_int), parameter :: ntest_suites = 9
    !! number of tests, this number needs to be modified and equal to the number of files we have with unit tests
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: style = '("#", *(1x, a))'

   stat = 0_int32
   allocate (testsuites(11))
   testsuites = [ &
                new_testsuite("base_utils", collect_suite1), &
                new_testsuite("pic_string", collect_pic_string_tests), &
                new_testsuite("pic_string_type", collect_pic_string_type_tests), &
                new_testsuite("pic_timers", collect_pic_timers_tests), &
                new_testsuite("pic_logger", collect_pic_logger_tests), &
                new_testsuite("pic_flop_recorder", collect_pic_flop_recorder_tests), &
                new_testsuite("pic_flop_rate", collect_flop_rate_tests), &
                new_testsuite("pic_matrix_printer_v2", collect_pic_matrix_printer_v2_tests), &
                new_testsuite("pic_array", collect_pic_array_tests), &
                new_testsuite("pic_optional", collect_pic_optional_tests), &
                new_testsuite("pic_sorting", collect_pic_sorting_tests) &
                ]
   ! here you add another test suite to the array

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (error_unit, style) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
            if (stat < 0) then
               error stop 1
            end if
         else
            write (error_unit, style) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write (error_unit, style) "Available testsuites"
         do is = 1, size(testsuites)
            write (error_unit, style) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (error_unit, style) "Testing all:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   if (stat > 0) then
      write (error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
      error stop 1
   end if

   call get_knowledge

end program pic_tester
