program pic_tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use pic_types, only: default_int, int32
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
   & select_suite, run_selected, get_argument
   use test_pic_blas_interfaces_sgemm, only: collect_pic_sgemm_tests
   use test_pic_blas_interfaces_dgemm, only: collect_pic_dgemm_tests
   use test_pic_blas_interfaces_sgemv, only: collect_pic_sgemv_tests
   use test_pic_blas_interfaces_dgemv, only: collect_pic_dgemv_tests
   use test_pic_blas_interfaces_asum, only: collect_pic_asum_tests
   use test_pic_blas_interfaces_axpy, only: collect_pic_axpy_tests
   use test_pic_blas_interfaces_copy, only: collect_pic_copy_tests
   use test_pic_blas_interfaces_dot, only: collect_pic_blas_dot_tests
   use test_pic_blas_interfaces_scal, only: collect_pic_scal_tests
   use test_pic_blas_iamax, only: collect_pic_blas_iamax_tests
   ! add here the module you want to test
   implicit none
   integer(int32) :: stat, is
   integer(default_int), parameter :: ntest_suites = 10
    !! number of tests, this number needs to be modified and equal to the number of files we have with unit tests
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0_int32
   allocate (testsuites(ntest_suites))
   ! here you add another test suite to the array
   testsuites(1) = new_testsuite("pic_blas_sgemm", collect_pic_sgemm_tests)
   testsuites(2) = new_testsuite("pic_blas_dgemm", collect_pic_dgemm_tests)
   testsuites(3) = new_testsuite("pic_blas_sgemv", collect_pic_sgemv_tests)
   testsuites(4) = new_testsuite("pic_blas_dgemv", collect_pic_dgemv_tests)
   testsuites(5) = new_testsuite("pic_blas_asum", collect_pic_asum_tests)
   testsuites(6) = new_testsuite("pic_blas_axpy", collect_pic_axpy_tests)
   testsuites(7) = new_testsuite("pic_blas_copy", collect_pic_copy_tests)
   testsuites(8) = new_testsuite("pic_blas_dot", collect_pic_blas_dot_tests)
   testsuites(9) = new_testsuite("pic_blas_scal", collect_pic_scal_tests)
   testsuites(10) = new_testsuite("pic_blas_iamax", collect_pic_blas_iamax_tests)

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
