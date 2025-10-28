program pic_tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use pic_types, only: int32
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
   !integer(default_int), parameter :: ntest_suites = 10
    !! number of tests, this number needs to be modified and equal to the number of files we have with unit tests
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: style = '("#", *(1x, a))'

   stat = 0_int32
   !allocate (testsuites(ntest_suites))
   ! here you add another test suite to the array
   testsuites = [ &
                new_testsuite("pic_blas_sgemm", collect_pic_sgemm_tests), &
                new_testsuite("pic_blas_dgemm", collect_pic_dgemm_tests), &
                new_testsuite("pic_blas_sgemv", collect_pic_sgemv_tests), &
                new_testsuite("pic_blas_dgemv", collect_pic_dgemv_tests), &
                new_testsuite("pic_blas_asum", collect_pic_asum_tests), &
                new_testsuite("pic_blas_axpy", collect_pic_axpy_tests), &
                new_testsuite("pic_blas_copy", collect_pic_copy_tests), &
                new_testsuite("pic_blas_dot", collect_pic_blas_dot_tests), &
                new_testsuite("pic_blas_scal", collect_pic_scal_tests), &
                new_testsuite("pic_blas_iamax", collect_pic_blas_iamax_tests) &
                ]

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

end program pic_tester
