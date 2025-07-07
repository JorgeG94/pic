# Unit testing
set(tests
    "base_utils"
    "pic_string_utils"
    "pic_timers"
    "pic_logger"
    "pic_flop_recorder"
    "pic_flop_rate"
    "pic_blas_sgemm"
    "pic_blas_dgemm"
    "pic_blas_dgemv"
    "pic_blas_sgemv"
    "pic_blas_asum"
    "pic_blas_axpy"
    "pic_blas_copy"
    "pic_blas_dot"
    "pic_blas_scal"
    "pic_blas_iamax")

set(test-srcs "main_tests.f90")
foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t)
  list(APPEND test-srcs "test_${t}.f90")
endforeach()
list(APPEND test-srcs helpers.f90)

add_executable("${project_name}-tester" "${test-srcs}")
set_target_properties(
  "${project_name}-tester"
  PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
             Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")
target_include_directories("${project_name}-tester"
                           PRIVATE "${CMAKE_BINARY_DIR}")
target_link_libraries("${project_name}-tester" PRIVATE "${core_lib}"
                                                       test-drive::test-drive)

message(STATUS "Adding the following tests to ctests...")
foreach(t IN LISTS tests)
  message(STATUS "${project_name}/${t} ${project_name}-tester ${t}")
  add_test("${project_name}/${t}" "${project_name}-tester" "${t}")
endforeach()
