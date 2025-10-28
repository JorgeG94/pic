# Unit testing
set(blas_tests
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

find_package(Threads REQUIRED)
set(blas-test-srcs "blas_tests.f90")
foreach(t IN LISTS blas_tests)
  string(MAKE_C_IDENTIFIER ${t} t)
  list(APPEND blas-test-srcs "test_${t}.f90")
endforeach()

add_executable("${project_name}-blas-tester" "${blas-test-srcs}")

set_target_properties(
  "${project_name}-blas-tester"
  PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
             Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/test/modules")
target_include_directories("${project_name}-blas-tester"
                           PRIVATE "${CMAKE_BINARY_DIR}/modules")
target_link_libraries(
  "${project_name}-blas-tester" PRIVATE "${main_lib}" test-drive::test-drive
                                        Threads::Threads)

message(STATUS "Adding the following tests to ctests...")
message(STATUS "Adding the following blas tests to ctests...")
foreach(t IN LISTS blas_tests)
  message(STATUS "${project_name}-blas/${t} ${project_name}-blas-tester ${t}")
  add_test("${project_name}-blas/${t}" "${project_name}-blas-tester" "${t}")
endforeach()
