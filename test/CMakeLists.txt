# Unit testing
set(tests
    "base_utils"
    "pic_string"
    "pic_string_assignment"
    "pic_string_functions"
    "pic_string_intrinsic"
    "pic_string_match"
    "pic_string_operator"
    "pic_string_strip_chomp"
    "pic_string_to_string"
    "pic_ascii"
    "pic_timers"
    "pic_logger"
    "pic_flop_recorder"
    "pic_flop_rate"
    "pic_matrix_printer_v2"
    "pic_array"
    "pic_optional"
    "pic_hash"
    "pic_sorting")
set(pre_proc_tests "pic_string_derivedtype_io")

find_package(Threads REQUIRED)
set(test-srcs "main_tests.f90")

foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t)
  list(APPEND test-srcs "test_${t}.f90")
endforeach()
foreach(t IN LISTS pre_proc_tests)
  string(MAKE_C_IDENTIFIER ${t} t)
  list(APPEND test-srcs "test_${t}.F90")
endforeach()
list(APPEND tests ${pre_proc_tests})

add_executable("${project_name}-tester" "${test-srcs}")

set_target_properties(
  "${project_name}-tester"
  PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
             Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/tests/modules")
target_include_directories("${project_name}-tester"
                           PRIVATE "${CMAKE_BINARY_DIR}/modules")
target_link_libraries(
  "${project_name}-tester" PRIVATE "${main_lib}" "${core_lib}"
                                   test-drive::test-drive)

message(STATUS "Adding the following tests to ctests...")
foreach(t IN LISTS tests)
  message(STATUS "${project_name}/${t} ${project_name}-tester ${t}")
  add_test("${project_name}/${t}" "${project_name}-tester" "${t}")
endforeach()
