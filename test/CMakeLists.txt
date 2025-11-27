# Unit testing

find_package(Threads REQUIRED)

# Macro to add a test executable for .f90 files
macro(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)
    set_target_properties(test_${name}
        PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/tests/modules/${name}")
    target_include_directories(test_${name} PRIVATE "${CMAKE_BINARY_DIR}/modules")
    target_link_libraries(test_${name} PRIVATE "${main_lib}" "${core_lib}" "test-drive::test-drive")
    add_test(NAME ${project_name}/${name}
             COMMAND $<TARGET_FILE:test_${name}>
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTEST)

# Macro to add a test executable for preprocessed .F90 files
macro(ADDTESTPP name)
    add_executable(test_${name} test_${name}.F90)
    set_target_properties(test_${name}
        PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/tests/modules/${name}")
    target_include_directories(test_${name} PRIVATE "${CMAKE_BINARY_DIR}/modules")
    target_link_libraries(test_${name} PRIVATE "${main_lib}" "${core_lib}" "test-drive::test-drive")
    add_test(NAME ${project_name}/${name}
             COMMAND $<TARGET_FILE:test_${name}>
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTESTPP)

# Add individual test executables
message(STATUS "Adding individual test executables...")

# Regular tests (.f90)
ADDTEST(base_utils)
ADDTEST(pic_string)
ADDTEST(pic_string_assignment)
ADDTEST(pic_string_functions)
ADDTEST(pic_string_intrinsic)
ADDTEST(pic_string_match)
ADDTEST(pic_string_operator)
ADDTEST(pic_string_strip_chomp)
ADDTEST(pic_string_to_string)
ADDTEST(pic_ascii)
ADDTEST(pic_timers)
ADDTEST(pic_logger)
ADDTEST(pic_flop_recorder)
ADDTEST(pic_flop_rate)
ADDTEST(pic_matrix_printer_v2)
ADDTEST(pic_array)
ADDTEST(pic_optional)
ADDTEST(pic_hash)
ADDTEST(pic_sorting)

# Preprocessed tests (.F90)
ADDTESTPP(pic_string_derivedtype_io)
