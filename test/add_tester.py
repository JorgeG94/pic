#!/usr/bin/env python3
import re
import sys
import os

# Mapping of file -> (module, collect_func, suite_name)
mappings = {
    'test_base_utils.f90': ('test_suite1', 'collect_suite1', 'base_utils'),
    'test_pic_string.f90': ('test_pic_string', 'collect_pic_string_tests', 'pic_string'),
    'test_pic_string_assignment.f90': ('pic_test_string_assignment', 'collect_string_assignment', 'pic_string_assignment'),
    'test_pic_string_functions.f90': ('pic_test_string_functions', 'collect_string_function_tests', 'pic_string_functions'),
    'test_pic_string_intrinsic.f90': ('pic_test_string_intrinsic', 'collect_string_intrinsic_tests', 'pic_string_intrinsic'),
    'test_pic_string_match.f90': ('pic_test_string_match', 'collect_string_match_tests', 'pic_string_match'),
    'test_pic_string_operator.f90': ('pic_test_string_operator', 'collect_string_operator_tests', 'pic_string_operator'),
    'test_pic_string_strip_chomp.f90': ('pic_test_strip_chomp', 'collect_strip_chomp_tests', 'pic_string_strip_chomp'),
    'test_pic_string_derivedtype_io.F90': ('pic_test_string_derivedtype_io', 'collect_string_derivedtype_io_tests', 'pic_string_derivedtype_io'),
    'test_pic_timers.f90': ('test_pic_timer', 'collect_pic_timers_tests', 'pic_timers'),
    'test_pic_logger.f90': ('test_pic_logger', 'collect_pic_logger_tests', 'pic_logger'),
    'test_pic_flop_recorder.f90': ('test_pic_flop_recorder', 'collect_pic_flop_recorder_tests', 'pic_flop_recorder'),
    'test_pic_flop_rate.f90': ('test_pic_flop_rate', 'collect_flop_rate_tests', 'pic_flop_rate'),
    'test_pic_matrix_printer_v2.f90': ('test_pic_matrix_printer_v2', 'collect_pic_matrix_printer_v2_tests', 'pic_matrix_printer_v2'),
    'test_pic_optional.f90': ('test_pic_optional', 'collect_pic_optional_tests', 'pic_optional'),
    'test_pic_sorting.f90': ('test_pic_sorting', 'collect_pic_sorting_tests', 'pic_sorting'),
    'test_pic_hash.f90': ('test_pic_hash', 'collect_pic_hash_tests', 'pic_hash'),
    'test_pic_ascii.f90': ('test_pic_ascii', 'collect_pic_ascii_tests', 'pic_ascii'),
}

def add_tester(filepath, module_name, collect_func, suite_name):
    with open(filepath, 'r') as f:
        content = f.read()

    # Check if program already exists (look for any program statement after the module)
    # Extract test name from filename for the program name check
    base_name = os.path.splitext(os.path.basename(filepath))[0]
    if base_name.startswith('test_'):
        program_name = 'tester_' + base_name[5:]
    else:
        program_name = 'tester_' + base_name

    if f'program {program_name}' in content.lower():
        print(f"Skipping {filepath} - already has program {program_name}")
        return False

    # Find the last 'end module' line
    lines = content.split('\n')
    last_end_module_idx = -1
    for i in range(len(lines) - 1, -1, -1):
        if re.match(r'^\s*end\s+module\s+', lines[i], re.IGNORECASE):
            last_end_module_idx = i
            break

    if last_end_module_idx == -1:
        print(f"ERROR: Could not find 'end module' in {filepath}")
        return False

    # Create the program tester section with unique program name
    # Extract test name from filename (e.g., test_pic_array.f90 -> tester_pic_array)
    base_name = os.path.splitext(os.path.basename(filepath))[0]
    # Replace 'test_' prefix with 'tester_' to avoid module name conflicts
    if base_name.startswith('test_'):
        program_name = 'tester_' + base_name[5:]  # Remove 'test_' and add 'tester_'
    else:
        program_name = 'tester_' + base_name

    tester_program = f"""

program {program_name}
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type
   use {module_name}, only: {collect_func}
   implicit none
   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
      new_testsuite("{suite_name}", {collect_func}) &
      ]

   do is = 1, size(testsuites)
      write(error_unit, fmt) "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if
end program {program_name}
"""

    # Insert after the end module line
    lines.insert(last_end_module_idx + 1, tester_program)

    # Write back
    with open(filepath, 'w') as f:
        f.write('\n'.join(lines))

    print(f"Added program tester to {filepath}")
    return True

if __name__ == '__main__':
    # Get the directory where this script is located
    script_dir = os.path.dirname(os.path.abspath(__file__))

    for filename, (module, collect, suite) in mappings.items():
        filepath = os.path.join(script_dir, filename)
        if os.path.exists(filepath):
            add_tester(filepath, module, collect, suite)
        else:
            print(f"File not found: {filepath}")
