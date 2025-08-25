# How to add unit tests


In the `CMakeLists.txt` in `test/` there is this line: `set(tests "base_utils" "pic_string_utils")`

This line controls the names of the tests that are compiled into `main_tests.f90`. The structure follows the
style `test_${name_of_test}` for example: `test_pic_string_utils.f90`.

The CMake build will pick up all files specified there and compile them. The following step is modifying the
`main_tests.f90`. Here you need to modify the number of test suites and use the appropriate name


TO create new unit tets, follow the blueprint left in the `base_utils` and `pic_string_utils` files.
