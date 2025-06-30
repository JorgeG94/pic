cmake_minimum_required(VERSION 3.22)
cmake_policy(SET CMP0144 NEW)
project(
  test_example
  LANGUAGES Fortran C
  VERSION 1.0
  DESCRIPTION "Jorge's cool stuff")

find_package(pic REQUIRED)

set(exe_name example)
add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/main.f90)

target_link_libraries(${exe_name} PRIVATE pic::pic)
