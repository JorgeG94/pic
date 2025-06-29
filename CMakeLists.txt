cmake_minimum_required(VERSION 3.22)

project(
  "PIC"
  LANGUAGES Fortran C
  VERSION 1.0
  DESCRIPTION "Jorge's cool stuff")
set(project_name pic)
enable_testing()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
include(GNUInstallDirs)

option(PIC_DEFAULT_INT8 "Use 8-byte integers as default" OFF)
option(PIC_ENABLE_MPI "Enable the use of MPI in PIC" OFF)
add_subdirectory(cmake)

if(NOT TARGET "test-drive::test-drive")
  find_package("test-drive" REQUIRED)
endif()
if(NOT TARGET "jonquil::jonquil")
  find_package("jonquil" REQUIRED)
endif()

set(core_lib pic)
add_library(${core_lib} STATIC)

# Enable position-independent code for shared-library linking support
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Make sure Fortran modules are handled properly
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

if(PIC_DEFAULT_INT8)
  target_compile_definitions(${core_lib} PRIVATE USE_INT8)
  set(BLA_SIZEOF_INTEGER 8)
else()
  set(BLA_SIZEOF_INTEGER 4)
endif()

# Print summary info
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "Using 8-byte integers: ${PIC_DEFAULT_INT8}")

add_subdirectory(src)
add_subdirectory(test)

find_package(
  OpenMP
  COMPONENTS Fortran
  REQUIRED)
if(PIC_ENABLE_MPI)
  find_package(
    MPI
    COMPONENTS Fortran
    REQUIRED)
  target_compile_definitions(${core_lib} PRIVATE USE_MPI)
else()
  message(STATUS "Builing ${PROJECT_NAME} without MPI support")
endif()

# find_package(BLAS REQUIRED) find_package(LAPACK REQUIRED)

# set(libraries_to_link OpenMP::OpenMP_Fortran LAPACK::LAPACK BLAS::BLAS)
set(libraries_to_link OpenMP::OpenMP_Fortran)

if(PIC_ENABLE_MPI)
  list(APPEND libraries_to_link MPI::MPI_Fortran)
endif()

target_link_libraries(${core_lib} ${libraries_to_link})
# Install targets
install(
  TARGETS ${core_lib}
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin)

# Install Fortran module files (assuming they are in the build directory)

set(exe_name app)
add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.F90)
target_include_directories(${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

target_link_libraries(${exe_name} PRIVATE ${core_lib} jonquil::jonquil
                                          test-drive::test-drive)

install(
  DIRECTORY ${CMAKE_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING
  PATTERN "*.mod")
