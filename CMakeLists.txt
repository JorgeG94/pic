cmake_minimum_required(VERSION 3.22)

project(
  PIC
  LANGUAGES Fortran
  VERSION 1.0
  DESCRIPTION "Jorge's cool stuff")

# Add the source directory where the actual Fortran sources are
option(PIC_DEFAULT_INT8 "Use 8-byte integers as default" OFF)

set(core_lib pic)
add_library(${core_lib} STATIC)

# Set a default build type if not specified
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

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
find_package(
  OpenMP
  COMPONENTS Fortran
  REQUIRED)
if(ENABLE_MPI)
  find_package(
    MPI
    COMPONENTS Fortran
    REQUIRED)
endif()

find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)

set(libraries_to_link OpenMP::OpenMP_Fortran LAPACK::LAPACK BLAS::BLAS)

if(ENABLE_MPI)
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
if(ENABLE_MPI)
  target_compile_definitions(${exe_name} PRIVATE USE_MPI)
endif()
target_include_directories(${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

target_link_libraries(${exe_name} PRIVATE ${core_lib} BLAS::BLAS)

install(
  DIRECTORY ${CMAKE_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING
  PATTERN "*.mod")
