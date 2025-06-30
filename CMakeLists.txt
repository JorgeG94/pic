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
option(PIC_ENABLE_OMP "Enable the use of OpenMP in PIC" OFF)

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
set_target_properties(${core_lib} PROPERTIES Fortran_MODULE_DIRECTORY
                                             ${CMAKE_BINARY_DIR}/modules)
if(PIC_ENABLE_OMP)
  find_package(
    OpenMP
    COMPONENTS Fortran
    REQUIRED)
endif()

if(PIC_ENABLE_MPI)
  find_package(
    MPI
    COMPONENTS Fortran
    REQUIRED)
  target_compile_definitions(${core_lib} PRIVATE USE_MPI)
endif()

# find_package(BLAS REQUIRED) find_package(LAPACK REQUIRED)

# set(libraries_to_link OpenMP::OpenMP_Fortran LAPACK::LAPACK BLAS::BLAS)
# set(libraries_to_link OpenMP::OpenMP_Fortran)

if(PIC_ENABLE_MPI)
  list(APPEND libraries_to_link MPI::MPI_Fortran)
endif()

target_link_libraries(${core_lib} ${libraries_to_link})
# Install targets
install(
  TARGETS ${core_lib}
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  INCLUDES
  DESTINATION include/pic
  RUNTIME DESTINATION bin)

# Install Fortran module files (assuming they are in the build directory)

set(exe_name app)
add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.F90)
target_include_directories(${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

target_link_libraries(${exe_name} PRIVATE ${core_lib} jonquil::jonquil
                                          test-drive::test-drive)

install(
  TARGETS pic
  EXPORT picTargets
  ARCHIVE DESTINATION lib
  INCLUDES
  DESTINATION include/pic)

install(
  DIRECTORY ${CMAKE_BINARY_DIR}/modules/
  DESTINATION include/pic
  FILES_MATCHING
  PATTERN "*.mod")

install(
  EXPORT picTargets
  FILE picTargets.cmake
  NAMESPACE pic::
  DESTINATION lib/cmake/pic)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/picConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/picConfig.cmake"
  INSTALL_DESTINATION lib/cmake/pic)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/picConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/picConfig.cmake"
              "${CMAKE_CURRENT_BINARY_DIR}/picConfigVersion.cmake"
        DESTINATION lib/cmake/pic)

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/picConfig.cmake"
  DESTINATION lib/cmake/pic
  RENAME pic-config.cmake)
