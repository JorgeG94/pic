cmake_minimum_required(VERSION 3.22)

project(
  "PIC"
  LANGUAGES Fortran C
  VERSION 1.0
  DESCRIPTION "Jorge's cool stuff")
set(project_name pic)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
include(GNUInstallDirs)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")
include(print_status)

option(PIC_DEFAULT_INT8 "Use 8-byte integers as default" OFF)
option(PIC_ENABLE_MPI "Enable the use of MPI in PIC" OFF)
option(PIC_ENABLE_OMP "Enable the use of OpenMP in PIC" OFF)
option(PIC_ENABLE_BLAS "Enable BLAS/LAPACK libraries in PIC" OFF)
option(PIC_ENABLE_TESTING "Enable testing for PIC" ON)
option(PIC_ENABLE_JONQUIL "Enable Jonquil for json/toml handling" OFF)

add_subdirectory(cmake)

set(external_libraries_to_link)
if(NOT TARGET test-drive::test-drive AND PIC_ENABLE_TESTING)
  enable_testing()
  find_package("test-drive" REQUIRED)
  list(APPEND external_libraries_to_link test-drive::test-drive)
endif()
if(NOT TARGET jonquil::jonquil AND PIC_ENABLE_JONQUIL)
  find_package("jonquil" REQUIRED)
  list(APPEND external_libraries_to_link jonquil::jonquil)
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
if(PIC_ENABLE_TESTING)
  add_subdirectory(test)
endif()
set_target_properties(${core_lib} PROPERTIES Fortran_MODULE_DIRECTORY
                                             ${CMAKE_BINARY_DIR}/modules)
set(libraries_to_link)
if(PIC_ENABLE_OMP)
  find_package(
    OpenMP
    COMPONENTS Fortran
    REQUIRED)
  list(APPEND libraries_to_link OpenMP::OpenMP_Fortran)
endif()

if(PIC_ENABLE_MPI)
  find_package(
    MPI
    COMPONENTS Fortran
    REQUIRED)
  target_compile_definitions(${core_lib} PRIVATE USE_MPI)
  list(APPEND libraries_to_link MPI::MPI_Fortran)
endif()

if(PIC_ENABLE_BLAS)
  find_package(BLAS REQUIRED)
  find_package(LAPACK REQUIRED)
  list(APPEND libraries_to_link LAPACK::LAPACK BLAS::BLAS)
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

target_link_libraries(${exe_name} PRIVATE ${core_lib}
                                          ${external_libraries_to_link})

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

print_pic_summary()
