cmake_minimum_required(VERSION 3.22)

project(
  "PIC"
  LANGUAGES Fortran C
  VERSION 0.1.0
  DESCRIPTION "Jorge's cool stuff")
set(project_name pic)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/.VERSION.in
               ${CMAKE_CURRENT_SOURCE_DIR}/.VERSION)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
include(GNUInstallDirs)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/modules")
include(print_status)

option(PIC_DEFAULT_INT8 "Use 8-byte integers as default" OFF)
option(PIC_ENABLE_OMP "Enable the use of OpenMP in PIC" OFF)
option(PIC_ENABLE_TESTING "Enable testing for PIC" ON)
option(PIC_ENABLE_JONQUIL "Enable Jonquil for json/toml handling" OFF)
option(PIC_BUILD_STRESS "Build dgemm flop stress case" OFF)

add_subdirectory(cmake)

set(external_libraries_to_link)
set(libraries_to_link)
if(NOT TARGET test-drive::test-drive AND PIC_ENABLE_TESTING)
  enable_testing()
  find_package("test-drive" REQUIRED)
  list(APPEND external_libraries_to_link test-drive::test-drive)
endif()
if(NOT TARGET jonquil::jonquil AND PIC_ENABLE_JONQUIL)
  find_package("jonquil" REQUIRED)
  list(APPEND external_libraries_to_link jonquil::jonquil)
endif()

set(pic_suffix)
if(PIC_DEFAULT_INT8)
  set(pic_suffix "_i8")
endif()
set(main_lib pic${pic_suffix})
set(core_lib pic_core${pic_suffix})
set(all_libs ${main_lib} ${core_lib})
add_library(${main_lib} STATIC)
add_library(${core_lib} STATIC)

# Enable position-independent code for shared-library linking support
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(PIC_DEFAULT_INT8)
  # only pic_types.F90 in the core lib needs this, the rest just inherit from
  # there
  target_compile_definitions(${core_lib} PRIVATE USE_INT8)
endif()

# Print summary info

add_subdirectory(src)
if(PIC_ENABLE_TESTING)
  add_subdirectory(test)
endif()

foreach(lib ${all_libs})
  set_target_properties(${lib} PROPERTIES Fortran_MODULE_DIRECTORY
                                          ${CMAKE_BINARY_DIR}/modules)
endforeach()
if(PIC_ENABLE_OMP)
  find_package(
    OpenMP
    COMPONENTS Fortran
    REQUIRED)
  list(APPEND libraries_to_link OpenMP::OpenMP_Fortran)
  target_link_libraries(${core_lib} OpenMP::OpenMP_Fortran)
endif()


target_link_libraries(${main_lib} ${core_lib} ${libraries_to_link})

# Install Fortran module files (assuming they are in the build directory)

set(exe_name app)
add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.F90)
target_include_directories(${exe_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")
target_link_libraries(${exe_name} PRIVATE ${main_lib} ${libraries_to_link}
                                          ${external_libraries_to_link})

foreach(tgt ${all_libs})
  message(STATUS "${tgt}")
  install(
    TARGETS ${tgt}
    EXPORT picTargets
    ARCHIVE DESTINATION lib
    INCLUDES
    DESTINATION include/pic)
endforeach()

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

if(CMAKE_BUILD_TYPE STREQUAL "Coverage")
  add_custom_target(
    coverage
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
    COMMAND ${CMAKE_COMMAND} -E remove -f coverage.info
    COMMAND lcov --directory . --capture --output-file coverage.info
    COMMAND lcov --ignore-errors unused --remove coverage.info '/usr/*'
            '*/build/*' --output-file coverage_filtered.info
    COMMAND genhtml coverage_filtered.info --output-directory coverage_report
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    COMMENT "Generating code coverage report")
endif()
