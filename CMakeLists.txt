cmake_minimum_required(VERSION 3.28)

project(
  "PIC"
  LANGUAGES Fortran C
  VERSION 0.9.0
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
option(PIC_BUILD_STRESS "Build dgemm flop stress case" OFF)
option(PIC_DISABLE_PROFILER "Disable the profiler for zero overhead" OFF)
option(PIC_USE_NVTX "Enable NVTX annotations for Nsight Systems" OFF)
# The terminal OS layer (term/): raw mode, terminal size, timed reads.
#
# OFF by default, and its sources live outside src/ rather than under it. Both
# of those are deliberate.
#
# These are the first C sources in pic and the first operating system
# conditionals anywhere in it, and pic exists to build everywhere -- including
# under LFortran, whose CI builds through both fpm and CMake. fpm compiles every
# .c under its library source-dir automatically and offers no way to opt out, so
# a C file under src/ would join every fpm build whether the user wanted the
# terminal or not. Keeping term/ outside src/ leaves fpm untouched; keeping the
# option OFF leaves the default CMake build untouched.
#
# Turn it on with -DPIC_ENABLE_TERM=ON. The dedicated CI job does exactly that,
# so the layer is still built and tested on Linux, macOS and Windows.
option(PIC_ENABLE_TERM
       "Build the terminal OS layer (pic_term); requires a C compiler" OFF)
option(PIC_USE_C_KINDS
       "Use iso_c_binding kinds for sp, dp, qp, and default_int" OFF)

if(PIC_DEFAULT_INT8 AND PIC_USE_C_KINDS)
  message(
    FATAL_ERROR
      "PIC_DEFAULT_INT8 and PIC_USE_C_KINDS are mutually exclusive.\n"
      "PIC_DEFAULT_INT8 sets default_int to int64, while PIC_USE_C_KINDS "
      "sets it to c_int (typically int32). Please enable only one of these options."
  )
endif()

add_subdirectory(cmake)

set(external_libraries_to_link)
set(libraries_to_link)
if(NOT TARGET test-drive::test-drive AND PIC_ENABLE_TESTING)
  enable_testing()
  # Disable extended-precision real kinds in test-drive: AOCC flang (and some
  # other minor compilers) lack an ieee_is_nan generic for real(qp)/real(xdp),
  # which trips test-drive's is_nan_qp / is_nan_xdp at compile time.
  set(TESTDRIVE_WITH_QP
      OFF
      CACHE BOOL "" FORCE)
  set(TESTDRIVE_WITH_XDP
      OFF
      CACHE BOOL "" FORCE)
  find_package("test-drive" REQUIRED)
  list(APPEND external_libraries_to_link test-drive::test-drive)
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

# Namespaced aliases, so that consumers can say pic::pic whichever integer width
# this build was configured for.
#
# The install export already uses NAMESPACE pic::, but it exports the *real*
# target names -- which means an -DPIC_DEFAULT_INT8=ON build offers pic::pic_i8,
# and anything written against pic::pic breaks. Consuming pic from a build tree
# (FetchContent, add_subdirectory) was worse still: no namespaced target existed
# at all, so a downstream `if(NOT TARGET pic::pic)` fell through to find_package
# and picked up the imported target from picConfig.cmake, whose bare `-lpic`
# then failed to link against the libpic_i8.a this build actually produces.
#
# Aliasing here fixes both: pic::pic and pic::pic_core resolve to the targets
# that exist, and because they are real targets rather than imported ones, build
# ordering and module include paths propagate as they should.
add_library(pic::${project_name} ALIAS ${main_lib})
add_library(pic::${project_name}_core ALIAS ${core_lib})

# Enable position-independent code for shared-library linking support
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(PIC_DEFAULT_INT8)
  # only pic_types.F90 in the core lib needs this, the rest just inherit from
  # there
  target_compile_definitions(${core_lib} PRIVATE USE_INT8)
endif()

if(PIC_USE_C_KINDS)
  target_compile_definitions(${core_lib} PRIVATE PIC_C_COMPAT)
endif()

# Print summary info

add_subdirectory(src)

if(PIC_ENABLE_TERM)
  add_subdirectory(term)
endif()
if(PIC_ENABLE_TESTING)
  add_subdirectory(test)
endif()

foreach(lib ${all_libs})
  set_target_properties(${lib} PROPERTIES Fortran_MODULE_DIRECTORY
                                          ${PROJECT_BINARY_DIR}/modules)
  # Carry that directory to anyone who links the target, so a consumer never has
  # to know where pic puts its .mod files -- and so moving them is not a
  # breaking change. 0.8.2 moved them from the top-level build tree into pic's
  # own, which was right but broke every consumer that had hardcoded the old
  # spelling, in a patch release.
  #
  # BUILD_INTERFACE only. The install side is already covered by the `INCLUDES
  # DESTINATION include/pic` on the install(TARGETS) call below, which is where
  # the .mod files actually land; adding a second, different install path here
  # would export a directory that does not exist.
  target_include_directories(
    ${lib} INTERFACE $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)
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

# pic's own demo executable, built only when pic is the top-level project.
#
# It is pic's banner program and nothing a consumer wants; building it for every
# FetchContent consumer costs them a compile and a link for a binary they will
# never run. It is also the first thing built, so when it fails it sends people
# looking at their own project rather than at pic.
if(PROJECT_IS_TOP_LEVEL)
  set(exe_name app)
  add_executable(${exe_name} ${PROJECT_SOURCE_DIR}/app/main.F90)
  target_include_directories(${exe_name}
                             PRIVATE "${PROJECT_BINARY_DIR}/modules")
  target_link_libraries(${exe_name} PRIVATE ${main_lib} ${libraries_to_link}
                                            ${external_libraries_to_link})
endif()

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
  DIRECTORY ${PROJECT_BINARY_DIR}/modules/
  DESTINATION include/pic
  FILES_MATCHING
  PATTERN "*.mod")

# The fypp macro library behind pic_soa, so that a consumer can generate its own
# container instead of editing pic's source tree, which is what the manual used
# to tell it to do. picConfig.cmake exports the installed location as
# PIC_AUTOGEN_DIR for passing to `fypp -I`.
include(GNUInstallDirs)
install(
  DIRECTORY tools/autogen/
  DESTINATION ${CMAKE_INSTALL_DATADIR}/pic/autogen
  FILES_MATCHING
  PATTERN "*.fypp")

install(
  EXPORT picTargets
  FILE picTargets.cmake
  NAMESPACE pic::
  DESTINATION lib/cmake/pic)

include(CMakePackageConfigHelpers)

set(PIC_AUTOGEN_DIR "${CMAKE_INSTALL_DATADIR}/pic/autogen")

configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/picConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/picConfig.cmake"
  INSTALL_DESTINATION lib/cmake/pic
  PATH_VARS PIC_AUTOGEN_DIR)

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
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
    COMMENT "Generating code coverage report")
endif()
