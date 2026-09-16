if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Build type to be used." FORCE)
  message(
    STATUS "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified.")
endif()

# specify module installation directory
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/modules/"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# Compiler-specific configurations What `-check` sub-checks the Intel compilers
# get in a Debug build.
#
# Not `all`. `-check all` bundles sub-checks that do not look for bugs but
# redefine conforming behaviour, and pic has tests asserting the behaviour they
# redefine:
#
# output_conversion  A value too wide for its edit descriptor is filled with
# asterisks by the standard, with iostat left zero. This check makes it an error
# instead, so `to_string(-100._dp, "F6.2")` returns pic's `[*]` sentinel rather
# than `******` -- which is what test_pic_string_to_string asserts. udio_iostat
# Conformance checking for user-defined derived-type I/O, which is exactly what
# `string_type`'s read and write procedures are, and what
# test_pic_string_derivedtype_io exercises across child data transfers. format
# Format and data-type consistency, including the DT edit descriptors that same
# defined I/O uses.
#
# What is left is the set that catches real memory and logic faults. The `-fpe0`
# that used to sit alongside this is gone for the same reason as the GNU trap
# list below: see that comment.
set(pic_intel_checks "bounds,uninit,pointers,stack")

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(standard_compile_flags "-std=f2018 -ffree-line-length-none -fbacktrace")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 15.1)
    # there's a bug with Wall in Gfortran 15.1 whcih is the default one in
    # Github runners
    set(warning_flags
        "-Wall -Wextra -Wpedantic -Wsurprising -Waliasing -Wampersand -Wno-array-bounds -Wcharacter-truncation -Wconversion -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow -Wunused-parameter -Wintrinsic-shadow -Wno-align-commons -Wsurprising"
    )
  endif()
  # `zero` only, deliberately.
  #
  # `invalid` and `overflow` are both reachable through pic's own public API by
  # design, not by accident: `parse_real` on a literal too large for real(dp)
  # needs strtod to raise overflow before it can report ERROR_PARSE, and
  # `array_hash` canonicalises NaN, which means forming and inspecting one.
  # Trapping those turned the two tests that cover exactly those contracts into
  # SIGFPE crashes, so a Debug build of pic could not run its own test suite.
  #
  # Division by zero is the one of the three that is essentially always a real
  # bug in library code, so it stays. Weakening the two tests to keep the other
  # two traps would be a worse trade: the tests assert behaviour pic promises,
  # the traps only catch a class of mistake pic has not made.
  set(fpe "-ffpe-trap=zero")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(standard_compile_flags "-traceback")
  set(fpe "-check ${pic_intel_checks}")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
  set(standard_compile_flags "-axAVX2 -traceback")
  set(fpe "-check ${pic_intel_checks}")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
  set(standard_compile_flags " -traceback")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LLVMFlang")
  set(standard_compile_flags "")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LFortran")
  set(standard_compile_flags "--std=f23")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  if(NOT CMAKE_GENERATOR MATCHES "Ninja")
    message(
      FATAL_ERROR
        "If you're using Cray compilers, please build with Ninja: cmake -G \"Ninja\""
    )
  endif()
  set(standard_compiler_flags "")
  set(debug "-g -O2 -hbounds")
  set(BUILD_TESTING
      OFF
      CACHE BOOL "Enable testing" FORCE)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
  set(standard_compiler_flags "")
  set(debug "-g -O2 ")
else()
  message(
    WARNING
      "The Fortran compiler ID ${CMAKE_Fortran_COMPILER_ID} is not in the list of known compilers, please add me!"
  )
endif()

# Customize compiler flags
set(CMAKE_Fortran_FLAGS_DEBUG
    "${CMAKE_Fortran_FLAGS_DEBUG} ${fpe} ${debug}"
    PARENT_SCOPE)

set(CMAKE_Fortran_FLAGS
    "${CMAKE_Fortran_FLAGS} ${standard_compile_flags} ${warning_flags}"
    PARENT_SCOPE)

set(CMAKE_Fortran_FLAGS_COVERAGE
    "-O0 -g -coverage -fprofile-arcs -ftest-coverage --free-line-length-none")
if(CMAKE_BUILD_TYPE STREQUAL "Coverage")
  set(CMAKE_Fortran_FLAGS
      "${CMAKE_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS_COVERAGE}"
      PARENT_SCOPE)
endif()
