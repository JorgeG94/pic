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

# Compiler-specific configurations
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(standard_compile_flags "-std=f2018 -ffree-line-length-none -fbacktrace")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 15.1)
    # there's a bug with Wall in Gfortran 15.1 whcih is the default one in
    # Github runners
    set(warning_flags
        "-Wall -Wextra -Wpedantic -Wsurprising -Waliasing -Wampersand -Wno-array-bounds -Wcharacter-truncation -Wconversion -Wline-truncation -Wintrinsics-std -Wno-tabs -Wunderflow -Wunused-parameter -Wintrinsic-shadow -Wno-align-commons -Wsurprising"
    )
  endif()
  set(fpe "-ffpe-trap=invalid,zero,overflow")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(standard_compile_flags "-traceback")
  set(fpe "-check all -fpe0")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
  set(standard_compile_flags "-axAVX2 -traceback")
  set(fpe "-check all -fpe0")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
  set(standard_compile_flags " -traceback")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LLVMFlang")
  set(standard_compile_flags "")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LFortran")
  set(standard_compile_flags "--debug-with-line-column")
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
