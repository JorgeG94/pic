if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Build type to be used." FORCE)
  message(
    STATUS "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified.")
endif()

# Add modules to the CMake build
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/modules")
set(CMAKE_MODULE_PATH
    "${CMAKE_MODULE_PATH}"
    PARENT_SCOPE)

# specify module installation directory
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/modules/"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# Compiler-specific configurations
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(dialects
      "-fdefault-real-8 -fdefault-double-8 -ffree-line-length-none -fbacktrace")
  set(bounds "-fbounds-check -ffpe-trap=invalid,zero,overflow")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(dialects "-axAVX2 -r8 -traceback")
  set(bounds "-check all -fpe0")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
  set(dialects "-Mbackslash -Mallocatable=03 -r8 -traceback")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "LLVMFlang")
  set(dialects "-fdefault-real-8 -fdefault-double-8")
endif()

# Customize compiler flags
set(CMAKE_Fortran_FLAGS_DEBUG
    "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}"
    PARENT_SCOPE)
set(CMAKE_Fortran_FLAGS
    "${CMAKE_Fortran_FLAGS} ${dialects}"
    PARENT_SCOPE)
