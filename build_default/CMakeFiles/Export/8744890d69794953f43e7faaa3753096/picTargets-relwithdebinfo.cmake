#----------------------------------------------------------------
# Generated CMake target import file for configuration "RelWithDebInfo".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "pic::pic" for configuration "RelWithDebInfo"
set_property(TARGET pic::pic APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(pic::pic PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libpic.a"
  )

list(APPEND _cmake_import_check_targets pic::pic )
list(APPEND _cmake_import_check_files_for_pic::pic "${_IMPORT_PREFIX}/lib/libpic.a" )

# Import target "pic::pic_core" for configuration "RelWithDebInfo"
set_property(TARGET pic::pic_core APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(pic::pic_core PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libpic_core.a"
  )

list(APPEND _cmake_import_check_targets pic::pic_core )
list(APPEND _cmake_import_check_files_for_pic::pic_core "${_IMPORT_PREFIX}/lib/libpic_core.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
