#----------------------------------------------------------------
# Generated CMake target import file for configuration "RelWithDebInfo".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "test-drive::test-drive-lib" for configuration "RelWithDebInfo"
set_property(TARGET test-drive::test-drive-lib APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(test-drive::test-drive-lib PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELWITHDEBINFO "Fortran"
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/lib/libtest-drive.a"
  )

list(APPEND _cmake_import_check_targets test-drive::test-drive-lib )
list(APPEND _cmake_import_check_files_for_test-drive::test-drive-lib "${_IMPORT_PREFIX}/lib/libtest-drive.a" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
