macro("pic_fetch_package" package url)
  string(TOLOWER "${package}" _pkg_lc)
  string(TOUPPER "${package}" _pkg_uc)

  # fetch from url case
  message(STATUS "Retrieving ${package} from ${url}")
  include(FetchContent) # module for fetching from repo
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(FETCHCONTENT_QUIET FALSE)
  endif()
  FetchContent_Declare(
    "${_pkg_lc}"
    GIT_REPOSITORY "${url}"
    GIT_TAG "HEAD")
  FetchContent_MakeAvailable("${_pkg_lc}")

  add_library("${package}::${package}" INTERFACE IMPORTED)
  target_link_libraries("${package}::${package}" INTERFACE "${package}")

  if(NOT EXISTS "${${_pkg_lc}_BINARY_DIR}/include")
    file(MAKE_DIRECTORY "${${_pkg_lc}_BINARY_DIR}/include")
  endif()

  unset(_pkg_lc)
  unset(_pkg_uc)

  # sanity check
  if(NOT TARGET "${package}::${package}")
    message(FATAL_ERROR "Could not find dependency ${package}")
  endif()
endmacro()
