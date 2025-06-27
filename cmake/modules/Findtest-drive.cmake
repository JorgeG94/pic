set(_lib "test-drive")
set(_pkg "test-drive")
set(_url "https://github.com/fortran-lang/test-drive")
set(_rev "v0.5.0")

include("${CMAKE_CURRENT_LIST_DIR}/pic_utils.cmake")

pic_fetch_package("${_lib}" "${_url}" "${_rev}")

unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
