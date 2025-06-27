set(_lib "jonquil")
set(_pkg "JONQUIL")
set(_url "https://github.com/toml-f/jonquil")
set(_rev "v0.3.0")

include("${CMAKE_CURRENT_LIST_DIR}/pic_utils.cmake")

pic_fetch_package("${_lib}" "${_url}" "${_rev}")

unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
