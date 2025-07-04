set(_lib "vapaa")
set(_pkg "VAPAA")
set(_url "https://github.com/JorgeG94/vapaa")
set(_rev "alpha")

include("${CMAKE_CURRENT_LIST_DIR}/pic_utils.cmake")

pic_fetch_package("${_lib}" "${_url}" "${_rev}")

unset(_lib)
unset(_pkg)
unset(_url)
unset(_rev)
