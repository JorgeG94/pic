# Manual demo, built with the rest of the terminal layer.
add_executable(term_keys main.f90)
target_include_directories(term_keys PRIVATE "${PROJECT_BINARY_DIR}/modules")
target_link_libraries(term_keys PRIVATE ${main_lib} ${core_lib})
