# A minimal stand-in for somebody else's project that uses pic.
#
# The point is not what it computes. The point is that pic is NOT the top-level
# project here, so `CMAKE_SOURCE_DIR` and `CMAKE_BINARY_DIR` are this project's
# directories rather than pic's. Anything inside pic that reaches for those
# instead of `PROJECT_SOURCE_DIR` / `PROJECT_BINARY_DIR` resolves to a path that
# does not exist, and configure fails.
cmake_minimum_required(VERSION 3.28)
project(pic_consumer_check LANGUAGES Fortran C)

set(PIC_ENABLE_TERM
    ON
    CACHE BOOL "" FORCE)
set(PIC_ENABLE_TESTING
    OFF
    CACHE BOOL "" FORCE)

add_subdirectory("${PIC_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/pic")

add_executable(consumer_app app.f90)
target_include_directories(consumer_app
                           PRIVATE "${CMAKE_BINARY_DIR}/pic/modules")
target_link_libraries(consumer_app PRIVATE pic pic_core)
