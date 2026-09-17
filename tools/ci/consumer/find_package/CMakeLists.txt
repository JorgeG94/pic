# The other half of the contract: an installed pic, found with find_package,
# linked by its namespaced target and nothing else.
#
# This half already worked when the subproject half did not, because `INCLUDES
# DESTINATION` on install(TARGETS) populates the imported target's
# INTERFACE_INCLUDE_DIRECTORIES. It is checked anyway so that it keeps working
# -- the two paths are configured in different places and can drift apart.
cmake_minimum_required(VERSION 3.28)
project(pic_find_package_check LANGUAGES Fortran)

find_package(pic REQUIRED)

add_executable(found_app ../app.f90)
target_link_libraries(found_app PRIVATE pic::pic_core)
