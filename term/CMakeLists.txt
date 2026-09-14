# The terminal OS layer.
#
# Built only when PIC_ENABLE_TERM is ON, and deliberately outside src/ so that
# fpm -- whose library source-dir is src -- never sees it. See the option's help
# text in the top-level CMakeLists.txt for why.
target_sources(${core_lib} PRIVATE pic_term.f90 pic_term_os.c)
set_source_files_properties(pic_term_os.c PROPERTIES LANGUAGE C)

if(PIC_ENABLE_TESTING)
  # A standalone program rather than a pic-tester suite: pic_term exists only
  # under this option, and registering it in the shared runner would put a
  # preprocessor conditional into a file every other suite also edits.
  add_executable(pic-term-tester test_pic_term.f90)
  # The library's .mod files land in ${CMAKE_BINARY_DIR}/modules, which is set
  # on the library targets after this directory is added.
  target_include_directories(pic-term-tester
                             PRIVATE "${CMAKE_BINARY_DIR}/modules")
  target_link_libraries(pic-term-tester PRIVATE ${main_lib} ${core_lib})
  add_test(NAME pic/pic_term COMMAND pic-term-tester)

  # Standard input at end of file must be distinguishable from an idle timeout.
  # Needs a stream that is actually closed, which ctest's own stdin is not, so
  # the redirection happens in the test command itself -- hence a shell rather
  # than the program on its own.
  add_executable(pic-term-eof-probe eof_probe.f90)
  target_include_directories(pic-term-eof-probe
                             PRIVATE "${CMAKE_BINARY_DIR}/modules")
  target_link_libraries(pic-term-eof-probe PRIVATE ${main_lib} ${core_lib})
  if(WIN32)
    add_test(NAME pic/pic_term_eof
             COMMAND cmd /c "$<TARGET_FILE:pic-term-eof-probe> < NUL")
  else()
    add_test(NAME pic/pic_term_eof
             COMMAND sh -c "exec $<TARGET_FILE:pic-term-eof-probe> < /dev/null")
  endif()

  # A third, tiny program for tools/ci/check_term_restores.sh, which runs it
  # under a pty from script(1). It is not a ctest test: it deliberately dies of
  # `error stop`, and what matters is the terminal state afterwards, which only
  # the shell around it can see.
  add_executable(pic-term-raw-crash raw_crash_check.f90)
  target_include_directories(pic-term-raw-crash
                             PRIVATE "${CMAKE_BINARY_DIR}/modules")
  target_link_libraries(pic-term-raw-crash PRIVATE ${main_lib} ${core_lib})
endif()

# The manual demo lives in examples/ but is only buildable with this option on.
add_subdirectory(${CMAKE_SOURCE_DIR}/examples/term_keys
                 ${CMAKE_BINARY_DIR}/examples/term_keys)
