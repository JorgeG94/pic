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
    # Two separate pieces of cmd.exe pedantry, each of which cost a CI cycle and
    # produced the identical, unhelpful "The syntax of the command is
    # incorrect."
    #
    # The redirect cannot go on ctest's command line. `cmd /c "probe < NUL"`
    # does not survive CreateProcess: cmd strips the quotes it is handed only
    # when nothing between them is special, and `<` is special, so it keeps them
    # and tries to run the whole quoted string as a program name. Hence a .bat,
    # where the redirect is read by cmd's own parser instead.
    #
    # And the path to that .bat has to be spelled with backslashes. CMake stores
    # paths with forward slashes, and in `cmd /c D:/a/pic/...` -- which is
    # unquoted, having no spaces in it -- cmd reads `/a`, `/pic` and the rest as
    # switches. file(TO_NATIVE_PATH) rather than $<SHELL_PATH>, because it
    # converts at configure time on the machine that will run the test, where
    # the answer is not in doubt; $<SHELL_PATH> decides from the generator's
    # shell and cannot be checked from a POSIX host at all. `$<CONFIG>` survives
    # it untouched, having no separator in it.
    set(_eof_script "${CMAKE_CURRENT_BINARY_DIR}/run_eof_probe_$<CONFIG>.bat")
    file(TO_NATIVE_PATH "${_eof_script}" _eof_script_native)
    file(
      GENERATE
      OUTPUT "${_eof_script}"
      CONTENT
        "@echo off\n\"$<TARGET_FILE:pic-term-eof-probe>\" < NUL\nexit /b %ERRORLEVEL%\n"
        NEWLINE_STYLE
        CRLF)
    # Printed because it is the string that has twice been wrong, and the
    # runner's error message ("The syntax of the command is incorrect.") names
    # neither the path nor what it objected to.
    message(STATUS "pic_term_eof runs: cmd /c ${_eof_script_native}")
    add_test(NAME pic/pic_term_eof COMMAND cmd /c "${_eof_script_native}")
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
