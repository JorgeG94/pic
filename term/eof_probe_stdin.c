/* SPDX-License-Identifier: MIT
 * Copyright (c) 2025 Jorge Luis Galvez Vallejo
 *
 * Test support for eof_probe.f90, and nothing else: it is not part of the
 * library and is not compiled into it.
 *
 * The probe needs a standard input that is open, permanently readable and
 * permanently empty -- the shape of a pipe whose writer has exited. The
 * obvious way to arrange that is to redirect from the null device in the test
 * command, and that is what this replaces.
 *
 * It replaces it because the redirection has to be written in a shell, and
 * the shell is not the same on every platform. Three attempts at spelling
 * `cmd /c "probe < NUL"` in a way that survives ctest, CMake's quoting and
 * cmd.exe's own parser produced three identical, contentless failures ("The
 * syntax of the command is incorrect.") for at least two different reasons.
 * Doing it in the process itself is the same handful of lines on both
 * platforms, needs no shell at all, and can be read and checked here rather
 * than inferred from a CI log.
 */

#if defined(_WIN32)
#  include <windows.h>
#else
#  include <fcntl.h>
#  include <unistd.h>
#endif

/* Point this process's standard input at the null device.
 *
 * Writes 0 on success and 1 on failure, rather than returning a value, so the
 * Fortran side needs no function-result interoperability rules.
 */
void pic_term_test_stdin_from_null(int *status)
{
#if defined(_WIN32)
    HANDLE null_in = CreateFileA("NUL", GENERIC_READ,
                                 FILE_SHARE_READ | FILE_SHARE_WRITE,
                                 NULL, OPEN_EXISTING, 0, NULL);
    if (null_in == INVALID_HANDLE_VALUE) {
        *status = 1;
        return;
    }
    /* pic_term_read reaches standard input through GetStdHandle, so this is
     * the handle it will find. */
    if (!SetStdHandle(STD_INPUT_HANDLE, null_in)) {
        CloseHandle(null_in);
        *status = 1;
        return;
    }
    *status = 0;
#else
    int null_in = open("/dev/null", O_RDONLY);
    if (null_in < 0) {
        *status = 1;
        return;
    }
    /* pic_term_read reads STDIN_FILENO, so replace descriptor 0 itself
     * rather than handing back a new one. */
    if (dup2(null_in, STDIN_FILENO) < 0) {
        close(null_in);
        *status = 1;
        return;
    }
    if (null_in != STDIN_FILENO) {
        close(null_in);
    }
    *status = 0;
#endif
}
