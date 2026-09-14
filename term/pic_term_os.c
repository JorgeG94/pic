/* SPDX-License-Identifier: MIT
 * Copyright (c) 2025 Jorge Luis Galvez Vallejo
 *
 * Every operating system conditional in pic lives in this file.
 *
 * The Fortran side (pic_term.f90) is identical on every platform and has no
 * preprocessor conditionals at all. That is the point: a `#ifdef` in Fortran
 * has to be right for each of the six compilers pic supports, whereas a
 * `#ifdef` in C has to be right for each of three operating systems, and the
 * second is a much smaller problem.
 *
 * The boundary carries only `int`, `int64_t` and `char *` with a length. No
 * struct crosses it. `struct termios` and `struct winsize` never appear in
 * Fortran, because their layouts differ between Linux, macOS and the BSDs and
 * a Fortran-side mirror would be wrong somewhere.
 *
 * This file never writes to stdout. Output stays on the Fortran side so that
 * a Fortran runtime's unit-6 buffering and C stdio buffering cannot interleave
 * and produce a garbled screen.
 */

/* Requested before any header is included. Under a strict -std=c99 the POSIX
 * declarations this file needs -- nanosleep, struct timespec, poll, isatty,
 * the termios calls -- are hidden unless the feature test macro asks for
 * them. 200809L is the oldest POSIX revision that has all of them. */
#if !defined(_WIN32)
#  if !defined(_POSIX_C_SOURCE)
#    define _POSIX_C_SOURCE 200809L
#  endif
#  if defined(__APPLE__) && !defined(_DARWIN_C_SOURCE)
/* macOS hides TIOCGWINSZ behind its own macro, because ioctl request numbers
 * are not POSIX in the first place. */
#    define _DARWIN_C_SOURCE
#  endif
#endif

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#  include <windows.h>
#  include <io.h>
#else
#  include <errno.h>
#  include <signal.h>
#  include <sys/ioctl.h>
#  include <termios.h>
#  include <time.h>
#  include <unistd.h>
#  include <poll.h>
#endif

/* Status codes shared with the Fortran side. Kept as small ints rather than
 * errno values, because errno numbers are not the same on every platform and
 * the Fortran side only ever needs to distinguish these cases. */
#define PIC_TERM_OK            0
#define PIC_TERM_NOT_A_TTY     1
#define PIC_TERM_UNAVAILABLE   2
#define PIC_TERM_FAILED        3
#define PIC_TERM_TIMEOUT       4

/* Stream selectors, matching the Fortran parameters. */
#define PIC_TERM_STDIN  0
#define PIC_TERM_STDOUT 1
#define PIC_TERM_STDERR 2

/* ---------------------------------------------------------------- raw mode
 *
 * The saved mode lives in static storage here rather than being handed back to
 * Fortran, so that the atexit and signal handlers can restore it without any
 * Fortran state being reachable -- which it would not be from a signal
 * handler in any case.
 */

static int pic_term_raw_active = 0;
static int pic_term_handlers_installed = 0;

#if defined(_WIN32)
static DWORD pic_term_saved_in_mode = 0;
static DWORD pic_term_saved_out_mode = 0;
static UINT  pic_term_saved_out_cp = 0;
#else
static struct termios pic_term_saved;
#endif

static void pic_term_restore_now(void)
{
    if (!pic_term_raw_active) {
        return;
    }
    pic_term_raw_active = 0;
#if defined(_WIN32)
    {
        HANDLE hin = GetStdHandle(STD_INPUT_HANDLE);
        HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
        if (hin != INVALID_HANDLE_VALUE) {
            SetConsoleMode(hin, pic_term_saved_in_mode);
        }
        if (hout != INVALID_HANDLE_VALUE) {
            SetConsoleMode(hout, pic_term_saved_out_mode);
        }
        if (pic_term_saved_out_cp != 0) {
            SetConsoleOutputCP(pic_term_saved_out_cp);
        }
    }
#else
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &pic_term_saved);
#endif
}

#if !defined(_WIN32)
/* Restore the terminal, then re-raise with the default disposition so that the
 * process still dies of the signal it was sent -- and reports the right exit
 * status to whatever is watching. Swallowing the signal here would turn a
 * Ctrl-C into a hang. */
static void pic_term_signal_handler(int sig)
{
    pic_term_restore_now();
    signal(sig, SIG_DFL);
    raise(sig);
}
#endif

static void pic_term_install_handlers(void)
{
    if (pic_term_handlers_installed) {
        return;
    }
    pic_term_handlers_installed = 1;

    /* Covers normal return from the program, `stop`, and `error stop`:
     * libgfortran's error stop exits through exit(), which runs atexit
     * handlers. A segfault restores nothing -- nothing can -- and the README
     * says to run `reset`. */
    atexit(pic_term_restore_now);

#if !defined(_WIN32)
    signal(SIGINT, pic_term_signal_handler);
    signal(SIGTERM, pic_term_signal_handler);
    signal(SIGHUP, pic_term_signal_handler);
#endif
}

/* ------------------------------------------------------------------ sleep */

int pic_term_sleep_ms(int64_t ms)
{
    if (ms <= 0) {
        return PIC_TERM_OK;
    }
#if defined(_WIN32)
    Sleep((DWORD)ms);
    return PIC_TERM_OK;
#else
    {
        struct timespec want;
        struct timespec left;
        want.tv_sec = (time_t)(ms / 1000);
        want.tv_nsec = (long)((ms % 1000) * 1000000L);
        /* Loop on EINTR with the time that is left, so a signal during the
         * sleep does not silently shorten it. */
        while (nanosleep(&want, &left) != 0) {
            if (errno != EINTR) {
                return PIC_TERM_FAILED;
            }
            want = left;
        }
        return PIC_TERM_OK;
    }
#endif
}

/* ------------------------------------------------------------------- isatty */

int pic_term_isatty(int stream)
{
#if defined(_WIN32)
    int fd = (stream == PIC_TERM_STDIN) ? 0 : (stream == PIC_TERM_STDERR ? 2 : 1);
    return _isatty(fd) ? 1 : 0;
#else
    int fd = (stream == PIC_TERM_STDIN) ? STDIN_FILENO
           : (stream == PIC_TERM_STDERR ? STDERR_FILENO : STDOUT_FILENO);
    return isatty(fd) ? 1 : 0;
#endif
}

/* -------------------------------------------------------------------- size */

int pic_term_size(int *rows, int *cols)
{
    *rows = 0;
    *cols = 0;
#if defined(_WIN32)
    {
        CONSOLE_SCREEN_BUFFER_INFO info;
        HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
        if (hout != INVALID_HANDLE_VALUE &&
            GetConsoleScreenBufferInfo(hout, &info)) {
            *cols = info.srWindow.Right - info.srWindow.Left + 1;
            *rows = info.srWindow.Bottom - info.srWindow.Top + 1;
            if (*rows > 0 && *cols > 0) {
                return PIC_TERM_OK;
            }
        }
    }
#else
    {
        struct winsize ws;
        if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 &&
            ws.ws_row > 0 && ws.ws_col > 0) {
            *rows = (int)ws.ws_row;
            *cols = (int)ws.ws_col;
            return PIC_TERM_OK;
        }
    }
#endif
    /* Fall back to the environment, which is how a size reaches a program
     * running under a tool that sets it but provides no tty. */
    {
        const char *lines = getenv("LINES");
        const char *columns = getenv("COLUMNS");
        if (lines != NULL && columns != NULL) {
            int r = atoi(lines);
            int c = atoi(columns);
            if (r > 0 && c > 0) {
                *rows = r;
                *cols = c;
                return PIC_TERM_OK;
            }
        }
    }
    /* Deliberately no made-up 24x80. A caller that is told the size is
     * unavailable can choose a fallback knowingly; one that is handed a
     * plausible lie cannot. */
    return PIC_TERM_UNAVAILABLE;
}

/* ---------------------------------------------------------- virtual terminal */

int pic_term_enable_vt(void)
{
#if defined(_WIN32)
    {
        HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
        DWORD mode = 0;
        if (hout == INVALID_HANDLE_VALUE || !GetConsoleMode(hout, &mode)) {
            return PIC_TERM_NOT_A_TTY;
        }
        if (!SetConsoleMode(hout, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
            return PIC_TERM_FAILED;
        }
        SetConsoleOutputCP(CP_UTF8);
        return PIC_TERM_OK;
    }
#else
    /* A POSIX terminal has understood these sequences since before the
     * standard existed. Nothing to turn on. */
    return PIC_TERM_OK;
#endif
}

/* ---------------------------------------------------------------- raw mode */

int pic_term_raw_enter(void)
{
    if (pic_term_raw_active) {
        return PIC_TERM_OK;             /* idempotent */
    }
    if (!pic_term_isatty(PIC_TERM_STDIN)) {
        return PIC_TERM_NOT_A_TTY;
    }
#if defined(_WIN32)
    {
        HANDLE hin = GetStdHandle(STD_INPUT_HANDLE);
        HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
        DWORD in_mode = 0;
        DWORD out_mode = 0;
        if (hin == INVALID_HANDLE_VALUE || !GetConsoleMode(hin, &in_mode)) {
            return PIC_TERM_NOT_A_TTY;
        }
        pic_term_saved_in_mode = in_mode;
        pic_term_saved_out_mode = 0;
        pic_term_saved_out_cp = GetConsoleOutputCP();
        if (hout != INVALID_HANDLE_VALUE && GetConsoleMode(hout, &out_mode)) {
            pic_term_saved_out_mode = out_mode;
        }
        /* ENABLE_VIRTUAL_TERMINAL_INPUT is what makes arrow keys arrive as
         * the same ESC [ A bytes as on POSIX, so pic_ansi's decode_keys is
         * the single decoder on every OS. */
        in_mode &= ~(DWORD)(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
        in_mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;
        if (!SetConsoleMode(hin, in_mode)) {
            return PIC_TERM_FAILED;
        }
    }
#else
    {
        struct termios raw;
        if (tcgetattr(STDIN_FILENO, &pic_term_saved) != 0) {
            return PIC_TERM_NOT_A_TTY;
        }
        raw = pic_term_saved;
        /* The cfmakeraw flag set, written out rather than called, because
         * cfmakeraw is not in POSIX and is missing on some targets. */
        raw.c_iflag &= ~(tcflag_t)(IGNBRK | BRKINT | PARMRK | ISTRIP |
                                   INLCR | IGNCR | ICRNL | IXON);
        raw.c_oflag &= ~(tcflag_t)(OPOST);
        raw.c_lflag &= ~(tcflag_t)(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
        raw.c_cflag &= ~(tcflag_t)(CSIZE | PARENB);
        raw.c_cflag |= (tcflag_t)CS8;
        /* Return as soon as any byte is available; the timeout belongs to
         * poll() in pic_term_read, not to the terminal driver. */
        raw.c_cc[VMIN] = 0;
        raw.c_cc[VTIME] = 0;
        if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) != 0) {
            return PIC_TERM_FAILED;
        }
    }
#endif
    pic_term_raw_active = 1;
    /* Installed only once raw mode is actually on, so that a program which
     * never enters raw mode does not acquire signal handlers it did not ask
     * for. */
    pic_term_install_handlers();
    return PIC_TERM_OK;
}

int pic_term_raw_leave(void)
{
    pic_term_restore_now();             /* idempotent */
    return PIC_TERM_OK;
}

int pic_term_raw_is_active(void)
{
    return pic_term_raw_active;
}

/* -------------------------------------------------------------------- read */

int pic_term_read(char *buf, int cap, int timeout_ms, int *nread)
{
    *nread = 0;
    if (cap <= 0) {
        return PIC_TERM_OK;
    }
#if defined(_WIN32)
    {
        HANDLE hin = GetStdHandle(STD_INPUT_HANDLE);
        DWORD waited;
        DWORD got = 0;
        if (hin == INVALID_HANDLE_VALUE) {
            return PIC_TERM_FAILED;
        }
        waited = WaitForSingleObject(hin, timeout_ms < 0 ? INFINITE : (DWORD)timeout_ms);
        if (waited == WAIT_TIMEOUT) {
            return PIC_TERM_TIMEOUT;
        }
        if (waited != WAIT_OBJECT_0) {
            return PIC_TERM_FAILED;
        }
        if (!ReadFile(hin, buf, (DWORD)cap, &got, NULL)) {
            return PIC_TERM_FAILED;
        }
        *nread = (int)got;
        return PIC_TERM_OK;
    }
#else
    {
        struct pollfd pfd;
        int ready;
        ssize_t got;
        pfd.fd = STDIN_FILENO;
        pfd.events = POLLIN;
        pfd.revents = 0;
        do {
            ready = poll(&pfd, 1, timeout_ms);
        } while (ready < 0 && errno == EINTR);
        if (ready < 0) {
            return PIC_TERM_FAILED;
        }
        if (ready == 0) {
            return PIC_TERM_TIMEOUT;
        }
        do {
            got = read(STDIN_FILENO, buf, (size_t)cap);
        } while (got < 0 && errno == EINTR);
        if (got < 0) {
            return PIC_TERM_FAILED;
        }
        *nread = (int)got;
        return PIC_TERM_OK;
    }
#endif
}
