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
/* Standard C, not POSIX: `sig_atomic_t` is what the restore flags must be on
 * every platform, Windows included, and it lives here. */
#include <signal.h>

#if defined(_WIN32)
#  include <windows.h>
#  include <io.h>
#else
#  include <errno.h>
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
#define PIC_TERM_EOF           5

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

/* Read and written from a signal handler, so neither plain `int` nor
 * anything the compiler may cache in a register will do. */
static volatile sig_atomic_t pic_term_raw_active = 0;
static volatile sig_atomic_t pic_term_vt_changed = 0;
static int pic_term_handlers_installed = 0;

#if defined(_WIN32)
static DWORD pic_term_saved_in_mode = 0;
static DWORD pic_term_saved_out_mode = 0;
static UINT  pic_term_saved_out_cp = 0;
static int   pic_term_console_saved = 0;
#else
static struct termios pic_term_saved;
#endif

#if defined(_WIN32)
/* Record the console's state once, before anything changes it.
 *
 * Both `enable_vt` and `raw_enter` modify console state, and the documented
 * order calls them in that sequence. Saving inside `raw_enter` alone meant it
 * recorded what `enable_vt` had already changed, and "restoring" put those
 * modified values back rather than the originals. Console mode and output
 * code page outlive the process, so that is the Windows equivalent of leaving
 * the terminal in a state the user did not ask for.
 */
static void pic_term_save_console_once(void)
{
    HANDLE hin;
    HANDLE hout;

    if (pic_term_console_saved) {
        return;
    }
    pic_term_console_saved = 1;

    hin = GetStdHandle(STD_INPUT_HANDLE);
    hout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hin != NULL && hin != INVALID_HANDLE_VALUE) {
        GetConsoleMode(hin, &pic_term_saved_in_mode);
    }
    if (hout != NULL && hout != INVALID_HANDLE_VALUE) {
        GetConsoleMode(hout, &pic_term_saved_out_mode);
    }
    pic_term_saved_out_cp = GetConsoleOutputCP();
}
#endif

static void pic_term_restore_now(void)
{
    if (!pic_term_raw_active && !pic_term_vt_changed) {
        return;
    }
    pic_term_raw_active = 0;
    pic_term_vt_changed = 0;
#if defined(_WIN32)
    if (pic_term_console_saved) {
        HANDLE hin = GetStdHandle(STD_INPUT_HANDLE);
        HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
        if (hin != NULL && hin != INVALID_HANDLE_VALUE) {
            SetConsoleMode(hin, pic_term_saved_in_mode);
        }
        if (hout != NULL && hout != INVALID_HANDLE_VALUE) {
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
    struct sigaction action;

    pic_term_restore_now();

    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    sigaction(sig, &action, NULL);
    raise(sig);
}
#endif

#if !defined(_WIN32)
static void pic_term_install_one(int sig)
{
    struct sigaction current;
    struct sigaction action;

    if (sigaction(sig, NULL, &current) != 0) {
        return;
    }
    /* Leave an ignored signal ignored, and leave somebody else's handler
     * alone. Only the default disposition is ours to take over. */
    if (current.sa_handler != SIG_DFL) {
        return;
    }
    action = current;
    action.sa_handler = pic_term_signal_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    sigaction(sig, &action, NULL);
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
    /* Installed with sigaction, and only where nothing is there already.
     *
     * A program run with `trap '' INT`, or under a runtime such as MPI that
     * installs its own handlers, has made a decision about these signals.
     * Overwriting SIG_IGN would turn an ignored signal into process death;
     * overwriting a user handler would hide it. Restoring the terminal is
     * worth doing, but not at the cost of changing what the signal means.
     */
    pic_term_install_one(SIGINT);
    pic_term_install_one(SIGTERM);
    pic_term_install_one(SIGHUP);
#endif
}

/* ------------------------------------------------------------------ sleep */

int pic_term_sleep_ms(int64_t ms)
{
    if (ms <= 0) {
        return PIC_TERM_OK;
    }
#if defined(_WIN32)
    /* Chunked, because `Sleep` takes a DWORD: a duration of 4294967295 ms
     * would become INFINITE and never return, and anything at or above 2**32
     * would wrap and return early. The contract is "at least ms". */
    while (ms > 0) {
        DWORD chunk = (ms > 0x7FFFFFFF) ? (DWORD)0x7FFFFFFF : (DWORD)ms;
        Sleep(chunk);
        ms -= (int64_t)chunk;
    }
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
        if (hout == NULL || hout == INVALID_HANDLE_VALUE ||
            !GetConsoleMode(hout, &mode)) {
            return PIC_TERM_NOT_A_TTY;
        }
        /* Both of the changes below outlive the process: a console's mode and
         * its output code page belong to the console, not to whoever set
         * them. So record the originals and arm the restore path before
         * touching either, exactly as raw mode does -- a program that only
         * enables VT and then exits must still hand the console back the way
         * it found it. */
        pic_term_save_console_once();
        pic_term_vt_changed = 1;
        pic_term_install_handlers();
        if (!SetConsoleMode(hout, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
            /* Nothing was changed, so leave the flag as raw mode found it. */
            pic_term_vt_changed = pic_term_raw_active;
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
        DWORD in_mode = 0;
        if (hin == NULL || hin == INVALID_HANDLE_VALUE ||
            !GetConsoleMode(hin, &in_mode)) {
            return PIC_TERM_NOT_A_TTY;
        }
        /* Saves whatever has not been saved yet -- which is the whole console
         * state if raw mode is entered first, and nothing if enable_vt
         * already recorded it. Re-reading it here would record enable_vt's
         * own changes as if they were the originals. */
        pic_term_save_console_once();
        /* Armed before the mode actually changes, so a Ctrl-C landing between
         * the two lines still restores. Setting it afterwards leaves a window
         * in which the console is raw and the handler believes it is not. */
        pic_term_raw_active = 1;
        pic_term_install_handlers();
        /* ENABLE_VIRTUAL_TERMINAL_INPUT is what makes arrow keys arrive as
         * the same ESC [ A bytes as on POSIX, so pic_ansi's decode_keys is
         * the single decoder on every OS. */
        in_mode &= ~(DWORD)(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
        in_mode |= ENABLE_VIRTUAL_TERMINAL_INPUT;
        if (!SetConsoleMode(hin, in_mode)) {
            pic_term_raw_active = 0;
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
        /* Armed before tcsetattr rather than after it. The saved termios is
         * already valid at this point, so an early restore is a harmless
         * no-op, whereas the other order leaves a window in which the
         * terminal is raw and a signal arriving in it would restore
         * nothing -- the shell prompt comes back with no echo. */
        pic_term_raw_active = 1;
        pic_term_install_handlers();
        if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) != 0) {
            pic_term_raw_active = 0;
            return PIC_TERM_FAILED;
        }
    }
#endif
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
        DWORD console_mode = 0;
        DWORD got = 0;
        ULONGLONG deadline = 0;

        if (hin == NULL || hin == INVALID_HANDLE_VALUE) {
            return PIC_TERM_FAILED;
        }

        if (!GetConsoleMode(hin, &console_mode)) {
            /* Not a console: stdin is a pipe or a file. WaitForSingleObject
             * on such a handle does not mean "data is ready" -- a file handle
             * is always signalled and an anonymous pipe is never signalled --
             * so there is no timeout to honour and a plain blocking read is
             * the only honest thing to do. */
            if (!ReadFile(hin, buf, (DWORD)cap, &got, NULL)) {
                /* A closed pipe is end of input, not a failure. */
                return (GetLastError() == ERROR_BROKEN_PIPE ||
                        GetLastError() == ERROR_HANDLE_EOF)
                           ? PIC_TERM_EOF : PIC_TERM_FAILED;
            }
            if (got == 0) {
                return PIC_TERM_EOF;
            }
            *nread = (int)got;
            return PIC_TERM_OK;
        }

        if (timeout_ms >= 0) {
            deadline = GetTickCount64() + (ULONGLONG)timeout_ms;
        }

        /* A console handle is signalled by *any* input record: a mouse move,
         * a focus change, a window resize, a key release. None of those
         * produce bytes, so calling ReadFile on the strength of the wait
         * alone blocks until a real keypress -- for as long as it takes,
         * ignoring the timeout the caller asked for. Move the mouse over a
         * terminal running a 100 ms render loop and the loop stops.
         *
         * So: wait, then look at what is actually queued, and drain the
         * records that cannot produce bytes rather than reading on them.
         */
        for (;;) {
            DWORD wait_ms = INFINITE;
            DWORD waited;
            DWORD pending = 0;
            DWORD peeked = 0;
            DWORD i;
            int readable = 0;
            INPUT_RECORD records[32];

            if (timeout_ms >= 0) {
                ULONGLONG now = GetTickCount64();
                wait_ms = (now >= deadline) ? 0
                                            : (DWORD)(deadline - now);
            }

            waited = WaitForSingleObject(hin, wait_ms);
            if (waited == WAIT_TIMEOUT) {
                return PIC_TERM_TIMEOUT;
            }
            if (waited != WAIT_OBJECT_0) {
                return PIC_TERM_FAILED;
            }

            if (!GetNumberOfConsoleInputEvents(hin, &pending)) {
                return PIC_TERM_FAILED;
            }
            if (pending == 0) {
                continue;               /* another thread took it */
            }
            if (pending > (DWORD)(sizeof records / sizeof records[0])) {
                pending = (DWORD)(sizeof records / sizeof records[0]);
            }
            if (!PeekConsoleInput(hin, records, pending, &peeked)) {
                return PIC_TERM_FAILED;
            }
            for (i = 0; i < peeked; ++i) {
                if (records[i].EventType == KEY_EVENT &&
                    records[i].Event.KeyEvent.bKeyDown) {
                    readable = 1;
                    break;
                }
            }
            if (readable) {
                break;
            }
            /* Nothing here yields bytes. Consume it so the handle stops
             * being signalled, then wait again on what is left of the
             * timeout. */
            if (!ReadConsoleInput(hin, records, peeked, &peeked)) {
                return PIC_TERM_FAILED;
            }
        }

        if (!ReadFile(hin, buf, (DWORD)cap, &got, NULL)) {
            return (GetLastError() == ERROR_BROKEN_PIPE ||
                    GetLastError() == ERROR_HANDLE_EOF)
                       ? PIC_TERM_EOF : PIC_TERM_FAILED;
        }
        if (got == 0) {
            /* Ctrl-Z on a console, or the handle closing under us. */
            return PIC_TERM_EOF;
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
        if (got == 0) {
            /* poll() said readable and read() returned nothing: the other end
             * is gone. Reporting this as a successful zero-byte read is what
             * turns every documented `do while` input loop into a spin at
             * 100% CPU, because the loop cannot tell it apart from an idle
             * tick -- when stdin is a closed pipe, poll() returns immediately
             * for ever. It is a distinct outcome and says so. */
            return PIC_TERM_EOF;
        }
        *nread = (int)got;
        return PIC_TERM_OK;
    }
#endif
}
