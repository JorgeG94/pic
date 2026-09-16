# 04 — `pic_ansi` and `pic_term` meet their first consumer

**Status:** waiting on fairport, not on pic. **Size:** reactive.

## Where things stand

`pic-features-design.md` tagged features 6 and 7 as M1, and that is exactly how
it played out. fairport M0 is batch-only and uses neither:

```
core/, app/   no `use pic_ansi`, no `use pic_term`
```

Both are built and tested. Neither has been used in anger. The first time
someone composes a real 2 Hz ops board out of `frame_t`, `decode_keys` and
`term_read` is the first time the API meets a workload, and that is when the
awkward parts will surface.

Nothing to do until then. Two notes worth recording now, while the context is
fresh.

## Open question §9.6 is answered: no change needed

`pic-features-design.md` §9 left this open:

> **Logger versus TUI.** `pic_logger` prints to stdout and will corrupt a frame.
> Confirm that console output can be silenced while file output stays on, using
> the existing `log_level` / `log_file_level` split. If it cannot, add a switch.

**It can, and fairport already relies on it.** `logger_type` carries both
thresholds as separate public components:

```
src/lib/core/logger/pic_logger.f90:34   integer(default_int), public :: log_level = info_level
src/lib/core/logger/pic_logger.f90:36   integer(default_int), public :: log_file_level = verbose_level
```

with independent setters (`configure`, `configure_file_output`) and an
independent file gate at line 353.

fairport's `--hash` mode depends on this working: it calls
`logger%configure(error_level)` to reduce the console to nothing so that stdout
is exactly one digest for the CI fan-in job to compare, and file output is
untouched. That is the same mechanism a TUI needs.

No new switch. Consider §9.6 closed.

## The one thing to watch

`term_write` writes through Fortran's `output_unit`, and `pic_term_os.c`
deliberately never writes to stdout, so C stdio and libgfortran buffering cannot
interleave. Good.

But `pic_logger` also writes to the console through Fortran I/O, and a TUI that
silences the console by *threshold* is still one stray `logger%error` away from
a corrupted frame — an error is exactly the thing you do not want to suppress,
and exactly the thing that will land in the middle of a redraw.

There is no bug here today. It is worth deciding, when fairport M1 actually
draws a frame, whether the answer is:

- the app sets `log_level` to silence everything including errors, and surfaces
  errors through the frame itself; or
- `pic_ansi`'s `frame_t` grows a way to reserve a status line that the logger
  can own; or
- nothing, and a corrupted frame on a fatal error is acceptable because the next
  thing that happens is the program exiting anyway.

The third is probably right. Record the choice rather than rediscovering it.

## Expected feedback areas

Guesses, for calibration rather than planning:

- **`decode_keys` state across reads.** Tested with synthetic byte strings. A
  real terminal delivering an arrow key split across a `poll` boundary under
  load is the case that finds the bug, if there is one.
- **`frame_t` row diffing.** The design goal is that an idle board updating only
  a clock writes one line. Whether that holds once rows carry colour escapes
  depends on the width accounting treating escapes as zero-width, which it
  claims to do.
- **Windows `ENABLE_VIRTUAL_TERMINAL_INPUT`.** The claim that arrow keys arrive
  as the same `ESC [ A` bytes as POSIX, so `decode_keys` is one decoder
  everywhere, is the sort of thing that is true until a specific terminal is
  tried.
