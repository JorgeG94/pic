# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/) (also mention if you do).

## [Unreleased]

## [0.8.2] – 2026-09-16
A single fix, for a bug that made the terminal layer unusable by anyone
except pic itself.

### Fixed
- `-DPIC_ENABLE_TERM=ON` was a hard configure failure for any project that
  consumed pic through `FetchContent` or `add_subdirectory`. `term/` located
  `examples/term_keys` through `CMAKE_SOURCE_DIR`, which is the *top-level*
  project's source directory — pic's own only when pic is the top-level
  project — so a consumer got `add_subdirectory given source
  "<their-project>/examples/term_keys" which is not an existing directory`,
  with no way to work around it since the broken line is inside the fetched
  source. Reported by the first external consumer of `pic_term`.

  Every `CMAKE_SOURCE_DIR` and `CMAKE_BINARY_DIR` in the build system is now
  `PROJECT_SOURCE_DIR` / `PROJECT_BINARY_DIR`, not just the line that failed.
  The others were consistent with each other so they worked, but they put
  pic's `.mod` files in the consumer's build root and would have installed the
  consumer's modules alongside pic's.

### Added
- `tools/ci/check_consumer_build.sh`, run by the terminal workflow: builds a
  project that consumes pic as a subproject with the terminal layer on, and
  runs the result. Every other job builds pic as the top-level project, which
  is exactly why the above was invisible.


## [0.8.1] – 2026-09-16
Release metadata and build-type fixes. No library behaviour changes, no new
public names.

### Fixed
- The version string. `fpm.toml` and `CMakeLists.txt` both still said 0.7.0
  after 0.8.0 was tagged, so `find_package(pic 0.8 REQUIRED)` failed against a
  0.8.0 tree, the installed `picConfigVersion.cmake` advertised 0.7.0, and the
  release links in the published documentation — interpolated from `.VERSION`,
  which `configure_file` writes from `PROJECT_VERSION` — pointed at the 0.7.0
  docs. The `v0.8.0` tag is left where it is; 0.8.1 is the first release whose
  metadata describes itself correctly.
- A Debug build could not run its own test suite. `-ffpe-trap=invalid,zero,overflow`
  on GNU and `-fpe0` on Intel turned `pic_tokenizer` and `pic_array_hash` into
  SIGFPE crashes, because those two suites cover exactly the conditions being
  trapped: `parse_real` on a literal too large for `real(dp)` needs `strtod` to
  raise overflow before it can report `ERROR_PARSE`, and `array_hash`
  canonicalises NaN, which means forming one. GNU now traps `zero` only — the
  one of the three that is essentially always a real bug in library code.
  Neither test was changed.
- Intel Debug builds, which had never been exercised: `-check all` was set on
  the Intel branches but applied only to `CMAKE_Fortran_FLAGS_DEBUG`, and no CI
  job built Debug. Three of its sub-checks do not look for bugs but redefine
  conforming behaviour that pic's tests assert — `output_conversion` turns the
  standard's asterisk fill for an over-wide edit descriptor into an iostat
  error, so `to_string(-100._dp, "F6.2")` returned pic's `[*]` sentinel instead
  of `******`; `udio_iostat` and `format` police the user-defined derived-type
  I/O that `string_type` implements. `uninit` is out for a harder reason: on
  ifx it is MemorySanitizer, which is only sound when every linked object is
  instrumented, and Intel's own runtime is not — so a report fires against a
  static initializer in libirc before `main` and every test binary dies having
  run nothing. The check set is now `bounds,pointers,stack`, and `-fpe0` is
  gone for the same reason it is on GNU.

### Added
- Intel compiler dispatch that can tell ifx from ifort. The branches used
  `MATCHES`, which is a regex, and `"IntelLLVM"` matches `"Intel"` — so ifort's
  arm swallowed ifx too and the `IntelLLVM` arm had never executed since it was
  written. Both now use `STREQUAL`. The `-axAVX2` that unreachable arm asked
  for is deliberately not reinstated: ifx has been built without it all along,
  and switching it on would change generated code for every ifx consumer.
- A Debug CI job, on GNU and Intel. Every other job builds Release, which is
  how the above went unnoticed across two releases.
- `tools/autogen/` is installed to `${CMAKE_INSTALL_DATADIR}/pic/autogen`, and
  `find_package(pic)` exports that path as `PIC_AUTOGEN_DIR`. A project can now
  generate its own struct-of-arrays container with
  `fypp -I ${PIC_AUTOGEN_DIR}` instead of editing pic's source tree, which is
  what the manual previously told it to do.

### Changed
- `tools/autogen/pic_soa.fypp` is now a macro library only. It used to end with
  a bare `$:soa_module('pic_soa_particle', ...)` at file scope, so any
  downstream template that included it also emitted `pic_soa_particle` into its
  own output. That invocation moved to `tools/autogen/pic_soa_particle.fypp`.
  `pic_soa_particle.f90` regenerates byte-identically.

## [0.8.0] – 2026-09-15
### Added
- `pic_clock`: monotonic elapsed time as whole milliseconds or microseconds
  (`monotonic_ms`, `monotonic_us`), wall-clock date and time (`now_local`,
  `now_utc`, `datetime_t`), and integer-only conversions (`unix_time_ms`,
  `format_iso8601`). Complements `pic_timer`, which reports `real(dp)`
  seconds: integers compare and accumulate exactly, so a loop pacing itself
  against the wall clock does not drift with rounding. `system_clock` is
  called with `integer(int64)` arguments to get a finer tick than the default
  kind provides, and a processor with no clock reports `PIC_CLOCK_NO_CLOCK`
  (-1) rather than zero, which is a valid reading. `datetime_t` carries a
  `utc_offset_known` flag, because zero is a real offset: a processor that
  cannot supply one would otherwise be indistinguishable from Greenwich, and
  its local time would format as `...Z`. `datetime_from_unix_ms` is public,
  as the inverse of `unix_time_ms`.
- `pic_vector`: growable, heap-backed arrays with amortised O(1) growth and
  bounds-checked access, for `int32`, `int64`, `dp` and `string_type`
  elements. Generated from `tools/autogen/pic_vector.fypp`. The method
  vocabulary matches `pic_fixed_array`, so moving from a bounded container to
  a growable one is mostly a type change -- the error code differs
  (`ERROR_BOUNDS` rather than `ERROR_VALIDATION`), `value` is left undefined
  rather than zeroed on a failed read, and there is no `vector_int_t`. `take`
  moves the storage
  out without copying, which is the idiom for building an array whose final
  length is not known until the input has been read.
- `pic_term`: the terminal operating system layer -- raw mode, terminal size,
  timed reads, `sleep_ms` and `term_is_tty`. Built only with
  `-DPIC_ENABLE_TERM=ON`, and its sources live in `term/` rather than `src/`,
  so neither the default CMake build nor any fpm build is affected. These are
  pic's first C sources and its first operating system conditionals, and all
  of them are in one file, `term/pic_term_os.c`: the Fortran side is
  byte-for-byte identical on Linux, macOS and Windows and contains no
  preprocessor conditional at all. Only `int`, `int64_t` and `char` with a
  length cross the boundary -- no struct, because `termios` and `winsize`
  differ between platforms. Raw mode is restored by an `atexit` handler and
  by SIGINT/SIGTERM/SIGHUP handlers that re-raise after restoring; a CI job
  verifies this under a real pty by comparing `stty -g` across an
  `error stop`, and refuses to pass if its own negative control cannot tell
  a raw terminal from a cooked one.
- `pic_ansi`: the half of a terminal interface that is pure string
  processing. Escape builders for the cursor, the alternate screen, the
  sixteen named colours, 256-colour and 24-bit colour, all `pure` functions
  returning a string and none of them doing I/O. A key decoder that turns raw
  bytes into `key_event_t` values and carries partial escape sequences across
  reads, so an arrow key split between two reads still decodes; both the
  `ESC [` and `ESC O` cursor forms are handled. A `frame_t` that composes a
  screen and renders only the rows that changed, so an otherwise idle board
  updating a clock writes one line. Rows are held as `string_type`, so a
  styled row is capped by nothing. Width is counted in columns -- UTF-8 code
  points, with escape sequences skipped -- so a row of box-drawing characters
  truncates without being cut mid-character, a styled row is measured by what
  it shows rather than how it is spelled, and a cut never lands inside an
  escape sequence.
- `pic_cli`: a declarative command line parser. Declare options, flags and
  positionals, `parse` once, then read values back by name with a generic
  `get` over `int32`, `int64`, `sp`, `dp`, `logical`, `character` and
  `string_type`. Conversions go through `pic_tokenizer`'s strict
  `parse_int`/`parse_real`, so `--seed 42x` is `ERROR_PARSE` rather than 42.
  Nothing is printed and nothing is stopped: `help_text()` returns the text
  and `help_requested()` reports the request, leaving both decisions to the
  caller. `parse_args` takes the arguments as an array and is the real
  implementation, so every path is testable without a shell. A repeated
  option is last-wins, and `occurrences(name)` reports the count so a caller
  that wants to reject a repeat can.
- `pic_random_dist`: integer-valued distributions that are bit-identical on
  every supported compiler -- `next_range` (inclusive, exactly uniform over
  the full width of `default_int`), `next_bernoulli_ppm`,
  `next_exponential_int` and `next_poisson_int`. Generic over both generators
  in `pic_rng`. Nothing here touches a real type, so nothing here depends on
  libm; the tests pin the first 32 outputs of every routine for a fixed seed
  on both generators, and those pins are the cross-compiler contract.
  Exponential deviates come from a committed inverse-CDF table generated by
  `tools/autogen/gen_exp_table.py` with 60-digit decimal arithmetic.
- `pic_random_dist_real`: `next_exponential_dp` and `next_normal_dp`. These
  call `log`, `sqrt` and `cos`, so their values differ between compilers.
  They are a separate module on purpose: code that never names it cannot
  reach a non-reproducible deviate by accident.
- `pic_array_hash` now also hashes at 64 bits: `array_hash64`,
  `array_hash64_t`, `array_hash64_hex` and `ARRAY_HASH64_OFFSET_BASIS`. 32
  bits is enough to compare two digests of the same thing, but not to use
  digests as identifiers: among 10**5 distinct 32-bit digests some pair
  collides with probability about 69%, against 3e-10 at 64 bits. The module
  is now generated from `tools/autogen/pic_array_hash.fypp`, so both widths
  come from one byte stream definition and cannot drift apart. The 64-bit FNV
  multiply exploits the prime being exactly 2**40 + 435, forming the
  remainder on two 32-bit limbs: two multiplies per byte, and 1.77x the
  32-bit cost per byte rather than the 10x a general `u64_mul` would give.
- `pic_soa` containers gain `state_hash64` next to `state_hash`, over
  byte-for-byte the same stream.
- `pic_uint64`: portable arithmetic modulo 2**64 on `integer(int64)` without
  signed overflow (`u64_add`, `u64_mul`, `u64_shr`, `u64_less`). Extracted
  from `pic_rng`, where the first three were private, so that hashing and
  distribution code can share one audited implementation. `u64_less` is new:
  Fortran's `<` compares the two's-complement patterns as signed, so it
  reports 2**63 as less than zero.

### Changed
- `get_first_arg_from_command_line` takes an optional `err`. With it, a
  missing argument is `ERROR_VALIDATION` and nothing is written or stopped;
  without it the old usage line and `stop 1` are kept, because callers
  written against that behaviour rely on not continuing past it. The
  stopping path is deprecated -- pass `err`, or use `pic_cli`.
- `pic_rng` now takes its modular arithmetic from `pic_uint64`. Generator
  output is unchanged, verified bit-for-bit over 1800 values across both
  generators (`next_u64`, `next`, `next_below`, `next_real_dp` bit patterns
  and `stream_for`).

### Deprecated
-

### Removed
-

### Fixed
- `u64_shr` (formerly `pic_rng`'s private `shr64`) returned zero instead of
  its argument for a shift of zero under LFortran 0.65.0, which evaluates the
  conforming `ibits(x, 0, 64)` as 0. No caller in 0.7.0 used a zero shift, so
  nothing was affected; the zero case is now handled without `ibits`.


## [0.7.0] – 2026-09-13
The largest release so far: eleven new modules, a unified error type adopted
across the library, and six crash or corruption fixes in code that shipped in
earlier versions. No public name was removed, so upgrading is additive.

### Added
- `pic_error` gains `ERROR_INTERNAL` and `ERROR_BOUNDS`, a documented code
  range for downstream projects (`PIC_ERROR_CODE_MAX = 99`, downstream uses
  100 and above), `error_raise`, and `fatal(unit, exit_code)`. Every mutator
  is `pure`, so errors can be reported from `pure` procedures.
- Optional `err` arguments of type `error_t` on `ord_sort`, `sort_index`,
  `radix_sort` and the `pic_array` routines. 140 `error stop` statements in
  the library now report through `error_t` instead of killing the caller.
  Purely additive: no existing call changes, and nothing that was `pure`
  became impure.
- `pic_rng` - reproducible SplitMix64 and PCG32 generators with explicit
  state, so results no longer depend on the compiler's `random_number`.
- `pic_tokenizer` - `split`, `tokenize`, and `parse_int` / `parse_real` that
  report malformed input through `error_t` instead of silently accepting it.
- `pic_format` - deterministic number-to-string conversion, byte-identical on
  every supported compiler.
- `pic_fixed_array` - bounded containers with no heap allocation in the hot
  path.
- `pic_heap` - binary heap / priority queue with O(n) `build_from`.
- `pic_hash_map` - string-keyed map whose `keys()` and `values()` return
  insertion order as a documented guarantee.
- `pic_csr` - compressed sparse row storage with `matvec` and `transpose`.
- `pic_graph` - `dijkstra`, `a_star`, `bfs`, `connected_components` and
  `path_cost` over CSR adjacency.
- `pic_serialize` - binary serialization with a self-describing envelope,
  using stream access so files move between compilers.
- `pic_array_hash` - stable content digests for intrinsic arrays, for
  comparing the state of two runs.
- `pic_soa` / `pic_soa_particle` - struct-of-arrays containers with `resize`,
  `checkpoint` and `state_hash`, generated from an fypp template.
- CI check that every fypp-generated source matches its template.
- Twelve new test suites; the suite count went from 25 to 37.

### Changed
- CMake `project()` version had been stuck at 0.4.2 since the v0.4.2 tag, so
  the installed `picConfigVersion.cmake` understated the version through
  v0.5.0, v0.6.0 and v0.6.1. It now tracks `fpm.toml`.
- Documentation rewritten against the source. Several manual examples
  referenced procedures that do not exist and could not compile; every
  Fortran example is now built against the library as part of the update.
- Timer tests no longer require strictly positive elapsed time: zero is a
  legitimate reading when the clock tick is coarser than the work measured.

### Deprecated
- N/A

### Removed
- N/A

### Fixed
- `pic_hash_32bit_fnv` performed signed integer overflow on every round, which
  is undefined behaviour and aborts under `-ftrapv` or UBSan. The multiply now
  happens in `int64` with an explicit reduction. All pinned digests are
  unchanged.
- `string_type`'s `read_line` silently corrupted any record longer than 512
  characters, because `SIZE=` in a child data transfer is cumulative over the
  parent transfer rather than per read. `iostat` stayed zero throughout.
- The four `error stop` statements in `string_type`'s defined I/O are gone; a
  defined-I/O procedure must report through `iostat` and `iomsg` rather than
  terminating the program.
- Intermittent nvfortran SIGILL in the character sort routines: an allocation
  check had been compiled out, leaving an unallocated allocatable associated
  with a non-allocatable dummy.
- Character sorts read out of bounds at sizes 0 and 1, allocating `buf(0:-1)`
  and entering the merge machinery. The same class of bug in `radix_sort`
  caused a Windows CI hang of over an hour.
- `radix_sort` could not report an allocation failure at all.
- `pic_format` rendered negative zero as `-0.000`; the sign of zero is not
  reliably preserved across compilers.
- `join_string` returned an empty string on AOCC's classic flang and on
  nvfortran.
- `pic_timer` divided by `system_clock`'s count rate without checking it. A
  processor with no clock reports a zero rate, so elapsed time came back as
  NaN or Inf and poisoned every derived figure, including FLOP rates.
- `len_trim`, `char(string, pos)` and `char(string, start, last)` used `merge`
  with an operand that referenced `string%raw` unconditionally, so a
  default-initialised `string_type` hit an unallocated allocatable. This
  aborted under LFortran with no special flags and under gfortran with
  `-fcheck=all`.
- `dp_radix_sort` reported errors under the name `sp_radix_sort`.


## [0.1.0] – 2025-10-31
### Added
- First official release of the package.
- Initial support for key routines such as arrays, strings, hashes, timers, flop counters, flop recorders
- Support for muliple compilers: GCC, Intel, Intel-LLVM, Flang, AMDFlang, NVFortran, LFortran across Linux, Mac, and Windows
- Functional CMake and FPM build system
- Happy halloween

### Changed
- N/A

### Deprecated
- N/A

### Removed
- N/A

### Fixed
- N/A
