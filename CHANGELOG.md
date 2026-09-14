# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/) (also mention if you do).

## [Unreleased]
### Added
- `pic_clock`: monotonic elapsed time as whole milliseconds or microseconds
  (`monotonic_ms`, `monotonic_us`), wall-clock date and time (`now_local`,
  `now_utc`, `datetime_t`), and integer-only conversions (`unix_time_ms`,
  `format_iso8601`). Complements `pic_timer`, which reports `real(dp)`
  seconds: integers compare and accumulate exactly, so a loop pacing itself
  against the wall clock does not drift with rounding. `system_clock` is
  called with `integer(int64)` arguments to get a finer tick than the default
  kind provides, and a processor with no clock reports `PIC_CLOCK_NO_CLOCK`
  (-1) rather than zero, which is a valid reading.

### Changed
-

### Deprecated
-

### Removed
-

### Fixed
-


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
