# PIC

PIC (a Huastec word) is a portable Fortran utility library providing commonly needed features (strings, logging, timers, arrays, sorting, hashing, etc.) with first-class support across GNU, Intel, NVIDIA HPC, LFortran, Cray, and Flang compilers. It exists because the Fortran stdlib does not build on all compilers (e.g. nvfortran).

## Build

Two build systems are supported: **CMake** (primary) and **FPM**.

```bash
# CMake
cmake -B build -G Ninja -DPIC_ENABLE_TESTING=ON
cmake --build build
ctest --test-dir build -V

# FPM
fpm build --profile release
fpm test --profile release
```

Key CMake options: `PIC_ENABLE_OMP`, `PIC_DEFAULT_INT8`, `PIC_ENABLE_BLAS`, `PIC_ENABLE_MPI`.

## Project layout

```
src/lib/core/
  types/          pic_types.F90, pic_global_definitions.f90
  strings/        pic_string_type.F90, pic_strings.f90, pic_ascii.f90, ...
  arrays/         pic_array.f90
  logger/         pic_logger.f90, pic_pure_logger.f90
  timer/          pic_timer.F90
  io/             pic_io.f90
  sort/           pic_sorting.f90, pic_sorting_*.f90
  hash/           pic_hash_32bit.f90, pic_hash_32bit_fnv.f90
  helpers/        pic_helpers.f90, pic_optional.f90
  constants/      pic_constants.f90
  command_line/   pic_command_line.f90
  flop_recorder/  pic_flop_recorder.f90
  flop_rate/      pic_flop_rate.f90
  knowledge/      pic_knowledge.f90
  error/          pic_error.F90
  profiler/       pic_profiler.F90
src/pic.f90       Umbrella module (banner)
test/             All tests + main_tests.f90 runner
```

### Error handling

Use `error_t` from `pic_error` for unified error handling with stack traces:

```fortran
use pic_error
type(error_t) :: err

call err%set(ERROR_IO, "failed to open file")
if (err%has_error()) call err%fatal()

! Wrap errors for context (Rust-style "caused by"):
call low_level_routine(err)
if (err%has_error()) then
   call err%wrap(ERROR_PARSE, "failed to parse input")
   return
end if
```

Error codes: `SUCCESS`, `ERROR_GENERIC`, `ERROR_IO`, `ERROR_PARSE`, `ERROR_VALIDATION`.

### Profiler

Use `pic_profiler` for named code regions with optional NVTX support:

```fortran
use pic_profiler

call profiler_init()
call profiler_start("outer")
call profiler_start("inner")
! ...
call profiler_stop()  ! stops "inner" (stack-based)
call profiler_stop()  ! stops "outer"
call profiler_report()
call profiler_finalize()
```

Compile with `-DPIC_USE_NVTX` for NVIDIA Nsight Systems integration, or `-DPIC_DISABLE_PROFILER` for zero overhead.

## Coding conventions

### Types and kinds

Always use `integer(default_int)` from `pic_types` — never bare `integer`, `integer(4)`, or `integer(8)`. The project must compile with both `int32` (default) and `int64` (`-DPIC_DEFAULT_INT8=ON` / `-DUSE_INT8`). Real kinds: `sp`, `dp`, `qp` from `pic_types`.

### Module structure

One module per file. File name matches module name. Every module follows:

```fortran
! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! FORD documentation for the module
module pic_feature
   !! module doc
   use pic_types, only: default_int, dp
   implicit none
   private
   public :: exported_things
contains
   ! implementations
end module pic_feature
```

- Prefix modules with `pic_`.
- `implicit none` is mandatory.
- `end module`, `end subroutine`, `end function` must repeat the name.
- `.f90` for standard source, `.F90` when preprocessor directives are needed.

### Documentation

Use `!!` (double-bang) for FORD documentation comments. Regular `!` comments are not included in generated docs. Document interfaces, not every `module procedure` behind them if the interface doc already covers it.

### Compiler portability

This is the primary concern. Guard compiler-specific code with preprocessor directives:

```fortran
#ifdef __NVCOMPILER_LLVM__
   ! NVIDIA-specific path
#endif
#ifdef _OPENMP
   use omp_lib
#endif
```

Test with [Compiler Explorer](https://godbolt.org/) if you don't have access to all compilers. CI tests GNU 10-14, Intel 2024/2025, NVIDIA HPC 25.1, and LFortran.

### Formatting and linting

- **fprettify** enforces formatting (runs via pre-commit hook).
- **fortitude** lints with config in `fpm.toml`: rules `C`, `E`, `S` selected; `C003` and `C072` ignored; line length 178; tests excluded.
- Install pre-commit: `python3 -m pip install pre-commit && pre-commit install`
- Run manually: `pre-commit run --all-files`

## Testing

Tests use the [test-drive](https://github.com/JorgeG94/test-drive) framework (JorgeG94 fork for portability).

### Adding a new test

1. Create `test/test_pic_feature.f90` with a `collect_pic_feature_tests` subroutine.
2. Add `"pic_feature"` to the `tests` list in `test/CMakeLists.txt`.
3. In `test/main_tests.f90`: add the `use` statement, bump the `allocate(testsuites(N))` count, and append the `new_testsuite(...)` entry.

Test pattern:

```fortran
module test_pic_feature
   use testdrive, only: new_unittest, unittest_type, error_type, check
   implicit none
   private
   public :: collect_pic_feature_tests
contains
   subroutine collect_pic_feature_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("test_name", test_procedure)]
   end subroutine

   subroutine test_procedure(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, condition, "message")
      if (allocated(error)) return
   end subroutine
end module
```

Run a single suite: `./build/pic-tester pic_feature`
Run a single test: `./build/pic-tester pic_feature test_name`

## Git workflow

- Branch naming: `feat/`, `fix/`, `docs/`, `chore/`, `experiment/` prefixes.
- PRs target `main`.
- CI must pass before merge (multi-compiler matrix).
