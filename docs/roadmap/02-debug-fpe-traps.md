# 02 — Two tests cannot run in a Debug build

**Status:** broken now, invisible to CI. **Size:** ~30 minutes.

## What is wrong

A Debug build of pic fails two tests with SIGFPE:

```
34 - pic/pic_tokenizer   (NUMERICAL)
44 - pic/pic_array_hash  (NUMERICAL)
```

Reproduced with gfortran on Linux at `1256ca40`:

```bash
cmake -B build-debug -DCMAKE_BUILD_TYPE=Debug -DPIC_ENABLE_TERM=ON
cmake --build build-debug -j
ctest --test-dir build-debug --output-on-failure
```

Both are hard crashes, not assertion failures:

```
Program received signal SIGFPE: Floating-point exception.
#3  0x... in overflow_value        at ./stdlib/strtod_l.c:192
#9  0x... in parse_real_wide       at src/lib/core/tokenizer/pic_tokenizer.f90:456
#11 0x... in test_parse_real_overflow at test/test_pic_tokenizer.f90:579
```

```
#3  0x... in feed_real_dp          at src/lib/core/hash/pic_array_hash.f90:709
#5  0x... in hash_rdp_1            at src/lib/core/hash/pic_array_hash.f90:1292
#6  0x... in test_nan_canonicalisation at test/test_pic_array_hash.f90:426
```

## Cause

pic sets floating-point trapping for GNU builds:

```
cmake/CMakeLists.txt:23   set(fpe "-ffpe-trap=invalid,zero,overflow")
cmake/CMakeLists.txt:60   "${CMAKE_Fortran_FLAGS_DEBUG} ${fpe} ${debug}"
```

Applied to `CMAKE_Fortran_FLAGS_DEBUG` only — Release is unaffected.

The two failing tests are the ones whose entire purpose is to exercise the
conditions being trapped:

- `test_parse_real_overflow` feeds `parse_real` a literal too large for `real(dp)`
  and expects `ERROR_PARSE`. Reaching that error requires glibc's `strtod` to
  raise overflow first, and `-ffpe-trap=overflow` kills the process before
  `parse_real` can convert it into an error code.
- `test_nan_canonicalisation` hashes NaN to assert every NaN hashes alike.
  Forming and inspecting a NaN raises `invalid`.

So the tests are correct and the flag is correct; they are simply mutually
exclusive.

## Why CI does not see it

Every CI job builds Release:

```
.github/workflows/*.yml   -DCMAKE_BUILD_TYPE=Release   (and one Coverage job)
```

There is no Debug job, so a build type that any contributor will reach for the
first time they debug something has been failing without anybody being told.

## Why it matters beyond pic

`airport-sim-design-fortran.md` §13.1 recommends exactly
`-ffpe-trap=invalid,zero,overflow` for a debug CI job, on the grounds that
`-fcheck=all` in Debug catches the most likely bug class. A consumer following
that advice inherits this failure the moment it links pic's tests. fairport
sidesteps it by not setting `-ffpe-trap` at all, on the grounds that its core is
integer-only — but that is a fairport-specific escape, not a general one.

## Options

**A. Exempt the two tests from trapping.** Set `-ffpe-trap=` off for
`test_pic_tokenizer.f90` and `test_pic_array_hash.f90` via
`set_source_files_properties(... COMPILE_OPTIONS ...)`. Keeps the flag's value
everywhere else. The subtlety is that the trap is a property of the *process*,
not the translation unit — the flag installs trapping at program start, so
compiling two files differently may not help. **Verify this actually works
before choosing it.**

**B. Drop `invalid` and `overflow`, keep `zero`.** Division by zero is the one
of the three that is nearly always a real bug in pic's own code, while `invalid`
and `overflow` are both legitimately reachable through the public API
(`parse_real` on out-of-range input, hashing a NaN). Smallest change, keeps a
Debug build usable, loses some diagnostic power.

**C. Disable trapping around the specific operations** using
`ieee_set_halting_mode` from `ieee_exceptions` inside `parse_real_wide` and
`feed_real_dp`, restoring it afterwards. Most precise, most portable in intent,
most code — and `ieee_exceptions` support is uneven across pic's compiler
matrix, which is the sort of thing pic exists to avoid.

**Recommendation: B, plus a Debug job in CI.** The flag's purpose is catching
accidental floating-point faults in library code; it is not worth making two
correct tests unrunnable to keep two thirds of it.

## Also do

Add one Debug job to the matrix. Whatever fix is chosen, the reason this went
unnoticed is that nothing builds Debug, and that will be true again next time.

## How to know it worked

```bash
cmake -B build-debug -DCMAKE_BUILD_TYPE=Debug
cmake --build build-debug -j
ctest --test-dir build-debug --output-on-failure   # 48/48
```

and the two tests still assert what they did before — `parse_real` on an
overflowing literal still returns `ERROR_PARSE`, and every NaN still hashes
alike. A fix that makes the tests pass by weakening them is worse than the bug.
