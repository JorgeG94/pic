#!/usr/bin/env bash
# Build a project that consumes pic as a subproject, with the terminal layer on.
#
# pic's own jobs all build pic as the top-level project, where CMAKE_SOURCE_DIR
# and PROJECT_SOURCE_DIR are the same directory. That makes a whole class of
# bug invisible: anything reaching for the top-level directory works in pic's
# CI and breaks for every consumer. One such bug shipped in 0.8.1 --
# `add_subdirectory(${CMAKE_SOURCE_DIR}/examples/term_keys ...)` -- and made
# `-DPIC_ENABLE_TERM=ON` a hard configure failure for anyone fetching pic.
#
# Usage: tools/ci/check_consumer_build.sh [PIC_SOURCE_DIR] [BUILD_DIR]
set -euo pipefail

PIC_SRC="$(cd "${1:-$(dirname "$0")/../..}" && pwd)"
BUILD="${2:-$(mktemp -d)}/consumer-check"
HERE="$(cd "$(dirname "$0")/consumer" && pwd)"

echo "== pic source:  $PIC_SRC"
echo "== build dir:   $BUILD"

cmake -S "$HERE" -B "$BUILD" -DPIC_SOURCE_DIR="$PIC_SRC" ${CMAKE_GENERATOR:+-G "$CMAKE_GENERATOR"}
# Between configure and build, which is the window the bug lives in.
# Fortran_MODULE_DIRECTORY is not created until build time on some CMake
# versions, and CMake refuses to generate against an interface include
# directory that does not exist -- so a consumer on such a version cannot
# configure at all, while one on a version that pre-creates it never notices.
# Asserting it here makes the invariant pic relies on explicit rather than
# leaving it to whichever CMake happens to be installed.
if [ ! -d "$BUILD/pic/modules" ]; then
  echo "error: $BUILD/pic/modules does not exist after configure." >&2
  echo "The BUILD_INTERFACE include pic exports points at it, so a consumer" >&2
  echo "on a CMake that does not pre-create it cannot generate." >&2
  exit 1
fi

cmake --build "$BUILD"

# Running it matters as much as building it: a wrong module path can still
# link if a stale .mod is lying around in a shared directory.
"$BUILD/consumer_app"

echo "-- subproject: ok"

# Second half: install pic, then consume it the way a packaged dependency is
# consumed. Linking the namespaced target must be enough on this path too.
INST="${BUILD}-install"
PREFIX="${BUILD}-prefix"
cmake -S "$PIC_SRC" -B "$INST" -DCMAKE_INSTALL_PREFIX="$PREFIX" \
      -DPIC_ENABLE_TESTING=OFF -DPIC_ENABLE_TERM=ON \
      ${CMAKE_GENERATOR:+-G "$CMAKE_GENERATOR"} >/dev/null
cmake --build "$INST" >/dev/null
cmake --install "$INST" >/dev/null

cmake -S "$HERE/find_package" -B "${BUILD}-fp" -Dpic_DIR="$PREFIX/lib/cmake/pic" \
      ${CMAKE_GENERATOR:+-G "$CMAKE_GENERATOR"} >/dev/null
cmake --build "${BUILD}-fp" >/dev/null
"${BUILD}-fp/found_app"
echo "-- find_package: ok"

echo
echo "ok: pic is usable as a subproject and as an installed package, in both"
echo "    cases by linking the target alone"
