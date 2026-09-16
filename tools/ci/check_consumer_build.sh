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
