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

echo
echo "ok: pic configures, builds and runs as a subproject with PIC_ENABLE_TERM=ON"
