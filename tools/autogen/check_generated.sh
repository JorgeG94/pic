#!/bin/bash
#
# Verify that the fypp-generated sources committed under src/ still match the
# templates in this directory.
#
# The committed files are fypp output *plus* the formatting pre-commit applies
# (fprettify), because tools/autogen/autogen.sh finishes with
# `pre-commit run --all`. This script reproduces exactly that pipeline into a
# scratch directory and diffs the result against what is in the tree, so it
# never writes to src/.
#
# The list of templates, output names and destination directories is parsed out
# of autogen.sh itself, so adding a module there is automatically covered here.
# The same goes for the `python3 <script>.py` lines, whose .inc output is
# committed alongside the templates that include it.
#
# Usage: tools/autogen/check_generated.sh
# Exit status: 0 when everything matches, 1 when a generated file has drifted.

set -uo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$here/../.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

status=0
drifted=()
checked=0

# Some templates #:include a file that is itself generated, by a Python script
# rather than by fypp -- the exponential inverse-CDF table, for one. Those
# scripts write into tools/autogen and their output is committed there, so
# check them by re-running each in a scratch copy of the directory and diffing.
while read -r script; do
   [ -n "$script" ] || continue
   gen_work="$work/gen-${script%.py}"
   mkdir -p "$gen_work"
   cp "$here"/*.py "$gen_work/" 2>/dev/null
   if ! (cd "$gen_work" && python3 "$script" >/dev/null); then
      echo "check_generated.sh: $script failed" >&2
      status=1
      continue
   fi
   for produced in "$gen_work"/*.inc; do
      [ -f "$produced" ] || continue
      name="$(basename "$produced")"
      checked=$((checked + 1))
      if diff -u --label "regenerated/$name" --label "tools/autogen/$name" \
              "$produced" "$here/$name"; then
         echo "ok: tools/autogen/$name matches $script"
      else
         echo "DRIFT: tools/autogen/$name no longer matches $script" >&2
         drifted+=("tools/autogen/$name")
         status=1
      fi
   done
done < <(sed -nE 's|^[[:space:]]*python3[[:space:]]+([^[:space:]]+\.py).*|\1|p' "$here/autogen.sh")

# Map generated file name -> destination directory, from the `cp` lines of
# autogen.sh (paths there are relative to tools/autogen).
declare -A dest
while read -r out dir; do
   [ -n "$out" ] || continue
   dest["$out"]="${dir%/}"
done < <(sed -nE 's|^[[:space:]]*cp[[:space:]]+([^[:space:]]+)[[:space:]]+([^[:space:]]+).*|\1 \2|p' "$here/autogen.sh")

# Walk the `fypp <template> >& <output>` lines of autogen.sh.
while read -r template out; do
   [ -n "$template" ] || continue
   dir="${dest[$out]:-}"
   if [ -z "$dir" ]; then
      echo "check_generated.sh: autogen.sh generates '$out' but never copies it anywhere" >&2
      status=1
      continue
   fi
   if [ ! -d "$here/$dir" ]; then
      echo "check_generated.sh: destination directory '$here/$dir' from autogen.sh does not exist" >&2
      status=1
      continue
   fi
   committed="$(cd "$here/$dir" && pwd)/$out"
   if [ ! -f "$committed" ]; then
      echo "check_generated.sh: expected generated file '$committed' is missing" >&2
      status=1
      continue
   fi

   if ! fypp "$here/$template" "$work/$out"; then
      echo "check_generated.sh: fypp failed on $template" >&2
      status=1
      continue
   fi
   # Same formatting step pre-commit applies to every Fortran source.
   if ! fprettify --silent "$work/$out"; then
      echo "check_generated.sh: fprettify failed on generated $out" >&2
      status=1
      continue
   fi

   checked=$((checked + 1))
   rel="${committed#"$repo_root"/}"
   if diff -u --label "regenerated/$out" --label "$rel" "$work/$out" "$committed"; then
      echo "ok: $rel matches $template"
   else
      echo "DRIFT: $rel no longer matches tools/autogen/$template" >&2
      drifted+=("$rel")
      status=1
   fi
done < <(sed -nE 's|^[[:space:]]*fypp[[:space:]]+([^[:space:]]+)[[:space:]]*>&[[:space:]]*([^[:space:]]+).*|\1 \2|p' "$here/autogen.sh")

if [ "$checked" -eq 0 ] && [ "$status" -eq 0 ]; then
   echo "check_generated.sh: parsed no fypp commands out of autogen.sh" >&2
   status=1
fi

if [ ${#drifted[@]} -gt 0 ]; then
   echo >&2
   echo "The following committed files no longer match their fypp templates:" >&2
   for f in "${drifted[@]}"; do
      echo "  - $f" >&2
   done
   echo >&2
   echo "These files are generated; hand edits to them are lost on the next" >&2
   echo "regeneration. Fix the template in tools/autogen/, then run" >&2
   echo "  tools/autogen/autogen.sh" >&2
   echo "from that directory and commit the regenerated sources." >&2
fi

exit "$status"
