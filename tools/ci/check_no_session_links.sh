#!/usr/bin/env bash
# Fail if any commit message, or any line added to a file, carries an assistant
# session URL.
#
# Such a URL points at a private transcript: it is meaningless to everyone
# reading the repository, it never resolves for them, and once it is in a
# commit message it can only be removed by rewriting history. Co-authorship
# trailers are fine and are deliberately not matched here -- attribution is
# useful, a dead private link is not.
#
# Usage:
#   tools/ci/check_no_session_links.sh [BASE [HEAD]]
#
# BASE defaults to origin/main (or main), HEAD to the current commit, so a
# bare run checks what this branch adds on top of main.
set -uo pipefail

# Assembled rather than written out, so that this file does not match its own
# pattern and report itself.
host='claude\.ai'
path='code/se''ssion'
PATTERN="${host}/${path}|^[[:space:]]*Claude-Se""ssion:"

SELF="tools/ci/check_no_session_links.sh"
WORKFLOW=".github/workflows/no-session-links.yml"

BASE="${1:-}"
HEAD="${2:-HEAD}"

if [ -z "$BASE" ]; then
   if git rev-parse --verify -q origin/main >/dev/null; then
      BASE=origin/main
   elif git rev-parse --verify -q main >/dev/null; then
      BASE=main
   else
      echo "cannot determine a base revision; pass one explicitly" >&2
      exit 2
   fi
fi

if ! MERGE_BASE=$(git merge-base "$BASE" "$HEAD" 2>/dev/null); then
   # Unrelated histories, or a base that is not an ancestor: check HEAD alone
   # rather than silently checking nothing.
   MERGE_BASE="$HEAD^"
fi

status=0

echo "Scanning $(git rev-parse --short "$MERGE_BASE")..$(git rev-parse --short "$HEAD")"
echo

# --- commit messages -------------------------------------------------------
bad_commits=()
while IFS= read -r sha; do
   [ -n "$sha" ] || continue
   if git log -1 --format='%B' "$sha" | grep -qEi "$PATTERN"; then
      bad_commits+=("$sha")
   fi
done < <(git rev-list "$MERGE_BASE..$HEAD")

if [ ${#bad_commits[@]} -gt 0 ]; then
   status=1
   echo "Commit messages containing a session URL:"
   for sha in "${bad_commits[@]}"; do
      printf '  %s  %s\n' "$(git rev-parse --short "$sha")" "$(git log -1 --format='%s' "$sha")"
   done
   echo
fi

# --- added lines in files --------------------------------------------------
# The checker and its workflow are excluded: they describe the pattern, so
# matching them would be a permanent self-report.
added=$(git diff --unified=0 "$MERGE_BASE..$HEAD" -- . \
   ":(exclude)$SELF" ":(exclude)$WORKFLOW" |
   grep -E '^\+' | grep -v '^+++' | grep -Ei "$PATTERN" || true)

if [ -n "$added" ]; then
   status=1
   echo "Lines added to files containing a session URL:"
   printf '%s\n' "$added" | sed 's/^/  /'
   echo
fi

if [ "$status" -ne 0 ]; then
   cat >&2 <<'EOF'
A session URL points at a private transcript that nobody else can open.
Remove it before merging.

  - in a file: delete the line and amend or add a commit
  - in a commit message: git rebase -i and reword the commits listed above
    (or, for the most recent commit, git commit --amend)

Co-authorship trailers are not matched by this check and may stay.
EOF
   exit 1
fi

echo "No session URLs in commit messages or added lines."
