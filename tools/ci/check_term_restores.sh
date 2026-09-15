#!/usr/bin/env bash
# Verify that pic_term restores the terminal mode after `error stop`.
#
# A TUI that leaves the shell in raw mode -- no echo, no line editing, and the
# user cannot see what they type to fix it -- is the most user-hostile failure
# available, so this is checked rather than assumed. It needs a real pty, which
# CI does not give a job by default; `script -qec` provides one.
#
# The check runs a program that enters raw mode and then dies of `error stop`,
# comparing `stty -g` before and after. It also runs a negative control that
# leaves the terminal raw on purpose, so a harness that cannot tell the
# difference fails loudly instead of passing everything.
#
# Usage: tools/ci/check_term_restores.sh <path-to-raw-crash-binary>
set -uo pipefail

prog="${1:?usage: check_term_restores.sh <raw-crash binary>}"
[ -x "$prog" ] || { echo "not executable: $prog" >&2; exit 2; }

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cat > "$work/under_pty.sh" <<'INNER'
#!/usr/bin/env bash
set -u
before=$(stty -g)
"$1"
echo "PROGRAM_EXIT=$?"
after=$(stty -g)
[ "$before" = "$after" ] && echo "MODE_RESTORED=yes" || {
   echo "MODE_RESTORED=no"
   echo "  before: $before"
   echo "  after:  $after"
}
INNER

cat > "$work/negctl.sh" <<'INNER'
#!/usr/bin/env bash
set -u
before=$(stty -g)
stty raw -echo
after=$(stty -g)
stty "$before"
[ "$before" = "$after" ] && echo "NEGCTL=blind" || echo "NEGCTL=sees"
INNER

chmod +x "$work/under_pty.sh" "$work/negctl.sh"

echo "== negative control: can this harness see an unrestored terminal? =="
neg=$(script -qec "$work/negctl.sh" /dev/null 2>&1 | tr -d '\r')
echo "$neg"
if ! grep -q "NEGCTL=sees" <<< "$neg"; then
   echo "REFUSING: the harness cannot distinguish raw from cooked, so a pass here" >&2
   echo "would mean nothing. Check that script(1) really allocates a pty." >&2
   exit 1
fi

echo
echo "== pic_term: raw mode, then error stop =="
out=$(script -qec "$work/under_pty.sh $prog" /dev/null 2>&1 | tr -d '\r')
echo "$out"

if ! grep -q "RAWON" <<< "$out"; then
   echo "REFUSING: the program never reached raw mode, so nothing was tested." >&2
   exit 1
fi
if ! grep -q "MODE_RESTORED=yes" <<< "$out"; then
   echo "FAILED: the terminal was left in raw mode after error stop." >&2
   exit 1
fi

echo
echo "ok: the terminal mode is restored after error stop"
