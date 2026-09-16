# pic roadmap

Engineering notes, not user documentation. FORD reads `docs/pages` and Sphinx
reads `docs/manual/source`, so nothing here reaches the published manual.

Written 2026-09-15, against `1256ca40` (tag `v0.8.0`).

Each item states what is wrong, the evidence for it, the fix, and how to know
the fix worked. Where a claim was verified in a scratch tree, it says so; where
it is a judgement call, it says that instead.

## Order

| # | Item | Why now | Rough size |
|---|---|---|---|
| [01](01-release-hygiene.md) | Finish the 0.8.0 release | The tag and the build metadata disagree **today** | ~20 min |
| [02](02-debug-fpe-traps.md) | Two tests cannot run in Debug | A whole build type is unusable and CI does not see it | ~30 min |
| [03](03-soa-template-packaging.md) | Make the SoA generator reachable | The documented way to use it is "edit pic's source" | half a day |
| [04](04-fairport-m1-consumers.md) | `pic_ansi` and `pic_term` meet their first user | Driven by fairport M1, not by pic | reactive |
| [05](05-soa-field-model.md) | Rank-2 and `character(len=N)` SoA fields | Only if fairport collapses its template onto pic's | 1-2 days |

**01 and 02 are the only items where something is actually broken.** Everything
below them is unfinished rather than wrong, and can wait for a reason.

## What prompted this

fairport milestone 0 was built against pic v0.8.0 in one session. Four of the
five features `pic-features-design.md` tagged M0 went straight into load-bearing
use:

| Feature | Where it landed in fairport |
|---|---|
| 64-bit array hash | `core_log` — the determinism digest, which is the point of the project |
| `pic_vector` | the airport loader, including `take` |
| `pic_cli` | `app/main.f90` |
| `pic_random_dist` | `sys_arrival`, integer tier |
| `pic_clock` | unused, correctly — its own rule bars it from `core/`, and its job is M1 frame pacing |

`pic_heap` was the unplanned win: it replaced the hand-rolled binary heap the
Fortran design document budgets ~80 lines for, and its FIFO tie-break on equal
keys supplies the event model's `seq` semantics for free, enforced by the
container rather than by every caller.

The SoA generator was the one M0-targeted piece fairport did **not** use, for a
reason that turned out to be packaging rather than design. That is item 03.
