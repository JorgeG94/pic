# 05 — Rank-2 and `character(len=N)` SoA fields

**Status:** speculative. Do not start this without a consumer. **Size:** 1-2 days.

## The gap

`soa_module` takes a flat list of rank-1 scalar fields:

```
tools/autogen/pic_soa.fypp:28
  SOA_DECL = {'i32': ..., 'i64': ..., 'r32': ..., 'r64': ..., 'bool': ...}
```

Two shapes a real consumer wants are not expressible.

**Rank-2 fields.** fairport's aircraft container holds a route matrix of shape
`(MAX_ROUTE, capacity)` — one column per aircraft. There is no way to declare
that, and `pic_serialize`'s `write_array` / `read_array`
(`src/lib/core/serialize/pic_serialize.f90:135-169`) are rank-1 only.

**Character fields.** fairport has three: `callsign`, `gate_name`,
`runway_name`, all `character(len=8)` arrays parallel to the entity arrays. They
currently live outside the container as sibling arrays in `world_t`.

## What fairport did instead, and why that is fine

fairport keeps its own small template (`tools/autogen/core_aircraft.fypp`) whose
generated bodies call pic's runtime primitives directly. For the route matrix it
flattens to a rank-1 record in Fortran column-major order and reshapes on read:

```fortran
flat = reshape(this%route(:, 1:this%n), [AIRCRAFT_ROUTE_ROWS*this%n])
call soa_write_field(stream, flat, AIRCRAFT_ROUTE_ROWS*this%n, fault)
```

That is ~10 lines and it works. It costs one temporary at load time, which is
not a hot path.

So the case for building this into pic is **not** "a consumer cannot proceed" —
one already has. It is "three or four consumers will each write that reshape,
and one of them will get the column-major order backwards."

## When it becomes worth doing

When fairport grows `gate_soa_t` and `runway_soa_t`. The Fortran design document
§3.5 wants both; they are currently loose parallel arrays in `world_t`
(`core/core_world.f90`). At that point the duplication is 3x rather than 1x, and
collapsing fairport's template onto pic's macro starts to pay — which needs this
item first.

Until then, building it is speculative generality in the library that exists
specifically to build everywhere without surprises.

## If it is done, do both halves together

**Character is the easier half and should come first.** `pic_serialize` already
has `write_char_record` / `read_char_record`, so a `('name', 'c8', ...)` field
kind is mostly schema encoding plus a generic specific. The schema string needs
the length in it (`name:c8`) so that a container written with `len=8` cannot be
read back into one expecting `len=16`.

**Rank-2 is the harder half.** It needs:

- a field tuple that carries the leading extent, e.g. `('route', 'i32', 64, ...)`;
- the extent in the schema string, for the same reason as the character length;
- a decision about whether the record is written flat with the extent implied by
  the schema, or with its own shape prefix. Flat-plus-schema is simpler and
  matches how fairport already does it by hand;
- rank-2 overloads in `pic_serialize`, or a documented flatten-at-the-edge rule.
  The second is less code and keeps `pic_serialize` rank-1, which is worth
  something.

Roughly 15 new module procedures plus tests, and all of it has to be re-proved
on LFortran, NVIDIA and Cray — which is where the real cost sits, not in the
Fortran.

## Prerequisite

This only makes sense after [03](03-soa-template-packaging.md), since the point
is to let a downstream generate from pic's macro rather than its own.
