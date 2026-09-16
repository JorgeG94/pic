# 03 — Make the SoA generator reachable from outside pic

**Status:** unfinished, not broken. **Size:** half a day.

## The situation

`pic_soa` is the largest single feature in pic — 1144 lines of hand-written
runtime (`src/lib/core/soa/pic_soa.f90`) sitting on `pic_serialize`, providing
growth policy, a 16-byte stream header with magic and byte-order mark, a
versioned schema prefix, length-prefixed type-tagged field records,
endianness-detecting reads, a hash prologue, size prediction and six validation
helpers.

`tools/autogen/pic_soa.fypp` generates thin containers over it, and
`src/lib/core/soa/pic_soa_particle.f90` is the one committed example.

**It has no external consumer, and cannot easily have one.** Three separate
things stand in the way.

## Wart 1 — the macro file invokes itself

`tools/autogen/pic_soa.fypp` is 339 lines. Line 338 is `#:enddef`; line 339 is a
bare `$:soa_module('pic_soa_particle', ...)` at file scope.

So a downstream template that does `#:include 'pic_soa.fypp'` gets
`pic_soa_particle` emitted into its own output file, alongside whatever it
actually wanted.

**Verified in a scratch tree:** splitting at `#:enddef` into a macro-only
`pic_soa.fypp` (lines 1-338) plus a two-line `pic_soa_particle.fypp`

```
#:include 'pic_soa.fypp'
$:soa_module('pic_soa_particle', 'particle_soa_t', 'particle', [...])
```

regenerates a `pic_soa_particle.f90` that is **byte-identical** to the committed
one, and a downstream template then emits only its own module. `fprettify` is a
no-op on this output, so the identity holds without it.

`tools/autogen/check_generated.sh` parses `fypp <template> >& <output>` lines
out of `autogen.sh`, so changing that one line is picked up with no other edit.

## Wart 2 — the templates are not installed

`CMakeLists.txt` has four `install()` calls (lines 156, 162, 180, 184) covering
archives, `.mod` files and the CMake package config. `tools/autogen/` is not
among them.

A `find_package(pic)` consumer therefore cannot reach `pic_soa.fypp` at all.
(A consumer using `add_subdirectory` or FetchContent of a source tree can, since
it has the whole checkout — which is how fairport would have done it.)

## Wart 3 — the documented interface is "edit our source"

```
docs/manual/source/features.rst:754-755

  To generate a container for your own particle type, edit
  ``tools/autogen/pic_soa.fypp`` and regenerate -- see :doc:`contributing`.
```

`contributing.rst:137-147` reinforces it.

This is the real problem, and it is worth stating plainly: the documented way to
use pic's largest feature is to modify pic's source tree. That is not an
interface. Warts 1 and 2 are consequences of nobody having needed one yet.

## The fix

1. **Split** `tools/autogen/pic_soa.fypp` at `#:enddef` into a macro library plus
   `pic_soa_particle.fypp`; update the one `fypp` line in `autogen.sh`. Output is
   provably unchanged.
2. **Install** the templates and make the path discoverable:
   ```cmake
   install(DIRECTORY tools/autogen/ DESTINATION ${CMAKE_INSTALL_DATADIR}/pic/autogen
           FILES_MATCHING PATTERN "*.fypp")
   ```
   and export that directory from `picConfig.cmake.in` as something like
   `PIC_AUTOGEN_DIR`, so a consumer can pass it to `fypp -I`.
3. **Rewrite** `features.rst:754-755` to document the actual interface:
   ```
   #:include 'pic_soa.fypp'
   $:soa_module('my_soa', 'my_soa_t', 'my', [('id', 'i32', 'Identifier.'), ...])
   ```
   with a note that the generated `.f90` should be committed, exactly as pic
   commits its own, so that consumers of *that* project need no fypp either.

## The decision hiding inside this

Doing 1-3 turns the macro into a **public API**, and pic then owes downstreams:

- stability of the macro signature,
- stability of the generated code's dependencies (`pic_types`, `pic_error`,
  `pic_array_hash`, `pic_soa` — all already public),
- and, more seriously, **backward compatibility of the serialized layout**. The
  schema string embeds `pic_soa/1`. The moment a consumer writes checkpoints,
  pic owes that consumer a stable wire format across pic releases. Today pic
  owes nobody that.

That is a real commitment and worth making deliberately rather than by accident.
It is also the reason the version of this work that fairport actually needs is
much smaller — see below.

## What a consumer needs first, and it is not this

fairport needed the *runtime*, not the template. Calling `soa_write_prologue`,
`soa_write_field`, `soa_read_field`, `soa_hash_begin` and `soa_hash_field`
directly requires **none** of items 1-3: they are ordinary public procedures in
an installed `.mod`.

fairport now does exactly that, keeping its own small template for the container
shape. It got the correct wire format — header, tags, length validation,
endianness — with none of the cross-repo coupling.

**So: 1-3 are worth doing for the documentation fix alone (wart 3 is
embarrassing), but they are not blocking anybody.** Treat them as a
documentation and packaging release, not as a prerequisite for anything.

## How to know it worked

```bash
tools/autogen/autogen.sh
git diff --exit-code src/lib/core/soa/pic_soa_particle.f90   # must be empty
ctest --test-dir build -R pic_soa                            # still green
```

and, in a scratch directory outside pic:

```bash
printf "#:include 'pic_soa.fypp'\n\$:soa_module('probe','probe_t','probe',[('a','i32','A.')])\n" > probe.fypp
fypp -I <installed>/share/pic/autogen probe.fypp | grep '^module'
# must print exactly: module probe
```
