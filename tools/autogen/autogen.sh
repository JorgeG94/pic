#!/bin/bash
#
# Regenerate the committed fypp sources under src/ and reformat them.
#
# NOTE: tools/autogen/check_generated.sh (run in CI by
# .github/workflows/check-autogen.yml) parses the `fypp <template> >& <output>`
# and `cp <output> <dir>` lines below to learn which modules are generated and
# where they live, so adding a module here is picked up automatically. Keep
# those two line shapes intact, one command per line.

# generate
python3 gen_exp_table.py
fypp pic_array_cpu.fypp >& pic_array.f90
fypp stdlib_pic_hash_32bit.fypp >& pic_hash_32bit.f90
fypp stdlib_pic_hash_32bit_fnv.fypp >& pic_hash_32bit_fnv.f90
fypp stdlib_pic_ascii.fypp >& pic_ascii.f90
fypp stdlib_pic_string_type.fypp >& pic_string_type.F90
fypp stdlib_pic_strings.fypp     >& pic_strings.f90
fypp stdlib_pic_string_type_constructor.fypp >& pic_string_type_constructor.f90
fypp stdlib_pic_strings_to_strings.fypp >& pic_strings_to_strings.F90
fypp pic_array_hash.fypp >& pic_array_hash.f90
fypp pic_soa.fypp >& pic_soa_particle.f90
fypp pic_vector.fypp >& pic_vector.f90
fypp pic_random_dist.fypp >& pic_random_dist.f90

# copy
cp pic_array.f90 ../../src/lib/core/arrays/
cp pic_hash_32bit.f90 ../../src/lib/core/hash
cp pic_hash_32bit_fnv.f90 ../../src/lib/core/hash
cp pic_ascii.f90 ../../src/lib/core/strings
cp pic_string_type.F90 ../../src/lib/core/strings
cp pic_strings.f90 ../../src/lib/core/strings
cp pic_string_type_constructor.f90 ../../src/lib/core/strings
cp pic_strings_to_strings.F90 ../../src/lib/core/strings
cp pic_array_hash.f90 ../../src/lib/core/hash
cp pic_soa_particle.f90 ../../src/lib/core/soa
cp pic_vector.f90 ../../src/lib/core/vector
cp pic_random_dist.f90 ../../src/lib/core/random_dist

# cleanup
rm *.f90
rm *.F90

pre-commit run --all
