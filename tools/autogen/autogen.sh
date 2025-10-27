#!/bin/bash

# generate
fypp pic_array_cpu.fypp >& pic_array.f90
fypp stdlib_pic_hash_32bit.fypp >& stdlib_pic_hash_32bit.f90
fypp stdlib_pic_hash_32bit_fnv.fypp >& stdlib_pic_hash_32bit_fnv.f90
fypp stdlib_pic_ascii.fypp >& stdlib_pic_ascii.f90
fypp stdlib_pic_string_type.fypp >& stdlib_pic_string_type.F90
fypp stdlib_pic_strings.fypp     >& stdlib_pic_strings.f90

# copy
cp pic_array.f90 ../../src/lib/core/arrays/
cp stdlib_pic_hash_32bit.f90 ../../src/lib/core/hash
cp stdlib_pic_hash_32bit_fnv.f90 ../../src/lib/core/hash
cp stdlib_pic_ascii.f90 ../../src/lib/core/strings
cp stdlib_pic_string_type.F90 ../../src/lib/core/strings
cp stdlib_pic_strings.f90 ../../src/lib/core/strings

# cleanup
rm *.f90
rm *.F90

pre-commit run --all
