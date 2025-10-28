#!/bin/bash

# generate
fypp pic_array_cpu.fypp >& pic_array.f90
fypp stdlib_pic_hash_32bit.fypp >& pic_hash_32bit.f90
fypp stdlib_pic_hash_32bit_fnv.fypp >& pic_hash_32bit_fnv.f90
fypp stdlib_pic_ascii.fypp >& pic_ascii.f90
fypp stdlib_pic_string_type.fypp >& pic_string_type.F90
fypp stdlib_pic_strings.fypp     >& pic_strings.f90
fypp stdlib_pic_string_type_constructor.fypp >& pic_string_type_constructor.f90
fypp stdlib_pic_strings_to_strings.fypp >& pic_strings_to_strings.F90

# copy
cp pic_array.f90 ../../src/lib/core/arrays/
cp pic_hash_32bit.f90 ../../src/lib/core/hash
cp pic_hash_32bit_fnv.f90 ../../src/lib/core/hash
cp pic_ascii.f90 ../../src/lib/core/strings
cp pic_string_type.F90 ../../src/lib/core/strings
cp pic_strings.f90 ../../src/lib/core/strings
cp pic_string_type_constructor.f90 ../../src/lib/core/strings
cp pic_strings_to_strings.F90 ../../src/lib/core/strings

# cleanup
rm *.f90
rm *.F90

pre-commit run --all
