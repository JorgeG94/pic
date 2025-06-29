!! This file is intended to contain tools that can be useful when debugging
!! large programs, simple printing routines, extracting data from an array etc.

module pic_debugging_tools
!! simple single include for printing routines
   use pic_matrix_printer, only: print_array, print_array_with_bounds
   use pic_string_utils, only: to_string
   implicit none(type, external)
   public

contains

end module pic_debugging_tools
