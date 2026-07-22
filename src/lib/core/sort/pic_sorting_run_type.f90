! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Shared `run_type` derived type for the ord_sort and sort_index submodules.
!!
!! This type lives in its own tiny module rather than at the parent
!! `pic_sorting` module scope (where submodules would otherwise see it via host
!! association) because NVHPC and AOCC flang do not honor host association of
!! private derived types from a parent module to its submodules — they reject
!! the reference with "Derived type has not been declared". Defining it inside
!! each submodule also fails: AOCC flang's linker then loses the type-descriptor
!! symbol. Routing through an explicit `use` of this auxiliary module is the
!! only form that satisfies GNU, Intel, NVHPC, and AOCC simultaneously.
module pic_sorting_run_type
   use pic_types, only: int_index
   implicit none
   private
   public :: run_type

   type :: run_type
      !! State carried in the merge-sort run stack used by ord_sort and
      !! sort_index helpers.
      integer(int_index) :: base = 0
      integer(int_index) :: len = 0
   end type run_type
end module pic_sorting_run_type
