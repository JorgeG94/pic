# PIC Fortran Style Guide

This document defines the coding conventions for all Fortran code within the PIC project.

## Naming Conventions

### Files
- Module files: `pic_<name>.f90` (lowercase with underscores)
- Preprocessed files: `pic_<name>.F90` (uppercase extension)
- Test files: `test_pic_<name>.f90`

### Modules
- Module names match filenames: `pic_<name>`
- One module per file (except for submodules)

```fortran
! Good
module pic_physical_fragment

! Bad
module PhysicalFragment
module physical_fragment  ! missing pic_ prefix
```

### Derived Types
- All derived types use `_t` suffix
- Use lowercase with underscores

```fortran
! Good
type :: calculation_result_t
type :: physical_fragment_t
type :: energy_t

! Bad
type :: CalculationResult
type :: calculation_result   ! missing _t suffix
type :: TFragment
```

### Variables and Procedures
- Use lowercase with underscores (snake_case)
- Use descriptive names - avoid single letters except for loop indices or if it is a very local, easy to deduce variable

```fortran
! Good
integer(default_int) :: n_atoms
real(dp) :: total_energy
subroutine calculate_gradient(fragment, result)

! Bad
integer(default_int) :: nAtoms      ! camelCase
integer(default_int) :: na          ! too short
real(dp) :: E                       ! too short, unclear
```

### Constants
- Use UPPERCASE with underscores for parameters

```fortran
! Good
integer(default_int), parameter :: MAX_ITERATIONS = 100
real(dp), parameter :: BOHR_TO_ANGSTROM = 0.529177210903_dp

! Bad
integer(default_int), parameter :: maxIterations = 100
```

## Required Practices

### Use Statements
- **Always use `only` clause** - enforced by `fortitude` linter

```fortran
! Good
use pic_types, only: dp, default_int
use pic_error, only: error_t

! Bad - will fail linter
use pic_types
use pic_error
```

### Implicit None
- **Always include `implicit none`** in modules and programs

```fortran
module pic_example
   use pic_types, only: dp
   implicit none
   private
   ! ...
end module pic_example
```

### Intent Declarations
- **Always declare intent** for all procedure arguments

```fortran
! Good
subroutine compute_energy(fragment, result)
   type(physical_fragment_t), intent(in) :: fragment
   type(calculation_result_t), intent(out) :: result

! Bad
subroutine compute_energy(fragment, result)
   type(physical_fragment_t) :: fragment  ! missing intent
```

### Private by Default (linter enforced)
- Modules should be `private` by default
- Explicitly declare `public` entities

```fortran
module pic_example
   implicit none
   private

   public :: my_public_type_t
   public :: my_public_subroutine

   type :: my_public_type_t
      ! ...
   end type

   type :: internal_helper_t  ! stays private
      ! ...
   end type
end module
```

### Limit Procedure Arguments
- Public subroutines/functions should have **6 or fewer arguments**
- If more are needed, group related arguments into a derived type
- This improves readability, maintainability, and makes API changes easier

```fortran
! Bad - too many arguments
subroutine run_calculation(coords, elements, charge, multiplicity, &
                           method, basis, max_iter, tolerance, &
                           energy, gradient, error)
   real(dp), intent(in) :: coords(:,:)
   integer(default_int), intent(in) :: elements(:)
   integer(default_int), intent(in) :: charge, multiplicity
   character(*), intent(in) :: method, basis
   integer(default_int), intent(in) :: max_iter
   real(dp), intent(in) :: tolerance
   real(dp), intent(out) :: energy
   real(dp), intent(out) :: gradient(:,:)
   type(error_t), intent(out) :: error
   ! 11 arguments - hard to use and maintain

! Good - grouped into logical types
subroutine run_calculation(system, config, result, error)
   type(system_t), intent(in) :: system                ! coords, elements, charge, mult
   type(calc_config_t), intent(in) :: config           ! method, basis, max_iter, tol
   type(calculation_result_t), intent(out) :: result   ! energy, gradient
   type(error_t), intent(out), optional :: error
   ! 4 arguments - clean and extensible
```

- **Exception**: Simple utility functions (e.g., `to_bohr(value)`) can have minimal arguments
- Internal/private procedures have more flexibility but should still aim for clarity

## Forbidden Practices

### No GOTO Statements
- Use structured control flow (`do`, `if`, `select case`, `exit`, `cycle`)

```fortran
! Bad
   if (error) goto 999
   ! ...
999 continue
   call logger%error("Error occurred")

! Good
   if (error) then
      call logger%error("Error occurred")
      return
   end if
```

### No Arithmetic IF
- Use `if-then-else` or `select case`

```fortran
! Bad (arithmetic IF - Fortran 77)
   if (x) 10, 20, 30

! Good
   if (x < 0) then
      ! negative case
   else if (x == 0) then
      ! zero case
   else
      ! positive case
   end if
```

### No Implicit SAVE
- Avoid module-level variables that retain state
- If state is needed, use explicit `save` attribute and document why

```fortran
! Bad - implicit save behavior
module pic_bad_example
   integer(default_int) :: counter = 0  ! implicitly saved, retains value between calls
end module

! Good - use derived types to manage state
module pic_good_example
   type :: counter_t
      integer(default_int) :: value = 0
   end type
end module
```

### No COMMON Blocks
- Use module variables or derived types instead

```fortran
! Bad
common /shared_data/ x, y, z

! Good
module pic_shared_data
   use pic_types, only: dp
   implicit none
   type :: shared_data_t
      real(dp) :: x, y, z
   end type
end module
```

### No Naked Print Statements
- **NEVER** use `print *` or `write(*,*)` for output
- **ALWAYS** use `pic_logger` (`global_logger`)
- This is enforced - naked prints will be rejected in code review

```fortran
! Bad - forbidden
print *, "Starting calculation"
write(*,*) "Energy:", energy
write(6,*) "Done"

! Good - use logger
use pic_logger, only: logger => global_logger
call logger%info("Starting calculation")
call logger%info("Energy: " // to_char(energy))
call logger%info("Done")
```

### No Emojis in Fortran Code
- No emojis in Fortran source files (`.f90`, `.F90`)
- This includes comments, strings, and documentation
- Keep output professional and portable

```fortran
! Bad
call logger%info("Calculation complete! 🎉")
!! 🚀 Fast implementation

! Good
call logger%info("Calculation complete")
!! Fast implementation
```

- Python scripts (validation, tooling) may use emojis if desired

### No EQUIVALENCE
- Use proper type conversions or `transfer()` if absolutely necessary

### No Fixed-Form Source
- All code must be free-form (`.f90` / `.F90`)
- No `.f` or `.F` files

### No Assumed-Size Arrays
- Use assumed-shape arrays with explicit interface

```fortran
! Bad
subroutine process_array(arr, n)
   real(dp) :: arr(*)  ! assumed-size
   integer(default_int) :: n

! Good
subroutine process_array(arr)
   real(dp), intent(in) :: arr(:)  ! assumed-shape
```

### No External Statements for Internal Procedures
- Use `contains` for internal procedures
- Use explicit interfaces via modules

```fortran
! Bad
external :: my_function

! Good - use modules
use pic_my_module, only: my_function
```

## Recommended Practices

### Use PIC Library Utilities
- **Always prefer pic functionality** over implementing your own
- Ensures consistency, reduces code duplication, and leverages tested implementations

| pic Module | Functionality | Use Instead Of |
|------------|---------------|----------------|
| `pic_types` | Kind parameters (`dp`, `default_int`, etc.) | Literal kinds (`real(8)`) |
| `pic_logger` | Logging (`global_logger`) | `print *` / `write(*,*)` (**forbidden**) |
| `pic_timer` | Performance timing | Manual `cpu_time` calls |
| `pic_strings` | String utilities | Custom string manipulation |
| `pic_sorting` | Sorting algorithms | Hand-rolled sorts |
| `pic_test_helpers` | Test utilities (`is_equal`) | Custom comparison functions |

```fortran
! Good - use pic utilities
use pic_logger, only: logger => global_logger
use pic_timer, only: timer_t
use pic_sorting, only: argsort

call logger%info("Starting calculation")
call logger%debug("Processing index " // to_char(i))
call logger%warning("Large system detected, may be slow")
call logger%error("Invalid input")

type(timer_t) :: timer
call timer%start()
! ... work ...
call timer%stop()
call logger%info("Elapsed: " // timer%elapsed_string())

indices = argsort(energies)  ! sorted indices

! Bad - NEVER use naked print statements
print *, "Starting calculation"
write(*,*) "Debug info"
```

### Kind Parameters
- Use `pic_types` for portable kind definitions
- Never use literal kind numbers

```fortran
! Good
use pic_types, only: dp, default_int
real(dp) :: energy
integer(default_int) :: count

! Bad
real(8) :: energy      ! non-portable
real*8 :: energy       ! obsolete syntax
double precision :: e  ! obsolete
```

### Array Operations
- Prefer whole-array operations over explicit loops when clear

```fortran
! Good
gradient = 0.0_dp
total = sum(energies)

! Also fine for complex operations
do i = 1, n_atoms
   gradient(:, i) = gradient(:, i) + contribution(:, i)
end do
```

### Block Constructs for Limiting Scope
- Use `block` to limit variable scope and improve readability
- Useful for temporary variables needed only in a small section
- Helps prevent accidental reuse of variables

```fortran
! Good - temporary variables scoped to where they're needed
subroutine process_fragments(fragments, total_energy)
   type(fragment_t), intent(in) :: fragments(:)
   real(dp), intent(out) :: total_energy

   integer(default_int) :: i

   total_energy = 0.0_dp
   do i = 1, size(fragments)
      block
         real(dp) :: frag_energy
         real(dp) :: correction

         call compute_energy(fragments(i), frag_energy)
         call compute_correction(fragments(i), correction)
         total_energy = total_energy + frag_energy + correction
      end block
   end do
end subroutine
```

### Associate Construct for Readability
- Use `associate` to create short aliases for long expressions
- Improves readability without runtime cost

```fortran
! Good
associate(coords => fragment%coordinates, &
          n => fragment%n_atoms)
   do i = 1, n
      distance = norm2(coords(:, i) - origin)
   end do
end associate
```

### Error Handling
- Use the `error_t` type from `pic_error`
- Check and propagate errors

```fortran
use pic_error, only: error_t

subroutine my_subroutine(input, output, error)
   type(input_t), intent(in) :: input
   type(output_t), intent(out) :: output
   type(error_t), intent(inout), optional :: error

   if (invalid_input) then
      if (present(error)) then
         call error%set(ERROR_VALIDATION, "Invalid input: reason")
      end if
      return
   end if
end subroutine
```

### Documentation
- Use `!!` for FORD documentation comments
- Document public interfaces

```fortran
type :: calculation_result_t
   !! Container for calculation results
   type(energy_t) :: energy
      !! Total and component energies
   real(dp), allocatable :: gradient(:,:)
      !! Gradient (3, n_atoms) in Hartree/Bohr
end type
```

### Memory Management
- Provide `destroy` procedures for types with allocatable components
- Clean up allocatable arrays when no longer needed

```fortran
type :: my_type_t
   real(dp), allocatable :: data(:)
contains
   procedure :: destroy => my_type_destroy
end type

subroutine my_type_destroy(this)
   class(my_type_t), intent(inout) :: this
   if (allocated(this%data)) deallocate(this%data)
end subroutine
```

### Prefer Allocatable Over Pointer
- Use `allocatable` instead of `pointer` when possible
- Allocatable arrays are automatically deallocated when out of scope
- Less risk of memory leaks and dangling pointers
- Compiler can optimize allocatable better

### Pure and Elemental Procedures
- Mark functions/subroutines as `pure` when they have no side effects
- Use `elemental` for scalar operations that should work on arrays
- Enables compiler optimizations and documents intent

```fortran
pure function kinetic_energy(mass, velocity) result(energy)
   real(dp), intent(in) :: mass, velocity
   real(dp) :: energy
   energy = 0.5_dp * mass * velocity**2
end function

elemental function to_bohr(angstrom) result(bohr)
   real(dp), intent(in) :: angstrom
   real(dp) :: bohr
   bohr = angstrom * ANGSTROM_TO_BOHR
end function
```

### Avoid Deep Nesting
- Maximum 3-4 levels of indentation
- Use early returns, `cycle`, `exit` to reduce nesting
- Extract deeply nested code to separate subroutines

### No Magic Numbers
- Use named constants for any non-obvious literal values
- Makes code self-documenting and easier to maintain

### Allocatable Character Strings
- Use `character(len=:), allocatable` for dynamic strings
- Avoid fixed-length strings that waste memory or truncate

### Do Concurrent (Use with Caution)
- `do concurrent` hints to compiler that iterations are independent
- Can enable auto-parallelization and vectorization
- Compiler support varies; correctness is more important than optimization

### Submodules for Large Modules
- Use submodules to separate interface from implementation
- Reduces recompilation when only implementation changes

## File Structure Template

```fortran
!! Brief module description
module pic_example
   !! Extended module documentation
   use pic_types, only: dp, default_int
   use pic_other_module, only: needed_type_t
   implicit none
   private

   public :: example_type_t
   public :: example_subroutine

   type :: example_type_t
      !! Type documentation
      integer(default_int) :: n_items
         !! Number of items
      real(dp), allocatable :: values(:)
         !! Array of values in Hartree
   contains
      procedure :: compute => example_compute
      procedure :: destroy => example_destroy
   end type example_type_t

contains

   subroutine example_compute(this, input, output)
      !! Subroutine documentation
      class(example_type_t), intent(inout) :: this
      real(dp), intent(in) :: input
      real(dp), intent(out) :: output

      ! Implementation
   end subroutine example_compute

   subroutine example_destroy(this)
      !! Clean up allocated memory
      class(example_type_t), intent(inout) :: this

      if (allocated(this%values)) deallocate(this%values)
   end subroutine example_destroy

end module pic_example
```

## Enforcement

- **Linter**: Run `fortitude check` before committing
- **CI**: GitHub Actions will fail if linter errors exist
- **Code Review**: Verify conventions are followed in PRs
