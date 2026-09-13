! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Bounded, fixed-capacity arrays with no heap allocation in the hot path.
!! The Fortran analogue of C++'s `std::inplace_vector`: push/pop/size
!! semantics on top of storage that lives entirely inside the object.
module pic_fixed_array
   !! Fixed-capacity ("in place") array containers.
   !!
   !! Motivation: HPC inner loops where the upper bound on the number of
   !! elements is known ahead of time, where an `allocate` is unacceptable,
   !! and where push/pop/size semantics with bounds checking are still
   !! wanted. The element storage is a *fixed-size* component, never
   !! `allocatable` and never a `pointer`, so an object of these types can
   !! live on the stack, in a `block` construct, or as a local of an
   !! OpenMP/OpenACC region without touching the heap.
   !!
   !! Capacity is the module parameter `PIC_FIXED_ARRAY_CAPACITY`. It is a
   !! compile-time constant shared by every instance; change it in one place
   !! if your problem needs a different bound. A deliberate design choice is
   !! that this module does *not* use a parameterized derived type (a `len`
   !! type parameter would be the textbook answer) because PDT support is
   !! poor or absent on several of the compilers PIC exists to support.
   !!
   !! Usage:
   !!   type(fixed_array_int_t) :: stack
   !!   type(error_t) :: err
   !!   integer(default_int) :: value
   !!
   !!   call stack%push_back(42_default_int, err)
   !!   if (err%has_error()) call err%fatal()
   !!   call stack%pop_back(value, err)
   !!
   !! Every operation that can fail reports through `error_t` with
   !! `ERROR_VALIDATION`: overflowing the capacity, popping an empty
   !! container and out-of-range indexing are all errors, never a silent
   !! write past the end and never a crash.
   !!
   !! The `err` argument is `intent(inout)` and is written to *only* when
   !! the operation fails; a successful call leaves it exactly as it was.
   !! This is deliberate and is what keeps the promise of no heap traffic:
   !! `error_t` has an allocatable `message` component, so an
   !! `intent(out)` error argument would make the compiler emit a
   !! deallocation of that component on entry to every single call,
   !! putting a `free` in the middle of the hot loop. Callers that reuse
   !! one `error_t` across several calls should `call err%clear()` between
   !! them, exactly as elsewhere in PIC.
   use pic_types, only: default_int, dp
   use pic_error, only: error_t, ERROR_VALIDATION
   implicit none
   private

   public :: fixed_array_int_t
   public :: fixed_array_dp_t
   public :: PIC_FIXED_ARRAY_CAPACITY

   integer(default_int), parameter :: PIC_FIXED_ARRAY_CAPACITY = 256_default_int
      !! Maximum number of elements any fixed array can hold. Compile-time
      !! constant: the storage of every instance is exactly this many
      !! elements, so a `fixed_array_dp_t` occupies 256 doubles (2 KiB)
      !! regardless of how many elements are live.

   character(len=*), parameter :: MSG_OVERFLOW = "fixed array is full, cannot push_back beyond PIC_FIXED_ARRAY_CAPACITY"
   character(len=*), parameter :: MSG_UNDERFLOW = "fixed array is empty, cannot pop_back"
   character(len=*), parameter :: MSG_OUT_OF_RANGE = "fixed array index out of range, valid range is 1:size()"

   type :: fixed_array_int_t
      !! Bounded array of `integer(default_int)` with fixed inline storage.
      integer(default_int), private :: n_items = 0
         !! Number of live elements, always in `0:PIC_FIXED_ARRAY_CAPACITY`
      integer(default_int), private :: items(PIC_FIXED_ARRAY_CAPACITY)
         !! Inline storage. Deliberately not `allocatable`: no heap, ever.
         !! Elements above `n_items` are undefined and must not be read.
   contains
      procedure :: push_back => fixed_array_int_push_back
      procedure :: pop_back => fixed_array_int_pop_back
      procedure :: at => fixed_array_int_at
      procedure :: get_unchecked => fixed_array_int_get_unchecked
      procedure :: size => fixed_array_int_size
      procedure :: capacity => fixed_array_int_capacity
      procedure :: is_empty => fixed_array_int_is_empty
      procedure :: is_full => fixed_array_int_is_full
      procedure :: clear => fixed_array_int_clear
      procedure :: as_array => fixed_array_int_as_array
   end type fixed_array_int_t

   type :: fixed_array_dp_t
      !! Bounded array of `real(dp)` with fixed inline storage.
      integer(default_int), private :: n_items = 0
         !! Number of live elements, always in `0:PIC_FIXED_ARRAY_CAPACITY`
      real(dp), private :: items(PIC_FIXED_ARRAY_CAPACITY)
         !! Inline storage. Deliberately not `allocatable`: no heap, ever.
         !! Elements above `n_items` are undefined and must not be read.
   contains
      procedure :: push_back => fixed_array_dp_push_back
      procedure :: pop_back => fixed_array_dp_pop_back
      procedure :: at => fixed_array_dp_at
      procedure :: get_unchecked => fixed_array_dp_get_unchecked
      procedure :: size => fixed_array_dp_size
      procedure :: capacity => fixed_array_dp_capacity
      procedure :: is_empty => fixed_array_dp_is_empty
      procedure :: is_full => fixed_array_dp_is_full
      procedure :: clear => fixed_array_dp_clear
      procedure :: as_array => fixed_array_dp_as_array
   end type fixed_array_dp_t

contains

   pure subroutine fixed_array_int_push_back(self, value, err)
      !! Append an element. If the array is already at capacity the element
      !! is *not* stored, the size is left unchanged and `err` is set to
      !! `ERROR_VALIDATION`. On success `err` is left untouched.
      class(fixed_array_int_t), intent(inout) :: self
      integer(default_int), intent(in) :: value
      type(error_t), intent(inout) :: err

      if (self%n_items >= PIC_FIXED_ARRAY_CAPACITY) then
         call err%set(ERROR_VALIDATION, MSG_OVERFLOW)
         return
      end if

      self%n_items = self%n_items + 1
      self%items(self%n_items) = value
   end subroutine fixed_array_int_push_back

   pure subroutine fixed_array_int_pop_back(self, value, err)
      !! Remove and return the last element (LIFO). On an empty array
      !! `value` is set to zero and `err` is set to `ERROR_VALIDATION`.
      !! On success `err` is left untouched.
      class(fixed_array_int_t), intent(inout) :: self
      integer(default_int), intent(out) :: value
      type(error_t), intent(inout) :: err

      if (self%n_items <= 0_default_int) then
         value = 0_default_int
         call err%set(ERROR_VALIDATION, MSG_UNDERFLOW)
         return
      end if

      value = self%items(self%n_items)
      self%n_items = self%n_items - 1
   end subroutine fixed_array_int_pop_back

   pure subroutine fixed_array_int_at(self, index, value, err)
      !! Bounds-checked element access, 1-based. Any index outside
      !! `1:size()` (including indices that are within the capacity but
      !! beyond the live elements) sets `value` to zero and `err` to
      !! `ERROR_VALIDATION`. On success `err` is left untouched.
      class(fixed_array_int_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      integer(default_int), intent(out) :: value
      type(error_t), intent(inout) :: err

      if (index < 1_default_int .or. index > self%n_items) then
         value = 0_default_int
         call err%set(ERROR_VALIDATION, MSG_OUT_OF_RANGE)
         return
      end if

      value = self%items(index)
   end subroutine fixed_array_int_at

   pure function fixed_array_int_get_unchecked(self, index) result(value)
      !! UNCHECKED element access for inner loops: no bounds check at all.
      !! The caller is responsible for guaranteeing `1 <= index <= size()`,
      !! typically by looping over `1, array%size()`. Reading an index
      !! outside that range returns undefined data. Use `at` unless the
      !! bounds check is measurably in the way.
      class(fixed_array_int_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      integer(default_int) :: value

      value = self%items(index)
   end function fixed_array_int_get_unchecked

   pure function fixed_array_int_size(self) result(n)
      !! Number of live elements.
      class(fixed_array_int_t), intent(in) :: self
      integer(default_int) :: n

      n = self%n_items
   end function fixed_array_int_size

   pure function fixed_array_int_capacity(self) result(n)
      !! Maximum number of elements, i.e. `PIC_FIXED_ARRAY_CAPACITY`.
      class(fixed_array_int_t), intent(in) :: self
      integer(default_int) :: n

      n = int(size(self%items), default_int)
   end function fixed_array_int_capacity

   pure function fixed_array_int_is_empty(self) result(empty)
      !! True when no elements are live.
      class(fixed_array_int_t), intent(in) :: self
      logical :: empty

      empty = (self%n_items <= 0_default_int)
   end function fixed_array_int_is_empty

   pure function fixed_array_int_is_full(self) result(full)
      !! True when a further `push_back` would fail.
      class(fixed_array_int_t), intent(in) :: self
      logical :: full

      full = (self%n_items >= PIC_FIXED_ARRAY_CAPACITY)
   end function fixed_array_int_is_full

   pure subroutine fixed_array_int_clear(self)
      !! Drop every element. The storage itself is untouched, so this is a
      !! single integer assignment; the array can be reused immediately.
      class(fixed_array_int_t), intent(inout) :: self

      self%n_items = 0_default_int
   end subroutine fixed_array_int_clear

   pure function fixed_array_int_as_array(self) result(arr)
      !! Copy of the live elements as a plain array of extent `size()`,
      !! not `capacity()`. Returns a zero-length array when empty. This is
      !! how the contents are handed to ordinary Fortran routines; note
      !! that it is the one operation here that does allocate, so keep it
      !! out of the hot path.
      class(fixed_array_int_t), intent(in) :: self
      integer(default_int), allocatable :: arr(:)

      allocate (arr(self%n_items))
      arr(1:self%n_items) = self%items(1:self%n_items)
   end function fixed_array_int_as_array

   pure subroutine fixed_array_dp_push_back(self, value, err)
      !! Append an element. If the array is already at capacity the element
      !! is *not* stored, the size is left unchanged and `err` is set to
      !! `ERROR_VALIDATION`. On success `err` is left untouched.
      class(fixed_array_dp_t), intent(inout) :: self
      real(dp), intent(in) :: value
      type(error_t), intent(inout) :: err

      if (self%n_items >= PIC_FIXED_ARRAY_CAPACITY) then
         call err%set(ERROR_VALIDATION, MSG_OVERFLOW)
         return
      end if

      self%n_items = self%n_items + 1
      self%items(self%n_items) = value
   end subroutine fixed_array_dp_push_back

   pure subroutine fixed_array_dp_pop_back(self, value, err)
      !! Remove and return the last element (LIFO). On an empty array
      !! `value` is set to zero and `err` is set to `ERROR_VALIDATION`.
      !! On success `err` is left untouched.
      class(fixed_array_dp_t), intent(inout) :: self
      real(dp), intent(out) :: value
      type(error_t), intent(inout) :: err

      if (self%n_items <= 0_default_int) then
         value = 0.0_dp
         call err%set(ERROR_VALIDATION, MSG_UNDERFLOW)
         return
      end if

      value = self%items(self%n_items)
      self%n_items = self%n_items - 1
   end subroutine fixed_array_dp_pop_back

   pure subroutine fixed_array_dp_at(self, index, value, err)
      !! Bounds-checked element access, 1-based. Any index outside
      !! `1:size()` (including indices that are within the capacity but
      !! beyond the live elements) sets `value` to zero and `err` to
      !! `ERROR_VALIDATION`. On success `err` is left untouched.
      class(fixed_array_dp_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      real(dp), intent(out) :: value
      type(error_t), intent(inout) :: err

      if (index < 1_default_int .or. index > self%n_items) then
         value = 0.0_dp
         call err%set(ERROR_VALIDATION, MSG_OUT_OF_RANGE)
         return
      end if

      value = self%items(index)
   end subroutine fixed_array_dp_at

   pure function fixed_array_dp_get_unchecked(self, index) result(value)
      !! UNCHECKED element access for inner loops: no bounds check at all.
      !! The caller is responsible for guaranteeing `1 <= index <= size()`,
      !! typically by looping over `1, array%size()`. Reading an index
      !! outside that range returns undefined data. Use `at` unless the
      !! bounds check is measurably in the way.
      class(fixed_array_dp_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      real(dp) :: value

      value = self%items(index)
   end function fixed_array_dp_get_unchecked

   pure function fixed_array_dp_size(self) result(n)
      !! Number of live elements.
      class(fixed_array_dp_t), intent(in) :: self
      integer(default_int) :: n

      n = self%n_items
   end function fixed_array_dp_size

   pure function fixed_array_dp_capacity(self) result(n)
      !! Maximum number of elements, i.e. `PIC_FIXED_ARRAY_CAPACITY`.
      class(fixed_array_dp_t), intent(in) :: self
      integer(default_int) :: n

      n = int(size(self%items), default_int)
   end function fixed_array_dp_capacity

   pure function fixed_array_dp_is_empty(self) result(empty)
      !! True when no elements are live.
      class(fixed_array_dp_t), intent(in) :: self
      logical :: empty

      empty = (self%n_items <= 0_default_int)
   end function fixed_array_dp_is_empty

   pure function fixed_array_dp_is_full(self) result(full)
      !! True when a further `push_back` would fail.
      class(fixed_array_dp_t), intent(in) :: self
      logical :: full

      full = (self%n_items >= PIC_FIXED_ARRAY_CAPACITY)
   end function fixed_array_dp_is_full

   pure subroutine fixed_array_dp_clear(self)
      !! Drop every element. The storage itself is untouched, so this is a
      !! single integer assignment; the array can be reused immediately.
      class(fixed_array_dp_t), intent(inout) :: self

      self%n_items = 0_default_int
   end subroutine fixed_array_dp_clear

   pure function fixed_array_dp_as_array(self) result(arr)
      !! Copy of the live elements as a plain array of extent `size()`,
      !! not `capacity()`. Returns a zero-length array when empty. This is
      !! how the contents are handed to ordinary Fortran routines; note
      !! that it is the one operation here that does allocate, so keep it
      !! out of the hot path.
      class(fixed_array_dp_t), intent(in) :: self
      real(dp), allocatable :: arr(:)

      allocate (arr(self%n_items))
      arr(1:self%n_items) = self%items(1:self%n_items)
   end function fixed_array_dp_as_array

end module pic_fixed_array
