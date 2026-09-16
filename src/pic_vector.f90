! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Growable, heap-backed arrays with bounds-checked access.
module pic_vector
   !! Growable vectors: `push_back`, `pop_back`, `at`, and amortised O(1)
   !! growth over heap storage.
   !!
   !! ### Choosing between this and `pic_fixed_array`
   !!
   !! `pic_fixed_array` stores its elements inside the object, never touches
   !! the heap, and stops at `PIC_FIXED_ARRAY_CAPACITY`. Use it in hot loops
   !! and inside OpenMP/OpenACC regions.
   !!
   !! `pic_vector` grows without a compile-time bound, at the cost of
   !! allocating. Use it where the final length is not known until the input
   !! has been read. The two share a method vocabulary on purpose, so moving
   !! between them is mostly a type change -- but not entirely, and the
   !! differences are the kind that fail quietly:
   !!
   !! * A bounds or underflow failure is `ERROR_BOUNDS` here and
   !!   `ERROR_VALIDATION` in `pic_fixed_array`. Code that branches on the code
   !!   stops branching after the swap.
   !! * `pic_fixed_array` sets `value` to zero when `at` or `pop_back` fails;
   !!   here `value` is `intent(out)` and left undefined, so a caller that
   !!   ignores `err` reads something deterministic before the swap and not
   !!   after.
   !! * There is no `vector_int_t`. `fixed_array_int_t` holds `default_int`,
   !!   which changes width with `PIC_DEFAULT_INT8`; see below.
   !!
   !! ### Element kinds are fixed width
   !!
   !! There is no `vector_int_t` following `default_int`: the same source
   !! would mean a 32-bit container in one build and a 64-bit one in the
   !! other, and anything serialized from it would not survive the switch.
   !! Sizes and indices are `default_int`, as everywhere else in PIC.
   !!
   !! ### Storage is private
   !!
   !! Only `1:size()` of the backing array is meaningful. Exposing it would
   !! let `v%items(v%size() + 1)` compile and read spare capacity. The two
   !! cases that need to avoid a copy are served directly: `take` moves the
   !! storage out, and `get_unchecked` skips the bounds test.
   !!
   !! Usage:
   !!   type(vector_int32_t) :: v
   !!   type(error_t) :: err
   !!   integer(int32), allocatable :: final(:)
   !!
   !!   call v%push_back(42_int32, err)
   !!   call v%take(final)        ! v is now empty, `final` is exactly sized
   use pic_types, only: default_int, int32, int64, dp
   use pic_string_type, only: string_type
   use pic_error, only: error_t, error_raise, ERROR_ALLOC, ERROR_BOUNDS, ERROR_VALIDATION

   implicit none
   private

   public :: vector_int32_t
   public :: vector_int64_t
   public :: vector_dp_t
   public :: vector_string_t
   public :: PIC_VECTOR_MIN_CAPACITY
   public :: PIC_VECTOR_MAX_CAPACITY

   integer(default_int), parameter :: PIC_VECTOR_MIN_CAPACITY = 8_default_int
      !! Capacity of a vector after its first `push_back`.
   integer(default_int), parameter :: PIC_VECTOR_MAX_CAPACITY = huge(0_default_int)
      !! Largest representable capacity. Growth clamps here rather than
      !! overflowing `default_int`, and a request beyond it is `ERROR_ALLOC`.

   type :: vector_int32_t
      !! Growable array of integer(int32).
      integer(default_int), private :: n_items = 0
         !! Number of live elements. Always `0 <= n_items <= capacity()`.
      integer(int32), allocatable, private :: items(:)
         !! Backing storage. Only `items(1:n_items)` is meaningful; the rest is
         !! spare capacity whose contents are unspecified.
   contains
      procedure :: push_back => vec_int32_push_back
      procedure :: append => vec_int32_append
      procedure :: pop_back => vec_int32_pop_back
      procedure :: at => vec_int32_at
      procedure :: set => vec_int32_set
      procedure :: get_unchecked => vec_int32_get_unchecked
      procedure :: size => vec_int32_size
      procedure :: capacity => vec_int32_capacity
      procedure :: is_empty => vec_int32_is_empty
      procedure :: reserve => vec_int32_reserve
      procedure :: resize => vec_int32_resize
      procedure :: clear => vec_int32_clear
      procedure :: shrink_to_fit => vec_int32_shrink_to_fit
      procedure :: as_array => vec_int32_as_array
      procedure :: take => vec_int32_take
      procedure :: destroy => vec_int32_destroy
   end type vector_int32_t
   type :: vector_int64_t
      !! Growable array of integer(int64).
      integer(default_int), private :: n_items = 0
         !! Number of live elements. Always `0 <= n_items <= capacity()`.
      integer(int64), allocatable, private :: items(:)
         !! Backing storage. Only `items(1:n_items)` is meaningful; the rest is
         !! spare capacity whose contents are unspecified.
   contains
      procedure :: push_back => vec_int64_push_back
      procedure :: append => vec_int64_append
      procedure :: pop_back => vec_int64_pop_back
      procedure :: at => vec_int64_at
      procedure :: set => vec_int64_set
      procedure :: get_unchecked => vec_int64_get_unchecked
      procedure :: size => vec_int64_size
      procedure :: capacity => vec_int64_capacity
      procedure :: is_empty => vec_int64_is_empty
      procedure :: reserve => vec_int64_reserve
      procedure :: resize => vec_int64_resize
      procedure :: clear => vec_int64_clear
      procedure :: shrink_to_fit => vec_int64_shrink_to_fit
      procedure :: as_array => vec_int64_as_array
      procedure :: take => vec_int64_take
      procedure :: destroy => vec_int64_destroy
   end type vector_int64_t
   type :: vector_dp_t
      !! Growable array of real(dp).
      integer(default_int), private :: n_items = 0
         !! Number of live elements. Always `0 <= n_items <= capacity()`.
      real(dp), allocatable, private :: items(:)
         !! Backing storage. Only `items(1:n_items)` is meaningful; the rest is
         !! spare capacity whose contents are unspecified.
   contains
      procedure :: push_back => vec_dp_push_back
      procedure :: append => vec_dp_append
      procedure :: pop_back => vec_dp_pop_back
      procedure :: at => vec_dp_at
      procedure :: set => vec_dp_set
      procedure :: get_unchecked => vec_dp_get_unchecked
      procedure :: size => vec_dp_size
      procedure :: capacity => vec_dp_capacity
      procedure :: is_empty => vec_dp_is_empty
      procedure :: reserve => vec_dp_reserve
      procedure :: resize => vec_dp_resize
      procedure :: clear => vec_dp_clear
      procedure :: shrink_to_fit => vec_dp_shrink_to_fit
      procedure :: as_array => vec_dp_as_array
      procedure :: take => vec_dp_take
      procedure :: destroy => vec_dp_destroy
   end type vector_dp_t
   type :: vector_string_t
      !! Growable array of type(string_type).
      integer(default_int), private :: n_items = 0
         !! Number of live elements. Always `0 <= n_items <= capacity()`.
      type(string_type), allocatable, private :: items(:)
         !! Backing storage. Only `items(1:n_items)` is meaningful; the rest is
         !! spare capacity whose contents are unspecified.
   contains
      procedure :: push_back => vec_string_push_back
      procedure :: append => vec_string_append
      procedure :: pop_back => vec_string_pop_back
      procedure :: at => vec_string_at
      procedure :: set => vec_string_set
      procedure :: get_unchecked => vec_string_get_unchecked
      procedure :: size => vec_string_size
      procedure :: capacity => vec_string_capacity
      procedure :: is_empty => vec_string_is_empty
      procedure :: reserve => vec_string_reserve
      procedure :: resize => vec_string_resize
      procedure :: clear => vec_string_clear
      procedure :: shrink_to_fit => vec_string_shrink_to_fit
      procedure :: as_array => vec_string_as_array
      procedure :: take => vec_string_take
      procedure :: destroy => vec_string_destroy
   end type vector_string_t

contains

   pure subroutine vec_int32_grow(self, needed, err)
      !! Ensure capacity for at least `needed` elements, preserving the live
      !! ones. Capacity starts at PIC_VECTOR_MIN_CAPACITY and doubles, which
      !! keeps a run of `push_back` calls amortised O(1).
      class(vector_int32_t), intent(inout) :: self
      integer(default_int), intent(in) :: needed
      type(error_t), intent(inout), optional :: err

      integer(int32), allocatable :: bigger(:)
      integer(default_int) :: new_capacity
      integer :: stat

      if (needed <= vec_int32_capacity(self)) return

      ! `needed` is negative only if a caller's arithmetic wrapped on the way
      ! in. It cannot be compared against PIC_VECTOR_MAX_CAPACITY, which is
      ! `huge(0_default_int)` and so can never be exceeded by a value of that
      ! kind -- a guard written that way is dead code. The callers below check
      ! their headroom before adding; this catches anything that slips past.
      if (needed < 0_default_int) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_int32_t: requested capacity overflowed.")
         return
      end if

      new_capacity = max(vec_int32_capacity(self), PIC_VECTOR_MIN_CAPACITY)
      do while (new_capacity < needed)
         ! Doubling is done only when it cannot overflow default_int; past that
         ! point the clamp is the only remaining step, and `needed` is already
         ! known to fit.
         if (new_capacity > PIC_VECTOR_MAX_CAPACITY/2) then
            new_capacity = PIC_VECTOR_MAX_CAPACITY
            exit
         end if
         new_capacity = 2*new_capacity
      end do

      allocate (bigger(new_capacity), stat=stat)
      ! `allocated` rather than `stat`: nvfortran does not set stat reliably
      ! for every element type, while `allocated` always reports the truth.
      if (.not. allocated(bigger)) then
         call error_raise(err, ERROR_ALLOC, "vector_int32_t: allocation of backing storage failed.")
         return
      end if

      if (self%n_items > 0) bigger(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(bigger, self%items)
   end subroutine vec_int32_grow

   pure subroutine vec_int32_push_back(self, value, err)
      !! Append one element. `ERROR_ALLOC` if storage cannot be grown, in which
      !! case the vector is unchanged.
      class(vector_int32_t), intent(inout) :: self
      integer(int32), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      ! Checked before adding, not after: `n_items + 1` at the maximum wraps
      ! negative, and every downstream test would then read as "plenty of
      ! room" while the append walked off the end of the array.
      if (self%n_items >= PIC_VECTOR_MAX_CAPACITY) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_int32_t: already holds PIC_VECTOR_MAX_CAPACITY elements.")
         return
      end if

      call vec_int32_grow(self, self%n_items + 1, err)
      if (self%n_items + 1 > vec_int32_capacity(self)) return

      self%n_items = self%n_items + 1
      self%items(self%n_items) = value
   end subroutine vec_int32_push_back

   pure subroutine vec_int32_append(self, values, err)
      !! Append many elements, growing once rather than once per element.
      class(vector_int32_t), intent(inout) :: self
      integer(int32), intent(in) :: values(:)
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: n_new

      n_new = size(values, kind=default_int)
      if (n_new == 0) return

      ! Rearranged from `n_items + n_new > MAX` so that the sum is never
      ! formed; the subtraction cannot overflow because `n_items` is
      ! non-negative and PIC_VECTOR_MAX_CAPACITY is `huge`.
      if (n_new > PIC_VECTOR_MAX_CAPACITY - self%n_items) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_int32_t: append would exceed PIC_VECTOR_MAX_CAPACITY.")
         return
      end if

      call vec_int32_grow(self, self%n_items + n_new, err)
      if (self%n_items + n_new > vec_int32_capacity(self)) return

      self%items(self%n_items + 1:self%n_items + n_new) = values
      self%n_items = self%n_items + n_new
   end subroutine vec_int32_append

   pure subroutine vec_int32_pop_back(self, value, err)
      !! Remove and return the last element. `ERROR_BOUNDS` when empty, in
      !! which case the vector is unchanged.
      class(vector_int32_t), intent(inout) :: self
      integer(int32), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (self%n_items <= 0) then
         call error_raise(err, ERROR_BOUNDS, "vector_int32_t: pop_back on an empty vector.")
         return
      end if

      value = self%items(self%n_items)
      self%n_items = self%n_items - 1
   end subroutine vec_int32_pop_back

   pure subroutine vec_int32_at(self, index, value, err)
      !! Read element `index`, 1-based. `ERROR_BOUNDS` when out of range.
      class(vector_int32_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      integer(int32), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_int32_t: at() index is out of range.")
         return
      end if

      value = self%items(index)
   end subroutine vec_int32_at

   pure subroutine vec_int32_set(self, index, value, err)
      !! Overwrite element `index`, 1-based. `ERROR_BOUNDS` when out of range;
      !! this never extends the vector, use `push_back` or `resize` for that.
      class(vector_int32_t), intent(inout) :: self
      integer(default_int), intent(in) :: index
      integer(int32), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_int32_t: set() index is out of range.")
         return
      end if

      self%items(index) = value
   end subroutine vec_int32_set

   pure function vec_int32_get_unchecked(self, index) result(value)
      !! Element `index` with no bounds check, for hot loops that have already
      !! established the range. Reading outside `1:size()` is undefined.
      class(vector_int32_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      integer(int32) :: value

      value = self%items(index)
   end function vec_int32_get_unchecked

   pure function vec_int32_size(self) result(n)
      !! Number of live elements.
      class(vector_int32_t), intent(in) :: self
      integer(default_int) :: n

      n = self%n_items
   end function vec_int32_size

   pure function vec_int32_capacity(self) result(n)
      !! Elements that fit before the next reallocation.
      class(vector_int32_t), intent(in) :: self
      integer(default_int) :: n

      if (allocated(self%items)) then
         n = size(self%items, kind=default_int)
      else
         n = 0
      end if
   end function vec_int32_capacity

   pure function vec_int32_is_empty(self) result(flag)
      !! `.true.` when there are no live elements.
      class(vector_int32_t), intent(in) :: self
      logical :: flag

      flag = (self%n_items == 0)
   end function vec_int32_is_empty

   pure subroutine vec_int32_reserve(self, n, err)
      !! Make room for at least `n` elements. `size()` is unchanged.
      class(vector_int32_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout), optional :: err

      if (n <= 0) return
      call vec_int32_grow(self, n, err)
   end subroutine vec_int32_reserve

   pure subroutine vec_int32_resize(self, n, fill, err)
      !! Set the length to `n`, padding with `fill` when growing and dropping
      !! elements from the end when shrinking.
      class(vector_int32_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      integer(int32), intent(in) :: fill
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: i

      if (n < 0) then
         call error_raise(err, ERROR_VALIDATION, "vector_int32_t: resize() to a negative length.")
         return
      end if

      if (n <= self%n_items) then
         self%n_items = n
         return
      end if

      call vec_int32_grow(self, n, err)
      if (n > vec_int32_capacity(self)) return

      do i = self%n_items + 1, n
         self%items(i) = fill
      end do
      self%n_items = n
   end subroutine vec_int32_resize

   pure subroutine vec_int32_clear(self)
      !! Drop every element but keep the allocated storage, so a vector reused
      !! across iterations of an outer loop stops reallocating.
      class(vector_int32_t), intent(inout) :: self

      self%n_items = 0
   end subroutine vec_int32_clear

   pure subroutine vec_int32_shrink_to_fit(self, err)
      !! Release capacity beyond `size()`.
      class(vector_int32_t), intent(inout) :: self
      type(error_t), intent(inout), optional :: err

      integer(int32), allocatable :: exact(:)
      integer :: stat

      if (.not. allocated(self%items)) return
      if (vec_int32_capacity(self) == self%n_items) return

      allocate (exact(self%n_items), stat=stat)
      if (.not. allocated(exact)) then
         call error_raise(err, ERROR_ALLOC, "vector_int32_t: shrink_to_fit allocation failed.")
         return
      end if

      if (self%n_items > 0) exact(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(exact, self%items)
   end subroutine vec_int32_shrink_to_fit

   pure function vec_int32_as_array(self) result(copy)
      !! A copy holding exactly `size()` elements.
      class(vector_int32_t), intent(in) :: self
      integer(int32), allocatable :: copy(:)

      allocate (copy(self%n_items))
      if (self%n_items > 0) copy = self%items(1:self%n_items)
   end function vec_int32_as_array

   pure subroutine vec_int32_take(self, array, err)
      !! Hand the storage over to `array`, exactly sized, without copying the
      !! elements. The vector is left empty and without storage.
      !!
      !! This is the load-time idiom: build a vector while parsing an input of
      !! unknown length, then `take` the result into the permanent array.
      !! `as_array` copies; this does not.
      class(vector_int32_t), intent(inout) :: self
      integer(int32), allocatable, intent(out) :: array(:)
      type(error_t), intent(inout), optional :: err

      ! Spare capacity would otherwise be handed over as live elements, so the
      ! shrink is part of the contract rather than an optimisation.
      call vec_int32_shrink_to_fit(self, err)
      if (vec_int32_capacity(self) /= self%n_items) return

      if (.not. allocated(self%items)) allocate (self%items(0))
      call move_alloc(self%items, array)
      self%n_items = 0
   end subroutine vec_int32_take

   pure subroutine vec_int32_destroy(self)
      !! Release all storage and reset to the empty state.
      class(vector_int32_t), intent(inout) :: self

      self%n_items = 0
      if (allocated(self%items)) deallocate (self%items)
   end subroutine vec_int32_destroy
   pure subroutine vec_int64_grow(self, needed, err)
      !! Ensure capacity for at least `needed` elements, preserving the live
      !! ones. Capacity starts at PIC_VECTOR_MIN_CAPACITY and doubles, which
      !! keeps a run of `push_back` calls amortised O(1).
      class(vector_int64_t), intent(inout) :: self
      integer(default_int), intent(in) :: needed
      type(error_t), intent(inout), optional :: err

      integer(int64), allocatable :: bigger(:)
      integer(default_int) :: new_capacity
      integer :: stat

      if (needed <= vec_int64_capacity(self)) return

      ! `needed` is negative only if a caller's arithmetic wrapped on the way
      ! in. It cannot be compared against PIC_VECTOR_MAX_CAPACITY, which is
      ! `huge(0_default_int)` and so can never be exceeded by a value of that
      ! kind -- a guard written that way is dead code. The callers below check
      ! their headroom before adding; this catches anything that slips past.
      if (needed < 0_default_int) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_int64_t: requested capacity overflowed.")
         return
      end if

      new_capacity = max(vec_int64_capacity(self), PIC_VECTOR_MIN_CAPACITY)
      do while (new_capacity < needed)
         ! Doubling is done only when it cannot overflow default_int; past that
         ! point the clamp is the only remaining step, and `needed` is already
         ! known to fit.
         if (new_capacity > PIC_VECTOR_MAX_CAPACITY/2) then
            new_capacity = PIC_VECTOR_MAX_CAPACITY
            exit
         end if
         new_capacity = 2*new_capacity
      end do

      allocate (bigger(new_capacity), stat=stat)
      ! `allocated` rather than `stat`: nvfortran does not set stat reliably
      ! for every element type, while `allocated` always reports the truth.
      if (.not. allocated(bigger)) then
         call error_raise(err, ERROR_ALLOC, "vector_int64_t: allocation of backing storage failed.")
         return
      end if

      if (self%n_items > 0) bigger(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(bigger, self%items)
   end subroutine vec_int64_grow

   pure subroutine vec_int64_push_back(self, value, err)
      !! Append one element. `ERROR_ALLOC` if storage cannot be grown, in which
      !! case the vector is unchanged.
      class(vector_int64_t), intent(inout) :: self
      integer(int64), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      ! Checked before adding, not after: `n_items + 1` at the maximum wraps
      ! negative, and every downstream test would then read as "plenty of
      ! room" while the append walked off the end of the array.
      if (self%n_items >= PIC_VECTOR_MAX_CAPACITY) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_int64_t: already holds PIC_VECTOR_MAX_CAPACITY elements.")
         return
      end if

      call vec_int64_grow(self, self%n_items + 1, err)
      if (self%n_items + 1 > vec_int64_capacity(self)) return

      self%n_items = self%n_items + 1
      self%items(self%n_items) = value
   end subroutine vec_int64_push_back

   pure subroutine vec_int64_append(self, values, err)
      !! Append many elements, growing once rather than once per element.
      class(vector_int64_t), intent(inout) :: self
      integer(int64), intent(in) :: values(:)
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: n_new

      n_new = size(values, kind=default_int)
      if (n_new == 0) return

      ! Rearranged from `n_items + n_new > MAX` so that the sum is never
      ! formed; the subtraction cannot overflow because `n_items` is
      ! non-negative and PIC_VECTOR_MAX_CAPACITY is `huge`.
      if (n_new > PIC_VECTOR_MAX_CAPACITY - self%n_items) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_int64_t: append would exceed PIC_VECTOR_MAX_CAPACITY.")
         return
      end if

      call vec_int64_grow(self, self%n_items + n_new, err)
      if (self%n_items + n_new > vec_int64_capacity(self)) return

      self%items(self%n_items + 1:self%n_items + n_new) = values
      self%n_items = self%n_items + n_new
   end subroutine vec_int64_append

   pure subroutine vec_int64_pop_back(self, value, err)
      !! Remove and return the last element. `ERROR_BOUNDS` when empty, in
      !! which case the vector is unchanged.
      class(vector_int64_t), intent(inout) :: self
      integer(int64), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (self%n_items <= 0) then
         call error_raise(err, ERROR_BOUNDS, "vector_int64_t: pop_back on an empty vector.")
         return
      end if

      value = self%items(self%n_items)
      self%n_items = self%n_items - 1
   end subroutine vec_int64_pop_back

   pure subroutine vec_int64_at(self, index, value, err)
      !! Read element `index`, 1-based. `ERROR_BOUNDS` when out of range.
      class(vector_int64_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      integer(int64), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_int64_t: at() index is out of range.")
         return
      end if

      value = self%items(index)
   end subroutine vec_int64_at

   pure subroutine vec_int64_set(self, index, value, err)
      !! Overwrite element `index`, 1-based. `ERROR_BOUNDS` when out of range;
      !! this never extends the vector, use `push_back` or `resize` for that.
      class(vector_int64_t), intent(inout) :: self
      integer(default_int), intent(in) :: index
      integer(int64), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_int64_t: set() index is out of range.")
         return
      end if

      self%items(index) = value
   end subroutine vec_int64_set

   pure function vec_int64_get_unchecked(self, index) result(value)
      !! Element `index` with no bounds check, for hot loops that have already
      !! established the range. Reading outside `1:size()` is undefined.
      class(vector_int64_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      integer(int64) :: value

      value = self%items(index)
   end function vec_int64_get_unchecked

   pure function vec_int64_size(self) result(n)
      !! Number of live elements.
      class(vector_int64_t), intent(in) :: self
      integer(default_int) :: n

      n = self%n_items
   end function vec_int64_size

   pure function vec_int64_capacity(self) result(n)
      !! Elements that fit before the next reallocation.
      class(vector_int64_t), intent(in) :: self
      integer(default_int) :: n

      if (allocated(self%items)) then
         n = size(self%items, kind=default_int)
      else
         n = 0
      end if
   end function vec_int64_capacity

   pure function vec_int64_is_empty(self) result(flag)
      !! `.true.` when there are no live elements.
      class(vector_int64_t), intent(in) :: self
      logical :: flag

      flag = (self%n_items == 0)
   end function vec_int64_is_empty

   pure subroutine vec_int64_reserve(self, n, err)
      !! Make room for at least `n` elements. `size()` is unchanged.
      class(vector_int64_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout), optional :: err

      if (n <= 0) return
      call vec_int64_grow(self, n, err)
   end subroutine vec_int64_reserve

   pure subroutine vec_int64_resize(self, n, fill, err)
      !! Set the length to `n`, padding with `fill` when growing and dropping
      !! elements from the end when shrinking.
      class(vector_int64_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      integer(int64), intent(in) :: fill
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: i

      if (n < 0) then
         call error_raise(err, ERROR_VALIDATION, "vector_int64_t: resize() to a negative length.")
         return
      end if

      if (n <= self%n_items) then
         self%n_items = n
         return
      end if

      call vec_int64_grow(self, n, err)
      if (n > vec_int64_capacity(self)) return

      do i = self%n_items + 1, n
         self%items(i) = fill
      end do
      self%n_items = n
   end subroutine vec_int64_resize

   pure subroutine vec_int64_clear(self)
      !! Drop every element but keep the allocated storage, so a vector reused
      !! across iterations of an outer loop stops reallocating.
      class(vector_int64_t), intent(inout) :: self

      self%n_items = 0
   end subroutine vec_int64_clear

   pure subroutine vec_int64_shrink_to_fit(self, err)
      !! Release capacity beyond `size()`.
      class(vector_int64_t), intent(inout) :: self
      type(error_t), intent(inout), optional :: err

      integer(int64), allocatable :: exact(:)
      integer :: stat

      if (.not. allocated(self%items)) return
      if (vec_int64_capacity(self) == self%n_items) return

      allocate (exact(self%n_items), stat=stat)
      if (.not. allocated(exact)) then
         call error_raise(err, ERROR_ALLOC, "vector_int64_t: shrink_to_fit allocation failed.")
         return
      end if

      if (self%n_items > 0) exact(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(exact, self%items)
   end subroutine vec_int64_shrink_to_fit

   pure function vec_int64_as_array(self) result(copy)
      !! A copy holding exactly `size()` elements.
      class(vector_int64_t), intent(in) :: self
      integer(int64), allocatable :: copy(:)

      allocate (copy(self%n_items))
      if (self%n_items > 0) copy = self%items(1:self%n_items)
   end function vec_int64_as_array

   pure subroutine vec_int64_take(self, array, err)
      !! Hand the storage over to `array`, exactly sized, without copying the
      !! elements. The vector is left empty and without storage.
      !!
      !! This is the load-time idiom: build a vector while parsing an input of
      !! unknown length, then `take` the result into the permanent array.
      !! `as_array` copies; this does not.
      class(vector_int64_t), intent(inout) :: self
      integer(int64), allocatable, intent(out) :: array(:)
      type(error_t), intent(inout), optional :: err

      ! Spare capacity would otherwise be handed over as live elements, so the
      ! shrink is part of the contract rather than an optimisation.
      call vec_int64_shrink_to_fit(self, err)
      if (vec_int64_capacity(self) /= self%n_items) return

      if (.not. allocated(self%items)) allocate (self%items(0))
      call move_alloc(self%items, array)
      self%n_items = 0
   end subroutine vec_int64_take

   pure subroutine vec_int64_destroy(self)
      !! Release all storage and reset to the empty state.
      class(vector_int64_t), intent(inout) :: self

      self%n_items = 0
      if (allocated(self%items)) deallocate (self%items)
   end subroutine vec_int64_destroy
   pure subroutine vec_dp_grow(self, needed, err)
      !! Ensure capacity for at least `needed` elements, preserving the live
      !! ones. Capacity starts at PIC_VECTOR_MIN_CAPACITY and doubles, which
      !! keeps a run of `push_back` calls amortised O(1).
      class(vector_dp_t), intent(inout) :: self
      integer(default_int), intent(in) :: needed
      type(error_t), intent(inout), optional :: err

      real(dp), allocatable :: bigger(:)
      integer(default_int) :: new_capacity
      integer :: stat

      if (needed <= vec_dp_capacity(self)) return

      ! `needed` is negative only if a caller's arithmetic wrapped on the way
      ! in. It cannot be compared against PIC_VECTOR_MAX_CAPACITY, which is
      ! `huge(0_default_int)` and so can never be exceeded by a value of that
      ! kind -- a guard written that way is dead code. The callers below check
      ! their headroom before adding; this catches anything that slips past.
      if (needed < 0_default_int) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_dp_t: requested capacity overflowed.")
         return
      end if

      new_capacity = max(vec_dp_capacity(self), PIC_VECTOR_MIN_CAPACITY)
      do while (new_capacity < needed)
         ! Doubling is done only when it cannot overflow default_int; past that
         ! point the clamp is the only remaining step, and `needed` is already
         ! known to fit.
         if (new_capacity > PIC_VECTOR_MAX_CAPACITY/2) then
            new_capacity = PIC_VECTOR_MAX_CAPACITY
            exit
         end if
         new_capacity = 2*new_capacity
      end do

      allocate (bigger(new_capacity), stat=stat)
      ! `allocated` rather than `stat`: nvfortran does not set stat reliably
      ! for every element type, while `allocated` always reports the truth.
      if (.not. allocated(bigger)) then
         call error_raise(err, ERROR_ALLOC, "vector_dp_t: allocation of backing storage failed.")
         return
      end if

      if (self%n_items > 0) bigger(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(bigger, self%items)
   end subroutine vec_dp_grow

   pure subroutine vec_dp_push_back(self, value, err)
      !! Append one element. `ERROR_ALLOC` if storage cannot be grown, in which
      !! case the vector is unchanged.
      class(vector_dp_t), intent(inout) :: self
      real(dp), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      ! Checked before adding, not after: `n_items + 1` at the maximum wraps
      ! negative, and every downstream test would then read as "plenty of
      ! room" while the append walked off the end of the array.
      if (self%n_items >= PIC_VECTOR_MAX_CAPACITY) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_dp_t: already holds PIC_VECTOR_MAX_CAPACITY elements.")
         return
      end if

      call vec_dp_grow(self, self%n_items + 1, err)
      if (self%n_items + 1 > vec_dp_capacity(self)) return

      self%n_items = self%n_items + 1
      self%items(self%n_items) = value
   end subroutine vec_dp_push_back

   pure subroutine vec_dp_append(self, values, err)
      !! Append many elements, growing once rather than once per element.
      class(vector_dp_t), intent(inout) :: self
      real(dp), intent(in) :: values(:)
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: n_new

      n_new = size(values, kind=default_int)
      if (n_new == 0) return

      ! Rearranged from `n_items + n_new > MAX` so that the sum is never
      ! formed; the subtraction cannot overflow because `n_items` is
      ! non-negative and PIC_VECTOR_MAX_CAPACITY is `huge`.
      if (n_new > PIC_VECTOR_MAX_CAPACITY - self%n_items) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_dp_t: append would exceed PIC_VECTOR_MAX_CAPACITY.")
         return
      end if

      call vec_dp_grow(self, self%n_items + n_new, err)
      if (self%n_items + n_new > vec_dp_capacity(self)) return

      self%items(self%n_items + 1:self%n_items + n_new) = values
      self%n_items = self%n_items + n_new
   end subroutine vec_dp_append

   pure subroutine vec_dp_pop_back(self, value, err)
      !! Remove and return the last element. `ERROR_BOUNDS` when empty, in
      !! which case the vector is unchanged.
      class(vector_dp_t), intent(inout) :: self
      real(dp), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (self%n_items <= 0) then
         call error_raise(err, ERROR_BOUNDS, "vector_dp_t: pop_back on an empty vector.")
         return
      end if

      value = self%items(self%n_items)
      self%n_items = self%n_items - 1
   end subroutine vec_dp_pop_back

   pure subroutine vec_dp_at(self, index, value, err)
      !! Read element `index`, 1-based. `ERROR_BOUNDS` when out of range.
      class(vector_dp_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      real(dp), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_dp_t: at() index is out of range.")
         return
      end if

      value = self%items(index)
   end subroutine vec_dp_at

   pure subroutine vec_dp_set(self, index, value, err)
      !! Overwrite element `index`, 1-based. `ERROR_BOUNDS` when out of range;
      !! this never extends the vector, use `push_back` or `resize` for that.
      class(vector_dp_t), intent(inout) :: self
      integer(default_int), intent(in) :: index
      real(dp), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_dp_t: set() index is out of range.")
         return
      end if

      self%items(index) = value
   end subroutine vec_dp_set

   pure function vec_dp_get_unchecked(self, index) result(value)
      !! Element `index` with no bounds check, for hot loops that have already
      !! established the range. Reading outside `1:size()` is undefined.
      class(vector_dp_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      real(dp) :: value

      value = self%items(index)
   end function vec_dp_get_unchecked

   pure function vec_dp_size(self) result(n)
      !! Number of live elements.
      class(vector_dp_t), intent(in) :: self
      integer(default_int) :: n

      n = self%n_items
   end function vec_dp_size

   pure function vec_dp_capacity(self) result(n)
      !! Elements that fit before the next reallocation.
      class(vector_dp_t), intent(in) :: self
      integer(default_int) :: n

      if (allocated(self%items)) then
         n = size(self%items, kind=default_int)
      else
         n = 0
      end if
   end function vec_dp_capacity

   pure function vec_dp_is_empty(self) result(flag)
      !! `.true.` when there are no live elements.
      class(vector_dp_t), intent(in) :: self
      logical :: flag

      flag = (self%n_items == 0)
   end function vec_dp_is_empty

   pure subroutine vec_dp_reserve(self, n, err)
      !! Make room for at least `n` elements. `size()` is unchanged.
      class(vector_dp_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout), optional :: err

      if (n <= 0) return
      call vec_dp_grow(self, n, err)
   end subroutine vec_dp_reserve

   pure subroutine vec_dp_resize(self, n, fill, err)
      !! Set the length to `n`, padding with `fill` when growing and dropping
      !! elements from the end when shrinking.
      class(vector_dp_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      real(dp), intent(in) :: fill
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: i

      if (n < 0) then
         call error_raise(err, ERROR_VALIDATION, "vector_dp_t: resize() to a negative length.")
         return
      end if

      if (n <= self%n_items) then
         self%n_items = n
         return
      end if

      call vec_dp_grow(self, n, err)
      if (n > vec_dp_capacity(self)) return

      do i = self%n_items + 1, n
         self%items(i) = fill
      end do
      self%n_items = n
   end subroutine vec_dp_resize

   pure subroutine vec_dp_clear(self)
      !! Drop every element but keep the allocated storage, so a vector reused
      !! across iterations of an outer loop stops reallocating.
      class(vector_dp_t), intent(inout) :: self

      self%n_items = 0
   end subroutine vec_dp_clear

   pure subroutine vec_dp_shrink_to_fit(self, err)
      !! Release capacity beyond `size()`.
      class(vector_dp_t), intent(inout) :: self
      type(error_t), intent(inout), optional :: err

      real(dp), allocatable :: exact(:)
      integer :: stat

      if (.not. allocated(self%items)) return
      if (vec_dp_capacity(self) == self%n_items) return

      allocate (exact(self%n_items), stat=stat)
      if (.not. allocated(exact)) then
         call error_raise(err, ERROR_ALLOC, "vector_dp_t: shrink_to_fit allocation failed.")
         return
      end if

      if (self%n_items > 0) exact(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(exact, self%items)
   end subroutine vec_dp_shrink_to_fit

   pure function vec_dp_as_array(self) result(copy)
      !! A copy holding exactly `size()` elements.
      class(vector_dp_t), intent(in) :: self
      real(dp), allocatable :: copy(:)

      allocate (copy(self%n_items))
      if (self%n_items > 0) copy = self%items(1:self%n_items)
   end function vec_dp_as_array

   pure subroutine vec_dp_take(self, array, err)
      !! Hand the storage over to `array`, exactly sized, without copying the
      !! elements. The vector is left empty and without storage.
      !!
      !! This is the load-time idiom: build a vector while parsing an input of
      !! unknown length, then `take` the result into the permanent array.
      !! `as_array` copies; this does not.
      class(vector_dp_t), intent(inout) :: self
      real(dp), allocatable, intent(out) :: array(:)
      type(error_t), intent(inout), optional :: err

      ! Spare capacity would otherwise be handed over as live elements, so the
      ! shrink is part of the contract rather than an optimisation.
      call vec_dp_shrink_to_fit(self, err)
      if (vec_dp_capacity(self) /= self%n_items) return

      if (.not. allocated(self%items)) allocate (self%items(0))
      call move_alloc(self%items, array)
      self%n_items = 0
   end subroutine vec_dp_take

   pure subroutine vec_dp_destroy(self)
      !! Release all storage and reset to the empty state.
      class(vector_dp_t), intent(inout) :: self

      self%n_items = 0
      if (allocated(self%items)) deallocate (self%items)
   end subroutine vec_dp_destroy
   pure subroutine vec_string_grow(self, needed, err)
      !! Ensure capacity for at least `needed` elements, preserving the live
      !! ones. Capacity starts at PIC_VECTOR_MIN_CAPACITY and doubles, which
      !! keeps a run of `push_back` calls amortised O(1).
      class(vector_string_t), intent(inout) :: self
      integer(default_int), intent(in) :: needed
      type(error_t), intent(inout), optional :: err

      type(string_type), allocatable :: bigger(:)
      integer(default_int) :: new_capacity
      integer :: stat

      if (needed <= vec_string_capacity(self)) return

      ! `needed` is negative only if a caller's arithmetic wrapped on the way
      ! in. It cannot be compared against PIC_VECTOR_MAX_CAPACITY, which is
      ! `huge(0_default_int)` and so can never be exceeded by a value of that
      ! kind -- a guard written that way is dead code. The callers below check
      ! their headroom before adding; this catches anything that slips past.
      if (needed < 0_default_int) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_string_t: requested capacity overflowed.")
         return
      end if

      new_capacity = max(vec_string_capacity(self), PIC_VECTOR_MIN_CAPACITY)
      do while (new_capacity < needed)
         ! Doubling is done only when it cannot overflow default_int; past that
         ! point the clamp is the only remaining step, and `needed` is already
         ! known to fit.
         if (new_capacity > PIC_VECTOR_MAX_CAPACITY/2) then
            new_capacity = PIC_VECTOR_MAX_CAPACITY
            exit
         end if
         new_capacity = 2*new_capacity
      end do

      allocate (bigger(new_capacity), stat=stat)
      ! `allocated` rather than `stat`: nvfortran does not set stat reliably
      ! for every element type, while `allocated` always reports the truth.
      if (.not. allocated(bigger)) then
         call error_raise(err, ERROR_ALLOC, "vector_string_t: allocation of backing storage failed.")
         return
      end if

      if (self%n_items > 0) bigger(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(bigger, self%items)
   end subroutine vec_string_grow

   pure subroutine vec_string_push_back(self, value, err)
      !! Append one element. `ERROR_ALLOC` if storage cannot be grown, in which
      !! case the vector is unchanged.
      class(vector_string_t), intent(inout) :: self
      type(string_type), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      ! Checked before adding, not after: `n_items + 1` at the maximum wraps
      ! negative, and every downstream test would then read as "plenty of
      ! room" while the append walked off the end of the array.
      if (self%n_items >= PIC_VECTOR_MAX_CAPACITY) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_string_t: already holds PIC_VECTOR_MAX_CAPACITY elements.")
         return
      end if

      call vec_string_grow(self, self%n_items + 1, err)
      if (self%n_items + 1 > vec_string_capacity(self)) return

      self%n_items = self%n_items + 1
      self%items(self%n_items) = value
   end subroutine vec_string_push_back

   pure subroutine vec_string_append(self, values, err)
      !! Append many elements, growing once rather than once per element.
      class(vector_string_t), intent(inout) :: self
      type(string_type), intent(in) :: values(:)
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: n_new

      n_new = size(values, kind=default_int)
      if (n_new == 0) return

      ! Rearranged from `n_items + n_new > MAX` so that the sum is never
      ! formed; the subtraction cannot overflow because `n_items` is
      ! non-negative and PIC_VECTOR_MAX_CAPACITY is `huge`.
      if (n_new > PIC_VECTOR_MAX_CAPACITY - self%n_items) then
         call error_raise(err, ERROR_ALLOC, &
                          "vector_string_t: append would exceed PIC_VECTOR_MAX_CAPACITY.")
         return
      end if

      call vec_string_grow(self, self%n_items + n_new, err)
      if (self%n_items + n_new > vec_string_capacity(self)) return

      self%items(self%n_items + 1:self%n_items + n_new) = values
      self%n_items = self%n_items + n_new
   end subroutine vec_string_append

   pure subroutine vec_string_pop_back(self, value, err)
      !! Remove and return the last element. `ERROR_BOUNDS` when empty, in
      !! which case the vector is unchanged.
      class(vector_string_t), intent(inout) :: self
      type(string_type), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (self%n_items <= 0) then
         call error_raise(err, ERROR_BOUNDS, "vector_string_t: pop_back on an empty vector.")
         return
      end if

      value = self%items(self%n_items)
      self%n_items = self%n_items - 1
   end subroutine vec_string_pop_back

   pure subroutine vec_string_at(self, index, value, err)
      !! Read element `index`, 1-based. `ERROR_BOUNDS` when out of range.
      class(vector_string_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      type(string_type), intent(out) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_string_t: at() index is out of range.")
         return
      end if

      value = self%items(index)
   end subroutine vec_string_at

   pure subroutine vec_string_set(self, index, value, err)
      !! Overwrite element `index`, 1-based. `ERROR_BOUNDS` when out of range;
      !! this never extends the vector, use `push_back` or `resize` for that.
      class(vector_string_t), intent(inout) :: self
      integer(default_int), intent(in) :: index
      type(string_type), intent(in) :: value
      type(error_t), intent(inout), optional :: err

      if (index < 1 .or. index > self%n_items) then
         call error_raise(err, ERROR_BOUNDS, "vector_string_t: set() index is out of range.")
         return
      end if

      self%items(index) = value
   end subroutine vec_string_set

   pure function vec_string_get_unchecked(self, index) result(value)
      !! Element `index` with no bounds check, for hot loops that have already
      !! established the range. Reading outside `1:size()` is undefined.
      class(vector_string_t), intent(in) :: self
      integer(default_int), intent(in) :: index
      type(string_type) :: value

      value = self%items(index)
   end function vec_string_get_unchecked

   pure function vec_string_size(self) result(n)
      !! Number of live elements.
      class(vector_string_t), intent(in) :: self
      integer(default_int) :: n

      n = self%n_items
   end function vec_string_size

   pure function vec_string_capacity(self) result(n)
      !! Elements that fit before the next reallocation.
      class(vector_string_t), intent(in) :: self
      integer(default_int) :: n

      if (allocated(self%items)) then
         n = size(self%items, kind=default_int)
      else
         n = 0
      end if
   end function vec_string_capacity

   pure function vec_string_is_empty(self) result(flag)
      !! `.true.` when there are no live elements.
      class(vector_string_t), intent(in) :: self
      logical :: flag

      flag = (self%n_items == 0)
   end function vec_string_is_empty

   pure subroutine vec_string_reserve(self, n, err)
      !! Make room for at least `n` elements. `size()` is unchanged.
      class(vector_string_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout), optional :: err

      if (n <= 0) return
      call vec_string_grow(self, n, err)
   end subroutine vec_string_reserve

   pure subroutine vec_string_resize(self, n, fill, err)
      !! Set the length to `n`, padding with `fill` when growing and dropping
      !! elements from the end when shrinking.
      class(vector_string_t), intent(inout) :: self
      integer(default_int), intent(in) :: n
      type(string_type), intent(in) :: fill
      type(error_t), intent(inout), optional :: err

      integer(default_int) :: i

      if (n < 0) then
         call error_raise(err, ERROR_VALIDATION, "vector_string_t: resize() to a negative length.")
         return
      end if

      if (n <= self%n_items) then
         self%n_items = n
         return
      end if

      call vec_string_grow(self, n, err)
      if (n > vec_string_capacity(self)) return

      do i = self%n_items + 1, n
         self%items(i) = fill
      end do
      self%n_items = n
   end subroutine vec_string_resize

   pure subroutine vec_string_clear(self)
      !! Drop every element but keep the allocated storage, so a vector reused
      !! across iterations of an outer loop stops reallocating.
      class(vector_string_t), intent(inout) :: self

      self%n_items = 0
   end subroutine vec_string_clear

   pure subroutine vec_string_shrink_to_fit(self, err)
      !! Release capacity beyond `size()`.
      class(vector_string_t), intent(inout) :: self
      type(error_t), intent(inout), optional :: err

      type(string_type), allocatable :: exact(:)
      integer :: stat

      if (.not. allocated(self%items)) return
      if (vec_string_capacity(self) == self%n_items) return

      allocate (exact(self%n_items), stat=stat)
      if (.not. allocated(exact)) then
         call error_raise(err, ERROR_ALLOC, "vector_string_t: shrink_to_fit allocation failed.")
         return
      end if

      if (self%n_items > 0) exact(1:self%n_items) = self%items(1:self%n_items)
      call move_alloc(exact, self%items)
   end subroutine vec_string_shrink_to_fit

   pure function vec_string_as_array(self) result(copy)
      !! A copy holding exactly `size()` elements.
      class(vector_string_t), intent(in) :: self
      type(string_type), allocatable :: copy(:)

      allocate (copy(self%n_items))
      if (self%n_items > 0) copy = self%items(1:self%n_items)
   end function vec_string_as_array

   pure subroutine vec_string_take(self, array, err)
      !! Hand the storage over to `array`, exactly sized, without copying the
      !! elements. The vector is left empty and without storage.
      !!
      !! This is the load-time idiom: build a vector while parsing an input of
      !! unknown length, then `take` the result into the permanent array.
      !! `as_array` copies; this does not.
      class(vector_string_t), intent(inout) :: self
      type(string_type), allocatable, intent(out) :: array(:)
      type(error_t), intent(inout), optional :: err

      ! Spare capacity would otherwise be handed over as live elements, so the
      ! shrink is part of the contract rather than an optimisation.
      call vec_string_shrink_to_fit(self, err)
      if (vec_string_capacity(self) /= self%n_items) return

      if (.not. allocated(self%items)) allocate (self%items(0))
      call move_alloc(self%items, array)
      self%n_items = 0
   end subroutine vec_string_take

   pure subroutine vec_string_destroy(self)
      !! Release all storage and reset to the empty state.
      class(vector_string_t), intent(inout) :: self

      self%n_items = 0
      if (allocated(self%items)) deallocate (self%items)
   end subroutine vec_string_destroy
end module pic_vector
