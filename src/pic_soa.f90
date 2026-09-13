! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Runtime substrate for struct-of-arrays (SoA) containers.
!!
!! This module is the reusable half of PIC's SoA support. It is ordinary
!! hand-written Fortran and needs no code generation: it owns the growth
!! policy, the field resize primitives, the serialization envelope and the
!! state-hash prologue that every SoA container needs. A concrete SoA type --
!! whether emitted from `tools/autogen/pic_soa.fypp` or written by hand -- is
!! a thin shell of field declarations plus bound procedures that call into
!! here.
module pic_soa
   !! Growth, resize, serialization and state-hash primitives shared by every
   !! struct-of-arrays container.
   !!
   !! ## What an SoA container looks like
   !!
   !! A container holds one contiguous rank-1 array per field, all of the same
   !! logical length, plus a `soa_extent_t` recording how many elements are
   !! live (`used`) and how many are allocated (`capacity`).
   !!
   !! ```fortran
   !! type :: particle_soa_t
   !!    type(soa_extent_t) :: extent
   !!    integer(int32), allocatable :: id(:)
   !!    real(dp), allocatable :: x(:)
   !! end type particle_soa_t
   !! ```
   !!
   !! Every field array is a plain `allocatable` rank-1 array, so `p%x` is
   !! contiguous and `p%x(1:p%size())` is a contiguous slice of it. Both can be
   !! handed straight to BLAS or to a vectorised loop with no copy: that is the
   !! entire point of the layout and nothing in this module disturbs it.
   !!
   !! ## Capacity and growth
   !!
   !! `capacity` is the allocated length of every field array, `used` is the
   !! number of live elements, and `used <= capacity` always holds. Growth is
   !! geometric with factor `SOA_GROWTH_FACTOR` (2), starting at
   !! `SOA_MIN_CAPACITY` (8), so appending n elements one at a time costs
   !! O(n) copies in total rather than O(n**2).
   !!
   !! **Capacity never shrinks on a resize.** Resizing down only lowers `used`;
   !! the memory stays with the container so that a shrink followed by a grow
   !! does not reallocate. Release it with the container's `deallocate`
   !! binding, which frees every field array and zeroes both counters.
   !!
   !! Elements that a resize newly exposes -- indices `keep+1 .. new_size`,
   !! where `keep = min(old_size, new_size)` -- are set to the zero value of
   !! their type (`0`, `0.0`, `.false.`). This is deliberate: it makes the
   !! state of a container a function of the operations applied to it, so two
   !! runs that perform the same resizes produce the same `state_hash`.
   !! Elements past `used` and below `capacity` are *not* specified and must
   !! not be read.
   !!
   !! ## Supported field types
   !!
   !! `integer(int32)`, `integer(int64)`, `real(sp)`, `real(dp)` and default
   !! `logical`.
   !!
   !! A field of kind `default_int` is deliberately **not** supported. Its
   !! width changes with `PIC_DEFAULT_INT8`, which would make a checkpoint
   !! written by one build unreadable by the other. Fixed-width field types are
   !! the whole reason the serialized layout is build independent; pick
   !! `int32` or `int64` explicitly.
   !!
   !! ## Serialized layout
   !!
   !! A container is written as a `pic_serialize` stream:
   !!
   !! | order | record                      | contents                          |
   !! |-------|-----------------------------|-----------------------------------|
   !! | 0     | stream header, 16 bytes     | magic, byte-order mark, version   |
   !! | 1     | `PIC_TAG_CHAR`              | the schema string, see below      |
   !! | 2     | `PIC_TAG_INT64`, 1 element  | element count `n`                 |
   !! | 3..   | one record per field        | `n` elements, declaration order   |
   !!
   !! Records 1 and 2 are the *prologue*, written by `soa_write_prologue` and
   !! validated by `soa_read_prologue`. Field records follow in declaration
   !! order, one per field, each holding exactly `n` elements. A `logical`
   !! field travels as a `PIC_TAG_INT32` record of 0/1 codes, because the
   !! envelope has no logical tag and because the storage size of a Fortran
   !! `logical` is not fixed by the standard.
   !!
   !! Nothing in the stream depends on `default_int`: the element count is an
   !! explicit `int64` and every field record carries an explicit fixed-width
   !! tag. A file written by a `PIC_DEFAULT_INT8=ON` build is byte identical to
   !! one written by a default build.
   !!
   !! ## The schema string, and why it matters
   !!
   !! The prologue carries a schema string that names the container and every
   !! field, in order, with its type:
   !!
   !! ```
   !! pic_soa/1;particle;id:i32,x:r64,y:r64,z:r64,mass:r64,active:bool
   !! ```
   !!
   !! `soa_read_prologue` compares it against the schema of the type being read
   !! into and fails with `ERROR_VALIDATION` when they differ, naming both. Any
   !! change to the field list -- a renamed field, a retyped field, a reordered
   !! field, an added or removed field -- changes the string, so an old
   !! checkpoint read by new code is rejected loudly instead of being decoded
   !! as garbage. The `pic_soa/1` prefix (`SOA_SCHEMA_PREFIX`) versions the SoA
   !! layout itself, independently of the `pic_serialize` envelope version.
   !!
   !! The same string is folded into `state_hash` first (see
   !! `soa_hash_begin`), so two containers with different field lists cannot
   !! collide even when their numeric payloads happen to agree.
   !!
   !! ## Error contract
   !!
   !! Every procedure here takes `type(error_t), intent(inout) :: err` and
   !! writes it *only on failure*, never clearing it on entry. This matches
   !! `pic_serialize`, which this module is layered on. Pass a fresh or cleared
   !! `error_t` and check it after each call.
   use pic_types, only: default_int, int32, int64, sp, dp
   use pic_io, only: to_char
   use pic_error, only: error_t, ERROR_ALLOC, ERROR_VALIDATION
   use pic_serialize, only: PIC_HEADER_BYTES, PIC_TAG_INT32, PIC_TAG_INT64, &
                            PIC_TAG_REAL_SP, PIC_TAG_REAL_DP, PIC_TAG_CHAR, &
                            record_bytes, write_header, read_header, &
                            write_array, read_array
   use pic_array_hash, only: array_hash_t
   implicit none
   private

   public :: soa_extent_t
   public :: SOA_SCHEMA_PREFIX, SOA_MIN_CAPACITY, SOA_GROWTH_FACTOR
   public :: SOA_FIELD_INT32, SOA_FIELD_INT64, SOA_FIELD_REAL_SP
   public :: SOA_FIELD_REAL_DP, SOA_FIELD_LOGICAL
   public :: soa_field_code
   public :: soa_grow_capacity
   public :: soa_plan_resize
   public :: soa_resize_field
   public :: soa_write_field, soa_read_field
   public :: soa_write_prologue, soa_read_prologue
   public :: soa_stream_bytes
   public :: soa_hash_begin, soa_hash_field
   public :: soa_check_alloc, soa_check_resize_args
   public :: soa_check_field_count, soa_check_live_count

   character(len=*), parameter :: SOA_SCHEMA_PREFIX = "pic_soa/1;"
      !! Prefix every schema string starts with. Bump the number here when the
      !! SoA record layout itself changes, which retires every older
      !! checkpoint through the ordinary schema mismatch path.

   integer(default_int), parameter :: SOA_MIN_CAPACITY = 8
      !! Capacity a container jumps to on its first growth, so that small
      !! containers do not reallocate on every one of the first few appends.

   integer(default_int), parameter :: SOA_GROWTH_FACTOR = 2
      !! Geometric growth factor. Capacity doubles until it covers the request,
      !! which amortises a sequence of single-element appends to O(1) copies
      !! each.

   integer(int32), parameter :: SOA_FIELD_INT32 = 1_int32
      !! Field kind code for an `integer(int32)` field. Fixed-width `int32`
      !! because these codes describe the on-disk record tags.
   integer(int32), parameter :: SOA_FIELD_INT64 = 2_int32
      !! Field kind code for an `integer(int64)` field.
   integer(int32), parameter :: SOA_FIELD_REAL_SP = 3_int32
      !! Field kind code for a `real(sp)` field.
   integer(int32), parameter :: SOA_FIELD_REAL_DP = 4_int32
      !! Field kind code for a `real(dp)` field.
   integer(int32), parameter :: SOA_FIELD_LOGICAL = 5_int32
      !! Field kind code for a default `logical` field, stored on disk as an
      !! `int32` record of 0/1 codes.

   type :: soa_extent_t
      !! Size and capacity bookkeeping for one SoA container.
      !!
      !! Held as a component of the container rather than inherited from a base
      !! type: PIC targets compilers whose support for type extension plus
      !! type-bound generics is uneven, and composition costs nothing here.
      integer(default_int) :: used = 0
         !! Number of live elements. This is what `size()` returns.
      integer(default_int) :: capacity = 0
         !! Allocated length of every field array.
   end type soa_extent_t

   interface soa_resize_field
      !! Give one field array a new allocated length, preserving a prefix.
      !!
      !! Usage: `call soa_resize_field(values, new_capacity, keep, fill_to, err)`
      !!
      !! `values(1:keep)` survives the call. Indices `keep+1 .. fill_to` are set
      !! to the zero value of the type; indices above `fill_to` are left
      !! unspecified. When `values` already has exactly `new_capacity` elements
      !! no reallocation happens and only the fill is performed, which is what
      !! makes a shrink followed by a grow free.
      !!
      !! Fails with `ERROR_VALIDATION` on inconsistent arguments and with
      !! `ERROR_ALLOC` if the new array cannot be allocated.
      module procedure soa_resize_int32_field
      module procedure soa_resize_int64_field
      module procedure soa_resize_real_sp_field
      module procedure soa_resize_real_dp_field
      module procedure soa_resize_logical_field
   end interface soa_resize_field

   interface soa_write_field
      !! Write the first `n` elements of one field as a stream record.
      !!
      !! Usage: `call soa_write_field(unit, this%x, n, err)`
      !!
      !! The field array is taken whole, as an `allocatable`, together with the
      !! live count: a container that has never been allocated has unallocated
      !! field arrays, and forming the slice `this%x(1:0)` on one of those
      !! would be a reference to an unallocated variable. Handling that here
      !! keeps every generated container safe by construction. An unallocated
      !! field is legal and writes an empty record, provided `n` is zero.
      !!
      !! The prologue must already have been written. A `logical` field is
      !! encoded as an `int32` record of 0/1 codes; every other type maps
      !! straight onto the matching `pic_serialize` record.
      !!
      !! Fails with `ERROR_VALIDATION` when `n` is negative or larger than the
      !! field actually holds.
      module procedure soa_write_int32_field
      module procedure soa_write_int64_field
      module procedure soa_write_real_sp_field
      module procedure soa_write_real_dp_field
      module procedure soa_write_logical_field
   end interface soa_write_field

   interface soa_read_field
      !! Read one field record back and check its length.
      !!
      !! Usage: `call soa_read_field(unit, name, values, n, swapped, err)`
      !!
      !! `values` is reallocated to exactly `n` elements. `name` appears in the
      !! error message so a mismatch names the offending field. `swapped` is
      !! the flag returned by `soa_read_prologue`.
      !!
      !! Fails with `ERROR_VALIDATION` when the record holds a number of
      !! elements other than `n`, or when a logical record holds a code other
      !! than 0 or 1, and with `ERROR_IO` when the stream is truncated.
      module procedure soa_read_int32_field
      module procedure soa_read_int64_field
      module procedure soa_read_real_sp_field
      module procedure soa_read_real_dp_field
      module procedure soa_read_logical_field
   end interface soa_read_field

   interface soa_hash_field
      !! Fold the first `n` elements of one field into a hash accumulator.
      !!
      !! Usage: `call soa_hash_field(hasher, this%x, n)`
      !!
      !! Takes the field whole as an `allocatable`, for the same reason
      !! `soa_write_field` does: a container that has never been allocated has
      !! unallocated fields and no slice of one may be formed. An unallocated
      !! field, and a zero live count, both contribute nothing, which is the
      !! same thing a zero-sized `update` would do.
      module procedure soa_hash_int32_field
      module procedure soa_hash_int64_field
      module procedure soa_hash_real_sp_field
      module procedure soa_hash_real_dp_field
      module procedure soa_hash_logical_field
   end interface soa_hash_field

contains

   ! ------------------------------------------------------------- validation

   subroutine soa_check_alloc(stat, context, err)
      !! Turn a non-zero `stat=` from an `allocate` into `ERROR_ALLOC`.
      integer(default_int), intent(in) :: stat
         !! Value returned by `allocate(..., stat=stat)`.
      character(len=*), intent(in) :: context
         !! What was being allocated, for the message.
      type(error_t), intent(inout) :: err
         !! Error state, written only when `stat` is non-zero.

      if (stat /= 0) then
         call err%set(ERROR_ALLOC, "soa: "//context//" failed, stat "//to_char(int(stat, int64)))
      end if
   end subroutine soa_check_alloc

   subroutine soa_check_resize_args(new_capacity, keep, fill_to, err)
      !! Validate the arguments of a field resize.
      !!
      !! Rejects negative lengths and any request to preserve or fill more
      !! elements than the new array will hold.
      integer(default_int), intent(in) :: new_capacity
         !! Requested allocated length.
      integer(default_int), intent(in) :: keep
         !! Number of leading elements to preserve.
      integer(default_int), intent(in) :: fill_to
         !! Highest index to zero-fill above `keep`.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_VALIDATION` on an inconsistent request.

      if (new_capacity < 0 .or. keep < 0 .or. fill_to < 0) then
         call err%set(ERROR_VALIDATION, "soa: negative length in resize request")
         return
      end if
      if (keep > new_capacity .or. fill_to > new_capacity) then
         call err%set(ERROR_VALIDATION, "soa: resize would keep or fill past the new capacity "// &
                      to_char(int(new_capacity, int64)))
      end if
   end subroutine soa_check_resize_args

   subroutine soa_check_field_count(name, actual, expected, err)
      !! Verify that a field record read back holds the expected element count.
      character(len=*), intent(in) :: name
         !! Field name, for the message.
      integer(default_int), intent(in) :: actual
         !! Elements found in the record.
      integer(default_int), intent(in) :: expected
         !! Elements the prologue said the container has.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_VALIDATION` on a mismatch.

      if (actual /= expected) then
         call err%set(ERROR_VALIDATION, "soa field '"//name//"' holds "//to_char(int(actual, int64))// &
                      " elements, expected "//to_char(int(expected, int64)))
      end if
   end subroutine soa_check_field_count

   subroutine soa_check_live_count(n, allocated_size, err)
      !! Verify that `n` live elements really exist in a field array.
      !!
      !! Pass `allocated_size = -1` for a field that is not allocated at all,
      !! which is only consistent with `n == 0`; a container that has never
      !! been allocated is in exactly that state and must still be
      !! serializable.
      integer(default_int), intent(in) :: n
         !! Number of live elements the container claims to have.
      integer(default_int), intent(in) :: allocated_size
         !! Allocated length of the field array, or -1 if it is unallocated.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_VALIDATION` when the claim cannot hold.

      if (n < 0) then
         call err%set(ERROR_VALIDATION, "soa: live element count "//to_char(int(n, int64))// &
                      " is negative")
         return
      end if
      if (allocated_size < 0) then
         if (n > 0) then
            call err%set(ERROR_VALIDATION, "soa: "//to_char(int(n, int64))// &
                         " live elements requested from an unallocated field")
         end if
         return
      end if
      if (n > allocated_size) then
         call err%set(ERROR_VALIDATION, "soa: "//to_char(int(n, int64))// &
                      " live elements requested from a field holding "// &
                      to_char(int(allocated_size, int64)))
      end if
   end subroutine soa_check_live_count

   ! ----------------------------------------------------------------- growth

   pure function soa_grow_capacity(current_capacity, required) result(new_capacity)
      !! Capacity that covers `required` elements under the geometric policy.
      !!
      !! Returns `current_capacity` unchanged when it is already enough, so
      !! capacity never shrinks here. Otherwise it starts from
      !! `max(current_capacity, SOA_MIN_CAPACITY)` and multiplies by
      !! `SOA_GROWTH_FACTOR` until it covers the request. Near the top of the
      !! integer range doubling would overflow, so the exact request is used
      !! instead; that is the only point where growth is not geometric.
      integer(default_int), intent(in) :: current_capacity
         !! Capacity the container has now.
      integer(default_int), intent(in) :: required
         !! Number of elements that must fit.
      integer(default_int) :: new_capacity

      integer(default_int), parameter :: DOUBLING_LIMIT = huge(0_default_int)/SOA_GROWTH_FACTOR

      if (required <= current_capacity) then
         new_capacity = current_capacity
         return
      end if

      new_capacity = max(current_capacity, SOA_MIN_CAPACITY)
      do while (new_capacity < required)
         if (new_capacity > DOUBLING_LIMIT) then
            new_capacity = required
         else
            new_capacity = new_capacity*SOA_GROWTH_FACTOR
         end if
      end do
   end function soa_grow_capacity

   subroutine soa_plan_resize(extent, new_size, new_capacity, keep, err)
      !! Work out what a resize to `new_size` elements has to do.
      !!
      !! Computes the capacity every field array must end up with and how many
      !! leading elements carry over. Does not touch `extent`: the caller
      !! updates it once every field has actually been resized, so a failed
      !! resize leaves the container consistent with its fields.
      type(soa_extent_t), intent(in) :: extent
         !! Current size and capacity of the container.
      integer(default_int), intent(in) :: new_size
         !! Requested number of live elements.
      integer(default_int), intent(out) :: new_capacity
         !! Capacity the field arrays must have afterwards.
      integer(default_int), intent(out) :: keep
         !! Number of leading elements whose values must survive.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_VALIDATION` when `new_size` is negative.

      new_capacity = extent%capacity
      keep = 0

      if (new_size < 0) then
         call err%set(ERROR_VALIDATION, "soa: requested size "//to_char(int(new_size, int64))// &
                      " is negative")
         return
      end if

      keep = min(extent%used, new_size)
      if (new_size > extent%capacity) then
         new_capacity = soa_grow_capacity(extent%capacity, new_size)
      end if
   end subroutine soa_plan_resize

   ! ---------------------------------------------------------- field resizes

   subroutine soa_resize_int32_field(values, new_capacity, keep, fill_to, err)
      !! Resize an `integer(int32)` field.
      integer(int32), allocatable, intent(inout) :: values(:)
      integer(default_int), intent(in) :: new_capacity
      integer(default_int), intent(in) :: keep
      integer(default_int), intent(in) :: fill_to
      type(error_t), intent(inout) :: err

      integer(int32), allocatable :: buffer(:)
      integer(default_int) :: stat

      call soa_check_resize_args(new_capacity, keep, fill_to, err)
      if (err%has_error()) return

      if (allocated(values)) then
         if (size(values, kind=default_int) == new_capacity) then
            if (fill_to > keep) values(keep + 1:fill_to) = 0_int32
            return
         end if
      end if

      allocate (buffer(new_capacity), stat=stat)
      call soa_check_alloc(stat, "resizing an int32 field", err)
      if (err%has_error()) return
      if (keep > 0) buffer(1:keep) = values(1:keep)
      if (fill_to > keep) buffer(keep + 1:fill_to) = 0_int32
      call move_alloc(buffer, values)
   end subroutine soa_resize_int32_field

   subroutine soa_resize_int64_field(values, new_capacity, keep, fill_to, err)
      !! Resize an `integer(int64)` field.
      integer(int64), allocatable, intent(inout) :: values(:)
      integer(default_int), intent(in) :: new_capacity
      integer(default_int), intent(in) :: keep
      integer(default_int), intent(in) :: fill_to
      type(error_t), intent(inout) :: err

      integer(int64), allocatable :: buffer(:)
      integer(default_int) :: stat

      call soa_check_resize_args(new_capacity, keep, fill_to, err)
      if (err%has_error()) return

      if (allocated(values)) then
         if (size(values, kind=default_int) == new_capacity) then
            if (fill_to > keep) values(keep + 1:fill_to) = 0_int64
            return
         end if
      end if

      allocate (buffer(new_capacity), stat=stat)
      call soa_check_alloc(stat, "resizing an int64 field", err)
      if (err%has_error()) return
      if (keep > 0) buffer(1:keep) = values(1:keep)
      if (fill_to > keep) buffer(keep + 1:fill_to) = 0_int64
      call move_alloc(buffer, values)
   end subroutine soa_resize_int64_field

   subroutine soa_resize_real_sp_field(values, new_capacity, keep, fill_to, err)
      !! Resize a `real(sp)` field.
      real(sp), allocatable, intent(inout) :: values(:)
      integer(default_int), intent(in) :: new_capacity
      integer(default_int), intent(in) :: keep
      integer(default_int), intent(in) :: fill_to
      type(error_t), intent(inout) :: err

      real(sp), allocatable :: buffer(:)
      integer(default_int) :: stat

      call soa_check_resize_args(new_capacity, keep, fill_to, err)
      if (err%has_error()) return

      if (allocated(values)) then
         if (size(values, kind=default_int) == new_capacity) then
            if (fill_to > keep) values(keep + 1:fill_to) = 0.0_sp
            return
         end if
      end if

      allocate (buffer(new_capacity), stat=stat)
      call soa_check_alloc(stat, "resizing a real(sp) field", err)
      if (err%has_error()) return
      if (keep > 0) buffer(1:keep) = values(1:keep)
      if (fill_to > keep) buffer(keep + 1:fill_to) = 0.0_sp
      call move_alloc(buffer, values)
   end subroutine soa_resize_real_sp_field

   subroutine soa_resize_real_dp_field(values, new_capacity, keep, fill_to, err)
      !! Resize a `real(dp)` field.
      real(dp), allocatable, intent(inout) :: values(:)
      integer(default_int), intent(in) :: new_capacity
      integer(default_int), intent(in) :: keep
      integer(default_int), intent(in) :: fill_to
      type(error_t), intent(inout) :: err

      real(dp), allocatable :: buffer(:)
      integer(default_int) :: stat

      call soa_check_resize_args(new_capacity, keep, fill_to, err)
      if (err%has_error()) return

      if (allocated(values)) then
         if (size(values, kind=default_int) == new_capacity) then
            if (fill_to > keep) values(keep + 1:fill_to) = 0.0_dp
            return
         end if
      end if

      allocate (buffer(new_capacity), stat=stat)
      call soa_check_alloc(stat, "resizing a real(dp) field", err)
      if (err%has_error()) return
      if (keep > 0) buffer(1:keep) = values(1:keep)
      if (fill_to > keep) buffer(keep + 1:fill_to) = 0.0_dp
      call move_alloc(buffer, values)
   end subroutine soa_resize_real_dp_field

   subroutine soa_resize_logical_field(values, new_capacity, keep, fill_to, err)
      !! Resize a default `logical` field.
      logical, allocatable, intent(inout) :: values(:)
      integer(default_int), intent(in) :: new_capacity
      integer(default_int), intent(in) :: keep
      integer(default_int), intent(in) :: fill_to
      type(error_t), intent(inout) :: err

      logical, allocatable :: buffer(:)
      integer(default_int) :: stat

      call soa_check_resize_args(new_capacity, keep, fill_to, err)
      if (err%has_error()) return

      if (allocated(values)) then
         if (size(values, kind=default_int) == new_capacity) then
            if (fill_to > keep) values(keep + 1:fill_to) = .false.
            return
         end if
      end if

      allocate (buffer(new_capacity), stat=stat)
      call soa_check_alloc(stat, "resizing a logical field", err)
      if (err%has_error()) return
      if (keep > 0) buffer(1:keep) = values(1:keep)
      if (fill_to > keep) buffer(keep + 1:fill_to) = .false.
      call move_alloc(buffer, values)
   end subroutine soa_resize_logical_field

   ! ------------------------------------------------------------ field write

   subroutine soa_write_int32_field(unit, values, n, err)
      !! Write the first `n` elements of an `integer(int32)` field.
      integer(default_int), intent(in) :: unit
      integer(int32), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout) :: err

      integer(int32) :: empty(0)
      integer(default_int) :: nalloc

      nalloc = -1
      if (allocated(values)) nalloc = size(values, kind=default_int)
      call soa_check_live_count(n, nalloc, err)
      if (err%has_error()) return

      if (nalloc < 0) then
         call write_array(unit, empty, err)
      else
         call write_array(unit, values(1:n), err)
      end if
   end subroutine soa_write_int32_field

   subroutine soa_write_int64_field(unit, values, n, err)
      !! Write the first `n` elements of an `integer(int64)` field.
      integer(default_int), intent(in) :: unit
      integer(int64), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout) :: err

      integer(int64) :: empty(0)
      integer(default_int) :: nalloc

      nalloc = -1
      if (allocated(values)) nalloc = size(values, kind=default_int)
      call soa_check_live_count(n, nalloc, err)
      if (err%has_error()) return

      if (nalloc < 0) then
         call write_array(unit, empty, err)
      else
         call write_array(unit, values(1:n), err)
      end if
   end subroutine soa_write_int64_field

   subroutine soa_write_real_sp_field(unit, values, n, err)
      !! Write the first `n` elements of a `real(sp)` field.
      integer(default_int), intent(in) :: unit
      real(sp), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout) :: err

      real(sp) :: empty(0)
      integer(default_int) :: nalloc

      nalloc = -1
      if (allocated(values)) nalloc = size(values, kind=default_int)
      call soa_check_live_count(n, nalloc, err)
      if (err%has_error()) return

      if (nalloc < 0) then
         call write_array(unit, empty, err)
      else
         call write_array(unit, values(1:n), err)
      end if
   end subroutine soa_write_real_sp_field

   subroutine soa_write_real_dp_field(unit, values, n, err)
      !! Write the first `n` elements of a `real(dp)` field.
      integer(default_int), intent(in) :: unit
      real(dp), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout) :: err

      real(dp) :: empty(0)
      integer(default_int) :: nalloc

      nalloc = -1
      if (allocated(values)) nalloc = size(values, kind=default_int)
      call soa_check_live_count(n, nalloc, err)
      if (err%has_error()) return

      if (nalloc < 0) then
         call write_array(unit, empty, err)
      else
         call write_array(unit, values(1:n), err)
      end if
   end subroutine soa_write_real_dp_field

   subroutine soa_write_logical_field(unit, values, n, err)
      !! Write the first `n` elements of a default `logical` field as an
      !! `int32` record of 0/1 codes.
      !!
      !! The envelope has no logical tag, and the storage size of a Fortran
      !! `logical` is not fixed by the standard, so an explicit 0/1 encoding is
      !! the only portable thing to put in a file.
      integer(default_int), intent(in) :: unit
      logical, allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout) :: err

      integer(int32), allocatable :: coded(:)
      integer(default_int) :: i, nalloc, stat

      nalloc = -1
      if (allocated(values)) nalloc = size(values, kind=default_int)
      call soa_check_live_count(n, nalloc, err)
      if (err%has_error()) return

      allocate (coded(max(n, 0_default_int)), stat=stat)
      call soa_check_alloc(stat, "encoding a logical field", err)
      if (err%has_error()) return

      do i = 1, n
         if (values(i)) then
            coded(i) = 1_int32
         else
            coded(i) = 0_int32
         end if
      end do
      call write_array(unit, coded, err)
   end subroutine soa_write_logical_field

   ! -------------------------------------------------------------- hash fold

   subroutine soa_hash_int32_field(hasher, values, n)
      !! Fold the first `n` elements of an `integer(int32)` field into a hash.
      type(array_hash_t), intent(inout) :: hasher
      integer(int32), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n

      if (.not. allocated(values)) return
      if (n <= 0) return
      call hasher%update(values(1:n))
   end subroutine soa_hash_int32_field

   subroutine soa_hash_int64_field(hasher, values, n)
      !! Fold the first `n` elements of an `integer(int64)` field into a hash.
      type(array_hash_t), intent(inout) :: hasher
      integer(int64), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n

      if (.not. allocated(values)) return
      if (n <= 0) return
      call hasher%update(values(1:n))
   end subroutine soa_hash_int64_field

   subroutine soa_hash_real_sp_field(hasher, values, n)
      !! Fold the first `n` elements of a `real(sp)` field into a hash.
      type(array_hash_t), intent(inout) :: hasher
      real(sp), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n

      if (.not. allocated(values)) return
      if (n <= 0) return
      call hasher%update(values(1:n))
   end subroutine soa_hash_real_sp_field

   subroutine soa_hash_real_dp_field(hasher, values, n)
      !! Fold the first `n` elements of a `real(dp)` field into a hash.
      type(array_hash_t), intent(inout) :: hasher
      real(dp), allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n

      if (.not. allocated(values)) return
      if (n <= 0) return
      call hasher%update(values(1:n))
   end subroutine soa_hash_real_dp_field

   subroutine soa_hash_logical_field(hasher, values, n)
      !! Fold the first `n` elements of a default `logical` field into a hash.
      type(array_hash_t), intent(inout) :: hasher
      logical, allocatable, intent(in) :: values(:)
      integer(default_int), intent(in) :: n

      if (.not. allocated(values)) return
      if (n <= 0) return
      call hasher%update(values(1:n))
   end subroutine soa_hash_logical_field

   ! ------------------------------------------------------------- field read

   subroutine soa_read_int32_field(unit, name, values, n, swapped, err)
      !! Read an `integer(int32)` field record.
      integer(default_int), intent(in) :: unit
      character(len=*), intent(in) :: name
      integer(int32), allocatable, intent(out) :: values(:)
      integer(default_int), intent(in) :: n
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(int32), allocatable :: buffer(:)

      call read_array(unit, buffer, swapped, err)
      if (err%has_error()) return
      call soa_check_field_count(name, size(buffer, kind=default_int), n, err)
      if (err%has_error()) return
      call move_alloc(buffer, values)
   end subroutine soa_read_int32_field

   subroutine soa_read_int64_field(unit, name, values, n, swapped, err)
      !! Read an `integer(int64)` field record.
      integer(default_int), intent(in) :: unit
      character(len=*), intent(in) :: name
      integer(int64), allocatable, intent(out) :: values(:)
      integer(default_int), intent(in) :: n
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(int64), allocatable :: buffer(:)

      call read_array(unit, buffer, swapped, err)
      if (err%has_error()) return
      call soa_check_field_count(name, size(buffer, kind=default_int), n, err)
      if (err%has_error()) return
      call move_alloc(buffer, values)
   end subroutine soa_read_int64_field

   subroutine soa_read_real_sp_field(unit, name, values, n, swapped, err)
      !! Read a `real(sp)` field record.
      integer(default_int), intent(in) :: unit
      character(len=*), intent(in) :: name
      real(sp), allocatable, intent(out) :: values(:)
      integer(default_int), intent(in) :: n
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      real(sp), allocatable :: buffer(:)

      call read_array(unit, buffer, swapped, err)
      if (err%has_error()) return
      call soa_check_field_count(name, size(buffer, kind=default_int), n, err)
      if (err%has_error()) return
      call move_alloc(buffer, values)
   end subroutine soa_read_real_sp_field

   subroutine soa_read_real_dp_field(unit, name, values, n, swapped, err)
      !! Read a `real(dp)` field record.
      integer(default_int), intent(in) :: unit
      character(len=*), intent(in) :: name
      real(dp), allocatable, intent(out) :: values(:)
      integer(default_int), intent(in) :: n
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      real(dp), allocatable :: buffer(:)

      call read_array(unit, buffer, swapped, err)
      if (err%has_error()) return
      call soa_check_field_count(name, size(buffer, kind=default_int), n, err)
      if (err%has_error()) return
      call move_alloc(buffer, values)
   end subroutine soa_read_real_dp_field

   subroutine soa_read_logical_field(unit, name, values, n, swapped, err)
      !! Read a default `logical` field from an `int32` record of 0/1 codes.
      !!
      !! Any code other than 0 or 1 is rejected rather than being coerced to
      !! true: a record that did not come from `soa_write_field` is more likely
      !! to be a mislabelled integer field than a sloppy boolean.
      integer(default_int), intent(in) :: unit
      character(len=*), intent(in) :: name
      logical, allocatable, intent(out) :: values(:)
      integer(default_int), intent(in) :: n
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(int32), allocatable :: buffer(:)
      integer(default_int) :: i, stat

      call read_array(unit, buffer, swapped, err)
      if (err%has_error()) return
      call soa_check_field_count(name, size(buffer, kind=default_int), n, err)
      if (err%has_error()) return

      do i = 1, n
         if (buffer(i) /= 0_int32 .and. buffer(i) /= 1_int32) then
            call err%set(ERROR_VALIDATION, "soa field '"//name//"' element "// &
                         to_char(int(i, int64))//" is not a logical code: "//to_char(buffer(i)))
            return
         end if
      end do

      allocate (values(n), stat=stat)
      call soa_check_alloc(stat, "decoding a logical field", err)
      if (err%has_error()) return
      do i = 1, n
         values(i) = (buffer(i) == 1_int32)
      end do
   end subroutine soa_read_logical_field

   ! ---------------------------------------------------------- stream layout

   pure function soa_field_code(field_kind) result(code)
      !! Short schema spelling of a field kind code, or `"?"` if unknown.
      !!
      !! These are the tokens that appear after the colon in a schema string:
      !! `i32`, `i64`, `r32`, `r64`, `bool`.
      integer(int32), intent(in) :: field_kind
         !! One of the `SOA_FIELD_*` constants.
      character(len=:), allocatable :: code

      select case (field_kind)
      case (SOA_FIELD_INT32)
         code = "i32"
      case (SOA_FIELD_INT64)
         code = "i64"
      case (SOA_FIELD_REAL_SP)
         code = "r32"
      case (SOA_FIELD_REAL_DP)
         code = "r64"
      case (SOA_FIELD_LOGICAL)
         code = "bool"
      case default
         code = "?"
      end select
   end function soa_field_code

   pure function soa_field_tag(field_kind) result(tag)
      !! `pic_serialize` record tag a field kind is stored as, or -1 if the
      !! kind is unknown. A `logical` field maps onto the `int32` tag.
      integer(int32), intent(in) :: field_kind
      integer(int32) :: tag

      select case (field_kind)
      case (SOA_FIELD_INT32, SOA_FIELD_LOGICAL)
         tag = PIC_TAG_INT32
      case (SOA_FIELD_INT64)
         tag = PIC_TAG_INT64
      case (SOA_FIELD_REAL_SP)
         tag = PIC_TAG_REAL_SP
      case (SOA_FIELD_REAL_DP)
         tag = PIC_TAG_REAL_DP
      case default
         tag = -1_int32
      end select
   end function soa_field_tag

   pure function soa_stream_bytes(schema, n, field_kinds) result(nbytes)
      !! Exact size in bytes of the stream a container of `n` elements writes.
      !!
      !! Returns -1 when `n` is negative or a field kind is unknown. Useful to
      !! reserve space up front, and it lets a test pin the on-disk layout
      !! against the real file size.
      character(len=*), intent(in) :: schema
         !! The container's schema string.
      integer(default_int), intent(in) :: n
         !! Number of live elements.
      integer(int32), intent(in) :: field_kinds(:)
         !! One `SOA_FIELD_*` code per field, in declaration order.
      integer(int64) :: nbytes

      integer(default_int) :: i
      integer(int32) :: tag

      if (n < 0) then
         nbytes = -1_int64
         return
      end if

      nbytes = PIC_HEADER_BYTES + record_bytes(PIC_TAG_CHAR, int(len(schema), int64)) &
               + record_bytes(PIC_TAG_INT64, 1_int64)

      do i = 1, size(field_kinds, kind=default_int)
         tag = soa_field_tag(field_kinds(i))
         if (tag < 0_int32) then
            nbytes = -1_int64
            return
         end if
         nbytes = nbytes + record_bytes(tag, int(n, int64))
      end do
   end function soa_stream_bytes

   subroutine soa_write_prologue(unit, schema, n, err)
      !! Write the stream header, the schema record and the element count.
      !!
      !! Call this once on a freshly opened stream unit, before any field
      !! record.
      integer(default_int), intent(in) :: unit
         !! Stream unit opened for unformatted stream writing.
      character(len=*), intent(in) :: schema
         !! The container's schema string.
      integer(default_int), intent(in) :: n
         !! Number of live elements about to be written.
      type(error_t), intent(inout) :: err
         !! Error state.

      if (n < 0) then
         call err%set(ERROR_VALIDATION, "soa: cannot serialize a negative element count "// &
                      to_char(int(n, int64)))
         return
      end if

      call write_header(unit, err)
      if (err%has_error()) return
      call write_array(unit, schema, err)
      if (err%has_error()) return
      call write_array(unit, [int(n, int64)], err)
   end subroutine soa_write_prologue

   subroutine soa_read_prologue(unit, schema, n, swapped, err)
      !! Read and validate the prologue, returning the element count.
      !!
      !! Fails with `ERROR_VALIDATION` when the schema in the stream is not
      !! exactly `schema`, naming both, and when the element count record is
      !! malformed or does not fit this build's `default_int`. The envelope
      !! itself -- magic, byte order, version -- is validated by
      !! `pic_serialize`.
      integer(default_int), intent(in) :: unit
         !! Stream unit opened for unformatted stream reading.
      character(len=*), intent(in) :: schema
         !! Schema the calling container expects.
      integer(default_int), intent(out) :: n
         !! Number of elements the stream holds, 0 when the prologue is
         !! rejected.
      logical, intent(out) :: swapped
         !! True when the stream came from the opposite byte order; pass it on
         !! to every `soa_read_field` call.
      type(error_t), intent(inout) :: err
         !! Error state.

      integer(int32) :: version
      character(len=:), allocatable :: found
      integer(int64), allocatable :: counts(:)

      n = 0
      swapped = .false.

      call read_header(unit, version, swapped, err)
      if (err%has_error()) return

      call read_array(unit, found, swapped, err)
      if (err%has_error()) return

      ! Length is compared as well as content because Fortran character
      ! comparison blank-pads the shorter operand, which would let a schema
      ! with trailing blanks masquerade as this one.
      if (len(found) /= len(schema) .or. found /= schema) then
         call err%set(ERROR_VALIDATION, "soa layout mismatch: stream declares '"//found// &
                      "', this container is '"//schema//"'")
         return
      end if

      call read_array(unit, counts, swapped, err)
      if (err%has_error()) return

      if (size(counts, kind=default_int) /= 1) then
         call err%set(ERROR_VALIDATION, "soa: element count record holds "// &
                      to_char(int(size(counts, kind=default_int), int64))//" values, expected 1")
         return
      end if
      if (counts(1) < 0_int64 .or. counts(1) > int(huge(0_default_int), int64)) then
         call err%set(ERROR_VALIDATION, "soa: element count "//to_char(counts(1))// &
                      " is out of range for this build")
         return
      end if

      n = int(counts(1), default_int)
   end subroutine soa_read_prologue

   ! ------------------------------------------------------------- state hash

   subroutine soa_hash_begin(hasher, schema, n)
      !! Reset a hash accumulator and fold in a container's identity.
      !!
      !! Feeds the schema string and then the element count as an explicit
      !! `int64`, so the digest is the same in a default build and in a
      !! `PIC_DEFAULT_INT8` build. Folding the schema first means two
      !! containers with different field lists cannot produce the same digest
      !! even when their numeric payloads agree, and folding the count means an
      !! empty container is distinguishable from an absent one.
      !!
      !! The caller then folds in every field's live slice, in declaration
      !! order, and takes `hasher%digest()`.
      type(array_hash_t), intent(inout) :: hasher
         !! Accumulator to restart.
      character(len=*), intent(in) :: schema
         !! The container's schema string.
      integer(default_int), intent(in) :: n
         !! Number of live elements.

      call hasher%reset()
      call hasher%update(schema)
      call hasher%update(int(n, int64))
   end subroutine soa_hash_begin

end module pic_soa
