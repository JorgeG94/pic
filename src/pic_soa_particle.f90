! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Struct-of-arrays container for particle data.
!!
!! GENERATED FILE -- DO NOT EDIT.
!! Produced by `tools/autogen/pic_soa.fypp`; edit the template and rerun
!! `tools/autogen/autogen.sh` instead.
module pic_soa_particle
   !! A struct-of-arrays container holding 6 parallel field arrays.
   !!
   !! Each field is a separate contiguous rank-1 array, which is the point of
   !! the layout: `this%id` is contiguous, and so is the live slice
   !! `this%id(1:this%size())`, so either can be handed straight to BLAS or
   !! to a vectorised loop without a copy.
   !!
   !! Growth, the serialized record layout, the schema check on deserialize and
   !! the state-hash fold order are all documented in `pic_soa`. This type's
   !! schema string is
   !!
   !! ```
   !! particle;id:i32,x:r64,y:r64,z:r64,mass:r64,active:bool
   !! ```
   !!
   !! prefixed by `SOA_SCHEMA_PREFIX`. Fields are serialized and folded into
   !! the hash in exactly the order they are declared below.
   use pic_types, only: default_int, int32, int64, dp
   use pic_error, only: error_t
   use pic_array_hash, only: array_hash_t, array_hash64_t
   use pic_soa, only: soa_extent_t, SOA_SCHEMA_PREFIX, &
                      SOA_FIELD_INT32, SOA_FIELD_LOGICAL, SOA_FIELD_REAL_DP, &
                      soa_plan_resize, soa_resize_field, &
                      soa_write_field, soa_read_field, soa_hash_field, &
                      soa_write_prologue, soa_read_prologue, &
                      soa_stream_bytes, soa_hash_begin
   implicit none
   private

   public :: particle_soa_t
   public :: PARTICLE_SOA_SCHEMA
   public :: PARTICLE_SOA_N_FIELDS
   public :: PARTICLE_SOA_FIELD_KINDS

   character(len=*), parameter :: PARTICLE_SOA_SCHEMA = SOA_SCHEMA_PREFIX//"particle;id:i32,x:r64,y:r64,z:r64,mass:r64,active:bool"
      !! Layout identity written into every stream and folded into every
      !! digest. Any change to the field list changes it, which is what makes a
      !! stale checkpoint fail loudly instead of decoding as garbage.

   integer(default_int), parameter :: PARTICLE_SOA_N_FIELDS = 6
      !! Number of fields, which is also the number of field records a stream
      !! carries after the prologue.

   integer(int32), parameter :: PARTICLE_SOA_FIELD_KINDS(PARTICLE_SOA_N_FIELDS) = [ &
                                SOA_FIELD_INT32, &
                                SOA_FIELD_REAL_DP, &
                                SOA_FIELD_REAL_DP, &
                                SOA_FIELD_REAL_DP, &
                                SOA_FIELD_REAL_DP, &
                                SOA_FIELD_LOGICAL &
                                ]
      !! Field kind codes in declaration order. Fixed-width `int32` because
      !! these describe on-disk record tags. Pass to `soa_stream_bytes`.

   type :: particle_soa_t
      !! 6 parallel field arrays plus size/capacity bookkeeping.
      type(soa_extent_t) :: extent
         !! Live element count and allocated capacity.
      integer(int32), allocatable :: id(:)
         !! Particle identifier.
      real(dp), allocatable :: x(:)
         !! Cartesian x coordinate.
      real(dp), allocatable :: y(:)
         !! Cartesian y coordinate.
      real(dp), allocatable :: z(:)
         !! Cartesian z coordinate.
      real(dp), allocatable :: mass(:)
         !! Particle mass.
      logical, allocatable :: active(:)
         !! Whether the particle takes part in the simulation.
   contains
      procedure :: allocate => particle_soa_allocate
      procedure :: resize => particle_soa_resize
      procedure :: deallocate => particle_soa_deallocate
      procedure :: clear => particle_soa_clear
      procedure :: size => particle_soa_size
      procedure :: capacity => particle_soa_capacity
      procedure :: serialized_bytes => particle_soa_serialized_bytes
      procedure :: serialize => particle_soa_serialize
      procedure :: deserialize => particle_soa_deserialize
      procedure :: state_hash => particle_soa_state_hash
      procedure :: state_hash64 => particle_soa_state_hash64
   end type particle_soa_t

contains

   subroutine particle_soa_allocate(this, n, err)
      !! Give the container `n` live elements, discarding any it already had.
      !!
      !! Equivalent to `clear` followed by `resize(n)`, so calling it twice is
      !! well defined: the second call simply replaces the contents. Every
      !! element is set to the zero value of its field's type. Capacity is
      !! never lowered, so a smaller second call reuses the existing memory.
      !!
      !! On failure the container is left empty rather than half filled.
      class(particle_soa_t), intent(inout) :: this
         !! Container to (re)allocate.
      integer(default_int), intent(in) :: n
         !! Number of live elements. Must not be negative.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_VALIDATION` on a negative `n` and
         !! `ERROR_ALLOC` if the field arrays cannot be allocated.

      this%extent%used = 0
      call this%resize(n, err)
   end subroutine particle_soa_allocate

   subroutine particle_soa_resize(this, n, err)
      !! Change the number of live elements, preserving the elements that
      !! survive.
      !!
      !! `min(size(), n)` leading elements of every field keep their values.
      !! Growing sets the newly exposed elements to the zero value of their
      !! type; shrinking only lowers the live count. Capacity grows
      !! geometrically and never shrinks -- see `pic_soa`.
      !!
      !! On failure the container is left exactly as it was.
      class(particle_soa_t), intent(inout) :: this
         !! Container to resize.
      integer(default_int), intent(in) :: n
         !! New number of live elements. Must not be negative.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_VALIDATION` on a negative `n` and
         !! `ERROR_ALLOC` if a field array cannot be allocated.

      integer(default_int) :: new_capacity, keep

      call soa_plan_resize(this%extent, n, new_capacity, keep, err)
      if (err%has_error()) return

      call soa_resize_field(this%id, new_capacity, keep, n, err)
      if (err%has_error()) return
      call soa_resize_field(this%x, new_capacity, keep, n, err)
      if (err%has_error()) return
      call soa_resize_field(this%y, new_capacity, keep, n, err)
      if (err%has_error()) return
      call soa_resize_field(this%z, new_capacity, keep, n, err)
      if (err%has_error()) return
      call soa_resize_field(this%mass, new_capacity, keep, n, err)
      if (err%has_error()) return
      call soa_resize_field(this%active, new_capacity, keep, n, err)
      if (err%has_error()) return

      this%extent%used = n
      this%extent%capacity = new_capacity
   end subroutine particle_soa_resize

   subroutine particle_soa_deallocate(this)
      !! Release every field array and reset both counters to zero.
      class(particle_soa_t), intent(inout) :: this
         !! Container to empty.

      if (allocated(this%id)) deallocate (this%id)
      if (allocated(this%x)) deallocate (this%x)
      if (allocated(this%y)) deallocate (this%y)
      if (allocated(this%z)) deallocate (this%z)
      if (allocated(this%mass)) deallocate (this%mass)
      if (allocated(this%active)) deallocate (this%active)
      this%extent%used = 0
      this%extent%capacity = 0
   end subroutine particle_soa_deallocate

   subroutine particle_soa_clear(this)
      !! Drop every live element but keep the allocated capacity.
      !!
      !! Use this to reuse a container across iterations without paying for a
      !! reallocation. Use `deallocate` to actually give the memory back.
      class(particle_soa_t), intent(inout) :: this
         !! Container to empty.

      this%extent%used = 0
   end subroutine particle_soa_clear

   pure function particle_soa_size(this) result(n)
      !! Number of live elements.
      class(particle_soa_t), intent(in) :: this
         !! Container to query.
      integer(default_int) :: n

      n = this%extent%used
   end function particle_soa_size

   pure function particle_soa_capacity(this) result(n)
      !! Allocated length of every field array. Always at least `size()`.
      class(particle_soa_t), intent(in) :: this
         !! Container to query.
      integer(default_int) :: n

      n = this%extent%capacity
   end function particle_soa_capacity

   pure function particle_soa_serialized_bytes(this) result(nbytes)
      !! Exact size in bytes of the stream `serialize` would write now.
      class(particle_soa_t), intent(in) :: this
         !! Container to measure.
      integer(int64) :: nbytes

      nbytes = soa_stream_bytes(PARTICLE_SOA_SCHEMA, this%extent%used, &
                                PARTICLE_SOA_FIELD_KINDS)
   end function particle_soa_serialized_bytes

   subroutine particle_soa_serialize(this, unit, err)
      !! Write the container to an open stream unit.
      !!
      !! Writes the prologue -- stream header, schema record, element count --
      !! and then one record per field in declaration order, each holding
      !! exactly `size()` elements. The unit must be open for unformatted
      !! stream writing, for instance from `serialize_open_write`.
      class(particle_soa_t), intent(in) :: this
         !! Container to write.
      integer(default_int), intent(in) :: unit
         !! Stream unit opened for unformatted stream writing.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_IO` if the stream cannot be written.

      integer(default_int) :: n

      n = this%extent%used
      call soa_write_prologue(unit, PARTICLE_SOA_SCHEMA, n, err)
      if (err%has_error()) return

      call soa_write_field(unit, this%id, n, err)
      if (err%has_error()) return
      call soa_write_field(unit, this%x, n, err)
      if (err%has_error()) return
      call soa_write_field(unit, this%y, n, err)
      if (err%has_error()) return
      call soa_write_field(unit, this%z, n, err)
      if (err%has_error()) return
      call soa_write_field(unit, this%mass, n, err)
      if (err%has_error()) return
      call soa_write_field(unit, this%active, n, err)
      if (err%has_error()) return
   end subroutine particle_soa_serialize

   subroutine particle_soa_deserialize(this, unit, err)
      !! Read a container back from an open stream unit.
      !!
      !! Validates the envelope and the schema string before touching any
      !! payload, so a stream written from a different field list is rejected
      !! with `ERROR_VALIDATION` rather than decoded as garbage. Every field
      !! record must hold exactly the element count the prologue announced.
      !!
      !! On success the container holds exactly the elements from the stream
      !! and its capacity equals its size. On failure it is left deallocated
      !! rather than half filled.
      class(particle_soa_t), intent(inout) :: this
         !! Container to fill. Any previous contents are discarded.
      integer(default_int), intent(in) :: unit
         !! Stream unit opened for unformatted stream reading.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_VALIDATION` on a layout or length mismatch and
         !! `ERROR_IO` on a truncated stream.

      integer(default_int) :: n
      logical :: swapped

      call this%deallocate()

      call soa_read_prologue(unit, PARTICLE_SOA_SCHEMA, n, swapped, err)
      if (err%has_error()) return

      call soa_read_field(unit, "id", this%id, n, swapped, err)
      if (err%has_error()) then
         call this%deallocate()
         return
      end if
      call soa_read_field(unit, "x", this%x, n, swapped, err)
      if (err%has_error()) then
         call this%deallocate()
         return
      end if
      call soa_read_field(unit, "y", this%y, n, swapped, err)
      if (err%has_error()) then
         call this%deallocate()
         return
      end if
      call soa_read_field(unit, "z", this%z, n, swapped, err)
      if (err%has_error()) then
         call this%deallocate()
         return
      end if
      call soa_read_field(unit, "mass", this%mass, n, swapped, err)
      if (err%has_error()) then
         call this%deallocate()
         return
      end if
      call soa_read_field(unit, "active", this%active, n, swapped, err)
      if (err%has_error()) then
         call this%deallocate()
         return
      end if

      this%extent%used = n
      this%extent%capacity = n
   end subroutine particle_soa_deserialize

   function particle_soa_state_hash(this) result(digest)
      !! 32-bit FNV-1a digest of the container's live state.
      !!
      !! Folds the schema string, then the element count as an explicit
      !! `int64`, then every field's live slice in declaration order. Two runs
      !! that reach identical state therefore produce identical digests, on any
      !! supported compiler and in both `default_int` builds, which is what
      !! makes this usable to validate a checkpoint.
      !!
      !! Changing any single element of any single field changes the digest.
      !! See `pic_array_hash` for how -0.0 and NaN are canonicalised.
      class(particle_soa_t), intent(in) :: this
         !! Container to fingerprint.
      integer(int32) :: digest

      type(array_hash_t) :: hasher
      integer(default_int) :: n

      n = this%extent%used
      call soa_hash_begin(hasher, PARTICLE_SOA_SCHEMA, n)
      call soa_hash_field(hasher, this%id, n)
      call soa_hash_field(hasher, this%x, n)
      call soa_hash_field(hasher, this%y, n)
      call soa_hash_field(hasher, this%z, n)
      call soa_hash_field(hasher, this%mass, n)
      call soa_hash_field(hasher, this%active, n)
      digest = hasher%digest()
   end function particle_soa_state_hash

   function particle_soa_state_hash64(this) result(digest)
      !! 64-bit FNV-1a digest of the container's live state.
      !!
      !! Byte for byte the same stream as `state_hash`: the schema string, the
      !! element count as an explicit `int64`, then every field's live slice in
      !! declaration order. Only the digest width differs.
      !!
      !! Prefer this one when digests are used as identifiers rather than
      !! compared pairwise -- a determinism log keyed by digest, or
      !! deduplicating identical states. Among 10**5 distinct 32-bit digests
      !! the chance that some pair collides is about 69%; at 64 bits it is
      !! 3e-10.
      class(particle_soa_t), intent(in) :: this
         !! Container to fingerprint.
      integer(int64) :: digest

      type(array_hash64_t) :: hasher
      integer(default_int) :: n

      n = this%extent%used
      call soa_hash_begin(hasher, PARTICLE_SOA_SCHEMA, n)
      call soa_hash_field(hasher, this%id, n)
      call soa_hash_field(hasher, this%x, n)
      call soa_hash_field(hasher, this%y, n)
      call soa_hash_field(hasher, this%z, n)
      call soa_hash_field(hasher, this%mass, n)
      call soa_hash_field(hasher, this%active, n)
      digest = hasher%digest()
   end function particle_soa_state_hash64

end module pic_soa_particle
