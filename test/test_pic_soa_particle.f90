! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
module test_pic_soa_particle
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64, dp
   use pic_error, only: error_t, ERROR_IO, ERROR_VALIDATION
   use pic_serialize, only: serialize_open_write, serialize_open_read, serialize_close, &
                            write_array, write_header
   use pic_soa, only: SOA_SCHEMA_PREFIX, SOA_MIN_CAPACITY, soa_stream_bytes
   use pic_soa_particle, only: particle_soa_t, PARTICLE_SOA_SCHEMA, &
                               PARTICLE_SOA_N_FIELDS, PARTICLE_SOA_FIELD_KINDS
   implicit none
   private
   public :: collect_pic_soa_particle_tests

contains

   subroutine collect_pic_soa_particle_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("schema_string", test_schema_string), &
                  new_unittest("allocate_and_query", test_allocate_and_query), &
                  new_unittest("allocate_twice", test_allocate_twice), &
                  new_unittest("allocate_zero", test_allocate_zero), &
                  new_unittest("resize_preserves_data", test_resize_preserves_data), &
                  new_unittest("resize_down_then_up", test_resize_down_then_up), &
                  new_unittest("resize_to_same_size", test_resize_to_same_size), &
                  new_unittest("resize_to_zero", test_resize_to_zero), &
                  new_unittest("clear_keeps_capacity", test_clear_keeps_capacity), &
                  new_unittest("deallocate_releases", test_deallocate_releases), &
                  new_unittest("negative_size_rejected", test_negative_size_rejected), &
                  new_unittest("fields_are_contiguous", test_fields_are_contiguous), &
                  new_unittest("round_trip", test_round_trip), &
                  new_unittest("round_trip_sizes", test_round_trip_sizes), &
                  new_unittest("serialized_bytes", test_serialized_bytes), &
                  new_unittest("serialize_fresh_container", test_serialize_fresh_container), &
                  new_unittest("hash_is_stable", test_hash_is_stable), &
                  new_unittest("hash_tracks_every_field", test_hash_tracks_every_field), &
                  new_unittest("hash_tracks_size", test_hash_tracks_size), &
                  new_unittest("hash_survives_round_trip", test_hash_survives_round_trip), &
                  new_unittest("schema_mismatch_rejected", test_schema_mismatch_rejected), &
                  new_unittest("short_field_rejected", test_short_field_rejected), &
                  new_unittest("every_field_rolls_back", test_every_field_rolls_back), &
                  new_unittest("truncated_stream_rejected", test_truncated_stream_rejected) &
                  ]
   end subroutine collect_pic_soa_particle_tests

   ! ---------------------------------------------------------------- helpers

   subroutine remove_file(fname)
      character(len=*), intent(in) :: fname
      integer :: unit, ios
      open (newunit=unit, file=fname, status="old", iostat=ios)
      if (ios == 0) close (unit, status="delete")
   end subroutine remove_file

   function file_bytes(fname) result(nbytes)
      character(len=*), intent(in) :: fname
      integer(int64) :: nbytes
      integer :: unit
      open (newunit=unit, file=fname, form="unformatted", access="stream", &
            action="read", status="old")
      inquire (unit, size=nbytes)
      close (unit)
   end function file_bytes

   subroutine copy_prefix(src, dst, keep)
      character(len=*), intent(in) :: src, dst
      integer(int64), intent(in) :: keep
      integer :: unit
      character(len=1), allocatable :: buffer(:)

      allocate (buffer(keep))
      open (newunit=unit, file=src, form="unformatted", access="stream", &
            action="read", status="old")
      read (unit) buffer
      close (unit)
      open (newunit=unit, file=dst, form="unformatted", access="stream", &
            action="write", status="replace")
      write (unit) buffer
      close (unit)
   end subroutine copy_prefix

   subroutine fill(p, n, err)
      !! Give a container `n` elements with distinct values in every field.
      type(particle_soa_t), intent(inout) :: p
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout) :: err

      integer(default_int) :: i

      call p%allocate(n, err)
      if (err%has_error()) return
      do i = 1, n
         p%id(i) = int(i, int32)
         p%x(i) = real(i, dp)
         p%y(i) = real(i, dp) + 0.25_dp
         p%z(i) = real(i, dp) + 0.5_dp
         p%mass(i) = real(i, dp)*1.5_dp
         p%active(i) = (mod(i, 2_default_int) == 1)
      end do
   end subroutine fill

   subroutine write_container(fname, p, err)
      character(len=*), intent(in) :: fname
      type(particle_soa_t), intent(in) :: p
      type(error_t), intent(inout) :: err

      integer(default_int) :: unit

      call serialize_open_write(fname, unit, err)
      if (err%has_error()) return
      call p%serialize(unit, err)
      call serialize_close(unit, err)
   end subroutine write_container

   subroutine read_container(fname, p, err)
      character(len=*), intent(in) :: fname
      type(particle_soa_t), intent(inout) :: p
      type(error_t), intent(inout) :: err

      integer(default_int) :: unit

      call serialize_open_read(fname, unit, err)
      if (err%has_error()) return
      call p%deserialize(unit, err)
      if (err%has_error()) then
         call serialize_close(unit, err)
         return
      end if
      call serialize_close(unit, err)
   end subroutine read_container

   ! ------------------------------------------------------------------ shape

   subroutine test_schema_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, PARTICLE_SOA_SCHEMA == SOA_SCHEMA_PREFIX// &
                 "particle;id:i32,x:r64,y:r64,z:r64,mass:r64,active:bool", &
                 "the generated schema string is the documented one")
      if (allocated(error)) return
      call check(error, PARTICLE_SOA_N_FIELDS == 6, "six fields")
      if (allocated(error)) return
      call check(error, size(PARTICLE_SOA_FIELD_KINDS) == PARTICLE_SOA_N_FIELDS, &
                 "one kind code per field")
   end subroutine test_schema_string

   subroutine test_allocate_and_query(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call check(error, p%size() == 0 .and. p%capacity() == 0, "a fresh container is empty")
      if (allocated(error)) return

      call p%allocate(3_default_int, err)
      call check(error,.not. err%has_error(), "allocate must succeed")
      if (allocated(error)) return
      call check(error, p%size() == 3, "size is what was asked for")
      if (allocated(error)) return
      call check(error, p%capacity() == SOA_MIN_CAPACITY, "capacity jumps to the minimum")
      if (allocated(error)) return
      call check(error, all(p%id(1:3) == 0_int32), "int32 field starts zeroed")
      if (allocated(error)) return
      call check(error, all(p%x(1:3) == 0.0_dp), "real field starts zeroed")
      if (allocated(error)) return
      call check(error,.not. any(p%active(1:3)), "logical field starts false")
   end subroutine test_allocate_and_query

   subroutine test_allocate_twice(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 5_default_int, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call p%allocate(4_default_int, err)
      call check(error,.not. err%has_error(), "a second allocate must succeed")
      if (allocated(error)) return
      call check(error, p%size() == 4, "size follows the second call")
      if (allocated(error)) return
      call check(error, p%capacity() == SOA_MIN_CAPACITY, "capacity is reused, not shrunk")
      if (allocated(error)) return
      call check(error, all(p%id(1:4) == 0_int32), "the previous contents are discarded")
      if (allocated(error)) return
      call check(error, all(p%mass(1:4) == 0.0_dp), "every field is reset")

      call p%allocate(30_default_int, err)
      if (allocated(error)) return
      call check(error, p%size() == 30 .and. p%capacity() == 32, &
                 "a larger second allocate grows geometrically")
   end subroutine test_allocate_twice

   subroutine test_allocate_zero(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call p%allocate(0_default_int, err)
      call check(error,.not. err%has_error(), "allocating zero elements is legal")
      if (allocated(error)) return
      call check(error, p%size() == 0 .and. p%capacity() == 0, "nothing is reserved")
      if (allocated(error)) return
      call check(error, allocated(p%id) .and. size(p%id) == 0, "fields exist but are empty")
   end subroutine test_allocate_zero

   subroutine test_resize_preserves_data(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 4_default_int, err)
      call p%resize(20_default_int, err)
      call check(error,.not. err%has_error(), "growth must succeed")
      if (allocated(error)) return
      call check(error, p%size() == 20 .and. p%capacity() == 32, "geometric growth past capacity")
      if (allocated(error)) return
      call check(error, all(p%id(1:4) == [1_int32, 2_int32, 3_int32, 4_int32]), "ids survive")
      if (allocated(error)) return
      call check(error, p%x(4) == 4.0_dp .and. p%mass(3) == 4.5_dp, "reals survive")
      if (allocated(error)) return
      call check(error, p%active(1) .and. .not. p%active(2), "logicals survive")
      if (allocated(error)) return
      call check(error, all(p%id(5:20) == 0_int32), "new elements are zeroed")
      if (allocated(error)) return
      call check(error,.not. any(p%active(5:20)), "new logicals are false")
   end subroutine test_resize_preserves_data

   subroutine test_resize_down_then_up(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 6_default_int, err)
      call p%resize(2_default_int, err)
      call check(error, p%size() == 2, "size drops")
      if (allocated(error)) return
      call check(error, p%capacity() == SOA_MIN_CAPACITY, "capacity is not given back")
      if (allocated(error)) return
      call check(error, p%id(1) == 1_int32 .and. p%id(2) == 2_int32, "survivors keep their values")
      if (allocated(error)) return

      call p%resize(5_default_int, err)
      call check(error, p%size() == 5, "size grows again inside capacity")
      if (allocated(error)) return
      call check(error, p%id(1) == 1_int32 .and. p%id(2) == 2_int32, "survivors still intact")
      if (allocated(error)) return
      ! The elements between 3 and 5 were live before the shrink. They must
      ! come back zeroed, not with their stale values, or the state hash would
      ! depend on history rather than on state.
      call check(error, all(p%id(3:5) == 0_int32), "re-exposed elements are zeroed")
      if (allocated(error)) return
      call check(error, all(p%mass(3:5) == 0.0_dp), "re-exposed reals are zeroed")
      if (allocated(error)) return
      call check(error,.not. any(p%active(3:5)), "re-exposed logicals are false")
   end subroutine test_resize_down_then_up

   subroutine test_resize_to_same_size(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err
      integer(int32) :: before

      call fill(p, 5_default_int, err)
      before = p%state_hash()
      call p%resize(5_default_int, err)
      call check(error,.not. err%has_error(), "a no-op resize must succeed")
      if (allocated(error)) return
      call check(error, p%size() == 5, "size is unchanged")
      if (allocated(error)) return
      call check(error, p%state_hash() == before, "a no-op resize does not disturb the state")
   end subroutine test_resize_to_same_size

   subroutine test_resize_to_zero(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 5_default_int, err)
      call p%resize(0_default_int, err)
      call check(error,.not. err%has_error(), "resize to zero must succeed")
      if (allocated(error)) return
      call check(error, p%size() == 0, "no live elements left")
      if (allocated(error)) return
      call check(error, p%capacity() == SOA_MIN_CAPACITY, "capacity is retained")
   end subroutine test_resize_to_zero

   subroutine test_clear_keeps_capacity(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 5_default_int, err)
      call p%clear()
      call check(error, p%size() == 0, "clear empties the container")
      if (allocated(error)) return
      call check(error, p%capacity() == SOA_MIN_CAPACITY, "clear keeps the memory")
      if (allocated(error)) return
      call check(error, allocated(p%x), "the field arrays stay allocated")
   end subroutine test_clear_keeps_capacity

   subroutine test_deallocate_releases(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 5_default_int, err)
      call p%deallocate()
      call check(error, p%size() == 0 .and. p%capacity() == 0, "both counters are zeroed")
      if (allocated(error)) return
      call check(error,.not. allocated(p%id) .and. .not. allocated(p%active), &
                 "every field array is released")
      if (allocated(error)) return

      ! Deallocating twice must be safe.
      call p%deallocate()
      call check(error, p%capacity() == 0, "a second deallocate is a no-op")
   end subroutine test_deallocate_releases

   subroutine test_negative_size_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call p%resize(-1_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "a negative resize is ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, p%size() == 0, "the container is untouched")
      if (allocated(error)) return

      call err%clear()
      call p%allocate(-3_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "a negative allocate is ERROR_VALIDATION")
   end subroutine test_negative_size_rejected

   subroutine test_fields_are_contiguous(error)
      !! The whole point of struct-of-arrays: every field, and every live slice
      !! of it, is a contiguous run of memory that can go straight to BLAS.
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 7_default_int, err)
      call check(error, is_contiguous(p%x), "the whole field array is contiguous")
      if (allocated(error)) return
      call check(error, is_contiguous(p%x(1:p%size())), "the live slice is contiguous")
      if (allocated(error)) return
      call check(error, is_contiguous(p%id(1:p%size())), "so is an integer field")
      if (allocated(error)) return
      call check(error, is_contiguous(p%active(1:p%size())), "so is a logical field")
      if (allocated(error)) return
      call check(error, sum(p%x(1:p%size())) == 28.0_dp, "the slice holds the live data")
   end subroutine test_fields_are_contiguous

   ! ------------------------------------------------------------- round trip

   subroutine test_round_trip(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_rt.bin"
      type(particle_soa_t) :: src, dst
      type(error_t) :: err

      call fill(src, 5_default_int, err)
      call write_container(fname, src, err)
      call check(error,.not. err%has_error(), "serialize must succeed")
      if (allocated(error)) return

      call read_container(fname, dst, err)
      call check(error,.not. err%has_error(), "deserialize must succeed")
      if (allocated(error)) return

      call check(error, dst%size() == 5, "element count round-trips")
      if (allocated(error)) return
      call check(error, dst%capacity() == 5, "a deserialized container is exactly sized")
      if (allocated(error)) return
      call check(error, all(dst%id(1:5) == src%id(1:5)), "id round-trips")
      if (allocated(error)) return
      call check(error, all(dst%x(1:5) == src%x(1:5)), "x round-trips")
      if (allocated(error)) return
      call check(error, all(dst%y(1:5) == src%y(1:5)), "y round-trips")
      if (allocated(error)) return
      call check(error, all(dst%z(1:5) == src%z(1:5)), "z round-trips")
      if (allocated(error)) return
      call check(error, all(dst%mass(1:5) == src%mass(1:5)), "mass round-trips")
      if (allocated(error)) return
      call check(error, all(dst%active(1:5) .eqv. src%active(1:5)), "active round-trips")

      call remove_file(fname)
   end subroutine test_round_trip

   subroutine test_round_trip_sizes(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_sizes.bin"
      integer(default_int), parameter :: counts(4) = [0, 1, 2, 17]
      type(particle_soa_t) :: src, dst
      type(error_t) :: err
      integer :: k

      do k = 1, size(counts)
         call fill(src, counts(k), err)
         call write_container(fname, src, err)
         call read_container(fname, dst, err)
         call check(error,.not. err%has_error(), "round trip must succeed at every size")
         if (allocated(error)) return
         call check(error, dst%size() == counts(k), "size round-trips")
         if (allocated(error)) return
         call check(error, dst%state_hash() == src%state_hash(), &
                    "the digest survives a round trip at every size")
         if (allocated(error)) return
      end do

      call remove_file(fname)
   end subroutine test_round_trip_sizes

   subroutine test_serialized_bytes(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_bytes.bin"
      type(particle_soa_t) :: p
      type(error_t) :: err

      call fill(p, 9_default_int, err)
      call write_container(fname, p, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call check(error, p%serialized_bytes() == file_bytes(fname), &
                 "serialized_bytes must predict the real file size")
      if (allocated(error)) return
      call check(error, p%serialized_bytes() == &
                 soa_stream_bytes(PARTICLE_SOA_SCHEMA, 9_default_int, PARTICLE_SOA_FIELD_KINDS), &
                 "and it must agree with the substrate")
      if (allocated(error)) return

      ! Pinned on purpose. Nothing in the stream may depend on default_int, so
      ! this number must be the same in a default and in a PIC_DEFAULT_INT8
      ! build: 16 byte header, 12 + 64 schema record, 12 + 8 count record,
      ! 12 + 36 for id, four times 12 + 72 for the reals, 12 + 36 for active.
      call check(error, len(PARTICLE_SOA_SCHEMA) == 64, "the schema string length is pinned")
      if (allocated(error)) return
      call check(error, p%serialized_bytes() == 544_int64, "the stream length is pinned")

      call remove_file(fname)
   end subroutine test_serialized_bytes

   subroutine test_serialize_fresh_container(error)
      !! A container that was never allocated has unallocated field arrays and
      !! must still serialize, and round-trip, as an empty one.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_fresh.bin"
      type(particle_soa_t) :: src, dst
      type(error_t) :: err

      call check(error,.not. allocated(src%x), "the fixture really is unallocated")
      if (allocated(error)) return

      call write_container(fname, src, err)
      call check(error,.not. err%has_error(), "a fresh container must serialize")
      if (allocated(error)) return

      call read_container(fname, dst, err)
      call check(error,.not. err%has_error(), "and deserialize")
      if (allocated(error)) return
      call check(error, dst%size() == 0, "as an empty container")
      if (allocated(error)) return
      call check(error, dst%state_hash() == src%state_hash(), "with the same digest")

      call remove_file(fname)
   end subroutine test_serialize_fresh_container

   ! ------------------------------------------------------------- state hash

   subroutine test_hash_is_stable(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: a, b
      type(error_t) :: err

      call fill(a, 6_default_int, err)
      call fill(b, 6_default_int, err)
      call check(error, a%state_hash() == b%state_hash(), &
                 "identical state gives identical digests")
      if (allocated(error)) return

      ! Reaching the same state by a different route must not change anything.
      call b%resize(40_default_int, err)
      call b%resize(6_default_int, err)
      call check(error, a%state_hash() == b%state_hash(), &
                 "the digest depends on state, not on capacity or history")
      if (allocated(error)) return
      call check(error, a%capacity() /= b%capacity(), "the two really do differ in capacity")
   end subroutine test_hash_is_stable

   subroutine test_hash_tracks_every_field(error)
      !! Every single field must take part in the fold. A fold that silently
      !! skips one is invisible any other way.
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p
      type(error_t) :: err
      integer(int32) :: base

      call fill(p, 4_default_int, err)
      base = p%state_hash()

      p%id(2) = 99_int32
      call check(error, p%state_hash() /= base, "id takes part in the digest")
      if (allocated(error)) return
      p%id(2) = 2_int32
      call check(error, p%state_hash() == base, "and restoring it restores the digest")
      if (allocated(error)) return

      p%x(3) = -1.0_dp
      call check(error, p%state_hash() /= base, "x takes part in the digest")
      if (allocated(error)) return
      p%x(3) = 3.0_dp
      call check(error, p%state_hash() == base, "and restoring it restores the digest")
      if (allocated(error)) return

      p%y(1) = 12.5_dp
      call check(error, p%state_hash() /= base, "y takes part in the digest")
      if (allocated(error)) return
      p%y(1) = 1.25_dp
      call check(error, p%state_hash() == base, "and restoring it restores the digest")
      if (allocated(error)) return

      p%z(4) = 7.75_dp
      call check(error, p%state_hash() /= base, "z takes part in the digest")
      if (allocated(error)) return
      p%z(4) = 4.5_dp
      call check(error, p%state_hash() == base, "and restoring it restores the digest")
      if (allocated(error)) return

      p%mass(2) = 100.0_dp
      call check(error, p%state_hash() /= base, "mass takes part in the digest")
      if (allocated(error)) return
      p%mass(2) = 3.0_dp
      call check(error, p%state_hash() == base, "and restoring it restores the digest")
      if (allocated(error)) return

      p%active(2) = .not. p%active(2)
      call check(error, p%state_hash() /= base, "active takes part in the digest")
      if (allocated(error)) return
      p%active(2) = .not. p%active(2)
      call check(error, p%state_hash() == base, "and restoring it restores the digest")
   end subroutine test_hash_tracks_every_field

   subroutine test_hash_tracks_size(error)
      type(error_type), allocatable, intent(out) :: error
      type(particle_soa_t) :: p, q
      type(error_t) :: err
      integer(int32) :: empty_digest

      call p%allocate(0_default_int, err)
      empty_digest = p%state_hash()

      call q%allocate(1_default_int, err)
      call check(error, q%state_hash() /= empty_digest, &
                 "one zeroed element differs from no elements")
      if (allocated(error)) return

      call p%resize(3_default_int, err)
      call check(error, p%state_hash() /= empty_digest, "size is part of the digest")
      if (allocated(error)) return
      call p%resize(0_default_int, err)
      call check(error, p%state_hash() == empty_digest, "and shrinking back restores it")
   end subroutine test_hash_tracks_size

   subroutine test_hash_survives_round_trip(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_hash.bin"
      type(particle_soa_t) :: src, dst
      type(error_t) :: err

      call fill(src, 11_default_int, err)
      src%x(4) = -0.0_dp
      src%mass(7) = 1.0e-300_dp
      call write_container(fname, src, err)
      call read_container(fname, dst, err)
      call check(error,.not. err%has_error(), "round trip must succeed")
      if (allocated(error)) return
      call check(error, dst%state_hash() == src%state_hash(), &
                 "a checkpoint validates against the container it came from")

      call remove_file(fname)
   end subroutine test_hash_survives_round_trip

   ! ------------------------------------------------------------ error paths

   subroutine test_schema_mismatch_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_wrong.bin"
      type(particle_soa_t) :: p
      integer(default_int) :: unit
      type(error_t) :: err

      ! A stream with a plausible but different field list: one field fewer.
      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, SOA_SCHEMA_PREFIX//"particle;id:i32,x:r64,y:r64,z:r64,mass:r64", err)
      call write_array(unit, [2_int64], err)
      call write_array(unit, [1_int32, 2_int32], err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call read_container(fname, p, err)
      call check(error, err%is(ERROR_VALIDATION), "a foreign layout must be rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "layout mismatch") > 0, &
                 "the message explains the rejection")
      if (allocated(error)) return
      call check(error, p%size() == 0 .and. .not. allocated(p%id), &
                 "nothing is left half decoded")

      call err%clear()
      call remove_file(fname)
   end subroutine test_schema_mismatch_rejected

   subroutine test_short_field_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_short.bin"
      type(particle_soa_t) :: p
      integer(default_int) :: unit
      type(error_t) :: err

      ! Correct schema, correct count, but the second field is one short.
      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, PARTICLE_SOA_SCHEMA, err)
      call write_array(unit, [3_int64], err)
      call write_array(unit, [1_int32, 2_int32, 3_int32], err)
      call write_array(unit, [1.0_dp, 2.0_dp], err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call read_container(fname, p, err)
      call check(error, err%is(ERROR_VALIDATION), "a short field must be rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "'x'") > 0, "the message names the field")
      if (allocated(error)) return
      call check(error,.not. allocated(p%id), "the partial decode is rolled back")

      call err%clear()
      call remove_file(fname)
   end subroutine test_short_field_rejected

   subroutine test_every_field_rolls_back(error)
      !! Walk the short record through every field position in turn. A rollback
      !! that only works for the first field would otherwise pass unnoticed.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_each.bin"
      type(particle_soa_t) :: p
      integer(default_int) :: unit
      integer :: bad, j
      type(error_t) :: err

      do bad = 1, 6
         call serialize_open_write(fname, unit, err)
         call write_header(unit, err)
         call write_array(unit, PARTICLE_SOA_SCHEMA, err)
         call write_array(unit, [2_int64], err)
         do j = 1, 6
            ! Fields 1 and 6 travel as int32 records, fields 2 to 5 as real(dp).
            if (j == 1 .or. j == 6) then
               if (j == bad) then
                  call write_array(unit, [1_int32], err)
               else
                  call write_array(unit, [1_int32, 0_int32], err)
               end if
            else
               if (j == bad) then
                  call write_array(unit, [1.0_dp], err)
               else
                  call write_array(unit, [1.0_dp, 2.0_dp], err)
               end if
            end if
         end do
         call serialize_close(unit, err)
         call check(error,.not. err%has_error(), "setup failed")
         if (allocated(error)) return

         call read_container(fname, p, err)
         call check(error, err%is(ERROR_VALIDATION), "a short field must be rejected")
         if (allocated(error)) return
         call check(error,.not. allocated(p%id) .and. .not. allocated(p%active), &
                    "the partial decode is rolled back whichever field is short")
         if (allocated(error)) return
         call check(error, p%size() == 0 .and. p%capacity() == 0, "and the counters are zeroed")
         if (allocated(error)) return
         call err%clear()
      end do

      call remove_file(fname)
   end subroutine test_every_field_rolls_back

   subroutine test_truncated_stream_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_particle_trunc_src.bin"
      character(len=*), parameter :: cut = "pic_soa_particle_trunc.bin"
      type(particle_soa_t) :: src, dst
      type(error_t) :: err
      integer(int64) :: total

      call fill(src, 6_default_int, err)
      call write_container(fname, src, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      total = file_bytes(fname)
      call copy_prefix(fname, cut, total - 16_int64)
      call read_container(cut, dst, err)
      call check(error, err%is(ERROR_IO), "a truncated stream must be ERROR_IO")
      if (allocated(error)) return
      call check(error, dst%size() == 0, "nothing is left half decoded")

      call err%clear()
      call remove_file(fname)
      call remove_file(cut)
   end subroutine test_truncated_stream_rejected

end module test_pic_soa_particle
