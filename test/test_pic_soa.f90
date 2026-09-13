! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
module test_pic_soa
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64, sp, dp
   use pic_error, only: error_t, ERROR_ALLOC, ERROR_IO, ERROR_VALIDATION
   use pic_array_hash, only: array_hash_t, ARRAY_HASH_OFFSET_BASIS
   use pic_serialize, only: serialize_open_write, serialize_open_read, serialize_close, &
                            write_array, write_header, &
                            PIC_HEADER_BYTES, PIC_RECORD_HEADER_BYTES
   use pic_soa, only: soa_extent_t, SOA_SCHEMA_PREFIX, SOA_MIN_CAPACITY, SOA_GROWTH_FACTOR, &
                      SOA_FIELD_INT32, SOA_FIELD_INT64, SOA_FIELD_REAL_SP, &
                      SOA_FIELD_REAL_DP, SOA_FIELD_LOGICAL, &
                      soa_field_code, soa_grow_capacity, soa_plan_resize, &
                      soa_resize_field, soa_write_field, soa_read_field, &
                      soa_hash_field, soa_hash_begin, &
                      soa_write_prologue, soa_read_prologue, soa_stream_bytes, &
                      soa_check_alloc, soa_check_resize_args, &
                      soa_check_field_count, soa_check_live_count
   implicit none
   private
   public :: collect_pic_soa_tests

   character(len=*), parameter :: SCHEMA = SOA_SCHEMA_PREFIX//"probe;a:i32,b:i64,c:r32,d:r64,e:bool"

contains

   subroutine collect_pic_soa_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("check_alloc", test_check_alloc), &
                  new_unittest("check_resize_args", test_check_resize_args), &
                  new_unittest("check_field_count", test_check_field_count), &
                  new_unittest("check_live_count", test_check_live_count), &
                  new_unittest("grow_capacity", test_grow_capacity), &
                  new_unittest("plan_resize", test_plan_resize), &
                  new_unittest("resize_int32", test_resize_int32), &
                  new_unittest("resize_int64", test_resize_int64), &
                  new_unittest("resize_real_sp", test_resize_real_sp), &
                  new_unittest("resize_real_dp", test_resize_real_dp), &
                  new_unittest("resize_logical", test_resize_logical), &
                  new_unittest("resize_rejects_bad_args", test_resize_rejects_bad_args), &
                  new_unittest("field_codes", test_field_codes), &
                  new_unittest("stream_bytes", test_stream_bytes), &
                  new_unittest("round_trip_fields", test_round_trip_fields), &
                  new_unittest("round_trip_empty_fields", test_round_trip_empty_fields), &
                  new_unittest("write_unallocated_fields", test_write_unallocated_fields), &
                  new_unittest("write_rejects_overlong_count", test_write_rejects_overlong_count), &
                  new_unittest("hash_fields", test_hash_fields), &
                  new_unittest("hash_begin_is_build_stable", test_hash_begin_stable), &
                  new_unittest("prologue_rejects_negative", test_prologue_rejects_negative), &
                  new_unittest("schema_mismatch", test_schema_mismatch), &
                  new_unittest("schema_length_mismatch", test_schema_length_mismatch), &
                  new_unittest("bad_count_record", test_bad_count_record), &
                  new_unittest("field_count_mismatch", test_field_count_mismatch), &
                  new_unittest("bad_logical_code", test_bad_logical_code), &
                  new_unittest("truncated_stream", test_truncated_stream) &
                  ]
   end subroutine collect_pic_soa_tests

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

   ! ------------------------------------------------------------- validators

   subroutine test_check_alloc(error)
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call soa_check_alloc(0_default_int, "a happy allocation", err)
      call check(error,.not. err%has_error(), "stat 0 must not raise")
      if (allocated(error)) return

      call soa_check_alloc(17_default_int, "a doomed allocation", err)
      call check(error, err%is(ERROR_ALLOC), "non-zero stat must be ERROR_ALLOC")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "17") > 0, "message should carry the stat")
   end subroutine test_check_alloc

   subroutine test_check_resize_args(error)
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call soa_check_resize_args(8_default_int, 4_default_int, 6_default_int, err)
      call check(error,.not. err%has_error(), "consistent arguments must pass")
      if (allocated(error)) return

      call err%clear()
      call soa_check_resize_args(-1_default_int, 0_default_int, 0_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "negative capacity rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_check_resize_args(4_default_int, -1_default_int, 0_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "negative keep rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_check_resize_args(4_default_int, 0_default_int, -1_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "negative fill rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_check_resize_args(4_default_int, 5_default_int, 0_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "keep past capacity rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_check_resize_args(4_default_int, 0_default_int, 5_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "fill past capacity rejected")
   end subroutine test_check_resize_args

   subroutine test_check_field_count(error)
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call soa_check_field_count("x", 3_default_int, 3_default_int, err)
      call check(error,.not. err%has_error(), "matching counts must pass")
      if (allocated(error)) return

      call soa_check_field_count("mass", 2_default_int, 3_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "count mismatch is ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "mass") > 0, "message names the field")
   end subroutine test_check_field_count

   subroutine test_check_live_count(error)
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err

      call soa_check_live_count(3_default_int, 8_default_int, err)
      call check(error,.not. err%has_error(), "3 live of 8 allocated is fine")
      if (allocated(error)) return

      call soa_check_live_count(0_default_int, -1_default_int, err)
      call check(error,.not. err%has_error(), "0 live of an unallocated field is fine")
      if (allocated(error)) return

      call err%clear()
      call soa_check_live_count(-1_default_int, 8_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "negative live count rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_check_live_count(1_default_int, -1_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "live elements of an unallocated field rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_check_live_count(9_default_int, 8_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "more live than allocated rejected")
   end subroutine test_check_live_count

   ! ----------------------------------------------------------------- growth

   subroutine test_grow_capacity(error)
      type(error_type), allocatable, intent(out) :: error
      integer(default_int) :: cap
      integer(default_int), parameter :: HUGE_INT = huge(0_default_int)

      cap = soa_grow_capacity(16_default_int, 4_default_int)
      call check(error, cap == 16, "capacity never shrinks")
      if (allocated(error)) return

      cap = soa_grow_capacity(0_default_int, 1_default_int)
      call check(error, cap == SOA_MIN_CAPACITY, "first growth jumps to the minimum capacity")
      if (allocated(error)) return

      cap = soa_grow_capacity(0_default_int, SOA_MIN_CAPACITY + 1)
      call check(error, cap == SOA_MIN_CAPACITY*SOA_GROWTH_FACTOR, "growth is geometric")
      if (allocated(error)) return

      cap = soa_grow_capacity(8_default_int, 100_default_int)
      call check(error, cap == 128, "doubling continues until the request fits")
      if (allocated(error)) return

      ! Near the top of the integer range doubling would overflow, so the exact
      ! request is used instead. Pure arithmetic, no allocation involved.
      cap = soa_grow_capacity(HUGE_INT - 1, HUGE_INT)
      call check(error, cap == HUGE_INT, "growth clamps to the request near the integer limit")
   end subroutine test_grow_capacity

   subroutine test_plan_resize(error)
      type(error_type), allocatable, intent(out) :: error
      type(soa_extent_t) :: extent
      integer(default_int) :: cap, keep
      type(error_t) :: err

      extent%used = 5
      extent%capacity = 8

      call soa_plan_resize(extent, 3_default_int, cap, keep, err)
      call check(error,.not. err%has_error(), "shrink must not fail")
      if (allocated(error)) return
      call check(error, cap == 8 .and. keep == 3, "shrink keeps capacity and 3 elements")
      if (allocated(error)) return

      call soa_plan_resize(extent, 8_default_int, cap, keep, err)
      call check(error, cap == 8 .and. keep == 5, "growth inside capacity does not reallocate")
      if (allocated(error)) return

      call soa_plan_resize(extent, 20_default_int, cap, keep, err)
      call check(error, cap == 32 .and. keep == 5, "growth past capacity doubles")
      if (allocated(error)) return

      call soa_plan_resize(extent, -1_default_int, cap, keep, err)
      call check(error, err%is(ERROR_VALIDATION), "negative size is ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, keep == 0, "keep is zeroed on failure")
   end subroutine test_plan_resize

   ! ---------------------------------------------------------- field resizes

   subroutine test_resize_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: a(:)
      type(error_t) :: err

      call soa_resize_field(a, 4_default_int, 0_default_int, 4_default_int, err)
      call check(error,.not. err%has_error(), "first resize must succeed")
      if (allocated(error)) return
      call check(error, size(a) == 4, "capacity is 4")
      if (allocated(error)) return
      call check(error, all(a(1:4) == 0_int32), "new elements are zeroed")
      if (allocated(error)) return

      a(1:4) = [1_int32, 2_int32, 3_int32, 4_int32]

      ! Same capacity: no reallocation, only the newly exposed tail is zeroed.
      call soa_resize_field(a, 4_default_int, 2_default_int, 4_default_int, err)
      call check(error, a(1) == 1_int32 .and. a(2) == 2_int32, "prefix preserved in place")
      if (allocated(error)) return
      call check(error, a(3) == 0_int32 .and. a(4) == 0_int32, "tail re-zeroed in place")
      if (allocated(error)) return

      a(1:4) = [5_int32, 6_int32, 7_int32, 8_int32]
      call soa_resize_field(a, 9_default_int, 4_default_int, 6_default_int, err)
      call check(error, size(a) == 9, "reallocated to the new capacity")
      if (allocated(error)) return
      call check(error, all(a(1:4) == [5_int32, 6_int32, 7_int32, 8_int32]), "prefix copied over")
      if (allocated(error)) return
      call check(error, a(5) == 0_int32 .and. a(6) == 0_int32, "exposed elements zeroed")
   end subroutine test_resize_int32

   subroutine test_resize_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: a(:)
      type(error_t) :: err

      call soa_resize_field(a, 3_default_int, 0_default_int, 3_default_int, err)
      call check(error, size(a) == 3 .and. all(a == 0_int64), "allocated and zeroed")
      if (allocated(error)) return

      a = [10_int64, 20_int64, 30_int64]
      call soa_resize_field(a, 6_default_int, 3_default_int, 5_default_int, err)
      call check(error, size(a) == 6, "grown")
      if (allocated(error)) return
      call check(error, all(a(1:3) == [10_int64, 20_int64, 30_int64]), "prefix preserved")
      if (allocated(error)) return
      call check(error, a(4) == 0_int64 .and. a(5) == 0_int64, "exposed elements zeroed")
      if (allocated(error)) return

      call soa_resize_field(a, 6_default_int, 2_default_int, 4_default_int, err)
      call check(error, a(3) == 0_int64, "in-place refill zeroes the reused element")
   end subroutine test_resize_int64

   subroutine test_resize_real_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: a(:)
      type(error_t) :: err

      call soa_resize_field(a, 2_default_int, 0_default_int, 2_default_int, err)
      call check(error, size(a) == 2 .and. all(a == 0.0_sp), "allocated and zeroed")
      if (allocated(error)) return

      a = [1.5_sp, 2.5_sp]
      call soa_resize_field(a, 5_default_int, 2_default_int, 4_default_int, err)
      call check(error, size(a) == 5, "grown")
      if (allocated(error)) return
      call check(error, a(1) == 1.5_sp .and. a(2) == 2.5_sp, "prefix preserved")
      if (allocated(error)) return
      call check(error, a(3) == 0.0_sp .and. a(4) == 0.0_sp, "exposed elements zeroed")
      if (allocated(error)) return

      call soa_resize_field(a, 5_default_int, 1_default_int, 3_default_int, err)
      call check(error, a(2) == 0.0_sp, "in-place refill zeroes the reused element")
   end subroutine test_resize_real_sp

   subroutine test_resize_real_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: a(:)
      type(error_t) :: err

      call soa_resize_field(a, 2_default_int, 0_default_int, 2_default_int, err)
      call check(error, size(a) == 2 .and. all(a == 0.0_dp), "allocated and zeroed")
      if (allocated(error)) return

      a = [1.25_dp, 2.25_dp]
      call soa_resize_field(a, 5_default_int, 2_default_int, 4_default_int, err)
      call check(error, size(a) == 5, "grown")
      if (allocated(error)) return
      call check(error, a(1) == 1.25_dp .and. a(2) == 2.25_dp, "prefix preserved")
      if (allocated(error)) return
      call check(error, a(3) == 0.0_dp .and. a(4) == 0.0_dp, "exposed elements zeroed")
      if (allocated(error)) return

      call soa_resize_field(a, 5_default_int, 1_default_int, 3_default_int, err)
      call check(error, a(2) == 0.0_dp, "in-place refill zeroes the reused element")
   end subroutine test_resize_real_dp

   subroutine test_resize_logical(error)
      type(error_type), allocatable, intent(out) :: error
      logical, allocatable :: a(:)
      type(error_t) :: err

      call soa_resize_field(a, 2_default_int, 0_default_int, 2_default_int, err)
      call check(error, size(a) == 2 .and. .not. any(a), "allocated and set false")
      if (allocated(error)) return

      a = [.true., .true.]
      call soa_resize_field(a, 5_default_int, 2_default_int, 4_default_int, err)
      call check(error, size(a) == 5, "grown")
      if (allocated(error)) return
      call check(error, a(1) .and. a(2), "prefix preserved")
      if (allocated(error)) return
      call check(error,.not. a(3) .and. .not. a(4), "exposed elements set false")
      if (allocated(error)) return

      call soa_resize_field(a, 5_default_int, 1_default_int, 3_default_int, err)
      call check(error,.not. a(2), "in-place refill clears the reused element")
   end subroutine test_resize_logical

   subroutine test_resize_rejects_bad_args(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical, allocatable :: e(:)
      type(error_t) :: err

      call soa_resize_field(a, -1_default_int, 0_default_int, 0_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "int32 resize rejects a negative capacity")
      if (allocated(error)) return
      call check(error,.not. allocated(a), "nothing allocated on failure")
      if (allocated(error)) return

      call err%clear()
      call soa_resize_field(b, 2_default_int, 3_default_int, 0_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "int64 resize rejects keep past capacity")
      if (allocated(error)) return

      call err%clear()
      call soa_resize_field(c, 2_default_int, 0_default_int, 3_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "real(sp) resize rejects fill past capacity")
      if (allocated(error)) return

      call err%clear()
      call soa_resize_field(d, -2_default_int, 0_default_int, 0_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "real(dp) resize rejects a negative capacity")
      if (allocated(error)) return

      call err%clear()
      call soa_resize_field(e, -2_default_int, 0_default_int, 0_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "logical resize rejects a negative capacity")
   end subroutine test_resize_rejects_bad_args

   ! ---------------------------------------------------------- stream layout

   subroutine test_field_codes(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, soa_field_code(SOA_FIELD_INT32) == "i32", "int32 code")
      if (allocated(error)) return
      call check(error, soa_field_code(SOA_FIELD_INT64) == "i64", "int64 code")
      if (allocated(error)) return
      call check(error, soa_field_code(SOA_FIELD_REAL_SP) == "r32", "real(sp) code")
      if (allocated(error)) return
      call check(error, soa_field_code(SOA_FIELD_REAL_DP) == "r64", "real(dp) code")
      if (allocated(error)) return
      call check(error, soa_field_code(SOA_FIELD_LOGICAL) == "bool", "logical code")
      if (allocated(error)) return
      call check(error, soa_field_code(99_int32) == "?", "unknown kinds are reported")
   end subroutine test_field_codes

   subroutine test_stream_bytes(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: kinds(5) = [SOA_FIELD_INT32, SOA_FIELD_INT64, &
                                               SOA_FIELD_REAL_SP, SOA_FIELD_REAL_DP, &
                                               SOA_FIELD_LOGICAL]
      integer(int64) :: expected, got

      ! header + schema record + count record + five field records of 3
      ! elements each, sized 4, 8, 4, 8 and 4 bytes per element.
      expected = PIC_HEADER_BYTES &
                 + PIC_RECORD_HEADER_BYTES + int(len(SCHEMA), int64) &
                 + PIC_RECORD_HEADER_BYTES + 8_int64 &
                 + 5_int64*PIC_RECORD_HEADER_BYTES + 3_int64*(4 + 8 + 4 + 8 + 4)
      got = soa_stream_bytes(SCHEMA, 3_default_int, kinds)
      call check(error, got == expected, "stream size matches the documented layout")
      if (allocated(error)) return

      got = soa_stream_bytes(SCHEMA, -1_default_int, kinds)
      call check(error, got == -1_int64, "a negative element count is rejected")
      if (allocated(error)) return

      got = soa_stream_bytes(SCHEMA, 3_default_int, [99_int32])
      call check(error, got == -1_int64, "an unknown field kind is rejected")
   end subroutine test_stream_bytes

   ! ------------------------------------------------------------- round trip

   subroutine write_probe_stream(fname, n, err)
      !! Write a five field probe stream holding `n` elements.
      character(len=*), intent(in) :: fname
      integer(default_int), intent(in) :: n
      type(error_t), intent(inout) :: err

      integer(default_int) :: unit, i
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical, allocatable :: e(:)

      call soa_resize_field(a, n, 0_default_int, n, err)
      call soa_resize_field(b, n, 0_default_int, n, err)
      call soa_resize_field(c, n, 0_default_int, n, err)
      call soa_resize_field(d, n, 0_default_int, n, err)
      call soa_resize_field(e, n, 0_default_int, n, err)
      if (err%has_error()) return

      do i = 1, n
         a(i) = int(i, int32)
         b(i) = int(100*i, int64)
         c(i) = real(i, sp)*0.5_sp
         d(i) = real(i, dp)*0.25_dp
         e(i) = (mod(i, 2_default_int) == 0)
      end do

      call serialize_open_write(fname, unit, err)
      if (err%has_error()) return
      call soa_write_prologue(unit, SCHEMA, n, err)
      call soa_write_field(unit, a, n, err)
      call soa_write_field(unit, b, n, err)
      call soa_write_field(unit, c, n, err)
      call soa_write_field(unit, d, n, err)
      call soa_write_field(unit, e, n, err)
      call serialize_close(unit, err)
   end subroutine write_probe_stream

   subroutine test_round_trip_fields(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_round.bin"
      integer(int32), parameter :: kinds(5) = [SOA_FIELD_INT32, SOA_FIELD_INT64, &
                                               SOA_FIELD_REAL_SP, SOA_FIELD_REAL_DP, &
                                               SOA_FIELD_LOGICAL]
      integer(default_int) :: unit, n
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical, allocatable :: e(:)
      logical :: swapped
      type(error_t) :: err

      call write_probe_stream(fname, 3_default_int, err)
      call check(error,.not. err%has_error(), "writing the probe stream must succeed")
      if (allocated(error)) return

      call check(error, file_bytes(fname) == soa_stream_bytes(SCHEMA, 3_default_int, kinds), &
                 "soa_stream_bytes must predict the real file size")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call check(error,.not. err%has_error(), "prologue must read back")
      if (allocated(error)) return
      call check(error, n == 3, "element count round-trips")
      if (allocated(error)) return

      call soa_read_field(unit, "a", a, n, swapped, err)
      call soa_read_field(unit, "b", b, n, swapped, err)
      call soa_read_field(unit, "c", c, n, swapped, err)
      call soa_read_field(unit, "d", d, n, swapped, err)
      call soa_read_field(unit, "e", e, n, swapped, err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "every field must read back")
      if (allocated(error)) return

      call check(error, all(a == [1_int32, 2_int32, 3_int32]), "int32 field round-trips")
      if (allocated(error)) return
      call check(error, all(b == [100_int64, 200_int64, 300_int64]), "int64 field round-trips")
      if (allocated(error)) return
      call check(error, all(c == [0.5_sp, 1.0_sp, 1.5_sp]), "real(sp) field round-trips")
      if (allocated(error)) return
      call check(error, all(d == [0.25_dp, 0.5_dp, 0.75_dp]), "real(dp) field round-trips")
      if (allocated(error)) return
      call check(error, (.not. e(1)) .and. e(2) .and. (.not. e(3)), "logical field round-trips")

      call remove_file(fname)
   end subroutine test_round_trip_fields

   subroutine test_round_trip_empty_fields(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_empty.bin"
      integer(default_int) :: unit, n
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical, allocatable :: e(:)
      logical :: swapped
      type(error_t) :: err

      call write_probe_stream(fname, 0_default_int, err)
      call check(error,.not. err%has_error(), "writing an empty stream must succeed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call soa_read_field(unit, "a", a, n, swapped, err)
      call soa_read_field(unit, "b", b, n, swapped, err)
      call soa_read_field(unit, "c", c, n, swapped, err)
      call soa_read_field(unit, "d", d, n, swapped, err)
      call soa_read_field(unit, "e", e, n, swapped, err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "an empty stream must read back cleanly")
      if (allocated(error)) return
      call check(error, n == 0, "element count is zero")
      if (allocated(error)) return
      call check(error, size(a) == 0 .and. size(b) == 0 .and. size(c) == 0 &
                 .and. size(d) == 0 .and. size(e) == 0, "every field comes back empty")

      call remove_file(fname)
   end subroutine test_round_trip_empty_fields

   subroutine test_write_unallocated_fields(error)
      !! A container that was never allocated has unallocated fields. Writing
      !! it must still produce a well formed, readable stream.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_unalloc.bin"
      integer(default_int) :: unit, n
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical, allocatable :: e(:)
      logical :: swapped
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call soa_write_prologue(unit, SCHEMA, 0_default_int, err)
      call soa_write_field(unit, a, 0_default_int, err)
      call soa_write_field(unit, b, 0_default_int, err)
      call soa_write_field(unit, c, 0_default_int, err)
      call soa_write_field(unit, d, 0_default_int, err)
      call soa_write_field(unit, e, 0_default_int, err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "unallocated fields must write as empty records")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call soa_read_field(unit, "a", a, n, swapped, err)
      call soa_read_field(unit, "e", e, n, swapped, err)
      call serialize_close(unit, err)
      call check(error, n == 0, "the stream declares zero elements")

      call remove_file(fname)
   end subroutine test_write_unallocated_fields

   subroutine test_write_rejects_overlong_count(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_overlong.bin"
      integer(default_int) :: unit
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical, allocatable :: e(:)
      type(error_t) :: err

      call soa_resize_field(a, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(b, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(c, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(d, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(e, 2_default_int, 0_default_int, 2_default_int, err)

      call serialize_open_write(fname, unit, err)
      call soa_write_prologue(unit, SCHEMA, 2_default_int, err)

      call soa_write_field(unit, a, 3_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "int32 write rejects an overlong count")
      if (allocated(error)) return

      call err%clear()
      call soa_write_field(unit, b, 3_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "int64 write rejects an overlong count")
      if (allocated(error)) return

      call err%clear()
      call soa_write_field(unit, c, 3_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "real(sp) write rejects an overlong count")
      if (allocated(error)) return

      call err%clear()
      call soa_write_field(unit, d, -1_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "real(dp) write rejects a negative count")
      if (allocated(error)) return

      call err%clear()
      call soa_write_field(unit, e, 3_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "logical write rejects an overlong count")
      if (allocated(error)) return

      call err%clear()
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_write_rejects_overlong_count

   ! ------------------------------------------------------------- state hash

   subroutine test_hash_fields(error)
      type(error_type), allocatable, intent(out) :: error
      type(array_hash_t) :: hasher
      integer(int32) :: empty_digest, full_digest, changed_digest
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical, allocatable :: e(:)
      type(error_t) :: err

      ! Unallocated fields and a zero live count both contribute nothing.
      call soa_hash_begin(hasher, SCHEMA, 0_default_int)
      call soa_hash_field(hasher, a, 0_default_int)
      call soa_hash_field(hasher, b, 0_default_int)
      call soa_hash_field(hasher, c, 0_default_int)
      call soa_hash_field(hasher, d, 0_default_int)
      call soa_hash_field(hasher, e, 0_default_int)
      empty_digest = hasher%digest()

      call soa_resize_field(a, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(b, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(c, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(d, 2_default_int, 0_default_int, 2_default_int, err)
      call soa_resize_field(e, 2_default_int, 0_default_int, 2_default_int, err)

      call soa_hash_begin(hasher, SCHEMA, 0_default_int)
      call soa_hash_field(hasher, a, 0_default_int)
      call soa_hash_field(hasher, b, 0_default_int)
      call soa_hash_field(hasher, c, 0_default_int)
      call soa_hash_field(hasher, d, 0_default_int)
      call soa_hash_field(hasher, e, 0_default_int)
      call check(error, hasher%digest() == empty_digest, &
                 "an allocated but empty field hashes like an unallocated one")
      if (allocated(error)) return

      a = [7_int32, 8_int32]
      b = [9_int64, 10_int64]
      c = [1.5_sp, 2.5_sp]
      d = [3.5_dp, 4.5_dp]
      e = [.true., .false.]

      call soa_hash_begin(hasher, SCHEMA, 2_default_int)
      call soa_hash_field(hasher, a, 2_default_int)
      call soa_hash_field(hasher, b, 2_default_int)
      call soa_hash_field(hasher, c, 2_default_int)
      call soa_hash_field(hasher, d, 2_default_int)
      call soa_hash_field(hasher, e, 2_default_int)
      full_digest = hasher%digest()
      call check(error, full_digest /= empty_digest, "real data changes the digest")
      if (allocated(error)) return

      e(2) = .true.
      call soa_hash_begin(hasher, SCHEMA, 2_default_int)
      call soa_hash_field(hasher, a, 2_default_int)
      call soa_hash_field(hasher, b, 2_default_int)
      call soa_hash_field(hasher, c, 2_default_int)
      call soa_hash_field(hasher, d, 2_default_int)
      call soa_hash_field(hasher, e, 2_default_int)
      changed_digest = hasher%digest()
      call check(error, changed_digest /= full_digest, "a single logical flip changes the digest")
   end subroutine test_hash_fields

   subroutine test_hash_begin_stable(error)
      !! The prologue folds the count as an explicit int64, so the digest does
      !! not depend on whether this is a default or a PIC_DEFAULT_INT8 build.
      type(error_type), allocatable, intent(out) :: error
      type(array_hash_t) :: reference, subject

      call reference%reset()
      call reference%update(SCHEMA)
      call reference%update(5_int64)

      call soa_hash_begin(subject, SCHEMA, 5_default_int)
      call check(error, subject%digest() == reference%digest(), &
                 "soa_hash_begin folds schema then an int64 count")
      if (allocated(error)) return

      call soa_hash_begin(subject, SCHEMA, 6_default_int)
      call check(error, subject%digest() /= reference%digest(), "the count is part of the digest")
      if (allocated(error)) return

      call soa_hash_begin(subject, SCHEMA//"x", 5_default_int)
      call check(error, subject%digest() /= reference%digest(), "the schema is part of the digest")
      if (allocated(error)) return

      call reference%reset()
      call check(error, reference%digest() == ARRAY_HASH_OFFSET_BASIS, "reset restores the basis")
   end subroutine test_hash_begin_stable

   ! ------------------------------------------------------------ error paths

   subroutine test_prologue_rejects_negative(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_neg.bin"
      integer(default_int) :: unit
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call soa_write_prologue(unit, SCHEMA, -1_default_int, err)
      call check(error, err%is(ERROR_VALIDATION), "a negative element count is refused")
      if (allocated(error)) return

      call err%clear()
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_prologue_rejects_negative

   subroutine test_schema_mismatch(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_schema.bin"
      character(len=*), parameter :: other = SOA_SCHEMA_PREFIX//"probe;a:i32,b:i64,c:r32,d:r64,f:bool"
      integer(default_int) :: unit, n
      logical :: swapped
      type(error_t) :: err

      call write_probe_stream(fname, 2_default_int, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call soa_read_prologue(unit, other, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "a different field list is rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "layout mismatch") > 0, &
                 "the message says what went wrong")
      if (allocated(error)) return
      call check(error, n == 0, "the element count is zeroed on failure")
      if (allocated(error)) return

      call err%clear()
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_schema_mismatch

   subroutine test_schema_length_mismatch(error)
      !! Fortran blank-pads the shorter operand of a character comparison, so
      !! the length is compared too.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_schlen.bin"
      integer(default_int) :: unit, n
      logical :: swapped
      type(error_t) :: err

      call write_probe_stream(fname, 1_default_int, err)
      call serialize_open_read(fname, unit, err)
      call soa_read_prologue(unit, SCHEMA//"  ", n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "a trailing-blank schema is still a mismatch")
      if (allocated(error)) return

      call err%clear()
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_schema_length_mismatch

   subroutine test_bad_count_record(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: two = "pic_soa_probe_count2.bin"
      character(len=*), parameter :: neg = "pic_soa_probe_countneg.bin"
      character(len=*), parameter :: big = "pic_soa_probe_countbig.bin"
      integer(int64), parameter :: TOO_MANY = 1099511627776_int64
      integer(default_int) :: unit, n
      logical :: swapped
      type(error_t) :: err

      call make_prologue_with_counts(two, [1_int64, 2_int64], err)
      call serialize_open_read(two, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "a multi-element count record is rejected")
      if (allocated(error)) return
      call err%clear()
      call serialize_close(unit, err)

      call make_prologue_with_counts(neg, [-5_int64], err)
      call serialize_open_read(neg, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "a negative element count is rejected")
      if (allocated(error)) return
      call err%clear()
      call serialize_close(unit, err)

      ! Only reachable where default_int is narrower than the stored count.
      if (int(huge(0_default_int), int64) < TOO_MANY) then
         call make_prologue_with_counts(big, [TOO_MANY], err)
         call serialize_open_read(big, unit, err)
         call soa_read_prologue(unit, SCHEMA, n, swapped, err)
         call check(error, err%is(ERROR_VALIDATION), "a count too large for this build is rejected")
         if (allocated(error)) return
         call err%clear()
         call serialize_close(unit, err)
      end if

      call remove_file(two)
      call remove_file(neg)
      call remove_file(big)
   end subroutine test_bad_count_record

   subroutine make_prologue_with_counts(fname, counts, err)
      !! Write a stream whose count record holds `counts` verbatim.
      character(len=*), intent(in) :: fname
      integer(int64), intent(in) :: counts(:)
      type(error_t), intent(inout) :: err

      integer(default_int) :: unit

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, SCHEMA, err)
      call write_array(unit, counts, err)
      call serialize_close(unit, err)
   end subroutine make_prologue_with_counts

   subroutine test_field_count_mismatch(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_short.bin"
      integer(default_int) :: unit, n
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      logical :: swapped
      type(error_t) :: err

      ! The prologue promises three elements but every field record holds two.
      call serialize_open_write(fname, unit, err)
      call soa_write_prologue(unit, SCHEMA, 3_default_int, err)
      call write_array(unit, [1_int32, 2_int32], err)
      call write_array(unit, [1_int64, 2_int64], err)
      call write_array(unit, [1.0_sp, 2.0_sp], err)
      call write_array(unit, [1.0_dp, 2.0_dp], err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)

      call soa_read_field(unit, "a", a, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "short int32 field is rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "'a'") > 0, "the message names the field")
      if (allocated(error)) return

      call err%clear()
      call soa_read_field(unit, "b", b, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "short int64 field is rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_read_field(unit, "c", c, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "short real(sp) field is rejected")
      if (allocated(error)) return

      call err%clear()
      call soa_read_field(unit, "d", d, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "short real(dp) field is rejected")
      if (allocated(error)) return

      call err%clear()
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_field_count_mismatch

   subroutine test_bad_logical_code(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_bool.bin"
      character(len=*), parameter :: short = "pic_soa_probe_boolshort.bin"
      integer(default_int) :: unit, n
      logical, allocatable :: e(:)
      logical :: swapped
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call soa_write_prologue(unit, SCHEMA, 3_default_int, err)
      call write_array(unit, [0_int32, 7_int32, 1_int32], err)
      call serialize_close(unit, err)

      call serialize_open_read(fname, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call soa_read_field(unit, "e", e, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "a non 0/1 logical code is rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "logical code") > 0, &
                 "the message explains the rejection")
      if (allocated(error)) return
      call err%clear()
      call serialize_close(unit, err)

      call serialize_open_write(short, unit, err)
      call soa_write_prologue(unit, SCHEMA, 3_default_int, err)
      call write_array(unit, [0_int32, 1_int32], err)
      call serialize_close(unit, err)

      call serialize_open_read(short, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call soa_read_field(unit, "e", e, n, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "a short logical field is rejected")
      if (allocated(error)) return
      call err%clear()
      call serialize_close(unit, err)

      call remove_file(fname)
      call remove_file(short)
   end subroutine test_bad_logical_code

   subroutine test_truncated_stream(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_soa_probe_full.bin"
      character(len=*), parameter :: cut = "pic_soa_probe_cut.bin"
      integer(default_int) :: unit, n
      integer(int32), allocatable :: a(:)
      logical :: swapped
      type(error_t) :: err

      call write_probe_stream(fname, 4_default_int, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      ! Cut inside the schema record: the prologue itself must fail.
      call copy_prefix(fname, cut, PIC_HEADER_BYTES + PIC_RECORD_HEADER_BYTES + 4_int64)
      call serialize_open_read(cut, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call check(error, err%is(ERROR_IO), "a stream cut inside the prologue is ERROR_IO")
      if (allocated(error)) return
      call err%clear()
      call serialize_close(unit, err)

      ! Cut inside the first field record: the prologue reads, the field does not.
      call copy_prefix(fname, cut, PIC_HEADER_BYTES &
                       + 2_int64*PIC_RECORD_HEADER_BYTES + int(len(SCHEMA), int64) + 8_int64 &
                       + PIC_RECORD_HEADER_BYTES + 4_int64)
      call serialize_open_read(cut, unit, err)
      call soa_read_prologue(unit, SCHEMA, n, swapped, err)
      call check(error,.not. err%has_error(), "the prologue of a truncated stream still reads")
      if (allocated(error)) return
      call soa_read_field(unit, "a", a, n, swapped, err)
      call check(error, err%is(ERROR_IO), "a truncated field record is ERROR_IO")
      if (allocated(error)) return
      call err%clear()
      call serialize_close(unit, err)

      call remove_file(fname)
      call remove_file(cut)
   end subroutine test_truncated_stream

end module test_pic_soa
