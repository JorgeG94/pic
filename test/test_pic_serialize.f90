! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
module test_pic_serialize
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: int8, int32, int64, sp, dp, default_int
   use pic_error, only: error_t, ERROR_IO, ERROR_VALIDATION
   use pic_serialize, only: PIC_MAGIC, PIC_FORMAT_VERSION, PIC_HEADER_BYTES, &
                            PIC_RECORD_HEADER_BYTES, PIC_TAG_INT32, PIC_TAG_INT64, &
                            PIC_TAG_REAL_SP, PIC_TAG_REAL_DP, PIC_TAG_CHAR, &
                            host_is_little_endian, record_bytes, &
                            serialize_open_write, serialize_open_read, serialize_close, &
                            write_header, read_header, write_array, read_array
   implicit none
   private
   public :: collect_pic_serialize_tests

   integer(int32), parameter :: ENDIAN_MARK = 16909060_int32

contains

   subroutine collect_pic_serialize_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("round_trip_int32", test_round_trip_int32), &
                  new_unittest("round_trip_int64", test_round_trip_int64), &
                  new_unittest("round_trip_real_sp", test_round_trip_real_sp), &
                  new_unittest("round_trip_real_dp", test_round_trip_real_dp), &
                  new_unittest("round_trip_char", test_round_trip_char), &
                  new_unittest("round_trip_mixed", test_round_trip_mixed), &
                  new_unittest("empty_records", test_empty_records), &
                  new_unittest("byte_length_pinned", test_byte_length_pinned), &
                  new_unittest("record_bytes_helper", test_record_bytes_helper), &
                  new_unittest("host_endianness", test_host_endianness), &
                  new_unittest("bad_magic", test_bad_magic), &
                  new_unittest("bad_endian_mark", test_bad_endian_mark), &
                  new_unittest("version_mismatch", test_version_mismatch), &
                  new_unittest("truncated_header", test_truncated_header), &
                  new_unittest("truncated_record", test_truncated_record), &
                  new_unittest("negative_count", test_negative_count), &
                  new_unittest("wrong_record_type", test_wrong_record_type), &
                  new_unittest("byte_swapped_stream", test_byte_swapped_stream), &
                  new_unittest("open_failures", test_open_failures), &
                  new_unittest("write_to_read_only_unit", test_write_to_read_only_unit) &
                  ]
   end subroutine collect_pic_serialize_tests

   ! ---------------------------------------------------------------- helpers

   pure function swap32(x) result(y)
      integer(int32), intent(in) :: x
      integer(int32) :: y
      y = ior(ior(ishft(ibits(x, 0, 8), 24), ishft(ibits(x, 8, 8), 16)), &
              ior(ishft(ibits(x, 16, 8), 8), ibits(x, 24, 8)))
   end function swap32

   pure function swap64(x) result(y)
      integer(int64), intent(in) :: x
      integer(int64) :: y
      y = ior(ior(ior(ishft(ibits(x, 0, 8), 56), ishft(ibits(x, 8, 8), 48)), &
                  ior(ishft(ibits(x, 16, 8), 40), ishft(ibits(x, 24, 8), 32))), &
              ior(ior(ishft(ibits(x, 32, 8), 24), ishft(ibits(x, 40, 8), 16)), &
                  ior(ishft(ibits(x, 48, 8), 8), ibits(x, 56, 8))))
   end function swap64

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
      !! Copy the first `keep` bytes of `src` into a fresh file `dst`.
      character(len=*), intent(in) :: src, dst
      integer(int64), intent(in) :: keep
      integer :: unit
      integer(int8), allocatable :: buffer(:)

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

   subroutine patch_int32(fname, pos, value)
      !! Overwrite the int32 at 1-based byte position `pos` of an existing file.
      character(len=*), intent(in) :: fname
      integer(int64), intent(in) :: pos
      integer(int32), intent(in) :: value
      integer :: unit
      open (newunit=unit, file=fname, form="unformatted", access="stream", &
            action="readwrite", status="old")
      write (unit, pos=pos) value
      close (unit)
   end subroutine patch_int32

   subroutine patch_chars(fname, pos, text)
      character(len=*), intent(in) :: fname
      integer(int64), intent(in) :: pos
      character(len=*), intent(in) :: text
      integer :: unit
      open (newunit=unit, file=fname, form="unformatted", access="stream", &
            action="readwrite", status="old")
      write (unit, pos=pos) text
      close (unit)
   end subroutine patch_chars

   subroutine make_simple_file(fname, err)
      !! Header plus one int32 record of four values.
      character(len=*), intent(in) :: fname
      type(error_t), intent(out) :: err
      integer(default_int) :: unit

      call serialize_open_write(fname, unit, err)
      if (err%has_error()) return
      call write_header(unit, err)
      if (err%has_error()) return
      call write_array(unit, [11_int32, 22_int32, 33_int32, 44_int32], err)
      if (err%has_error()) return
      call serialize_close(unit, err)
   end subroutine make_simple_file

   ! ------------------------------------------------------------ round trips

   subroutine test_round_trip_int32(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_rt_i32.bin"
      integer(int32), parameter :: expected(5) = [-2147483647_int32, -1_int32, 0_int32, 7_int32, 2147483647_int32]
      integer(int32), allocatable :: got(:)
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call check(error,.not. err%has_error(), "open for write failed")
      if (allocated(error)) return
      call write_header(unit, err)
      call check(error,.not. err%has_error(), "write_header failed")
      if (allocated(error)) return
      call write_array(unit, expected, err)
      call check(error,.not. err%has_error(), "write int32 failed")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "close failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call check(error,.not. err%has_error(), "read_header failed")
      if (allocated(error)) return
      call check(error, version == PIC_FORMAT_VERSION, "version mismatch")
      if (allocated(error)) return
      call check(error,.not. swapped, "native file should not be swapped")
      if (allocated(error)) return
      call read_array(unit, got, swapped, err)
      call check(error,.not. err%has_error(), "read int32 failed")
      if (allocated(error)) return
      call check(error, size(got) == 5, "wrong element count")
      if (allocated(error)) return
      call check(error, all(got == expected), "int32 values differ")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_round_trip_int32

   subroutine test_round_trip_int64(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_rt_i64.bin"
      integer(int64), parameter :: expected(4) = [-9223372036854775807_int64, -1_int64, 0_int64, 1234567890123_int64]
      integer(int64), allocatable :: got(:)
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, expected, err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "write path failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, got, swapped, err)
      call check(error,.not. err%has_error(), "read path failed")
      if (allocated(error)) return
      call check(error, size(got) == 4, "wrong element count")
      if (allocated(error)) return
      call check(error, all(got == expected), "int64 values differ")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_round_trip_int64

   subroutine test_round_trip_real_sp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_rt_sp.bin"
      real(sp), parameter :: expected(3) = [-1.5_sp, 0.0_sp, 3.25_sp]
      real(sp), allocatable :: got(:)
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, expected, err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "write path failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, got, swapped, err)
      call check(error,.not. err%has_error(), "read path failed")
      if (allocated(error)) return
      call check(error, size(got) == 3, "wrong element count")
      if (allocated(error)) return
      call check(error, all(abs(got - expected) < 1.0e-6_sp), "real(sp) values differ")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_round_trip_real_sp

   subroutine test_round_trip_real_dp(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_rt_dp.bin"
      real(dp), parameter :: expected(3) = [-1.0e-12_dp, 0.5_dp, 1.0e300_dp]
      real(dp), allocatable :: got(:)
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, expected, err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "write path failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, got, swapped, err)
      call check(error,.not. err%has_error(), "read path failed")
      if (allocated(error)) return
      call check(error, size(got) == 3, "wrong element count")
      if (allocated(error)) return
      call check(error, all(abs(got - expected) <= abs(expected)*1.0e-15_dp), "real(dp) values differ")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_round_trip_real_dp

   subroutine test_round_trip_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_rt_char.bin"
      character(len=*), parameter :: expected = "pic checkpoint v1"
      character(len=:), allocatable :: got
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, expected, err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "write path failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, got, swapped, err)
      call check(error,.not. err%has_error(), "read path failed")
      if (allocated(error)) return
      call check(error, len(got) == len(expected), "wrong string length")
      if (allocated(error)) return
      call check(error, got == expected, "string differs")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_round_trip_char

   subroutine test_round_trip_mixed(error)
      !! All five record types in one stream, read back in order.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_rt_mixed.bin"
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      character(len=:), allocatable :: e

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, [1_int32, 2_int32], err)
      call write_array(unit, [3_int64], err)
      call write_array(unit, [4.0_sp, 5.0_sp, 6.0_sp], err)
      call write_array(unit, [7.0_dp], err)
      call write_array(unit, "tail", err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "mixed write failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, a, swapped, err)
      call read_array(unit, b, swapped, err)
      call read_array(unit, c, swapped, err)
      call read_array(unit, d, swapped, err)
      call read_array(unit, e, swapped, err)
      call check(error,.not. err%has_error(), "mixed read failed")
      if (allocated(error)) return
      call check(error, all(a == [1_int32, 2_int32]), "int32 record differs")
      if (allocated(error)) return
      call check(error, all(b == [3_int64]), "int64 record differs")
      if (allocated(error)) return
      call check(error, all(abs(c - [4.0_sp, 5.0_sp, 6.0_sp]) < 1.0e-6_sp), "sp record differs")
      if (allocated(error)) return
      call check(error, abs(d(1) - 7.0_dp) < 1.0e-14_dp, "dp record differs")
      if (allocated(error)) return
      call check(error, e == "tail", "char record differs")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_round_trip_mixed

   subroutine test_empty_records(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_empty.bin"
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      character(len=:), allocatable :: e
      integer(int32) :: none_i32(0)
      integer(int64) :: none_i64(0)
      real(sp) :: none_sp(0)
      real(dp) :: none_dp(0)

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, none_i32, err)
      call write_array(unit, none_i64, err)
      call write_array(unit, none_sp, err)
      call write_array(unit, none_dp, err)
      call write_array(unit, "", err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "empty write failed")
      if (allocated(error)) return

      call check(error, file_bytes(fname) == PIC_HEADER_BYTES + 5_int64*PIC_RECORD_HEADER_BYTES, &
                 "empty records should be record headers only")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, a, swapped, err)
      call read_array(unit, b, swapped, err)
      call read_array(unit, c, swapped, err)
      call read_array(unit, d, swapped, err)
      call read_array(unit, e, swapped, err)
      call check(error,.not. err%has_error(), "empty read failed")
      if (allocated(error)) return
      call check(error, size(a) == 0 .and. size(b) == 0 .and. size(c) == 0 .and. size(d) == 0, &
                 "empty arrays should come back empty")
      if (allocated(error)) return
      call check(error, len(e) == 0, "empty string should come back empty")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_empty_records

   subroutine test_byte_length_pinned(error)
      !! Pin the exact on-disk size of a known stream. If this test starts
      !! failing because default_int changed, the file format has silently
      !! changed with it, which is the whole thing this guards against.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_pinned.bin"
      integer(int64), parameter :: expected_bytes = 85_int64
      integer(default_int) :: unit
      type(error_t) :: err

      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, [1_int32, 2_int32, 3_int32], err)
      call write_array(unit, [1.0_dp, 2.0_dp], err)
      call write_array(unit, "hello", err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "pinned write failed")
      if (allocated(error)) return

      ! 16 header + (12 + 3*4) + (12 + 2*8) + (12 + 5) = 85
      call check(error, file_bytes(fname) == expected_bytes, "on-disk byte length changed")
      if (allocated(error)) return
      call check(error, PIC_HEADER_BYTES == 16_int64, "header size changed")
      if (allocated(error)) return
      call check(error, PIC_RECORD_HEADER_BYTES == 12_int64, "record header size changed")
      if (allocated(error)) return
      call check(error, PIC_HEADER_BYTES + record_bytes(PIC_TAG_INT32, 3_int64) &
                 + record_bytes(PIC_TAG_REAL_DP, 2_int64) &
                 + record_bytes(PIC_TAG_CHAR, 5_int64) == expected_bytes, &
                 "record_bytes disagrees with the real file size")
      if (allocated(error)) return
      call remove_file(fname)
   end subroutine test_byte_length_pinned

   subroutine test_record_bytes_helper(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, record_bytes(PIC_TAG_INT32, 10_int64) == 52_int64, "int32 record size")
      if (allocated(error)) return
      call check(error, record_bytes(PIC_TAG_INT64, 10_int64) == 92_int64, "int64 record size")
      if (allocated(error)) return
      call check(error, record_bytes(PIC_TAG_REAL_SP, 10_int64) == 52_int64, "real(sp) record size")
      if (allocated(error)) return
      call check(error, record_bytes(PIC_TAG_REAL_DP, 10_int64) == 92_int64, "real(dp) record size")
      if (allocated(error)) return
      call check(error, record_bytes(PIC_TAG_CHAR, 10_int64) == 22_int64, "character record size")
      if (allocated(error)) return
      call check(error, record_bytes(99_int32, 1_int64) == -1_int64, "unknown tag should be rejected")
      if (allocated(error)) return
      call check(error, record_bytes(PIC_TAG_INT32, -1_int64) == -1_int64, "negative count should be rejected")
      if (allocated(error)) return
      call check(error, len(PIC_MAGIC) == 4, "magic must stay four bytes")
      if (allocated(error)) return
   end subroutine test_record_bytes_helper

   subroutine test_host_endianness(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: probe

      probe = transfer([1_int8, 0_int8, 0_int8, 0_int8], 0_int32)
      call check(error, host_is_little_endian() .eqv. (probe == 1_int32), &
                 "host_is_little_endian disagrees with an independent probe")
      if (allocated(error)) return
      call check(error, swap32(swap32(ENDIAN_MARK)) == ENDIAN_MARK, "swap32 is not an involution")
      if (allocated(error)) return
      call check(error, swap32(ENDIAN_MARK) == 67305985_int32, "0x01020304 must swap to 0x04030201")
      if (allocated(error)) return
   end subroutine test_host_endianness

   ! -------------------------------------------------------------- bad files

   subroutine test_bad_magic(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_badmagic.bin"
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call make_simple_file(fname, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return
      call patch_chars(fname, 1_int64, "NOPE")

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call check(error, err%has_error(), "bad magic should be an error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "bad magic should be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, version == 0_int32, "version should be zeroed on failure")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_bad_magic

   subroutine test_bad_endian_mark(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_badmark.bin"
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call make_simple_file(fname, err)
      call patch_int32(fname, 5_int64, 123456_int32)

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "bad byte-order mark should be ERROR_VALIDATION")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_bad_endian_mark

   subroutine test_version_mismatch(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_version.bin"
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err

      call make_simple_file(fname, err)

      ! A version from the future
      call patch_int32(fname, 9_int64, PIC_FORMAT_VERSION + 1_int32)
      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "future version should be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, version == 0_int32, "version should be zeroed on failure")
      if (allocated(error)) return
      call serialize_close(unit, err)

      ! A nonsensical version
      call err%clear()
      call patch_int32(fname, 9_int64, 0_int32)
      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "version 0 should be ERROR_VALIDATION")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_version_mismatch

   subroutine test_truncated_header(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_trunc_src.bin"
      character(len=*), parameter :: cut = "pic_serialize_trunc_hdr.bin"
      integer(int64), parameter :: cuts(4) = [0_int64, 6_int64, 10_int64, 14_int64]
      integer(default_int) :: unit
      integer(int32) :: version
      integer :: i
      logical :: swapped
      type(error_t) :: err

      call make_simple_file(fname, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      do i = 1, size(cuts)
         call err%clear()
         call copy_prefix(fname, cut, cuts(i))
         call serialize_open_read(cut, unit, err)
         call check(error,.not. err%has_error(), "open of truncated file failed")
         if (allocated(error)) return
         call read_header(unit, version, swapped, err)
         call check(error, err%is(ERROR_IO), "truncated header should be ERROR_IO")
         if (allocated(error)) return
         call check(error, version == 0_int32, "version should be zeroed on failure")
         if (allocated(error)) return
         call serialize_close(unit, err)
      end do

      call remove_file(fname)
      call remove_file(cut)
   end subroutine test_truncated_header

   subroutine test_truncated_record(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_trunc_src2.bin"
      character(len=*), parameter :: cut = "pic_serialize_trunc_rec.bin"
      ! 16 byte header + 12 byte record header + 4 int32 = 44 bytes.
      ! 18 cuts the tag, 20 cuts the count, 30 cuts the payload.
      integer(int64), parameter :: cuts(3) = [18_int64, 20_int64, 30_int64]
      integer(default_int) :: unit
      integer(int32) :: version
      integer(int32), allocatable :: got(:)
      integer :: i
      logical :: swapped
      type(error_t) :: err

      call make_simple_file(fname, err)
      call check(error, file_bytes(fname) == 44_int64, "unexpected size for the reference file")
      if (allocated(error)) return

      do i = 1, size(cuts)
         call err%clear()
         call copy_prefix(fname, cut, cuts(i))
         call serialize_open_read(cut, unit, err)
         call read_header(unit, version, swapped, err)
         call check(error,.not. err%has_error(), "header of truncated file should still be valid")
         if (allocated(error)) return
         call read_array(unit, got, swapped, err)
         call check(error, err%is(ERROR_IO), "truncated record should be ERROR_IO")
         if (allocated(error)) return
         call check(error, allocated(got), "output must still be allocated after failure")
         if (allocated(error)) return
         call check(error, size(got) == 0, "output must be empty after failure")
         if (allocated(error)) return
         call serialize_close(unit, err)
      end do

      call remove_file(fname)
      call remove_file(cut)
   end subroutine test_truncated_record

   subroutine test_negative_count(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_negcount.bin"
      integer(default_int) :: unit
      integer(int32) :: version
      integer(int32), allocatable :: got(:)
      logical :: swapped
      type(error_t) :: err
      integer :: raw

      call make_simple_file(fname, err)
      ! Record count lives at bytes 21..28; write -1 over it.
      open (newunit=raw, file=fname, form="unformatted", access="stream", &
            action="readwrite", status="old")
      write (raw, pos=21_int64) - 1_int64
      close (raw)

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, got, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "negative count should be ERROR_VALIDATION")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_negative_count

   subroutine test_wrong_record_type(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_wrongtype.bin"
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      character(len=:), allocatable :: e

      ! A file holding a single character record; every other reader must
      ! refuse it, and the int32 reader must refuse a character record too.
      call serialize_open_write(fname, unit, err)
      call write_header(unit, err)
      call write_array(unit, "not a number", err)
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)

      call read_array(unit, a, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "int32 reader should reject a char record")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "int32") > 0, "error should name the record kind")
      if (allocated(error)) return
      call check(error, size(a) == 0, "int32 output should be empty")
      if (allocated(error)) return
      call serialize_close(unit, err)

      call err%clear()
      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, b, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "int64 reader should reject a char record")
      if (allocated(error)) return
      call check(error, size(b) == 0, "int64 output should be empty")
      if (allocated(error)) return
      call serialize_close(unit, err)

      call err%clear()
      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, c, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "real(sp) reader should reject a char record")
      if (allocated(error)) return
      call check(error, size(c) == 0, "real(sp) output should be empty")
      if (allocated(error)) return
      call serialize_close(unit, err)

      call err%clear()
      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, d, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "real(dp) reader should reject a char record")
      if (allocated(error)) return
      call check(error, size(d) == 0, "real(dp) output should be empty")
      if (allocated(error)) return
      call serialize_close(unit, err)

      ! And a numeric file refused by the character reader.
      call remove_file(fname)
      call err%clear()
      call make_simple_file(fname, err)
      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call read_array(unit, e, swapped, err)
      call check(error, err%is(ERROR_VALIDATION), "char reader should reject an int32 record")
      if (allocated(error)) return
      call check(error, len(e) == 0, "char output should be empty")
      if (allocated(error)) return
      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_wrong_record_type

   ! ---------------------------------------------------------- the swap path

   subroutine test_byte_swapped_stream(error)
      !! Hand build a stream in the opposite byte order to this machine and
      !! prove the reader detects it and un-swaps every payload correctly.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_swapped.bin"
      integer(int32), parameter :: i32_vals(3) = [1_int32, -2_int32, 305419896_int32]
      integer(int64), parameter :: i64_vals(2) = [-1_int64, 81985529216486895_int64]
      real(sp), parameter :: sp_vals(2) = [1.5_sp, -0.25_sp]
      real(dp), parameter :: dp_vals(2) = [3.125_dp, -1.0e-5_dp]
      character(len=*), parameter :: text = "swapped"
      integer :: raw, i
      integer(default_int) :: unit
      integer(int32) :: version
      logical :: swapped
      type(error_t) :: err
      integer(int32), allocatable :: a(:)
      integer(int64), allocatable :: b(:)
      real(sp), allocatable :: c(:)
      real(dp), allocatable :: d(:)
      character(len=:), allocatable :: e

      open (newunit=raw, file=fname, form="unformatted", access="stream", &
            action="write", status="replace")
      write (raw) PIC_MAGIC
      write (raw) swap32(ENDIAN_MARK)
      write (raw) swap32(PIC_FORMAT_VERSION)
      write (raw) swap32(0_int32)

      write (raw) swap32(PIC_TAG_INT32)
      write (raw) swap64(3_int64)
      do i = 1, 3
         write (raw) swap32(i32_vals(i))
      end do

      write (raw) swap32(PIC_TAG_INT64)
      write (raw) swap64(2_int64)
      do i = 1, 2
         write (raw) swap64(i64_vals(i))
      end do

      write (raw) swap32(PIC_TAG_REAL_SP)
      write (raw) swap64(2_int64)
      do i = 1, 2
         write (raw) swap32(transfer(sp_vals(i), 0_int32))
      end do

      write (raw) swap32(PIC_TAG_REAL_DP)
      write (raw) swap64(2_int64)
      do i = 1, 2
         write (raw) swap64(transfer(dp_vals(i), 0_int64))
      end do

      write (raw) swap32(PIC_TAG_CHAR)
      write (raw) swap64(int(len(text), int64))
      write (raw) text
      close (raw)

      call serialize_open_read(fname, unit, err)
      call read_header(unit, version, swapped, err)
      call check(error,.not. err%has_error(), "swapped header should be accepted")
      if (allocated(error)) return
      call check(error, swapped, "reader must report a foreign byte order")
      if (allocated(error)) return
      call check(error, version == PIC_FORMAT_VERSION, "swapped version must be un-swapped")
      if (allocated(error)) return

      call read_array(unit, a, swapped, err)
      call check(error,.not. err%has_error(), "swapped int32 read failed")
      if (allocated(error)) return
      call check(error, all(a == i32_vals), "swapped int32 values differ")
      if (allocated(error)) return

      call read_array(unit, b, swapped, err)
      call check(error,.not. err%has_error(), "swapped int64 read failed")
      if (allocated(error)) return
      call check(error, all(b == i64_vals), "swapped int64 values differ")
      if (allocated(error)) return

      call read_array(unit, c, swapped, err)
      call check(error,.not. err%has_error(), "swapped real(sp) read failed")
      if (allocated(error)) return
      call check(error, all(abs(c - sp_vals) < 1.0e-6_sp), "swapped real(sp) values differ")
      if (allocated(error)) return

      call read_array(unit, d, swapped, err)
      call check(error,.not. err%has_error(), "swapped real(dp) read failed")
      if (allocated(error)) return
      call check(error, all(abs(d - dp_vals) <= abs(dp_vals)*1.0e-15_dp), "swapped real(dp) values differ")
      if (allocated(error)) return

      call read_array(unit, e, swapped, err)
      call check(error,.not. err%has_error(), "swapped char read failed")
      if (allocated(error)) return
      call check(error, e == text, "swapped char record differs")
      if (allocated(error)) return

      call serialize_close(unit, err)
      call remove_file(fname)
   end subroutine test_byte_swapped_stream

   ! ------------------------------------------------------------ I/O failure

   subroutine test_open_failures(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: unreachable = "pic_serialize_no_such_dir/out.bin"
      integer(default_int) :: unit
      integer(default_int) :: probe, ios
      type(error_t) :: err

      call serialize_open_read("pic_serialize_does_not_exist.bin", unit, err)
      call check(error, err%is(ERROR_IO), "opening a missing file should be ERROR_IO")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "iostat=") > 0, "message should carry the iostat")
      if (allocated(error)) return

      ! `serialize_open_write` can only report what the processor reports, and
      ! LFortran 0.65.0 returns iostat=0 from an OPEN into a nonexistent
      ! directory -- as well as from the WRITE and CLOSE that follow, so the
      ! data is silently dropped. Probe the processor with the same OPEN rather
      ! than skipping by compiler name: the assertion then still runs on every
      ! processor that can report the failure (GNU, Intel, Flang, NVHPC), and
      ! comes back by itself once LFortran reports it, with nothing to delete.
      ios = 0_default_int
      open (newunit=probe, file=unreachable, form="unformatted", access="stream", &
            action="write", status="replace", iostat=ios)
      if (ios == 0_default_int) close (probe, status="delete")

      if (ios /= 0_default_int) then
         call err%clear()
         call serialize_open_write(unreachable, unit, err)
         call check(error, err%is(ERROR_IO), "writing into a missing directory should be ERROR_IO")
         if (allocated(error)) return
      end if
   end subroutine test_open_failures

   subroutine test_write_to_read_only_unit(error)
      !! Every writer must report ERROR_IO, not abort, when the underlying
      !! unit refuses the write.
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fname = "pic_serialize_readonly.bin"
      integer(default_int) :: unit
      type(error_t) :: err

      call make_simple_file(fname, err)
      call serialize_open_read(fname, unit, err)
      call check(error,.not. err%has_error(), "setup failed")
      if (allocated(error)) return

      call write_header(unit, err)
      call check(error, err%is(ERROR_IO), "write_header on a read-only unit should be ERROR_IO")
      if (allocated(error)) return

      call err%clear()
      call write_array(unit, [1_int32], err)
      call check(error, err%is(ERROR_IO), "int32 write on a read-only unit should be ERROR_IO")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "int32 record") > 0, "error should name the record kind")
      if (allocated(error)) return

      call err%clear()
      call write_array(unit, [1_int64], err)
      call check(error, err%is(ERROR_IO), "int64 write on a read-only unit should be ERROR_IO")
      if (allocated(error)) return

      call err%clear()
      call write_array(unit, [1.0_sp], err)
      call check(error, err%is(ERROR_IO), "real(sp) write on a read-only unit should be ERROR_IO")
      if (allocated(error)) return

      call err%clear()
      call write_array(unit, [1.0_dp], err)
      call check(error, err%is(ERROR_IO), "real(dp) write on a read-only unit should be ERROR_IO")
      if (allocated(error)) return

      call err%clear()
      call write_array(unit, "nope", err)
      call check(error, err%is(ERROR_IO), "char write on a read-only unit should be ERROR_IO")
      if (allocated(error)) return

      call err%clear()
      call serialize_close(unit, err)
      call check(error,.not. err%has_error(), "close should succeed")
      if (allocated(error)) return
      call remove_file(fname)
   end subroutine test_write_to_read_only_unit

end module test_pic_serialize
