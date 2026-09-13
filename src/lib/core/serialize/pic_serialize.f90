! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Binary serialization helpers built on a small, self-describing envelope.
!!
!! The goal of this module is that a checkpoint/restart file fails *loudly*
!! when it is read back by a different build, on a machine with a different
!! byte order, or by a newer version of the code, instead of silently
!! producing garbage.
module pic_serialize
   !! Self-describing binary serialization for PIC.
   !!
   !! ## Stream layout
   !!
   !! Every stream starts with a 16 byte header followed by zero or more
   !! records. All multi-byte fields are written in the *native* byte order of
   !! the writing machine; the header carries a byte-order mark so that a
   !! reader on a foreign machine can detect this and un-swap on the fly.
   !!
   !! ### Header, 16 bytes
   !!
   !! | offset | size | field       | description                                   |
   !! |--------|------|-------------|-----------------------------------------------|
   !! | 0      | 4    | magic       | ASCII `"PIC1"`, byte order independent        |
   !! | 4      | 4    | endian mark | `integer(int32)` `0x01020304` (16909060)      |
   !! | 8      | 4    | version     | `integer(int32)` format version               |
   !! | 12     | 4    | reserved    | `integer(int32)` written as 0, readers skip it |
   !!
   !! There is deliberately **no** payload-length field in the header. The
   !! number and kind of records that follow is decided by the caller, and each
   !! record is individually length prefixed and bounds checked against the
   !! real size of the file, so a global length would only be a second source
   !! of truth that can disagree with the first. The reserved word exists so a
   !! future version can add one without changing the header size.
   !!
   !! ### Record, 12 byte record header plus payload
   !!
   !! | offset | size | field   | description                               |
   !! |--------|------|---------|-------------------------------------------|
   !! | 0      | 4    | tag     | `integer(int32)` type tag, see `PIC_TAG_*` |
   !! | 4      | 8    | count   | `integer(int64)` number of *elements*      |
   !! | 12     | n    | payload | `count * element size` bytes               |
   !!
   !! `count` counts elements, not bytes: three `real(dp)` values give
   !! `count == 3` and a 24 byte payload. For a character record `count` is the
   !! number of characters, so the element size is one.
   !!
   !! ## Fixed widths on purpose
   !!
   !! Every field that reaches the file is an explicit `int32`/`int64`, never
   !! `default_int`. A file written by a build with `PIC_DEFAULT_INT8=ON` is
   !! byte identical to one written by a default build; the test suite pins the
   !! exact byte length of a known stream so this cannot regress silently.
   !!
   !! ## Error contract
   !!
   !! Every procedure takes `type(error_t), intent(inout) :: err` and writes it
   !! *only on failure*; it is never cleared on entry. Pass a fresh or cleared
   !! `error_t` and check it after each call, which is the convention the rest
   !! of PIC uses. Keeping `err` `intent(inout)` rather than `intent(out)` also
   !! keeps the compiler from emitting a conditional deallocation of the error
   !! message on every single call into this module.
   !!
   !! ## Usage
   !!
   !!```fortran
   !! type(error_t) :: err
   !! integer(default_int) :: unit
   !! integer(int32) :: version
   !! logical :: swapped
   !! real(dp), allocatable :: values(:)
   !!
   !! call serialize_open_write("state.bin", unit, err)
   !! call write_header(unit, err)
   !! call write_array(unit, [1.0_dp, 2.0_dp], err)
   !! call serialize_close(unit, err)
   !!
   !! call serialize_open_read("state.bin", unit, err)
   !! call read_header(unit, version, swapped, err)
   !! call read_array(unit, values, swapped, err)
   !! call serialize_close(unit, err)
   !!```
   use pic_types, only: int8, int32, int64, sp, dp, default_int
   use pic_io, only: to_char
   use pic_error, only: error_t, ERROR_IO, ERROR_VALIDATION
   implicit none
   private

   public :: PIC_MAGIC, PIC_FORMAT_VERSION
   public :: PIC_HEADER_BYTES, PIC_RECORD_HEADER_BYTES
   public :: PIC_TAG_INT32, PIC_TAG_INT64, PIC_TAG_REAL_SP, PIC_TAG_REAL_DP, PIC_TAG_CHAR
   public :: host_is_little_endian
   public :: record_bytes
   public :: serialize_open_write, serialize_open_read, serialize_close
   public :: write_header, read_header
   public :: write_array, read_array

   character(len=*), parameter :: PIC_MAGIC = "PIC1"
      !! Four byte ASCII magic written at offset 0 of every stream. Being
      !! characters it has no byte order of its own, which is what makes it
      !! usable to reject a foreign file before anything else is decoded.

   integer(int32), parameter :: PIC_FORMAT_VERSION = 1_int32
      !! Current envelope version. Readers accept
      !! `1 <= version <= PIC_FORMAT_VERSION` and reject anything else with
      !! `ERROR_VALIDATION`.

   integer(int32), parameter :: PIC_ENDIAN_MARK = 16909060_int32
      !! `0x01020304`, spelled in decimal so that no BOZ literal appears in an
      !! initializer, since gfortran and ifx disagree about the kind of those.
      !! Written natively: a reader that sees the byte reversed value
      !! `0x04030201` knows the file came from the opposite byte order.

   integer(int64), parameter :: PIC_HEADER_BYTES = 16_int64
      !! Size of the stream header in bytes.
   integer(int64), parameter :: PIC_RECORD_HEADER_BYTES = 12_int64
      !! Size of one record header, tag plus element count, in bytes.

   integer(int32), parameter :: PIC_TAG_INT32 = 1_int32
      !! Record tag for `integer(int32)` arrays, 4 bytes per element.
   integer(int32), parameter :: PIC_TAG_INT64 = 2_int32
      !! Record tag for `integer(int64)` arrays, 8 bytes per element.
   integer(int32), parameter :: PIC_TAG_REAL_SP = 3_int32
      !! Record tag for `real(sp)` arrays, 4 bytes per element.
   integer(int32), parameter :: PIC_TAG_REAL_DP = 4_int32
      !! Record tag for `real(dp)` arrays, 8 bytes per element.
   integer(int32), parameter :: PIC_TAG_CHAR = 5_int32
      !! Record tag for a character scalar, 1 byte per element.

   integer(int32), parameter :: RESERVED_WORD = 0_int32
      !! Value written into the reserved header slot.

   integer(int32), parameter :: BITS_PER_BYTE = 8_int32
      !! Width of one byte field, used by the byte reversal helpers.

   interface write_array
      !! Write one length prefixed, type tagged record to a stream unit.
      !!
      !! Usage: `call write_array(unit, values, err)`
      !!
      !! The unit must have been opened with `access='stream'` and
      !! `form='unformatted'`, for instance by `serialize_open_write`, and
      !! `write_header` must already have been called on it. Zero sized arrays
      !! and zero length strings are legal and produce a record header with
      !! `count == 0` and no payload.
      module procedure write_int32_array
      module procedure write_int64_array
      module procedure write_real_sp_array
      module procedure write_real_dp_array
      module procedure write_char_record
   end interface write_array

   interface read_array
      !! Read one record back, validating its type tag and its length.
      !!
      !! Usage: `call read_array(unit, values, swapped, err)`
      !!
      !! `values` is `allocatable` and is allocated to the length stored in the
      !! record. `swapped` is the flag returned by `read_header`; when it is
      !! true every multi-byte element is byte reversed after reading.
      !!
      !! Fails with `ERROR_VALIDATION` if the record holds a different type
      !! than the one requested, and with `ERROR_IO` if the payload announced
      !! by the record header runs past the end of the file.
      module procedure read_int32_array
      module procedure read_int64_array
      module procedure read_real_sp_array
      module procedure read_real_dp_array
      module procedure read_char_record
   end interface read_array

contains

   pure function host_is_little_endian() result(is_little)
      !! Report whether the running machine stores the least significant byte
      !! of an integer first.
      !!
      !! The probe is a `transfer` between two *integer* kinds, `int8` to
      !! `int32`. A `transfer` between a real and an integer kind is
      !! deliberately avoided here: it is the construct whose behaviour is
      !! least dependable across nvfortran and LFortran, and there is no reason
      !! to risk it when an integer only probe answers the same question.
      !! Arithmetic alone, `iand` or `ishft`, cannot answer it at all, since
      !! those operate on values rather than on the storage order of the bytes.
      logical :: is_little

      is_little = (transfer([1_int8, 0_int8, 0_int8, 0_int8], 0_int32) == 1_int32)
   end function host_is_little_endian

   pure function element_bytes(tag) result(nbytes)
      !! Bytes occupied by one element of the record type `tag`, or -1 for an
      !! unknown tag.
      integer(int32), intent(in) :: tag
      integer(int64) :: nbytes

      select case (tag)
      case (PIC_TAG_INT32)
         nbytes = 4_int64
      case (PIC_TAG_INT64)
         nbytes = 8_int64
      case (PIC_TAG_REAL_SP)
         nbytes = 4_int64
      case (PIC_TAG_REAL_DP)
         nbytes = 8_int64
      case (PIC_TAG_CHAR)
         nbytes = 1_int64
      case default
         nbytes = -1_int64
      end select
   end function element_bytes

   pure function record_bytes(tag, count) result(nbytes)
      !! Total on disk size of a record, including its 12 byte record header.
      !!
      !! Returns -1 when the tag is unknown or the count is negative. Useful to
      !! size a file up front, and used by the test suite to pin the format.
      integer(int32), intent(in) :: tag
         !! One of the `PIC_TAG_*` constants.
      integer(int64), intent(in) :: count
         !! Number of elements in the record.
      integer(int64) :: nbytes

      integer(int64) :: esz

      esz = element_bytes(tag)
      if (esz < 0_int64 .or. count < 0_int64) then
         nbytes = -1_int64
      else
         nbytes = PIC_RECORD_HEADER_BYTES + count*esz
      end if
   end function record_bytes

   pure function bswap_int32(x) result(y)
      !! Reverse the four bytes of an `int32`.
      !!
      !! Written with `ibits`/`ishft` using only non-negative shift counts.
      !! `ishft` with a negative count is avoided because the compilers this
      !! project targets have historically disagreed about it, and `shiftr` is
      !! Fortran 2008 which is more than this needs to assume.
      integer(int32), intent(in) :: x
      integer(int32) :: y

      y = ior(ior(ishft(ibits(x, 0, BITS_PER_BYTE), 24), ishft(ibits(x, 8, BITS_PER_BYTE), 16)), &
              ior(ishft(ibits(x, 16, BITS_PER_BYTE), 8), ibits(x, 24, BITS_PER_BYTE)))
   end function bswap_int32

   pure function bswap_int64(x) result(y)
      !! Reverse the eight bytes of an `int64`.
      integer(int64), intent(in) :: x
      integer(int64) :: y

      y = ior(ior(ior(ishft(ibits(x, 0, BITS_PER_BYTE), 56), ishft(ibits(x, 8, BITS_PER_BYTE), 48)), &
                  ior(ishft(ibits(x, 16, BITS_PER_BYTE), 40), ishft(ibits(x, 24, BITS_PER_BYTE), 32))), &
              ior(ior(ishft(ibits(x, 32, BITS_PER_BYTE), 24), ishft(ibits(x, 40, BITS_PER_BYTE), 16)), &
                  ior(ishft(ibits(x, 48, BITS_PER_BYTE), 8), ibits(x, 56, BITS_PER_BYTE))))
   end function bswap_int64

   pure function bits_to_real_sp(bits) result(x)
      !! Reinterpret the bit pattern of an `int32` as a `real(sp)`.
      !!
      !! Only ever reached on the foreign endian read path. The native path
      !! reads `real(sp)` straight from the stream and never reinterprets
      !! anything, which keeps the overwhelmingly common case entirely free of
      !! any real/integer `transfer`.
      integer(int32), intent(in) :: bits
      real(sp) :: x

      x = transfer(bits, 0.0_sp)
   end function bits_to_real_sp

   pure function bits_to_real_dp(bits) result(x)
      !! Reinterpret the bit pattern of an `int64` as a `real(dp)`.
      !! See `bits_to_real_sp` for why this is confined to the swapped path.
      integer(int64), intent(in) :: bits
      real(dp) :: x

      x = transfer(bits, 0.0_dp)
   end function bits_to_real_dp

   subroutine check_io(ios, context, err)
      !! Turn a non zero `iostat` into an `ERROR_IO` carrying the raw value.
      !! Every I/O statement in this module funnels its status through here so
      !! that the failure message always names both the operation and the
      !! `iostat` the runtime produced.
      integer(default_int), intent(in) :: ios
      character(len=*), intent(in) :: context
      type(error_t), intent(inout) :: err

      if (ios /= 0) call err%set(ERROR_IO, context//", iostat="//to_char(int(ios, int32)))
   end subroutine check_io

   subroutine serialize_open_write(filename, unit, err)
      !! Open `filename` for writing a serialized stream, replacing any
      !! existing file.
      !!
      !! `access='stream'` is Fortran 2003 and is the right tool here: it gives
      !! byte addressable, record marker free output, so the file layout is
      !! exactly the one documented above rather than one decorated with
      !! compiler specific record length markers.
      character(len=*), intent(in) :: filename
         !! Path of the file to create.
      integer(default_int), intent(out) :: unit
         !! Newly opened unit number, -1 when the open failed.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_IO` if the file cannot be created.

      integer(default_int) :: ios

      unit = -1_default_int
      open (newunit=unit, file=filename, form="unformatted", access="stream", &
            action="write", status="replace", iostat=ios)
      call check_io(ios, "cannot open '"//filename//"' for writing", err)
   end subroutine serialize_open_write

   subroutine serialize_open_read(filename, unit, err)
      !! Open an existing serialized stream for reading.
      character(len=*), intent(in) :: filename
         !! Path of the file to read.
      integer(default_int), intent(out) :: unit
         !! Newly opened unit number, -1 when the open failed.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_IO` if the file cannot be opened.

      integer(default_int) :: ios

      unit = -1_default_int
      open (newunit=unit, file=filename, form="unformatted", access="stream", &
            action="read", status="old", iostat=ios)
      call check_io(ios, "cannot open '"//filename//"' for reading", err)
   end subroutine serialize_open_read

   subroutine serialize_close(unit, err)
      !! Close a unit previously opened by one of the open helpers.
      integer(default_int), intent(in) :: unit
         !! Unit to close.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_IO` if the close failed.

      integer(default_int) :: ios

      close (unit, iostat=ios)
      call check_io(ios, "failed to close serialized stream", err)
   end subroutine serialize_close

   subroutine write_header(unit, err)
      !! Write the 16 byte stream header at the current position.
      !!
      !! Call this once, before any record, on a freshly opened unit.
      integer(default_int), intent(in) :: unit
         !! Stream unit opened for unformatted stream writing.
      type(error_t), intent(inout) :: err
         !! Error state, `ERROR_IO` if any field could not be written.

      integer(default_int) :: ios

      write (unit, iostat=ios) PIC_MAGIC
      if (ios == 0) write (unit, iostat=ios) PIC_ENDIAN_MARK
      if (ios == 0) write (unit, iostat=ios) PIC_FORMAT_VERSION
      if (ios == 0) write (unit, iostat=ios) RESERVED_WORD
      call check_io(ios, "failed to write stream header", err)
   end subroutine write_header

   subroutine read_header(unit, version, swapped, err)
      !! Read and validate the stream header.
      !!
      !! Reports `ERROR_IO` when the file is shorter than a header, and
      !! `ERROR_VALIDATION` when the magic, the byte order mark or the version
      !! is not something this build understands.
      integer(default_int), intent(in) :: unit
         !! Stream unit opened for unformatted stream reading.
      integer(int32), intent(out) :: version
         !! Format version found in the file, 0 if the header was rejected.
      logical, intent(out) :: swapped
         !! True when the file was written by a machine of the opposite byte
         !! order, so every multi-byte field needs reversing on the way in.
      type(error_t), intent(inout) :: err
         !! Error state.

      integer(default_int) :: ios
      character(len=len(PIC_MAGIC)) :: magic
      integer(int32) :: mark, raw_version, reserved

      version = 0_int32
      swapped = .false.

      read (unit, iostat=ios) magic
      call check_io(ios, "unexpected end of stream reading magic", err)
      if (ios /= 0) return

      if (magic /= PIC_MAGIC) then
         call err%set(ERROR_VALIDATION, "bad magic: expected '"//PIC_MAGIC//"', found '"//magic//"'")
         return
      end if

      read (unit, iostat=ios) mark
      call check_io(ios, "unexpected end of stream reading byte-order mark", err)
      if (ios /= 0) return

      if (mark == PIC_ENDIAN_MARK) then
         swapped = .false.
      else if (mark == bswap_int32(PIC_ENDIAN_MARK)) then
         swapped = .true.
      else
         call err%set(ERROR_VALIDATION, "bad byte-order mark "//to_char(mark)// &
                      ", this is not a PIC serialized stream")
         return
      end if

      read (unit, iostat=ios) raw_version
      call check_io(ios, "unexpected end of stream reading version", err)
      if (ios /= 0) return

      if (swapped) raw_version = bswap_int32(raw_version)
      if (raw_version < 1_int32 .or. raw_version > PIC_FORMAT_VERSION) then
         call err%set(ERROR_VALIDATION, "unsupported format version "//to_char(raw_version)// &
                      ", this build understands 1 to "//to_char(PIC_FORMAT_VERSION))
         return
      end if

      read (unit, iostat=ios) reserved
      call check_io(ios, "unexpected end of stream reading reserved word", err)
      if (ios /= 0) return

      version = raw_version
   end subroutine read_header

   subroutine write_record_header(unit, tag, count, ok, err)
      !! Write a record header: type tag followed by the element count.
      !! `ok` rather than `err` carries the outcome so that a stale error left
      !! in `err` by an earlier call cannot be mistaken for a failure here.
      integer(default_int), intent(in) :: unit
      integer(int32), intent(in) :: tag
      integer(int64), intent(in) :: count
      logical, intent(out) :: ok
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios

      write (unit, iostat=ios) tag
      if (ios == 0) write (unit, iostat=ios) count
      call check_io(ios, "failed to write record header", err)
      ok = (ios == 0)
   end subroutine write_record_header

   subroutine read_record_header(unit, expected_tag, swapped, count, ok, err)
      !! Read a record header, check the type tag, and bounds check the
      !! announced payload against the real length of the file.
      !!
      !! Comparing the payload with `inquire(size=)` before allocating is what
      !! turns a truncated or corrupt file into a clean `ERROR_IO` instead of a
      !! wild allocation followed by a short read.
      integer(default_int), intent(in) :: unit
      integer(int32), intent(in) :: expected_tag
      logical, intent(in) :: swapped
      integer(int64), intent(out) :: count
      logical, intent(out) :: ok
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios
      integer(int32) :: tag
      integer(int64) :: raw_count, esz, file_size, cur_pos, available

      count = 0_int64
      ok = .false.

      read (unit, iostat=ios) tag
      call check_io(ios, "unexpected end of stream reading record tag", err)
      if (ios /= 0) return

      if (swapped) tag = bswap_int32(tag)
      if (tag /= expected_tag) then
         call err%set(ERROR_VALIDATION, "record type mismatch: expected tag "//to_char(expected_tag)// &
                      ", found "//to_char(tag))
         return
      end if

      read (unit, iostat=ios) raw_count
      call check_io(ios, "unexpected end of stream reading record length", err)
      if (ios /= 0) return

      if (swapped) raw_count = bswap_int64(raw_count)
      if (raw_count < 0_int64) then
         call err%set(ERROR_VALIDATION, "negative record element count "//to_char(raw_count))
         return
      end if

      inquire (unit, size=file_size, pos=cur_pos, iostat=ios)
      call check_io(ios, "cannot inquire stream size", err)
      if (ios /= 0) return

      esz = element_bytes(expected_tag)
      available = file_size - (cur_pos - 1_int64)
      ! Divide instead of multiplying so an absurd count cannot overflow int64.
      if (raw_count > available/esz) then
         call err%set(ERROR_IO, "truncated record: "//to_char(raw_count)//" elements announced but only "// &
                      to_char(available)//" payload bytes remain")
         return
      end if

      count = raw_count
      ok = .true.
   end subroutine read_record_header

   subroutine write_int32_array(unit, values, err)
      !! Write an `integer(int32)` array record.
      integer(default_int), intent(in) :: unit
      integer(int32), intent(in) :: values(:)
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok

      call write_record_header(unit, PIC_TAG_INT32, size(values, kind=int64), ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to write int32 record")
         return
      end if

      if (size(values, kind=int64) > 0_int64) then
         write (unit, iostat=ios) values
         call check_io(ios, "failed to write int32 payload", err)
      end if
   end subroutine write_int32_array

   subroutine write_int64_array(unit, values, err)
      !! Write an `integer(int64)` array record.
      integer(default_int), intent(in) :: unit
      integer(int64), intent(in) :: values(:)
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok

      call write_record_header(unit, PIC_TAG_INT64, size(values, kind=int64), ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to write int64 record")
         return
      end if

      if (size(values, kind=int64) > 0_int64) then
         write (unit, iostat=ios) values
         call check_io(ios, "failed to write int64 payload", err)
      end if
   end subroutine write_int64_array

   subroutine write_real_sp_array(unit, values, err)
      !! Write a `real(sp)` array record.
      integer(default_int), intent(in) :: unit
      real(sp), intent(in) :: values(:)
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok

      call write_record_header(unit, PIC_TAG_REAL_SP, size(values, kind=int64), ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to write real(sp) record")
         return
      end if

      if (size(values, kind=int64) > 0_int64) then
         write (unit, iostat=ios) values
         call check_io(ios, "failed to write real(sp) payload", err)
      end if
   end subroutine write_real_sp_array

   subroutine write_real_dp_array(unit, values, err)
      !! Write a `real(dp)` array record.
      integer(default_int), intent(in) :: unit
      real(dp), intent(in) :: values(:)
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok

      call write_record_header(unit, PIC_TAG_REAL_DP, size(values, kind=int64), ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to write real(dp) record")
         return
      end if

      if (size(values, kind=int64) > 0_int64) then
         write (unit, iostat=ios) values
         call check_io(ios, "failed to write real(dp) payload", err)
      end if
   end subroutine write_real_dp_array

   subroutine write_char_record(unit, text, err)
      !! Write a character scalar record. The stored count is the number of
      !! characters, so a record of `n` characters occupies `n` payload bytes.
      integer(default_int), intent(in) :: unit
      character(len=*), intent(in) :: text
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok

      call write_record_header(unit, PIC_TAG_CHAR, int(len(text), int64), ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to write character record")
         return
      end if

      if (len(text) > 0) then
         write (unit, iostat=ios) text
         call check_io(ios, "failed to write character payload", err)
      end if
   end subroutine write_char_record

   subroutine read_int32_array(unit, values, swapped, err)
      !! Read an `integer(int32)` array record.
      integer(default_int), intent(in) :: unit
      integer(int32), allocatable, intent(out) :: values(:)
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok
      integer(int64) :: count, i

      call read_record_header(unit, PIC_TAG_INT32, swapped, count, ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to read int32 record")
         allocate (values(0))
         return
      end if

      ! The payload was already bounds checked against the size of a file that
      ! really exists, so this allocation is bounded by that size.
      allocate (values(count))
      if (count > 0_int64) then
         read (unit, iostat=ios) values
         call check_io(ios, "short read on int32 payload", err)
         if (swapped .and. ios == 0) then
            do i = 1_int64, count
               values(i) = bswap_int32(values(i))
            end do
         end if
      end if
   end subroutine read_int32_array

   subroutine read_int64_array(unit, values, swapped, err)
      !! Read an `integer(int64)` array record.
      integer(default_int), intent(in) :: unit
      integer(int64), allocatable, intent(out) :: values(:)
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok
      integer(int64) :: count, i

      call read_record_header(unit, PIC_TAG_INT64, swapped, count, ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to read int64 record")
         allocate (values(0))
         return
      end if

      allocate (values(count))
      if (count > 0_int64) then
         read (unit, iostat=ios) values
         call check_io(ios, "short read on int64 payload", err)
         if (swapped .and. ios == 0) then
            do i = 1_int64, count
               values(i) = bswap_int64(values(i))
            end do
         end if
      end if
   end subroutine read_int64_array

   subroutine read_real_sp_array(unit, values, swapped, err)
      !! Read a `real(sp)` array record.
      !!
      !! On the native path the values are read straight into `values`. On the
      !! foreign endian path the same bytes are read into an `int32` buffer,
      !! reversed with integer arithmetic, and only then reinterpreted.
      integer(default_int), intent(in) :: unit
      real(sp), allocatable, intent(out) :: values(:)
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok
      integer(int64) :: count, i
      integer(int32), allocatable :: bits(:)

      call read_record_header(unit, PIC_TAG_REAL_SP, swapped, count, ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to read real(sp) record")
         allocate (values(0))
         return
      end if

      allocate (values(count))
      if (count > 0_int64) then
         if (swapped) then
            allocate (bits(count))
            read (unit, iostat=ios) bits
         else
            read (unit, iostat=ios) values
         end if
         call check_io(ios, "short read on real(sp) payload", err)
         if (swapped .and. ios == 0) then
            do i = 1_int64, count
               values(i) = bits_to_real_sp(bswap_int32(bits(i)))
            end do
         end if
      end if
   end subroutine read_real_sp_array

   subroutine read_real_dp_array(unit, values, swapped, err)
      !! Read a `real(dp)` array record. See `read_real_sp_array` for the way
      !! the foreign endian path is kept away from the native one.
      integer(default_int), intent(in) :: unit
      real(dp), allocatable, intent(out) :: values(:)
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok
      integer(int64) :: count, i
      integer(int64), allocatable :: bits(:)

      call read_record_header(unit, PIC_TAG_REAL_DP, swapped, count, ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to read real(dp) record")
         allocate (values(0))
         return
      end if

      allocate (values(count))
      if (count > 0_int64) then
         if (swapped) then
            allocate (bits(count))
            read (unit, iostat=ios) bits
         else
            read (unit, iostat=ios) values
         end if
         call check_io(ios, "short read on real(dp) payload", err)
         if (swapped .and. ios == 0) then
            do i = 1_int64, count
               values(i) = bits_to_real_dp(bswap_int64(bits(i)))
            end do
         end if
      end if
   end subroutine read_real_dp_array

   subroutine read_char_record(unit, text, swapped, err)
      !! Read a character scalar record into a deferred length string.
      !!
      !! Characters are single bytes so `swapped` never changes the payload,
      !! but it is still needed to decode the record header itself.
      integer(default_int), intent(in) :: unit
      character(len=:), allocatable, intent(out) :: text
      logical, intent(in) :: swapped
      type(error_t), intent(inout) :: err

      integer(default_int) :: ios, code
      logical :: ok
      integer(int64) :: count

      call read_record_header(unit, PIC_TAG_CHAR, swapped, count, ok, err)
      if (.not. ok) then
         code = err%get_code()
         call err%wrap(code, "failed to read character record")
         text = ""
         return
      end if

      allocate (character(len=count) :: text)
      if (count > 0_int64) then
         read (unit, iostat=ios) text
         call check_io(ios, "short read on character payload", err)
      end if
   end subroutine read_char_record

end module pic_serialize
