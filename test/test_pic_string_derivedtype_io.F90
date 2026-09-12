! SPDX-Identifer: MIT
module pic_test_string_derivedtype_io
   use testdrive, only: new_unittest, unittest_type, error_type, check
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
   use pic_string_type, only: string_type, assignment(=), slen, &
                              write (formatted), read (formatted), write (unformatted), read (unformatted), &
                              operator(==), &
                              PIC_IOSTAT_VLIST_OUTPUT, PIC_IOSTAT_DT_INPUT
#else
   use pic_string_type, only: string_type
#endif
   implicit none
   private
   public :: collect_string_derivedtype_io_tests

   !> Number of records written by test_listdirected_many_records
   integer, parameter :: n_records = 5

contains

   !> Collect all exported unit tests
   subroutine collect_string_derivedtype_io_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("listdirected_io", test_listdirected_io), &
                  new_unittest("formatted_io", test_formatted_io), &
                  new_unittest("unformatted_io", test_unformatted_io), &
                  new_unittest("listdirected_long_records", test_listdirected_long_records), &
                  new_unittest("listdirected_trailing_blanks", test_listdirected_trailing_blanks), &
                  new_unittest("unformatted_empty_string", test_unformatted_empty_string), &
                  new_unittest("listdirected_many_records", test_listdirected_many_records), &
                  new_unittest("unsupported_dt_io", test_unsupported_dt_io), &
                  new_unittest("unsupported_namelist_io", test_unsupported_namelist_io) &
                  ]
   end subroutine collect_string_derivedtype_io_tests

   subroutine test_listdirected_io(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io, stat
! LFortran inserts a processor-added leading blank around defined (UDDTIO)
! list-directed output, so the round-tripped value gains a spurious leading
! space. The formatted "(dt)" and unformatted paths below behave correctly,
! so only this list-directed case is excluded (as for NVHPC and Flang).
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG) && !defined(__LFORTRAN__)
      string = "Important saved value"

      open (newunit=io, form="formatted", status="scratch")
      write (io, *) string
      write (io, *)  ! Pad with a newline or we might run into EOF while reading

      string = ""
      rewind (io)

      read (io, *, iostat=stat) string
      close (io)

      call check(error, stat == 0)
      if (allocated(error)) return
      call check(error, slen(string) == 21)
      if (allocated(error)) return
      call check(error, string == "Important saved value")
#endif
   end subroutine test_listdirected_io

   subroutine test_formatted_io(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io, stat
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      string = "Important saved value"

      open (newunit=io, form="formatted", status="scratch")
      write (io, '(dt)') string
      write (io, '(a)')  ! Pad with a newline or we might run into EOF while reading

      string = ""
      rewind (io)

      read (io, *, iostat=stat) string
      close (io)

      call check(error, stat == 0)
      if (allocated(error)) return
      call check(error, slen(string) == 21)
      if (allocated(error)) return
      call check(error, string == "Important saved value")
#endif
   end subroutine test_formatted_io

   subroutine test_unformatted_io(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      string = "Important saved value"

      open (newunit=io, form="unformatted", status="scratch")
      write (io) string

      string = ""
      rewind (io)

      read (io) string
      close (io)

      call check(error, slen(string) == 21)
      if (allocated(error)) return
      call check(error, string == "Important saved value")
#endif
   end subroutine test_unformatted_io

   !> A list-directed read of a string_type reads the whole record through the
   !> defined input procedure, which used to mis-use the cumulative size=
   !> specifier of a child data transfer statement and re-append earlier
   !> chunks. Records on both sides of the internal 512 character buffer, and
   !> exactly on it, must round-trip unchanged.
   subroutine test_listdirected_long_records(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      integer :: k
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      integer, parameter :: sizes(6) = [1, 511, 512, 513, 1300, 2000]

      do k = 1, size(sizes)
         ! The last character differs from the filler so that a truncated or a
         ! duplicated tail is detected, not just a wrong length.
         call check_record_roundtrip(error, repeat("x", sizes(k) - 1)//"Z")
         if (allocated(error)) return
      end do
#endif
   end subroutine test_listdirected_long_records

   !> A non-advancing read blank pads the part of the buffer that the record
   !> did not fill, so the defined input procedure must not use trim to find
   !> the end of the data: blanks that genuinely belong to the record have to
   !> survive, both inside and beyond the internal buffer size.
   subroutine test_listdirected_trailing_blanks(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      call check_record_roundtrip(error, "abc   ")
      if (allocated(error)) return
      call check_record_roundtrip(error, "abc"//repeat(" ", 600))
      if (allocated(error)) return
      ! A record that is blank from the first character to well past the buffer
      ! size: the blanks are data, the padding of the last buffer is not.
      call check_record_roundtrip(error, "Z"//repeat(" ", 511))
      if (allocated(error)) return
      call check_record_roundtrip(error, "Z"//repeat(" ", 512))
#endif
   end subroutine test_listdirected_trailing_blanks

   !> An empty value must round-trip, which is checked through the unformatted
   !> path because the list-directed one cannot reach the defined input
   !> procedure at all for an empty record: a list-directed read treats a blank
   !> record as "no value present" and scans on by itself, so the record never
   !> reaches read(formatted). Processors differ in what that scan does when
   !> the file holds nothing but blank records (gfortran reports end of file;
   !> LLVM flang does not return), which makes a list-directed empty record a
   !> test of the processor rather than of this library. The zero length tail
   !> inside the defined input procedure is covered instead by the record of
   !> exactly 512 characters in test_listdirected_long_records, whose last read
   !> finds the record already exhausted.
   subroutine test_unformatted_empty_string(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      string = ""

      open (newunit=io, form="unformatted", status="scratch")
      write (io) string

      string = "not empty at all"
      rewind (io)

      read (io) string
      close (io)

      call check(error, slen(string) == 0, "an empty string must come back empty")
      if (allocated(error)) return
      call check(error, string == "", "an empty string must compare equal to an empty character")
#endif
   end subroutine test_unformatted_empty_string

   !> Reading repeatedly from one unit must keep returning whole, uncorrupted
   !> records. How many records a list-directed parent read consumes per
   !> statement is processor-dependent: the defined input procedure stops at the
   !> end of the record it read, and what the parent then does while looking for
   !> the end of the value differs (gfortran swallows one further record, ifx
   !> does not). So this does not assert a record per read statement, which
   !> would encode one processor's behaviour; it asserts what any processor must
   !> deliver, namely that every value returned is one of the records that were
   !> written, intact, and that they arrive in file order. A cumulative count
   !> leaking between parent statements, which is how the size= defect showed
   !> itself, produces a value matching no record at all.
   subroutine test_listdirected_many_records(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io, stat, k, seen, previous, current
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      open (newunit=io, form="formatted", status="scratch")
      do k = 1, n_records
         write (io, "(a)") nth_record(k)
      end do
      write (io, "(a)")  ! Pad with a newline or we might run into EOF while reading
      rewind (io)

      seen = 0
      previous = 0
      do k = 1, n_records
         string = ""
         read (io, *, iostat=stat) string
         if (stat /= 0) exit
         current = which_record(string)
         call check(error, current > previous, "a repeated read returned a record out of order or corrupted")
         if (allocated(error)) return
         previous = current
         seen = seen + 1
      end do
      close (io)

      ! The point of the test is that a second read from the same unit still
      ! returns an intact record, so more than one value has to come back. Five
      ! records leave room for a processor that swallows a couple of them per
      ! statement while still requiring the unit to be read more than once.
      call check(error, seen >= 2, "repeated reads from one unit returned fewer than two records")
#endif
   end subroutine test_listdirected_many_records

#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
   !> The k-th record written by test_listdirected_many_records. Several of them
   !> are longer than the 512 character internal buffer of the defined input
   !> procedure, and no two have the same length or filler, so a value that is
   !> short, truncated or spliced together matches none of them.
   function nth_record(k) result(record)
      !> Record number
      integer, intent(in) :: k
      character(len=:), allocatable :: record

      select case (k)
      case (1)
         record = repeat("p", 600)
      case (2)
         record = repeat("q", 70)
      case (3)
         record = repeat("r", 1300)
      case (4)
         record = repeat("s", 80)
      case default
         record = repeat("t", 700)
      end select
   end function nth_record

   !> Which record a value is, or 0 if it is none of them
   function which_record(string) result(k)
      !> Value that was read back
      type(string_type), intent(in) :: string
      integer :: k

      do k = 1, n_records
         if (string == nth_record(k)) return
      end do
      k = 0
   end function which_record
#endif

   !> A defined input/output procedure must report an unsupported request
   !> through iostat and iomsg rather than terminate the program. The dt edit
   !> descriptor cases are dispatched to the defined procedures by every
   !> conforming processor, so the documented PIC iostat values are checked
   !> exactly.
   subroutine test_unsupported_dt_io(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io, stat
      character(len=128) :: msg
#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      string = "Important saved value"

      ! A dt edit descriptor carrying a v-list is not supported on output.
      msg = ""
      stat = 0
      open (newunit=io, form="formatted", status="scratch")
      write (io, "(dt(5))", iostat=stat, iomsg=msg) string
      close (io)

      call check(error, stat == PIC_IOSTAT_VLIST_OUTPUT, "dt output with a v-list must report PIC_IOSTAT_VLIST_OUTPUT")
      if (allocated(error)) return
      call check(error, len_trim(msg) > 0, "dt output with a v-list must explain itself in iomsg")
      if (allocated(error)) return

      ! A dt edit descriptor is not supported on input at all.
      msg = ""
      stat = 0
      open (newunit=io, form="formatted", status="scratch")
      write (io, "(a)") "some text"
      rewind (io)
      read (io, "(dt)", iostat=stat, iomsg=msg) string
      close (io)

      call check(error, stat == PIC_IOSTAT_DT_INPUT, "dt input must report PIC_IOSTAT_DT_INPUT")
      if (allocated(error)) return
      call check(error, len_trim(msg) > 0, "dt input must explain itself in iomsg")
      if (allocated(error)) return
      call check(error, string == "Important saved value", "a failed dt input must leave the string alone")
#endif
   end subroutine test_unsupported_dt_io

   !> Namelist input/output of a string_type is not supported and must fail
   !> through iostat and iomsg instead of aborting. Only a non-zero iostat is
   !> required here, because a processor is free to diagnose the namelist
   !> itself before it ever reaches the defined input/output procedure.
   subroutine test_unsupported_namelist_io(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io, stat
      character(len=128) :: msg
! Namelist input/output of a type that has defined input/output procedures is
! exercised only where the processor itself survives it. Intel ifx corrupts the
! heap inside its own namelist machinery for this type (SIGSEGV in free() on
! Linux, STATUS_HEAP_CORRUPTION on Windows) even though it handles the dt cases
! above correctly, which is why this is an allow-list and not a list of
! exclusions: this test cannot distinguish a processor that lacks namelist
! support from one that is about to corrupt memory. The iostat and iomsg
! contract itself is covered portably by test_unsupported_dt_io.
#if defined(__GFORTRAN__) && !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
      namelist /pic_uddtio_group/ string

      string = "Important saved value"

      msg = ""
      stat = 0
      open (newunit=io, form="formatted", status="scratch")
      write (io, nml=pic_uddtio_group, iostat=stat, iomsg=msg)
      close (io)

      call check(error, stat /= 0, "namelist output of a string_type must fail instead of aborting")
      if (allocated(error)) return
      call check(error, len_trim(msg) > 0, "a failed namelist output must explain itself in iomsg")
      if (allocated(error)) return

      msg = ""
      stat = 0
      open (newunit=io, form="formatted", status="scratch")
      write (io, "(a)") "&pic_uddtio_group string = 'abc' /"
      rewind (io)
      read (io, nml=pic_uddtio_group, iostat=stat, iomsg=msg)
      close (io)

      call check(error, stat /= 0, "namelist input of a string_type must fail instead of aborting")
      if (allocated(error)) return
      call check(error, len_trim(msg) > 0, "a failed namelist input must explain itself in iomsg")
#endif
   end subroutine test_unsupported_namelist_io

#if !defined(__NVCOMPILER_LLVM__) && !defined(__FLANG)
   !> Write one record, read it back list-directed, and require it unchanged
   subroutine check_record_roundtrip(error, record)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      !> Record to round-trip
      character(len=*), intent(in) :: record

      type(string_type) :: string
      integer :: io, stat
      character(len=64) :: what

      write (what, "(a,i0,a)") "record of ", len(record), " characters"

      open (newunit=io, form="formatted", status="scratch")
      write (io, "(a)") record
      write (io, "(a)")  ! Pad with a newline or we might run into EOF while reading
      rewind (io)

      string = ""
      read (io, *, iostat=stat) string
      close (io)

      call check(error, stat == 0, trim(what)//" could not be read")
      if (allocated(error)) return
      call check(error, slen(string) == len(record), trim(what)//" came back with the wrong length")
      if (allocated(error)) return
      call check(error, string == record, trim(what)//" came back with the wrong contents")
   end subroutine check_record_roundtrip
#endif

end module pic_test_string_derivedtype_io
