! SPDX-Identifer: MIT
module pic_test_string_derivedtype_io
   use testdrive, only: new_unittest, unittest_type, error_type, check
#ifndef __NVCOMPILER_LLVM__
   use pic_stdlib_string_type, only: string_type, assignment(=), slen, &
                                     write (formatted), read (formatted), write (unformatted), read (unformatted), &
                                     operator(==)
#else
   use pic_stdlib_string_type, only: string_type
#endif
   implicit none
   private
   public :: collect_string_derivedtype_io_tests

contains

   !> Collect all exported unit tests
   subroutine collect_string_derivedtype_io_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("listdirected_io", test_listdirected_io), &
                  new_unittest("formatted_io", test_formatted_io), &
                  new_unittest("unformatted_io", test_unformatted_io) &
                  ]
   end subroutine collect_string_derivedtype_io_tests

   subroutine test_listdirected_io(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(string_type) :: string
      integer :: io, stat
#ifndef __NVCOMPILER_LLVM__
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
#ifndef __NVCOMPILER_LLVM__
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
#ifndef __NVCOMPILER_LLVM__
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

end module pic_test_string_derivedtype_io
