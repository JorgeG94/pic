! SPDX-Identifier: MIT
module pic_test_strip_chomp
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_ascii, only: TAB, VT, NUL, LF, CR, FF
   use pic_strings, only: strip, chomp
   use pic_string_type, only: string_type, operator(==), operator(//)
   implicit none
   private
   public :: collect_strip_chomp_tests

contains

   !> Collect all exported unit tests
   subroutine collect_strip_chomp_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("strip_char", test_strip_char), &
                  new_unittest("strip_string", test_strip_string), &
                  new_unittest("chomp_char", test_chomp_char), &
                  new_unittest("chomp_string", test_chomp_string), &
                  new_unittest("chomp_set_char", test_chomp_set_char), &
                  new_unittest("chomp_set_string", test_chomp_set_string), &
                  new_unittest("chomp_substring_char", test_chomp_substring_char), &
                  new_unittest("chomp_substring_string", test_chomp_substring_string) &
                  ]
   end subroutine collect_strip_chomp_tests

   subroutine test_strip_char(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, strip("   hello   ") == "hello")
      if (allocated(error)) return
      call check(error, strip(TAB//"goodbye"//CR//LF) == "goodbye")
      if (allocated(error)) return
      call check(error, strip(NUL//TAB//LF//VT//FF//CR) == NUL)
      if (allocated(error)) return
      call check(error, strip(" "//TAB//LF//VT//FF//CR) == "")
      if (allocated(error)) return
      call check(error, strip("  !  ")//"!" == "!!")
      if (allocated(error)) return
      call check(error, strip("Hello") == "Hello")
   end subroutine test_strip_char

   subroutine test_strip_string(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, strip(string_type("   hello   ")) == "hello")
      if (allocated(error)) return
      call check(error, strip(string_type(TAB//"goodbye"//CR//LF)) == "goodbye")
      if (allocated(error)) return
      call check(error, strip(string_type(NUL//TAB//LF//VT//FF//CR)) == NUL)
      if (allocated(error)) return
      call check(error, strip(string_type(" "//TAB//LF//VT//FF//CR)) == "")
      if (allocated(error)) return
      call check(error, strip(string_type("  !  "))//"!" == "!!")
      if (allocated(error)) return
      call check(error, strip(string_type("Hello")) == "Hello")
   end subroutine test_strip_string

   subroutine test_chomp_char(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, chomp("hello") == "hello")
      if (allocated(error)) return
      call check(error, chomp("hello"//LF) == "hello", "1")
      if (allocated(error)) return
      call check(error, chomp("hello"//CR//LF) == "hello", "2")
      if (allocated(error)) return
      call check(error, chomp("hello"//LF//CR) == "hello", "3")
      if (allocated(error)) return
      call check(error, chomp("hello"//CR) == "hello", "4")
      if (allocated(error)) return
      call check(error, chomp("hello "//LF//" there") == "hello "//LF//" there")
      if (allocated(error)) return
      call check(error, chomp("hello"//CR//LF//CR//LF) == "hello")
      if (allocated(error)) return
      call check(error, chomp("hello"//CR//LF//CR//CR//LF) == "hello")
      if (allocated(error)) return
      call check(error, chomp(NUL//TAB//LF//VT//FF//CR) == NUL)
      if (allocated(error)) return
      call check(error, chomp(" "//TAB//LF//VT//FF//CR) == "")
      if (allocated(error)) return
      call check(error, chomp("  !  ")//"!" == "  !!")
   end subroutine test_chomp_char

   subroutine test_chomp_string(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, chomp(string_type("hello")) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"//LF)) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"//CR//LF)) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"//LF//CR)) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"//CR)) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello "//LF//" there")) == "hello "//LF//" there")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"//CR//LF//CR//LF)) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"//CR//LF//CR//CR//LF)) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type(NUL//TAB//LF//VT//FF//CR)) == NUL)
      if (allocated(error)) return
      call check(error, chomp(string_type(" "//TAB//LF//VT//FF//CR)) == "")
      if (allocated(error)) return
      call check(error, chomp(string_type("  !  "))//"!" == "  !!")
   end subroutine test_chomp_string

   subroutine test_chomp_set_char(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, chomp("hello", ["l", "o"]) == "he")
      if (allocated(error)) return
      call check(error, chomp("hello", set=["l", "o"]) == "he")
   end subroutine test_chomp_set_char

   subroutine test_chomp_set_string(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, chomp(string_type("hello"), ["l", "o"]) == "he")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"), set=["l", "o"]) == "he")
      if (allocated(error)) return
      call check(error, chomp("hellooooo", ["o", "o"]) == "hell")
      if (allocated(error)) return
      call check(error, chomp("hellooooo", set=["o", "o"]) == "hell")
   end subroutine test_chomp_set_string

   subroutine test_chomp_substring_char(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, chomp("hello", "") == "hello")
      if (allocated(error)) return
      call check(error, chomp("hello", substring="") == "hello")
      if (allocated(error)) return
      call check(error, chomp("hello", "lo") == "hel")
      if (allocated(error)) return
      call check(error, chomp("hello", substring="lo") == "hel")
      if (allocated(error)) return
      call check(error, chomp("hellooooo", "oo") == "hello")
      if (allocated(error)) return
      call check(error, chomp("hellooooo", substring="oo") == "hello")
      if (allocated(error)) return
      call check(error, chomp("helhel", substring="hel") == "")
   end subroutine test_chomp_substring_char

   subroutine test_chomp_substring_string(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      call check(error, chomp(string_type("hello"), "") == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"), substring="") == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"), "lo") == "hel")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"), substring="lo") == "hel")
      if (allocated(error)) return
      call check(error, chomp("hello", string_type("lo")) == "hel")
      if (allocated(error)) return
      call check(error, chomp("hello", substring=string_type("lo")) == "hel")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"), string_type("lo")) == "hel")
      if (allocated(error)) return
      call check(error, chomp(string_type("hello"), substring=string_type("lo")) == "hel")
      if (allocated(error)) return
      call check(error, chomp(string_type("hellooooo"), "oo") == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hellooooo"), substring="oo") == "hello")
      if (allocated(error)) return
      call check(error, chomp("hellooooo", string_type("oo")) == "hello")
      if (allocated(error)) return
      call check(error, chomp("hellooooo", substring=string_type("oo")) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hellooooo"), string_type("oo")) == "hello")
      if (allocated(error)) return
      call check(error, chomp(string_type("hellooooo"), substring=string_type("oo")) == "hello")
   end subroutine test_chomp_substring_string

end module pic_test_strip_chomp
