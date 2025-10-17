module test_pic_string_type
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_string_mod, only: pic_string_type, operator(==), operator(/=)
   use pic_types, only: int64, dp
   implicit none
   private

   public :: collect_pic_string_type_tests

contains

   subroutine collect_pic_string_type_tests(testsuite2)
      type(unittest_type), allocatable, intent(out) :: testsuite2(:)
      testsuite2 = [ &
                   new_unittest("test_pic_string_type_basic", test_pic_string_type_basic), &
                   new_unittest("test_pic_string_type_empty", test_pic_string_type_empty), &
                   new_unittest("test_pic_string_type_assign", test_pic_string_type_assign), &
                   new_unittest("test_pic_string_type_append_to_append", test_pic_string_type_append_to_append), &
                   new_unittest("test_pic_string_type_basic_ops_assign", test_pic_string_type_basic_ops_assign), &
                   new_unittest("test_pic_string_type_push_back_chars", test_pic_string_type_push_back_chars), &
                   new_unittest("test_pic_string_type_reserve_growth", test_pic_string_type_reserve_growth), &
                   new_unittest("test_pic_string_type_clear_keeps_cap", test_pic_string_type_clear_keeps_cap), &
                   new_unittest("test_pic_string_type_to_char_roundtrip", test_pic_string_type_to_char_roundtrip), &
                   new_unittest("test_pic_string_type_trim_basic", test_pic_string_type_trim_basic), &
                   new_unittest("test_pic_string_type_trim_edge_cases", test_pic_string_type_trim_edge_cases), &
                   new_unittest("test_pic_string_type_starts_ends", test_pic_string_type_starts_ends), &
                   new_unittest("test_pic_string_type_find_basic", test_pic_string_type_find_basic), &
                   new_unittest("test_pic_string_type_find_with_from", test_pic_string_type_find_with_from), &
                   new_unittest("test_pic_string_type_find_not_found", test_pic_string_type_find_not_found), &
                   new_unittest("test_pic_string_type_substr_basic", test_pic_string_type_substr_basic), &
                   new_unittest("test_pic_string_type_substr_bounds", test_pic_string_type_substr_bounds), &
                   new_unittest("test_pic_string_type_equality_ops", test_pic_string_type_equality_ops) &
                   ]
   end subroutine collect_pic_string_type_tests

   subroutine test_pic_string_type_basic(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s

      call check(error, s%size() == 0_int64, "Initial size should be 0")
      if (allocated(error)) return

      call check(error, s%capacity() == 0_int64, "Initial capacity should be 0")
      if (allocated(error)) return

   end subroutine test_pic_string_type_basic

   subroutine test_pic_string_type_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

   end subroutine test_pic_string_type_empty

   subroutine test_pic_string_type_assign(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

      call s%assign("Hello")
      call check(error,.not. s%empty(), "String should not be empty after assign")
      if (allocated(error)) return

   end subroutine test_pic_string_type_assign

   subroutine test_pic_string_type_basic_ops_assign(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      character(len=:), allocatable :: c

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

      call s%assign("Hello")
      call check(error,.not. s%empty(), "String should not be empty after assign")
      if (allocated(error)) return

      call check(error, s%size() == 5_int64, "Size should be 5 after assign")
      if (allocated(error)) return

   end subroutine test_pic_string_type_basic_ops_assign

   subroutine test_pic_string_type_append_to_append(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      character(len=:), allocatable :: c

      call check(error, s%empty(), "String should be empty initially")
      if (allocated(error)) return

      call s%assign("Hello")
      call check(error,.not. s%empty(), "String should not be empty after assign")
      if (allocated(error)) return

      call check(error, s%size() == 5_int64, "Size should be 5 after assign")
      if (allocated(error)) return

      call s%append(", World!")
      call check(error, s%size() == 13_int64, "Size should be 13 after append")
      if (allocated(error)) return

   end subroutine test_pic_string_type_append_to_append

   subroutine test_pic_string_type_push_back_chars(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      call s%push_back('A')
      call s%push_back('B')
      call s%push_back('C')
      call check(error, s%size() == 3, "size after three push_back")
      if (allocated(error)) return
      call check(error, s%to_char() == "ABC", "content after three push_back")
   end subroutine test_pic_string_type_push_back_chars

   subroutine test_pic_string_type_reserve_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      integer :: cap_before, cap_after
      call s%reserve(10_int64)
      cap_before = s%capacity()
      call check(error, cap_before >= 10_int64, "reserve(10) should give >=10 capacity")
      if (allocated(error)) return
      call s%append("0123456789")  ! length 10 exactly
      call check(error, s%size() == 10_int64, "size should be 10 after append")
      if (allocated(error)) return
      cap_after = s%capacity()
      call check(error, cap_after >= cap_before, "capacity should not shrink during append")
      if (allocated(error)) return
      call s%append("X")  ! trigger growth if cap == 10
      call check(error, s%size() == 11, "size should be 11 after one more char")
      if (allocated(error)) return
      call check(error, s%to_char() == "0123456789X", "content after growth-append")
   end subroutine test_pic_string_type_reserve_growth

   subroutine test_pic_string_type_clear_keeps_cap(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      integer :: cap_before
      call s%assign("some text")
      cap_before = s%capacity()
      call s%clear()
      call check(error, s%size() == 0, "clear sets size to 0")
      if (allocated(error)) return
      call check(error, s%capacity() == cap_before, "clear should keep capacity")
      if (allocated(error)) return
      call s%append("x")
      call check(error, s%to_char() == "x", "content valid after clear+append")
   end subroutine test_pic_string_type_clear_keeps_cap

   subroutine test_pic_string_type_to_char_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      character(:), allocatable :: c
      call s%assign("roundtrip")
      c = s%to_char()
      call check(error, c == "roundtrip", "to_char returns identical contents")
   end subroutine test_pic_string_type_to_char_roundtrip

   subroutine test_pic_string_type_trim_basic(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      call s%assign("  abc  ")
      call s%ltrim()
      call check(error, s%to_char() == "abc  ", "ltrim failed")
      if (allocated(error)) return
      call s%assign("  abc  ")
      call s%rtrim()
      call check(error, s%to_char() == "  abc", "rtrim failed")
      if (allocated(error)) return
      call s%assign("  abc  ")
      call s%trim()
      call check(error, s%to_char() == "abc", "trim failed")
   end subroutine test_pic_string_type_trim_basic

   subroutine test_pic_string_type_trim_edge_cases(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      ! empty stays empty
      call s%assign("")
      call s%trim()
      call check(error, s%size() == 0, "trim(empty) should remain empty")
      if (allocated(error)) return
      ! all spaces -> empty
      call s%assign("     ")
      call s%trim()
      call check(error, s%size() == 0, "trim(all-spaces) should become empty")
   end subroutine test_pic_string_type_trim_edge_cases

   subroutine test_pic_string_type_starts_ends(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      call s%assign("foobar")
      call check(error, s%starts_with("foo"), "starts_with('foo') should be true")
      if (allocated(error)) return
      call check(error, s%ends_with("bar"), "ends_with('bar') should be true")
      if (allocated(error)) return
      call check(error, s%starts_with("foobar"), "starts_with equal whole string")
      if (allocated(error)) return
      call check(error, s%ends_with("foobar"), "ends_with equal whole string")
      if (allocated(error)) return
      ! empty pattern is always true
      call check(error, s%starts_with(""), "starts_with('') should be true")
      if (allocated(error)) return
      call check(error, s%ends_with(""), "ends_with('') should be true")
      if (allocated(error)) return
      ! negatives
      call check(error,.not. s%starts_with("bar"), "starts_with('bar') false")
      if (allocated(error)) return
      call check(error,.not. s%ends_with("foo"), "ends_with('foo') false")
   end subroutine test_pic_string_type_starts_ends

   subroutine test_pic_string_type_find_basic(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      integer(kind=dp) :: p
      call s%assign("abracadabra")
      p = s%find("ra")
      call check(error, p == 3_dp, "find('ra') should be 3")
      if (allocated(error)) return
      p = s%find("abra")     ! first occurrence
      call check(error, p == 1_dp, "find('abra') should be 1")
   end subroutine test_pic_string_type_find_basic

   subroutine test_pic_string_type_find_with_from(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      integer(kind=dp) :: p
      call s%assign("abracadabra")
      p = s%find("ra", from=4_dp)   ! next 'ra' after index 4
      print *, "DEBUG: p =", p
      call check(error, p == 10_dp, "find('ra', from=4) should be 10")
      if (allocated(error)) return
      p = s%find("abra", from=2_dp)
      call check(error, p == 8_dp, "find('abra', from=2) should be 8")
   end subroutine test_pic_string_type_find_with_from

   subroutine test_pic_string_type_find_not_found(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s
      integer(kind=dp) :: p
      call s%assign("hello")
      p = s%find("xyz")
      call check(error, p == 0_dp, "not found should be 0")
      if (allocated(error)) return
      p = s%find("", from=1_dp)     ! empty pattern => our API returns 0
      call check(error, p == 0_dp, "empty pattern returns 0 by design")
   end subroutine test_pic_string_type_find_not_found

   subroutine test_pic_string_type_substr_basic(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s, t
      call s%assign("abcdefg")
      t = s%substr(4_dp, 3_dp)  ! 1-based: 'd','e','f'
      call check(error, t%to_char() == "def", "substr(4,3) should be 'def'")
   end subroutine test_pic_string_type_substr_basic

   subroutine test_pic_string_type_substr_bounds(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: s, t
      call s%assign("abcdefg")
      t = s%substr(100_dp, 5_dp)
      call check(error, t%size() == 0, "substr out-of-range should be empty")
      if (allocated(error)) return
      t = s%substr(3_dp, 0_dp)
      call check(error, t%size() == 0, "substr with length 0 should be empty")
      if (allocated(error)) return
      t = s%substr(6_dp, 10_dp)
      call check(error, t%to_char() == "fg", "substr clamps to end")
   end subroutine test_pic_string_type_substr_bounds

   subroutine test_pic_string_type_equality_ops(error)
      type(error_type), allocatable, intent(out) :: error
      type(pic_string_type) :: a, b
      call a%assign("foo")
      call b%assign("foo")
      call check(error, a == b, "a == b should be true")
      if (allocated(error)) return
      call check(error, a == "foo", "a == 'foo' should be true")
      if (allocated(error)) return
      call check(error, "foo" == a, "'foo' == a should be true")
      if (allocated(error)) return
      call b%append("bar")
      call check(error, a /= b, "a /= b after change")
      if (allocated(error)) return
      call check(error, a /= "foobar", "a /= 'foobar' should be true")
   end subroutine test_pic_string_type_equality_ops

end module test_pic_string_type
