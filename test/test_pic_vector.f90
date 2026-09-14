! SPDX-Identifer: MIT
module test_pic_vector
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64, dp
   use pic_string_type, only: string_type, char, assignment(=), operator(==)
   use pic_error, only: error_t, ERROR_ALLOC, ERROR_BOUNDS, ERROR_VALIDATION
   use pic_vector, only: vector_int32_t, vector_int64_t, vector_dp_t, vector_string_t, &
                         PIC_VECTOR_MIN_CAPACITY
   implicit none
   private
   public :: collect_pic_vector_tests

contains

   subroutine collect_pic_vector_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("empty-vector", test_empty), &
                  new_unittest("push-and-read", test_push_and_read), &
                  new_unittest("growth-boundaries", test_growth_boundaries), &
                  new_unittest("soak-100k", test_soak), &
                  new_unittest("pop-back", test_pop_back), &
                  new_unittest("pop-empty-is-bounds-error", test_pop_empty), &
                  new_unittest("at-out-of-range", test_at_out_of_range), &
                  new_unittest("set", test_set), &
                  new_unittest("append-bulk", test_append), &
                  new_unittest("reserve-keeps-size", test_reserve), &
                  new_unittest("resize", test_resize), &
                  new_unittest("clear-keeps-capacity", test_clear), &
                  new_unittest("shrink-to-fit", test_shrink), &
                  new_unittest("as-array-is-exact", test_as_array), &
                  new_unittest("take-moves-storage", test_take), &
                  new_unittest("take-on-empty", test_take_empty), &
                  new_unittest("destroy", test_destroy), &
                  new_unittest("string-vector-survives-growth", test_string_growth), &
                  new_unittest("int64-and-dp-elements", test_other_kinds), &
                  new_unittest("callable-from-pure", test_pure_guard) &
                  ]
   end subroutine collect_pic_vector_tests

   subroutine test_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      call check(error, v%size() == 0, "a fresh vector has size 0")
      if (allocated(error)) return
      call check(error, v%capacity() == 0, "and no capacity until first use")
      if (allocated(error)) return
      call check(error, v%is_empty(), "and reports empty")
      if (allocated(error)) return
   end subroutine test_empty

   subroutine test_push_and_read(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got
      integer(default_int) :: i

      do i = 1, 5
         call v%push_back(int(i*10, int32), err)
      end do
      call check(error, .not. err%has_error(), "five pushes succeed")
      if (allocated(error)) return
      call check(error, v%size() == 5, "size is 5")
      if (allocated(error)) return
      call v%at(3_default_int, got, err)
      call check(error, got == 30_int32, "at(3) is the third value pushed")
      if (allocated(error)) return
      call check(error, v%get_unchecked(5_default_int) == 50_int32, "get_unchecked agrees")
      if (allocated(error)) return
   end subroutine test_push_and_read

   !> Crossing each doubling is where an off-by-one in the growth policy shows
   !> up, so every boundary is checked rather than a round number.
   subroutine test_growth_boundaries(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(default_int) :: i
      logical :: values_intact
      integer(int32) :: got

      do i = 1, 40
         call v%push_back(int(i, int32), err)
         if (v%capacity() < v%size()) then
            call check(error, .false., "capacity fell below size during growth")
            return
         end if
      end do
      call check(error, v%size() == 40, "40 elements after 40 pushes")
      if (allocated(error)) return
      call check(error, v%capacity() >= 40, "capacity covers them")
      if (allocated(error)) return

      ! first push must jump straight to the minimum capacity, not to 1
      block
         type(vector_int32_t) :: w
         call w%push_back(1_int32, err)
         call check(error, w%capacity() == PIC_VECTOR_MIN_CAPACITY, &
                    "first push allocates PIC_VECTOR_MIN_CAPACITY")
      end block
      if (allocated(error)) return

      values_intact = .true.
      do i = 1, 40
         call v%at(i, got, err)
         if (got /= int(i, int32)) values_intact = .false.
      end do
      call check(error, values_intact, "every element survives the reallocations")
      if (allocated(error)) return
   end subroutine test_growth_boundaries

   subroutine test_soak(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int64_t) :: v
      type(error_t) :: err
      integer(default_int) :: i
      integer(int64) :: got

      do i = 1, 100000
         call v%push_back(int(i, int64), err)
      end do
      call check(error, .not. err%has_error(), "100k pushes without error")
      if (allocated(error)) return
      call check(error, v%size() == 100000, "size is 100000")
      if (allocated(error)) return
      call v%at(100000_default_int, got, err)
      call check(error, got == 100000_int64, "last element is correct")
      if (allocated(error)) return
      call v%at(1_default_int, got, err)
      call check(error, got == 1_int64, "first element survived every move")
      if (allocated(error)) return
   end subroutine test_soak

   subroutine test_pop_back(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got
      integer(default_int) :: cap_before

      call v%push_back(7_int32, err)
      call v%push_back(9_int32, err)
      cap_before = v%capacity()
      call v%pop_back(got, err)
      call check(error, got == 9_int32, "pop returns the last element")
      if (allocated(error)) return
      call check(error, v%size() == 1, "size drops")
      if (allocated(error)) return
      call check(error, v%capacity() == cap_before, "pop does not release capacity")
      if (allocated(error)) return
   end subroutine test_pop_back

   subroutine test_pop_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got

      call v%pop_back(got, err)
      call check(error, err%is(ERROR_BOUNDS), "pop_back on empty is ERROR_BOUNDS")
      if (allocated(error)) return
      call check(error, v%size() == 0, "and leaves the vector unchanged")
      if (allocated(error)) return
   end subroutine test_pop_empty

   subroutine test_at_out_of_range(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got

      call v%push_back(1_int32, err)
      call err%clear()
      call v%at(0_default_int, got, err)
      call check(error, err%is(ERROR_BOUNDS), "at(0) is ERROR_BOUNDS")
      if (allocated(error)) return
      call err%clear()
      call v%at(2_default_int, got, err)
      call check(error, err%is(ERROR_BOUNDS), "at(size+1) is ERROR_BOUNDS")
      if (allocated(error)) return
      call check(error, v%size() == 1, "and the vector is unchanged")
      if (allocated(error)) return
   end subroutine test_at_out_of_range

   subroutine test_set(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got

      call v%push_back(1_int32, err)
      call v%set(1_default_int, 99_int32, err)
      call v%at(1_default_int, got, err)
      call check(error, got == 99_int32, "set overwrites in place")
      if (allocated(error)) return
      call err%clear()
      call v%set(2_default_int, 5_int32, err)
      call check(error, err%is(ERROR_BOUNDS), "set past the end is ERROR_BOUNDS, not an append")
      if (allocated(error)) return
      call check(error, v%size() == 1, "and does not extend the vector")
      if (allocated(error)) return
   end subroutine test_set

   subroutine test_append(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got

      call v%push_back(1_int32, err)
      call v%append([2_int32, 3_int32, 4_int32], err)
      call check(error, v%size() == 4, "append adds every element")
      if (allocated(error)) return
      call v%at(4_default_int, got, err)
      call check(error, got == 4_int32, "in order, after the existing ones")
      if (allocated(error)) return
      call v%append([integer(int32) ::], err)
      call check(error, v%size() == 4, "appending nothing is a no-op")
      if (allocated(error)) return
   end subroutine test_append

   subroutine test_reserve(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err

      call v%reserve(1000_default_int, err)
      call check(error, v%capacity() >= 1000, "reserve raises capacity")
      if (allocated(error)) return
      call check(error, v%size() == 0, "and leaves size alone")
      if (allocated(error)) return
   end subroutine test_reserve

   subroutine test_resize(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got

      call v%resize(3_default_int, -1_int32, err)
      call check(error, v%size() == 3, "resize up sets the length")
      if (allocated(error)) return
      call v%at(2_default_int, got, err)
      call check(error, got == -1_int32, "new elements take the fill value")
      if (allocated(error)) return
      call v%resize(1_default_int, 0_int32, err)
      call check(error, v%size() == 1, "resize down truncates")
      if (allocated(error)) return
      call err%clear()
      call v%resize(-5_default_int, 0_int32, err)
      call check(error, err%is(ERROR_VALIDATION), "a negative length is ERROR_VALIDATION")
      if (allocated(error)) return
   end subroutine test_resize

   subroutine test_clear(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(default_int) :: cap

      call v%append([1_int32, 2_int32, 3_int32], err)
      cap = v%capacity()
      call v%clear()
      call check(error, v%size() == 0, "clear empties")
      if (allocated(error)) return
      call check(error, v%capacity() == cap, "but keeps the storage, which is the point")
      if (allocated(error)) return
   end subroutine test_clear

   subroutine test_shrink(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32) :: got

      call v%reserve(500_default_int, err)
      call v%append([1_int32, 2_int32], err)
      call v%shrink_to_fit(err)
      call check(error, v%capacity() == 2, "capacity drops to the live length")
      if (allocated(error)) return
      call v%at(2_default_int, got, err)
      call check(error, got == 2_int32, "elements survive the shrink")
      if (allocated(error)) return
   end subroutine test_shrink

   subroutine test_as_array(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32), allocatable :: a(:)

      call v%reserve(100_default_int, err)
      call v%append([5_int32, 6_int32], err)
      a = v%as_array()
      call check(error, size(a) == 2, "as_array returns exactly size() elements, not capacity")
      if (allocated(error)) return
      call check(error, a(1) == 5_int32 .and. a(2) == 6_int32, "with the right values")
      if (allocated(error)) return
      call check(error, v%size() == 2, "and leaves the vector intact")
      if (allocated(error)) return
   end subroutine test_as_array

   subroutine test_take(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32), allocatable :: a(:)

      call v%reserve(1000_default_int, err)      ! lots of spare capacity
      call v%append([1_int32, 2_int32, 3_int32], err)
      call v%take(a, err)
      call check(error, allocated(a), "take yields an allocated array")
      if (allocated(error)) return
      call check(error, size(a) == 3, "sized to the live elements, not the capacity")
      if (allocated(error)) return
      call check(error, a(3) == 3_int32, "with the right contents")
      if (allocated(error)) return
      call check(error, v%size() == 0, "and the vector is left empty")
      if (allocated(error)) return
      call check(error, v%capacity() == 0, "with its storage handed over, not copied")
      if (allocated(error)) return
   end subroutine test_take

   subroutine test_take_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err
      integer(int32), allocatable :: a(:)

      call v%take(a, err)
      call check(error, .not. err%has_error(), "taking from a never-used vector is not an error")
      if (allocated(error)) return
      call check(error, allocated(a), "the result is allocated")
      if (allocated(error)) return
      call check(error, size(a) == 0, "and zero-sized")
      if (allocated(error)) return
   end subroutine test_take_empty

   subroutine test_destroy(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int32_t) :: v
      type(error_t) :: err

      call v%append([1_int32, 2_int32], err)
      call v%destroy()
      call check(error, v%size() == 0 .and. v%capacity() == 0, "destroy releases everything")
      if (allocated(error)) return
      call v%push_back(1_int32, err)
      call check(error, v%size() == 1, "and the vector is reusable afterwards")
      if (allocated(error)) return
   end subroutine test_destroy

   !> string_type has an allocatable component, so growth moves elements that
   !> own heap storage. This is where a shallow copy would corrupt things.
   subroutine test_string_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_string_t) :: v
      type(error_t) :: err
      type(string_type) :: s, got
      integer(default_int) :: i
      logical :: intact

      do i = 1, 50
         s = item_name(i)
         call v%push_back(s, err)
      end do
      call check(error, v%size() == 50, "50 strings pushed across several reallocations")
      if (allocated(error)) return

      intact = .true.
      do i = 1, 50
         call v%at(i, got, err)
         if (char(got) /= item_name(i)) intact = .false.
      end do
      call check(error, intact, "every string still holds its own characters")
      if (allocated(error)) return

      call v%at(1_default_int, got, err)
      call check(error, len(char(got)) == 6, "and its own length")
      if (allocated(error)) return
   end subroutine test_string_growth

   subroutine test_other_kinds(error)
      type(error_type), allocatable, intent(out) :: error
      type(vector_int64_t) :: vi
      type(vector_dp_t) :: vd
      type(error_t) :: err
      integer(int64) :: gi
      real(dp) :: gd

      call vi%push_back(huge(0_int64), err)
      call vi%at(1_default_int, gi, err)
      call check(error, gi == huge(0_int64), "int64 elements keep their full range")
      if (allocated(error)) return

      call vd%push_back(1.5_dp, err)
      call vd%at(1_default_int, gd, err)
      call check(error, gd == 1.5_dp, "dp elements round-trip")
      if (allocated(error)) return
   end subroutine test_other_kinds

   !> A compile-time guard: if any vector operation stopped being pure, this
   !> procedure would fail to compile rather than silently losing the property.
   subroutine test_pure_guard(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: total

      total = pure_vector_user()
      call check(error, total == 6_int32, "a pure procedure can build and read a vector")
      if (allocated(error)) return
   end subroutine test_pure_guard

   pure function pure_vector_user() result(total)
      integer(int32) :: total
      type(vector_int32_t) :: v
      integer(int32), allocatable :: a(:)
      integer(default_int) :: i

      do i = 1, 3
         call v%push_back(int(i, int32))
      end do
      call v%take(a)
      total = 0_int32
      do i = 1, size(a, kind=default_int)
         total = total + a(i)
      end do
      call v%destroy()
   end function pure_vector_user

   !> Six characters, varying with `i`, built without mixing integer kinds so
   !> that the test compiles under both PIC_DEFAULT_INT8 settings.
   pure function item_name(i) result(s)
      integer(default_int), intent(in) :: i
      character(len=6) :: s

      s = "item-"//achar(iachar("0") + int(mod(i, 10_default_int), int32))
   end function item_name

end module test_pic_vector
