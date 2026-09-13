module test_pic_fixed_array
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, dp
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_fixed_array, only: fixed_array_int_t, fixed_array_dp_t, &
                              PIC_FIXED_ARRAY_CAPACITY
   implicit none
   private
   public :: collect_pic_fixed_array_tests

   real(dp), parameter :: TOL = 1.0e-12_dp

contains

   subroutine collect_pic_fixed_array_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("int_empty_state", test_int_empty_state), &
                  new_unittest("int_push_lifo", test_int_push_lifo), &
                  new_unittest("int_overflow", test_int_overflow), &
                  new_unittest("int_underflow", test_int_underflow), &
                  new_unittest("int_at_bounds", test_int_at_bounds), &
                  new_unittest("int_as_array", test_int_as_array), &
                  new_unittest("int_clear_reuse", test_int_clear_reuse), &
                  new_unittest("int_get_unchecked", test_int_get_unchecked), &
                  new_unittest("err_is_inout", test_err_is_inout), &
                  new_unittest("dp_empty_state", test_dp_empty_state), &
                  new_unittest("dp_push_lifo", test_dp_push_lifo), &
                  new_unittest("dp_overflow", test_dp_overflow), &
                  new_unittest("dp_underflow", test_dp_underflow), &
                  new_unittest("dp_at_bounds", test_dp_at_bounds), &
                  new_unittest("dp_as_array", test_dp_as_array), &
                  new_unittest("dp_clear_reuse", test_dp_clear_reuse), &
                  new_unittest("dp_get_unchecked", test_dp_get_unchecked) &
                  ]
   end subroutine collect_pic_fixed_array_tests

   !> integer flavour

   subroutine test_int_empty_state(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr

      call check(error, arr%size() == 0_default_int, "fresh array must be size 0")
      if (allocated(error)) return
      call check(error, arr%capacity() == PIC_FIXED_ARRAY_CAPACITY, "capacity must be the module parameter")
      if (allocated(error)) return
      call check(error, arr%is_empty(), "fresh array must be empty")
      if (allocated(error)) return
      call check(error,.not. arr%is_full(), "fresh array must not be full")
      if (allocated(error)) return
      call check(error, size(arr%as_array()) == 0, "as_array on empty must be zero length")
      if (allocated(error)) return
   end subroutine test_int_empty_state

   subroutine test_int_push_lifo(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i, value

      do i = 1_default_int, 5_default_int
         call arr%push_back(10_default_int*i, err)
         call check(error,.not. err%has_error(), "push_back below capacity must succeed")
         if (allocated(error)) return
      end do

      call check(error, arr%size() == 5_default_int, "size must be 5 after 5 pushes")
      if (allocated(error)) return
      call check(error,.not. arr%is_empty(), "array with elements is not empty")
      if (allocated(error)) return

      do i = 5_default_int, 1_default_int, -1_default_int
         call arr%pop_back(value, err)
         call check(error,.not. err%has_error(), "pop_back on non-empty must succeed")
         if (allocated(error)) return
         call check(error, value == 10_default_int*i, "pop_back must return elements in LIFO order")
         if (allocated(error)) return
      end do

      call check(error, arr%is_empty(), "array must be empty after popping everything")
      if (allocated(error)) return
   end subroutine test_int_push_lifo

   subroutine test_int_overflow(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i

      do i = 1_default_int, PIC_FIXED_ARRAY_CAPACITY
         call arr%push_back(i, err)
         call check(error,.not. err%has_error(), "filling exactly to capacity must succeed")
         if (allocated(error)) return
      end do

      call check(error, arr%is_full(), "array must report full at capacity")
      if (allocated(error)) return
      call check(error, arr%size() == PIC_FIXED_ARRAY_CAPACITY, "size must equal capacity")
      if (allocated(error)) return

      call arr%push_back(-1_default_int, err)
      call check(error, err%has_error(), "push_back past capacity must set the error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "overflow must be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, arr%size() == PIC_FIXED_ARRAY_CAPACITY, "size must be unchanged after overflow")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0, "overflow error must carry a message")
      if (allocated(error)) return

      call check(error, arr%get_unchecked(PIC_FIXED_ARRAY_CAPACITY) == PIC_FIXED_ARRAY_CAPACITY, &
                 "last element must not have been overwritten")
      if (allocated(error)) return
   end subroutine test_int_overflow

   subroutine test_int_underflow(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int) :: value

      call arr%push_back(7_default_int, err)
      call arr%pop_back(value, err)
      call check(error, value == 7_default_int, "pop must return the pushed value")
      if (allocated(error)) return
      call check(error, arr%is_empty(), "array must be empty after popping the only element")
      if (allocated(error)) return

      call arr%pop_back(value, err)
      call check(error, err%has_error(), "pop_back on empty must set the error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "underflow must be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, value == 0_default_int, "underflow must zero the output value")
      if (allocated(error)) return
      call check(error, arr%size() == 0_default_int, "size must stay 0 after underflow")
      if (allocated(error)) return
   end subroutine test_int_underflow

   subroutine test_int_at_bounds(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i, value

      do i = 1_default_int, 3_default_int
         call arr%push_back(100_default_int + i, err)
      end do

      call arr%at(1_default_int, value, err)
      call check(error,.not. err%has_error(), "at(1) must succeed")
      if (allocated(error)) return
      call check(error, value == 101_default_int, "at(1) must return the first element")
      if (allocated(error)) return

      call arr%at(arr%size(), value, err)
      call check(error,.not. err%has_error(), "at(size) must succeed")
      if (allocated(error)) return
      call check(error, value == 103_default_int, "at(size) must return the last element")
      if (allocated(error)) return

      call arr%at(0_default_int, value, err)
      call check(error, err%is(ERROR_VALIDATION), "at(0) must be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, value == 0_default_int, "at(0) must zero the output value")
      if (allocated(error)) return

      call arr%at(arr%size() + 1_default_int, value, err)
      call check(error, err%is(ERROR_VALIDATION), "at(size+1) must be ERROR_VALIDATION")
      if (allocated(error)) return

      call arr%at(PIC_FIXED_ARRAY_CAPACITY + 1_default_int, value, err)
      call check(error, err%is(ERROR_VALIDATION), "at(capacity+1) must be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, len(err%get_message()) > 0, "out of range error must carry a message")
      if (allocated(error)) return
   end subroutine test_int_at_bounds

   subroutine test_int_as_array(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int), allocatable :: plain(:)
      integer(default_int) :: i

      plain = arr%as_array()
      call check(error, size(plain) == 0, "as_array on empty must have zero extent")
      if (allocated(error)) return

      do i = 1_default_int, 4_default_int
         call arr%push_back(i*i, err)
      end do

      plain = arr%as_array()
      call check(error, size(plain) == 4, "as_array must return size() elements, not capacity()")
      if (allocated(error)) return
      call check(error, all(plain == [1_default_int, 4_default_int, 9_default_int, 16_default_int]), &
                 "as_array must return the live elements in order")
      if (allocated(error)) return
      call check(error, sum(plain) == 30_default_int, "as_array contents must be usable by plain routines")
      if (allocated(error)) return
   end subroutine test_int_as_array

   subroutine test_int_clear_reuse(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i, value

      do i = 1_default_int, 6_default_int
         call arr%push_back(i, err)
      end do
      call arr%clear()

      call check(error, arr%size() == 0_default_int, "clear must reset the size")
      if (allocated(error)) return
      call check(error, arr%is_empty(), "clear must leave the array empty")
      if (allocated(error)) return
      call check(error, size(arr%as_array()) == 0, "as_array after clear must be zero length")
      if (allocated(error)) return

      call arr%push_back(99_default_int, err)
      call check(error,.not. err%has_error(), "array must be reusable after clear")
      if (allocated(error)) return
      call arr%at(1_default_int, value, err)
      call check(error, value == 99_default_int, "reused array must hold the new element")
      if (allocated(error)) return
      call check(error, arr%size() == 1_default_int, "reused array must have size 1")
      if (allocated(error)) return
   end subroutine test_int_clear_reuse

   subroutine test_int_get_unchecked(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i, total

      do i = 1_default_int, 8_default_int
         call arr%push_back(2_default_int*i, err)
      end do

      total = 0_default_int
      do i = 1_default_int, arr%size()
         total = total + arr%get_unchecked(i)
      end do

      call check(error, total == 72_default_int, "get_unchecked must read back the pushed values")
      if (allocated(error)) return
   end subroutine test_int_get_unchecked

   !> the err argument is intent(inout): successful calls must not touch it,
   !> and clearing it must make the container usable again
   subroutine test_err_is_inout(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_int_t) :: arr
      type(error_t) :: err
      integer(default_int) :: value

      call err%set(ERROR_VALIDATION, "pre-existing error from somewhere else")
      call arr%push_back(1_default_int, err)
      call check(error, err%has_error(), "a successful push_back must leave err untouched")
      if (allocated(error)) return
      call check(error, arr%size() == 1_default_int, "push_back must still have worked")
      if (allocated(error)) return

      call err%clear()
      call arr%at(1_default_int, value, err)
      call check(error,.not. err%has_error(), "cleared err must stay clear on a successful at")
      if (allocated(error)) return
      call check(error, value == 1_default_int, "at must return the stored element")
      if (allocated(error)) return

      call arr%pop_back(value, err)
      call check(error,.not. err%has_error(), "cleared err must stay clear on a successful pop_back")
      if (allocated(error)) return
   end subroutine test_err_is_inout

   !> double precision flavour

   subroutine test_dp_empty_state(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr

      call check(error, arr%size() == 0_default_int, "fresh array must be size 0")
      if (allocated(error)) return
      call check(error, arr%capacity() == PIC_FIXED_ARRAY_CAPACITY, "capacity must be the module parameter")
      if (allocated(error)) return
      call check(error, arr%is_empty(), "fresh array must be empty")
      if (allocated(error)) return
      call check(error,.not. arr%is_full(), "fresh array must not be full")
      if (allocated(error)) return
      call check(error, size(arr%as_array()) == 0, "as_array on empty must be zero length")
      if (allocated(error)) return
   end subroutine test_dp_empty_state

   subroutine test_dp_push_lifo(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i
      real(dp) :: value

      do i = 1_default_int, 5_default_int
         call arr%push_back(0.5_dp*real(i, dp), err)
         call check(error,.not. err%has_error(), "push_back below capacity must succeed")
         if (allocated(error)) return
      end do

      call check(error, arr%size() == 5_default_int, "size must be 5 after 5 pushes")
      if (allocated(error)) return
      call check(error,.not. arr%is_empty(), "array with elements is not empty")
      if (allocated(error)) return

      do i = 5_default_int, 1_default_int, -1_default_int
         call arr%pop_back(value, err)
         call check(error,.not. err%has_error(), "pop_back on non-empty must succeed")
         if (allocated(error)) return
         call check(error, abs(value - 0.5_dp*real(i, dp)) < TOL, "pop_back must return elements in LIFO order")
         if (allocated(error)) return
      end do

      call check(error, arr%is_empty(), "array must be empty after popping everything")
      if (allocated(error)) return
   end subroutine test_dp_push_lifo

   subroutine test_dp_overflow(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i

      do i = 1_default_int, PIC_FIXED_ARRAY_CAPACITY
         call arr%push_back(real(i, dp), err)
         call check(error,.not. err%has_error(), "filling exactly to capacity must succeed")
         if (allocated(error)) return
      end do

      call check(error, arr%is_full(), "array must report full at capacity")
      if (allocated(error)) return
      call check(error, arr%size() == PIC_FIXED_ARRAY_CAPACITY, "size must equal capacity")
      if (allocated(error)) return

      call arr%push_back(-1.0_dp, err)
      call check(error, err%has_error(), "push_back past capacity must set the error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "overflow must be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, arr%size() == PIC_FIXED_ARRAY_CAPACITY, "size must be unchanged after overflow")
      if (allocated(error)) return

      call check(error, abs(arr%get_unchecked(PIC_FIXED_ARRAY_CAPACITY) - real(PIC_FIXED_ARRAY_CAPACITY, dp)) < TOL, &
                 "last element must not have been overwritten")
      if (allocated(error)) return
   end subroutine test_dp_overflow

   subroutine test_dp_underflow(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr
      type(error_t) :: err
      real(dp) :: value

      call arr%push_back(7.25_dp, err)
      call arr%pop_back(value, err)
      call check(error, abs(value - 7.25_dp) < TOL, "pop must return the pushed value")
      if (allocated(error)) return
      call check(error, arr%is_empty(), "array must be empty after popping the only element")
      if (allocated(error)) return

      call arr%pop_back(value, err)
      call check(error, err%has_error(), "pop_back on empty must set the error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "underflow must be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, abs(value) < TOL, "underflow must zero the output value")
      if (allocated(error)) return
      call check(error, arr%size() == 0_default_int, "size must stay 0 after underflow")
      if (allocated(error)) return
   end subroutine test_dp_underflow

   subroutine test_dp_at_bounds(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i
      real(dp) :: value

      do i = 1_default_int, 3_default_int
         call arr%push_back(100.0_dp + real(i, dp), err)
      end do

      call arr%at(1_default_int, value, err)
      call check(error,.not. err%has_error(), "at(1) must succeed")
      if (allocated(error)) return
      call check(error, abs(value - 101.0_dp) < TOL, "at(1) must return the first element")
      if (allocated(error)) return

      call arr%at(arr%size(), value, err)
      call check(error,.not. err%has_error(), "at(size) must succeed")
      if (allocated(error)) return
      call check(error, abs(value - 103.0_dp) < TOL, "at(size) must return the last element")
      if (allocated(error)) return

      call arr%at(0_default_int, value, err)
      call check(error, err%is(ERROR_VALIDATION), "at(0) must be ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, abs(value) < TOL, "at(0) must zero the output value")
      if (allocated(error)) return

      call arr%at(arr%size() + 1_default_int, value, err)
      call check(error, err%is(ERROR_VALIDATION), "at(size+1) must be ERROR_VALIDATION")
      if (allocated(error)) return

      call arr%at(PIC_FIXED_ARRAY_CAPACITY + 1_default_int, value, err)
      call check(error, err%is(ERROR_VALIDATION), "at(capacity+1) must be ERROR_VALIDATION")
      if (allocated(error)) return
   end subroutine test_dp_at_bounds

   subroutine test_dp_as_array(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr
      type(error_t) :: err
      real(dp), allocatable :: plain(:)
      integer(default_int) :: i

      plain = arr%as_array()
      call check(error, size(plain) == 0, "as_array on empty must have zero extent")
      if (allocated(error)) return

      do i = 1_default_int, 4_default_int
         call arr%push_back(real(i, dp)*0.25_dp, err)
      end do

      plain = arr%as_array()
      call check(error, size(plain) == 4, "as_array must return size() elements, not capacity()")
      if (allocated(error)) return
      call check(error, abs(plain(1) - 0.25_dp) < TOL, "as_array element 1 must match")
      if (allocated(error)) return
      call check(error, abs(plain(4) - 1.0_dp) < TOL, "as_array element 4 must match")
      if (allocated(error)) return
      call check(error, abs(sum(plain) - 2.5_dp) < TOL, "as_array contents must be usable by plain routines")
      if (allocated(error)) return
   end subroutine test_dp_as_array

   subroutine test_dp_clear_reuse(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i
      real(dp) :: value

      do i = 1_default_int, 6_default_int
         call arr%push_back(real(i, dp), err)
      end do
      call arr%clear()

      call check(error, arr%size() == 0_default_int, "clear must reset the size")
      if (allocated(error)) return
      call check(error, arr%is_empty(), "clear must leave the array empty")
      if (allocated(error)) return
      call check(error, size(arr%as_array()) == 0, "as_array after clear must be zero length")
      if (allocated(error)) return

      call arr%push_back(99.5_dp, err)
      call check(error,.not. err%has_error(), "array must be reusable after clear")
      if (allocated(error)) return
      call arr%at(1_default_int, value, err)
      call check(error, abs(value - 99.5_dp) < TOL, "reused array must hold the new element")
      if (allocated(error)) return
      call check(error, arr%size() == 1_default_int, "reused array must have size 1")
      if (allocated(error)) return
   end subroutine test_dp_clear_reuse

   subroutine test_dp_get_unchecked(error)
      type(error_type), allocatable, intent(out) :: error
      type(fixed_array_dp_t) :: arr
      type(error_t) :: err
      integer(default_int) :: i
      real(dp) :: total

      do i = 1_default_int, 8_default_int
         call arr%push_back(2.0_dp*real(i, dp), err)
      end do

      total = 0.0_dp
      do i = 1_default_int, arr%size()
         total = total + arr%get_unchecked(i)
      end do

      call check(error, abs(total - 72.0_dp) < TOL, "get_unchecked must read back the pushed values")
      if (allocated(error)) return
   end subroutine test_dp_get_unchecked

end module test_pic_fixed_array
