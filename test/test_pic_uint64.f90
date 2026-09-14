! SPDX-Identifer: MIT
module test_pic_uint64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int64
   use pic_uint64, only: u64_add, u64_mul, u64_shr, u64_less
   implicit none
   private
   public :: collect_pic_uint64_tests

   !> 2**63 as a bit pattern. Written as -huge-1 rather than as a literal
   !> because 9223372036854775808 is not a representable int64 constant.
   integer(int64), parameter :: TWO_POW_63 = -huge(0_int64) - 1_int64
   integer(int64), parameter :: ALL_ONES = -1_int64          ! 2**64 - 1
   integer(int64), parameter :: TWO_POW_32 = 4294967296_int64
   integer(int64), parameter :: MASK32 = 4294967295_int64    ! 2**32 - 1

contains

   subroutine collect_pic_uint64_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("add-basic", test_add_basic), &
                  new_unittest("add-wraps", test_add_wraps), &
                  new_unittest("mul-basic", test_mul_basic), &
                  new_unittest("mul-wraps", test_mul_wraps), &
                  new_unittest("shr", test_shr), &
                  new_unittest("less-unsigned-order", test_less_unsigned_order), &
                  new_unittest("less-is-a-total-order", test_less_total_order), &
                  new_unittest("no-signed-overflow-under-stress", test_stress) &
                  ]
   end subroutine collect_pic_uint64_tests

   !> Values here come from an independent Python reference, not from running
   !> this library.
   subroutine test_add_basic(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, u64_add(0_int64, 0_int64) == 0_int64, "0 + 0")
      if (allocated(error)) return
      call check(error, u64_add(1_int64, 1_int64) == 2_int64, "1 + 1")
      if (allocated(error)) return
      ! carry must propagate across the 32-bit split the implementation uses
      call check(error, u64_add(MASK32, 1_int64) == TWO_POW_32, "2**32-1 + 1 = 2**32")
      if (allocated(error)) return
   end subroutine test_add_basic

   subroutine test_add_wraps(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, u64_add(ALL_ONES, 1_int64) == 0_int64, "(2**64-1) + 1 wraps to 0")
      if (allocated(error)) return
      call check(error, u64_add(TWO_POW_63, TWO_POW_63) == 0_int64, "2**63 + 2**63 wraps to 0")
      if (allocated(error)) return
      call check(error, u64_add(ALL_ONES, ALL_ONES) == -2_int64, "(2**64-1) doubled")
      if (allocated(error)) return
   end subroutine test_add_wraps

   subroutine test_mul_basic(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), parameter :: X = 81985529216486895_int64   ! 0x0123456789ABCDEF
      call check(error, u64_mul(0_int64, X) == 0_int64, "0 * x")
      if (allocated(error)) return
      call check(error, u64_mul(1_int64, X) == X, "1 * x")
      if (allocated(error)) return
      call check(error, u64_mul(X, 1_int64) == X, "x * 1")
      if (allocated(error)) return
   end subroutine test_mul_basic

   subroutine test_mul_wraps(error)
      type(error_type), allocatable, intent(out) :: error
      ! The two SplitMix64 finalising constants; their product is the value a
      ! wrapping 64-bit multiply must produce.
      call check(error, u64_mul(-49064778989728563_int64, -7046029254386353131_int64) &
                 == 3346935820291004625_int64, "0xFF51AFD7ED558CCD * 0x9E3779B97F4A7C15")
      if (allocated(error)) return
      call check(error, u64_mul(ALL_ONES, ALL_ONES) == 1_int64, "(2**64-1)**2 = 1 mod 2**64")
      if (allocated(error)) return
      ! every bit of the product lands above bit 63 and is discarded
      call check(error, u64_mul(TWO_POW_32, TWO_POW_32) == 0_int64, "2**32 * 2**32 = 0 mod 2**64")
      if (allocated(error)) return
      call check(error, u64_mul(TWO_POW_63, 2_int64) == 0_int64, "2**63 * 2 = 0 mod 2**64")
      if (allocated(error)) return
   end subroutine test_mul_wraps

   subroutine test_shr(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), parameter :: X = 81985529216486895_int64
      ! zero-filling, so a negative operand must not smear its sign bit
      call check(error, u64_shr(ALL_ONES, 63_default_int) == 1_int64, "shr(2**64-1, 63) = 1")
      if (allocated(error)) return
      call check(error, u64_shr(TWO_POW_63, 63_default_int) == 1_int64, "shr(2**63, 63) = 1")
      if (allocated(error)) return
      call check(error, u64_shr(-49064778989728563_int64, 30_default_int) == 17134174047_int64, &
                 "shr of a negative pattern is logical, not arithmetic")
      if (allocated(error)) return
      call check(error, u64_shr(X, 0_default_int) == X, "shr by 0 is identity")
      if (allocated(error)) return
   end subroutine test_shr

   !> The cases where unsigned and signed ordering disagree are the whole
   !> reason this function exists, so they are asserted explicitly.
   subroutine test_less_unsigned_order(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error,.not. u64_less(TWO_POW_63, 1_int64), &
                 "2**63 is not less than 1 (signed < would say it is)")
      if (allocated(error)) return
      call check(error, u64_less(1_int64, TWO_POW_63), "1 < 2**63")
      if (allocated(error)) return
      call check(error,.not. u64_less(ALL_ONES, 0_int64), &
                 "2**64-1 is not less than 0 (signed < would say it is)")
      if (allocated(error)) return
      call check(error, u64_less(0_int64, ALL_ONES), "0 < 2**64-1")
      if (allocated(error)) return
      call check(error,.not. u64_less(5_int64, 5_int64), "irreflexive")
      if (allocated(error)) return
   end subroutine test_less_unsigned_order

   subroutine test_less_total_order(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: v(6)
      integer(default_int) :: i, j
      logical :: ok

      v = [0_int64, 1_int64, MASK32, TWO_POW_63, TWO_POW_63 + 1_int64, ALL_ONES]
      ok = .true.
      do i = 1, size(v)
         do j = 1, size(v)
            ! exactly one of a<b, b<a, a==b must hold
            if (count([u64_less(v(i), v(j)), u64_less(v(j), v(i)), v(i) == v(j)]) /= 1) ok = .false.
         end do
      end do
      call check(error, ok, "trichotomy holds over values spanning the sign bit")
      if (allocated(error)) return
   end subroutine test_less_total_order

   !> Drives many values through the limb arithmetic. Under a build with
   !> -fsanitize=signed-integer-overflow this is what would abort if any
   !> intermediate left the signed range.
   subroutine test_stress(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: h, i
      logical :: consistent

      ! a SplitMix64-shaped loop: multiply and add, always near the top of the range
      h = -3750763034362895579_int64
      do i = 1, 2000_int64
         h = u64_mul(ieor(h, u64_shr(h, 30_default_int)), -7046029254386353131_int64)
         h = u64_add(h, -1640531527_int64)
      end do
      ! The loop's final value, computed independently in Python big-integer
      ! arithmetic. Commutativity was checked here before and proved worthless:
      ! every stage of `u64_mul` is symmetric in its operands
      ! (`a0*b1 + a1*b0`, and so on), so swapping them replays the identical
      ! sequence of intermediates. A wrong mask, a wrong carry width or a wrong
      ! reassembly shift all give the same wrong answer both ways round, and
      ! the assertion held for every one of them. This equality does not: it
      ! carries 2000 rounds of carry propagation through all four limbs.
      consistent = h == -8700969152592207136_int64
      call check(error, consistent, "2000 wrapping rounds diverged from the reference value")
      if (allocated(error)) return
   end subroutine test_stress

end module test_pic_uint64
