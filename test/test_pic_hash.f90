module test_pic_hash
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use stdlib_pic_hash_32bit, only: fnv_1_hash, fnv_1a_hash
   use pic_types, only: int8, int16, int32, int64
   implicit none
   private

   public :: collect_pic_hash_tests

contains

   subroutine collect_pic_hash_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)

      tests = [ &
              new_unittest("pic_hash_int8", test_pic_hash_int8), &
              new_unittest("pic_hash_int16", test_pic_hash_int16), &
              new_unittest("pic_hash_int32", test_pic_hash_int32), &
              new_unittest("pic_hash_int64", test_pic_hash_int64), &
              new_unittest("pic_hash_char", test_pic_hash_char) &
              ]

   end subroutine collect_pic_hash_tests

   subroutine test_pic_hash_int8(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8) :: data(5)
      integer(int32) :: hash1, hash2
      integer :: i

      do i = 1, size(data, 1)
         data(i) = int(i, int8)
      end do

      hash1 = fnv_1_hash(data)
      hash2 = fnv_1a_hash(data)

      call check(error, hash1 == -1172398102, 'FNV-1 hash mismatch for int8')
      call check(error, hash2 == -1075497752, 'FNV-1a hash mismatch for int8')

   end subroutine test_pic_hash_int8

   subroutine test_pic_hash_int16(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int16) :: data(5)
      integer(int32) :: hash1, hash2
      integer :: i
      do i = 1, size(data, 1)
         data(i) = int(i, int16)
      end do
      hash1 = fnv_1_hash(data)
      hash2 = fnv_1a_hash(data)

      call check(error, hash1 == -1850525606, 'FNV-1 hash mismatch for int16')
      call check(error, hash2 == -679010524, 'FNV-1a hash mismatch for int16')

   end subroutine test_pic_hash_int16

   subroutine test_pic_hash_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: data(5)
      integer(int32) :: hash1, hash2
      integer :: i
      do i = 1, size(data, 1)
         data(i) = int(i, int32)
      end do
      hash1 = fnv_1_hash(data)
      hash2 = fnv_1a_hash(data)

      call check(error, hash1 == -1796374390, 'FNV-1 hash mismatch for int32')
      call check(error, hash2 == 400233476, 'FNV-1a hash mismatch for int32')

   end subroutine test_pic_hash_int32

   subroutine test_pic_hash_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: data(5)
      integer(int32) :: hash1, hash2
      integer :: i
      do i = 1, size(data, 1)
         data(i) = int(i, int64)
      end do
      hash1 = fnv_1_hash(data)
      hash2 = fnv_1a_hash(data)

      call check(error, hash1 == 822868970, 'FNV-1 hash mismatch for int64')
      call check(error, hash2 == -1949181884, 'FNV-1a hash mismatch for int64')

   end subroutine test_pic_hash_int64

   subroutine test_pic_hash_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=5) :: data
      integer(int32) :: hash1, hash2
      data = 'hello'
      hash1 = fnv_1_hash(data)
      hash2 = fnv_1a_hash(data)

      call check(error, hash1 == -1225100953, 'FNV-1 hash mismatch for char')
      call check(error, hash2 == 1335831723, 'FNV-1a hash mismatch for char')

   end subroutine test_pic_hash_char

end module test_pic_hash
