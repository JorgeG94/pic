module test_pic_array
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: sp, dp, int32, int64, default_int
   use pic_array, only: fill, set_threading_mode, get_threading_mode, &
                        pic_transpose, pic_sum, copy
   use pic_test_helpers, only: is_equal
   implicit none
   private
   public :: collect_pic_array_tests

contains

   subroutine collect_pic_array_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      integer(default_int), parameter :: n_test = 56
      allocate (testsuite(n_test))
      testsuite(1) = new_unittest("get_threading_mode", test_get_threading_mode)
      testsuite(2) = new_unittest("set_threading_mode", test_set_threading_mode)
      testsuite(3) = new_unittest("fill_vector_int32", test_fill_vector_int32)
      testsuite(4) = new_unittest("fill_vector_int64", test_fill_vector_int64)
      testsuite(5) = new_unittest("fill_vector_sp", test_fill_vector_sp)
      testsuite(6) = new_unittest("fill_vector_dp", test_fill_vector_dp)
      testsuite(7) = new_unittest("fill_matrix_int32", test_fill_matrix_int32)
      testsuite(8) = new_unittest("fill_matrix_int64", test_fill_matrix_int64)
      testsuite(9) = new_unittest("fill_matrix_sp", test_fill_matrix_sp)
      testsuite(10) = new_unittest("fill_matrix_dp", test_fill_matrix_dp)
      testsuite(11) = new_unittest("fill_vector_int32_threaded", test_fill_vector_int32_threaded)
      testsuite(12) = new_unittest("fill_vector_int64_threaded", test_fill_vector_int64_threaded)
      testsuite(13) = new_unittest("fill_vector_sp_threaded", test_fill_vector_sp_threaded)
      testsuite(14) = new_unittest("fill_vector_dp_threaded", test_fill_vector_dp_threaded)
      testsuite(15) = new_unittest("fill_matrix_int32_threaded", test_fill_matrix_int32_threaded)
      testsuite(16) = new_unittest("fill_matrix_int64_threaded", test_fill_matrix_int64_threaded)
      testsuite(17) = new_unittest("fill_matrix_sp_threaded", test_fill_matrix_sp_threaded)
      testsuite(18) = new_unittest("fill_matrix_dp_threaded", test_fill_matrix_dp_threaded)
      testsuite(19) = new_unittest("copy_vector_int32", test_copy_vector_int32)
      testsuite(20) = new_unittest("copy_vector_int64", test_copy_vector_int64)
      testsuite(21) = new_unittest("copy_vector_sp", test_copy_vector_sp)
      testsuite(22) = new_unittest("copy_vector_dp", test_copy_vector_dp)
      testsuite(23) = new_unittest("copy_matrix_int32", test_copy_matrix_int32)
      testsuite(24) = new_unittest("copy_matrix_int64", test_copy_matrix_int64)
      testsuite(25) = new_unittest("copy_matrix_sp", test_copy_matrix_sp)
      testsuite(26) = new_unittest("copy_matrix_dp", test_copy_matrix_dp)
      testsuite(27) = new_unittest("copy_vector_int32_threaded", test_copy_vector_int32_threaded)
      testsuite(28) = new_unittest("copy_vector_int64_threaded", test_copy_vector_int64_threaded)
      testsuite(29) = new_unittest("copy_vector_sp_threaded", test_copy_vector_sp_threaded)
      testsuite(30) = new_unittest("copy_vector_dp_threaded", test_copy_vector_dp_threaded)
      testsuite(31) = new_unittest("copy_matrix_int32_threaded", test_copy_matrix_int32_threaded)
      testsuite(32) = new_unittest("copy_matrix_int64_threaded", test_copy_matrix_int64_threaded)
      testsuite(33) = new_unittest("copy_matrix_sp_threaded", test_copy_matrix_sp_threaded)
      testsuite(34) = new_unittest("copy_matrix_dp_threaded", test_copy_matrix_dp_threaded)
      testsuite(35) = new_unittest("pic_transpose_matrix_int32", test_pic_transpose_matrix_int32)
      testsuite(36) = new_unittest("pic_transpose_matrix_int64", test_pic_transpose_matrix_int64)
      testsuite(37) = new_unittest("pic_transpose_matrix_sp", test_pic_transpose_matrix_sp)
      testsuite(38) = new_unittest("pic_transpose_matrix_dp", test_pic_transpose_matrix_dp)
      testsuite(39) = new_unittest("pic_transpose_matrix_int32_threaded", test_pic_transpose_matrix_int32_threaded)
      testsuite(40) = new_unittest("pic_transpose_matrix_int64_threaded", test_pic_transpose_matrix_int64_threaded)
      testsuite(41) = new_unittest("pic_transpose_matrix_sp_threaded", test_pic_transpose_matrix_sp_threaded)
      testsuite(42) = new_unittest("pic_transpose_matrix_dp_threaded", test_pic_transpose_matrix_dp_threaded)
      testsuite(43) = new_unittest("pic_sum_vector_int32", test_pic_sum_vector_int32)
      testsuite(44) = new_unittest("pic_sum_vector_int64", test_pic_sum_vector_int64)
      testsuite(45) = new_unittest("pic_sum_vector_sp", test_pic_sum_vector_sp)
      testsuite(46) = new_unittest("pic_sum_vector_dp", test_pic_sum_vector_dp)
      testsuite(47) = new_unittest("pic_sum_matrix_int32", test_pic_sum_matrix_int32)
      testsuite(48) = new_unittest("pic_sum_matrix_int64", test_pic_sum_matrix_int64)
      testsuite(49) = new_unittest("pic_sum_matrix_sp", test_pic_sum_matrix_sp)
      testsuite(50) = new_unittest("pic_sum_matrix_dp", test_pic_sum_matrix_dp)
      testsuite(51) = new_unittest("pic_sum_vector_int32_threaded", test_pic_sum_vector_int32_threaded)
      testsuite(52) = new_unittest("pic_sum_vector_int64_threaded", test_pic_sum_vector_int64_threaded)
      testsuite(53) = new_unittest("pic_sum_vector_sp_threaded", test_pic_sum_vector_sp_threaded)
      testsuite(54) = new_unittest("pic_sum_vector_dp_threaded", test_pic_sum_vector_dp_threaded)
      testsuite(55) = new_unittest("pic_sum_matrix_int32_threaded", test_pic_sum_matrix_int32_threaded)
      testsuite(56) = new_unittest("pic_sum_matrix_int64_threaded", test_pic_sum_matrix_int64_threaded)

      ! Add more tests as needed

   end subroutine collect_pic_array_tests

   subroutine test_get_threading_mode(error)
      type(error_type), allocatable, intent(out) :: error
      logical :: mode

      mode = get_threading_mode()

      call check(error, mode, .false., "get_threading_mode should return .false. by default")
      if (allocated(error)) return

   end subroutine test_get_threading_mode

   subroutine test_set_threading_mode(error)
      type(error_type), allocatable, intent(out) :: error
      logical :: mode

      call set_threading_mode(.true.)
      mode = get_threading_mode()

      call check(error, mode, .true., "get_threading_mode should return .true. after set_threading(.true.)")
      if (allocated(error)) return

      call set_threading_mode(.false.)
      mode = get_threading_mode()

      call check(error, mode, .false., "get_threading_mode should return .false. after set_threading(.false.)")
      if (allocated(error)) return

   end subroutine test_set_threading_mode

   subroutine test_fill_vector_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int32

   subroutine test_fill_vector_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int64

   subroutine test_fill_vector_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_sp

   subroutine test_fill_vector_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_dp

   subroutine test_fill_matrix_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int32

   subroutine test_fill_matrix_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int64

   subroutine test_fill_matrix_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_sp

   subroutine test_fill_matrix_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_dp

   subroutine test_fill_vector_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha, .true.)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int32_threaded

   subroutine test_fill_vector_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha, .true.)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int64_threaded

   subroutine test_fill_vector_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha, .true.)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_sp_threaded

   subroutine test_fill_vector_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call fill(vector, alpha, .true.)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_dp_threaded

   subroutine test_fill_matrix_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha, .true.)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int32_threaded

   subroutine test_fill_matrix_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha, .true.)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int64_threaded

   subroutine test_fill_matrix_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha, .true.)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_sp_threaded

   subroutine test_fill_matrix_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call fill(matrix, alpha, .true.)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_dp_threaded

   subroutine test_copy_vector_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:), vector_copy(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector)

      call check(error, all(vector_copy == alpha), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_int32

   subroutine test_copy_vector_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:), vector_copy(:)
      integer(int64), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector)

      call check(error, all(vector_copy == alpha), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_int64

   subroutine test_copy_vector_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:), vector_copy(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector)

      call check(error, all(is_equal(vector_copy, alpha)), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_sp

   subroutine test_copy_vector_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:), vector_copy(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector)

      call check(error, all(is_equal(vector_copy, alpha)), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_dp

   subroutine test_copy_matrix_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :), matrix_copy(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix)

      call check(error, all(matrix_copy == alpha), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_int32

   subroutine test_copy_matrix_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :), matrix_copy(:, :)
      integer(int64), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix)

      call check(error, all(matrix_copy == alpha), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_int64

   subroutine test_copy_matrix_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :), matrix_copy(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix)

      call check(error, all(is_equal(matrix_copy, alpha)), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_sp

   subroutine test_copy_matrix_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :), matrix_copy(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix)

      call check(error, all(is_equal(matrix_copy, alpha)), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_dp

   subroutine test_copy_vector_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:), vector_copy(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector, .true.)

      call check(error, all(vector_copy == alpha), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_int32_threaded

   subroutine test_copy_vector_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:), vector_copy(:)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector, .true.)

      call check(error, all(vector_copy == alpha), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_int64_threaded

   subroutine test_copy_vector_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:), vector_copy(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector, .true.)

      call check(error, all(is_equal(vector_copy, alpha)), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_sp_threaded

   subroutine test_copy_vector_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:), vector_copy(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call fill(vector, alpha)
      call copy(vector_copy, vector, .true.)

      call check(error, all(is_equal(vector_copy, alpha)), .true., "copy should copy all elements from vector to vector_copy")
      if (allocated(error)) return

   end subroutine test_copy_vector_dp_threaded

   subroutine test_copy_matrix_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :), matrix_copy(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix, .true.)

      call check(error, all(matrix_copy == alpha), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_int32_threaded

   subroutine test_copy_matrix_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :), matrix_copy(:, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix, .true.)

      call check(error, all(matrix_copy == alpha), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_int64_threaded

   subroutine test_copy_matrix_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :), matrix_copy(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix, .true.)

      call check(error, all(is_equal(matrix_copy, alpha)), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_sp_threaded

   subroutine test_copy_matrix_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :), matrix_copy(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      allocate (matrix_copy(n, n))

      call fill(matrix, alpha)
      call copy(matrix_copy, matrix, .true.)

      call check(error, all(is_equal(matrix_copy, alpha)), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_dp_threaded

   subroutine test_pic_transpose_matrix_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: dim = 2
      integer(int32), parameter :: one = 1
      integer(int32), parameter :: two = 2
      integer(int32), parameter :: three = 3
      integer(int32), parameter :: four = 4
      integer(int32), parameter :: matrix(2, 2) = reshape([one, two, &
                                                           three, four], [2, 2])
      integer(int32) :: transposed_matrix(2, 2)
      integer(int32), parameter :: expected(2, 2) = reshape([one, three, &
                                                             two, four], [2, 2])

      call pic_transpose(matrix, transposed_matrix)

      call check(error, all(transposed_matrix == expected), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_int32

   subroutine test_pic_transpose_matrix_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), parameter :: dim = 2
      integer(int64), parameter :: one = 1_int64
      integer(int64), parameter :: two = 2_int64
      integer(int64), parameter :: three = 3_int64
      integer(int64), parameter :: four = 4_int64
      integer(int64), parameter :: matrix(2, 2) = reshape([one, two, &
                                                           three, four], [2, 2])
      integer(int64) :: transposed_matrix(2, 2)
      integer(int64), parameter :: expected(2, 2) = reshape([one, three, &
                                                             two, four], [2, 2])

      call pic_transpose(matrix, transposed_matrix)

      call check(error, all(transposed_matrix == expected), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_int64

   subroutine test_pic_transpose_matrix_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: dim = 2.0_sp
      real(sp), parameter :: one = 1.0_sp
      real(sp), parameter :: two = 2.0_sp
      real(sp), parameter :: three = 3.0_sp
      real(sp), parameter :: four = 4.0_sp
      real(sp), parameter :: matrix(2, 2) = reshape([one, two, &
                                                     three, four], [2, 2])
      real(sp), parameter :: expected(2, 2) = reshape([one, three, &
                                                       two, four], [2, 2])
      real(sp) :: transposed_matrix(2, 2)
      call pic_transpose(matrix, transposed_matrix)

      call check(error, all(is_equal(transposed_matrix, expected)), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_sp

   subroutine test_pic_transpose_matrix_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), parameter :: dim = 2.0_dp
      real(dp), parameter :: one = 1.0_dp
      real(dp), parameter :: two = 2.0_dp
      real(dp), parameter :: three = 3.0_dp
      real(dp), parameter :: four = 4.0_dp
      real(dp), parameter :: matrix(2, 2) = reshape([one, two, &
                                                     three, four], [2, 2])
      real(dp), parameter :: expected(2, 2) = reshape([one, three, &
                                                       two, four], [2, 2])
      real(dp) :: transposed_matrix(2, 2)
      call pic_transpose(matrix, transposed_matrix)

      call check(error, all(is_equal(transposed_matrix, expected)), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_dp

   subroutine test_pic_transpose_matrix_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), parameter :: dim = 2
      integer(int32), parameter :: one = 1
      integer(int32), parameter :: two = 2
      integer(int32), parameter :: three = 3
      integer(int32), parameter :: four = 4
      integer(int32), parameter :: matrix(2, 2) = reshape([one, two, &
                                                           three, four], [2, 2])
      integer(int32) :: transposed_matrix(2, 2)
      integer(int32), parameter :: expected(2, 2) = reshape([one, three, &
                                                             two, four], [2, 2])

      call pic_transpose(matrix, transposed_matrix, .true.)

      call check(error, all(transposed_matrix == expected), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_int32_threaded

   subroutine test_pic_transpose_matrix_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), parameter :: dim = 2
      integer(int64), parameter :: one = 1_int64
      integer(int64), parameter :: two = 2_int64
      integer(int64), parameter :: three = 3_int64
      integer(int64), parameter :: four = 4_int64
      integer(int64), parameter :: matrix(2, 2) = reshape([one, two, &
                                                           three, four], [2, 2])
      integer(int64) :: transposed_matrix(2, 2)
      integer(int64), parameter :: expected(2, 2) = reshape([one, three, &
                                                             two, four], [2, 2])

      call pic_transpose(matrix, transposed_matrix, .true.)

      call check(error, all(transposed_matrix == expected), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_int64_threaded

   subroutine test_pic_transpose_matrix_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), parameter :: dim = 2.0_sp
      real(sp), parameter :: one = 1.0_sp
      real(sp), parameter :: two = 2.0_sp
      real(sp), parameter :: three = 3.0_sp
      real(sp), parameter :: four = 4.0_sp
      real(sp), parameter :: matrix(2, 2) = reshape([one, two, &
                                                     three, four], [2, 2])
      real(sp), parameter :: expected(2, 2) = reshape([one, three, &
                                                       two, four], [2, 2])

      real(sp) :: transposed_matrix(2, 2)
      call pic_transpose(matrix, transposed_matrix, .true.)

      call check(error, all(is_equal(transposed_matrix, expected)), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_sp_threaded

   subroutine test_pic_transpose_matrix_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), parameter :: dim = 2.0_dp
      real(dp), parameter :: one = 1.0_dp
      real(dp), parameter :: two = 2.0_dp
      real(dp), parameter :: three = 3.0_dp
      real(dp), parameter :: four = 4.0_dp
      real(dp), parameter :: matrix(2, 2) = reshape([one, two, &
                                                     three, four], [2, 2])
      real(dp), parameter :: expected(2, 2) = reshape([one, three, &
                                                       two, four], [2, 2])

      real(dp) :: transposed_matrix(2, 2)
      call set_threading_mode(.true.)
      call pic_transpose(matrix, transposed_matrix)
      call set_threading_mode(.false.)

      call check(error, all(is_equal(transposed_matrix, expected)), &
                 .true., "pic_transpose should transpose the matrix correctly")
      if (allocated(error)) return

   end subroutine test_pic_transpose_matrix_dp_threaded

   subroutine test_pic_sum_vector_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10
      integer(int32) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector)

      call check(error, sum == n*alpha, .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_int32

   subroutine test_pic_sum_vector_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10
      integer(int64) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector)

      call check(error, sum == n*alpha, .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_int64

   subroutine test_pic_sum_vector_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10
      real(sp) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector)

      call check(error, is_equal(sum, n*alpha), .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_sp

   subroutine test_pic_sum_vector_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10
      real(dp) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector)

      call check(error, is_equal(sum, n*alpha), .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_dp

   subroutine test_pic_sum_matrix_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10
      integer(int32) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix)

      call check(error, sum == n*n*alpha, .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_int32

   subroutine test_pic_sum_matrix_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10
      integer(int64) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix)

      call check(error, sum == n*n*alpha, .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_int64

   subroutine test_pic_sum_matrix_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10
      real(sp) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix)

      call check(error, is_equal(sum, n*n*alpha), .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_sp

   subroutine test_pic_sum_matrix_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10
      real(dp) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix)

      call check(error, is_equal(sum, n*n*alpha), .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_dp

   subroutine test_pic_sum_vector_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10
      integer(int32) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector, .true.)

      call check(error, sum == n*alpha, .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_int32_threaded

   subroutine test_pic_sum_vector_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10
      integer(int64) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector, .true.)

      call check(error, sum == n*alpha, .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_int64_threaded

   subroutine test_pic_sum_vector_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10
      real(sp) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector, .true.)

      call check(error, is_equal(sum, n*alpha), .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_sp_threaded

   subroutine test_pic_sum_vector_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10
      real(dp) :: sum

      allocate (vector(n))
      call fill(vector, alpha)

      sum = pic_sum(vector, .true.)

      call check(error, is_equal(sum, n*alpha), .true., "pic_sum should return the correct sum of the vector")
      if (allocated(error)) return

   end subroutine test_pic_sum_vector_dp_threaded

   subroutine test_pic_sum_matrix_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10
      integer(int32) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix, .true.)

      call check(error, sum == n*n*alpha, .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_int32_threaded

   subroutine test_pic_sum_matrix_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10
      integer(int64) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix, .true.)

      call check(error, sum == n*n*alpha, .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_int64_threaded

   subroutine test_pic_sum_matrix_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10
      real(sp) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix, .true.)

      call check(error, is_equal(sum, n*n*alpha), .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_sp_threaded

   subroutine test_pic_sum_matrix_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10
      real(dp) :: sum

      allocate (matrix(n, n))
      call fill(matrix, alpha)

      sum = pic_sum(matrix, .true.)

      call check(error, is_equal(sum, n*n*alpha), .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_dp_threaded

end module test_pic_array
