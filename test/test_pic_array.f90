module test_pic_array
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: sp, dp, int32, int64, default_int
   use pic_array, only: pic_fill, set_threading_mode, get_threading_mode, &
                        pic_transpose, pic_sum, pic_copy, is_sorted, ASCENDING, &
                        DESCENDING, pic_scramble_array, pic_print_array
   use pic_error, only: error_t, ERROR_VALIDATION, ERROR_IO, SUCCESS
   use pic_test_helpers, only: is_equal
   implicit none
   private
   public :: collect_pic_array_tests

contains

   subroutine collect_pic_array_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("get_threading_mode", test_get_threading_mode), &
                  new_unittest("set_threading_mode", test_set_threading_mode), &
                  new_unittest("fill_vector_int32", test_fill_vector_int32), &
                  new_unittest("fill_vector_int64", test_fill_vector_int64), &
                  new_unittest("fill_vector_sp", test_fill_vector_sp), &
                  new_unittest("fill_vector_dp", test_fill_vector_dp), &
                  new_unittest("fill_matrix_int32", test_fill_matrix_int32), &
                  new_unittest("fill_matrix_int64", test_fill_matrix_int64), &
                  new_unittest("fill_matrix_sp", test_fill_matrix_sp), &
                  new_unittest("fill_matrix_dp", test_fill_matrix_dp), &
                  new_unittest("fill_3d_tensor_int32", test_fill_3d_tensor_int32), &
                  new_unittest("fill_3d_tensor_int64", test_fill_3d_tensor_int64), &
                  new_unittest("fill_3d_tensor_sp", test_fill_3d_tensor_sp), &
                  new_unittest("fill_3d_tensor_dp", test_fill_3d_tensor_dp), &
                  new_unittest("fill_vector_int32_threaded", test_fill_vector_int32_threaded), &
                  new_unittest("fill_vector_int64_threaded", test_fill_vector_int64_threaded), &
                  new_unittest("fill_vector_sp_threaded", test_fill_vector_sp_threaded), &
                  new_unittest("fill_vector_dp_threaded", test_fill_vector_dp_threaded), &
                  new_unittest("fill_matrix_int32_threaded", test_fill_matrix_int32_threaded), &
                  new_unittest("fill_matrix_int64_threaded", test_fill_matrix_int64_threaded), &
                  new_unittest("fill_matrix_sp_threaded", test_fill_matrix_sp_threaded), &
                  new_unittest("fill_matrix_dp_threaded", test_fill_matrix_dp_threaded), &
                  new_unittest("fill_3d_tensor_int32_threaded", test_fill_3d_tensor_int32_threaded), &
                  new_unittest("fill_3d_tensor_int64_threaded", test_fill_3d_tensor_int64_threaded), &
                  new_unittest("fill_3d_tensor_sp_threaded", test_fill_3d_tensor_sp_threaded), &
                  new_unittest("fill_3d_tensor_dp_threaded", test_fill_3d_tensor_dp_threaded), &
                  new_unittest("copy_vector_int32", test_copy_vector_int32), &
                  new_unittest("copy_vector_int64", test_copy_vector_int64), &
                  new_unittest("copy_vector_sp", test_copy_vector_sp), &
                  new_unittest("copy_vector_dp", test_copy_vector_dp), &
                  new_unittest("copy_matrix_int32", test_copy_matrix_int32), &
                  new_unittest("copy_matrix_int64", test_copy_matrix_int64), &
                  new_unittest("copy_matrix_sp", test_copy_matrix_sp), &
                  new_unittest("copy_matrix_dp", test_copy_matrix_dp), &
                  new_unittest("copy_3d_tensor_int32", test_copy_3d_tensor_int32), &
                  new_unittest("copy_3d_tensor_int64", test_copy_3d_tensor_int64), &
                  new_unittest("copy_3d_tensor_sp", test_copy_3d_tensor_sp), &
                  new_unittest("copy_3d_tensor_dp", test_copy_3d_tensor_dp), &
                  new_unittest("copy_vector_int32_threaded", test_copy_vector_int32_threaded), &
                  new_unittest("copy_vector_int64_threaded", test_copy_vector_int64_threaded), &
                  new_unittest("copy_vector_sp_threaded", test_copy_vector_sp_threaded), &
                  new_unittest("copy_vector_dp_threaded", test_copy_vector_dp_threaded), &
                  new_unittest("copy_matrix_int32_threaded", test_copy_matrix_int32_threaded), &
                  new_unittest("copy_matrix_int64_threaded", test_copy_matrix_int64_threaded), &
                  new_unittest("copy_matrix_sp_threaded", test_copy_matrix_sp_threaded), &
                  new_unittest("copy_matrix_dp_threaded", test_copy_matrix_dp_threaded), &
                  new_unittest("copy_3d_tensor_int32_threaded", test_copy_3d_tensor_int32_threaded), &
                  new_unittest("copy_3d_tensor_int64_threaded", test_copy_3d_tensor_int64_threaded), &
                  new_unittest("copy_3d_tensor_sp_threaded", test_copy_3d_tensor_sp_threaded), &
                  new_unittest("copy_3d_tensor_dp_threaded", test_copy_3d_tensor_dp_threaded), &
                  new_unittest("pic_transpose_matrix_int32", test_pic_transpose_matrix_int32), &
                  new_unittest("pic_transpose_matrix_int64", test_pic_transpose_matrix_int64), &
                  new_unittest("pic_transpose_matrix_sp", test_pic_transpose_matrix_sp), &
                  new_unittest("pic_transpose_matrix_dp", test_pic_transpose_matrix_dp), &
                  new_unittest("pic_transpose_matrix_int32_threaded", test_pic_transpose_matrix_int32_threaded), &
                  new_unittest("pic_transpose_matrix_int64_threaded", test_pic_transpose_matrix_int64_threaded), &
                  new_unittest("pic_transpose_matrix_sp_threaded", test_pic_transpose_matrix_sp_threaded), &
                  new_unittest("pic_transpose_matrix_dp_threaded", test_pic_transpose_matrix_dp_threaded), &
                  new_unittest("pic_sum_vector_int32", test_pic_sum_vector_int32), &
                  new_unittest("pic_sum_vector_int64", test_pic_sum_vector_int64), &
                  new_unittest("pic_sum_vector_sp", test_pic_sum_vector_sp), &
                  new_unittest("pic_sum_vector_dp", test_pic_sum_vector_dp), &
                  new_unittest("pic_sum_matrix_int32", test_pic_sum_matrix_int32), &
                  new_unittest("pic_sum_matrix_int64", test_pic_sum_matrix_int64), &
                  new_unittest("pic_sum_matrix_sp", test_pic_sum_matrix_sp), &
                  new_unittest("pic_sum_matrix_dp", test_pic_sum_matrix_dp), &
                  new_unittest("pic_sum_3d_tensor_int32", test_pic_sum_3d_tensor_int32), &
                  new_unittest("pic_sum_3d_tensor_int64", test_pic_sum_3d_tensor_int64), &
                  new_unittest("pic_sum_3d_tensor_sp", test_pic_sum_3d_tensor_sp), &
                  new_unittest("pic_sum_3d_tensor_dp", test_pic_sum_3d_tensor_dp), &
                  new_unittest("pic_sum_vector_int32_threaded", test_pic_sum_vector_int32_threaded), &
                  new_unittest("pic_sum_vector_int64_threaded", test_pic_sum_vector_int64_threaded), &
                  new_unittest("pic_sum_vector_sp_threaded", test_pic_sum_vector_sp_threaded), &
                  new_unittest("pic_sum_vector_dp_threaded", test_pic_sum_vector_dp_threaded), &
                  new_unittest("pic_sum_matrix_int32_threaded", test_pic_sum_matrix_int32_threaded), &
                  new_unittest("pic_sum_matrix_int64_threaded", test_pic_sum_matrix_int64_threaded), &
                  new_unittest("pic_sum_matrix_sp_threaded", test_pic_sum_matrix_sp_threaded), &
                  new_unittest("pic_sum_matrix_dp_threaded", test_pic_sum_matrix_dp_threaded), &
                  new_unittest("pic_sum_3d_tensor_int32_threaded", test_pic_sum_3d_tensor_int32_threaded), &
                  new_unittest("pic_sum_3d_tensor_int64_threaded", test_pic_sum_3d_tensor_int64_threaded), &
                  new_unittest("pic_sum_3d_tensor_sp_threaded", test_pic_sum_3d_tensor_sp_threaded), &
                  new_unittest("pic_sum_3d_tensor_dp_threaded", test_pic_sum_3d_tensor_dp_threaded), &
                  new_unittest("pic_is_sorted_int32", test_pic_is_sorted_int32), &
                  new_unittest("pic_is_sorted_int64", test_pic_is_sorted_int64), &
                  new_unittest("pic_is_sorted_sp", test_pic_is_sorted_sp), &
                  new_unittest("pic_is_sorted_dp", test_pic_is_sorted_dp), &
                  new_unittest("pic_is_sorted_char", test_pic_is_sorted_char), &
                  new_unittest("pic_scramble_array_int32", test_pic_scramble_array_int32), &
                  new_unittest("pic_scramble_array_int64", test_pic_scramble_array_int64), &
                  new_unittest("pic_scramble_array_sp", test_pic_scramble_array_sp), &
                  new_unittest("pic_scramble_array_dp", test_pic_scramble_array_dp), &
                  new_unittest("pic_scramble_array_char", test_pic_scramble_array_char), &
                  new_unittest("copy_vector_err_mismatch", test_copy_vector_err_mismatch), &
                  new_unittest("copy_matrix_err_mismatch", test_copy_matrix_err_mismatch), &
                  new_unittest("copy_3d_tensor_err_mismatch", test_copy_3d_tensor_err_mismatch), &
                  new_unittest("transpose_err_mismatch", test_transpose_err_mismatch), &
                  new_unittest("array_err_untouched_on_success", test_array_err_untouched_on_success), &
                  new_unittest("array_no_err_valid_input", test_array_no_err_valid_input), &
                  new_unittest("array_pure_guard", test_array_pure_guard), &
                  new_unittest("print_bad_format_err", test_print_bad_format_err), &
                  new_unittest("print_packed_bad_size_err", test_print_packed_bad_size_err), &
                  new_unittest("print_bad_input_without_err", test_print_bad_input_without_err), &
                  new_unittest("print_valid_input_err_clear", test_print_valid_input_err_clear), &
                  new_unittest("is_sorted_rejects_unsorted", test_is_sorted_rejects_unsorted), &
                  new_unittest("print_packed_rejects_bad_size", test_print_packed_rejects_bad_size), &
                  new_unittest("print_unknown_format_falls_back", test_print_unknown_format_falls_back) &
                  ]

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
      call pic_fill(vector, alpha)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int32

   subroutine test_fill_vector_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call pic_fill(vector, alpha)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int64

   subroutine test_fill_vector_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call pic_fill(vector, alpha)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_sp

   subroutine test_fill_vector_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call pic_fill(vector, alpha)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_dp

   subroutine test_fill_matrix_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int32

   subroutine test_fill_matrix_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int64

   subroutine test_fill_matrix_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_sp

   subroutine test_fill_matrix_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_dp

   subroutine test_fill_3d_tensor_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: tensor(:, :, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      call check(error, all(tensor == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_int32

   subroutine test_fill_3d_tensor_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: tensor(:, :, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      call check(error, all(tensor == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_int64

   subroutine test_fill_3d_tensor_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: tensor(:, :, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      call check(error, all(is_equal(tensor, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_sp

   subroutine test_fill_3d_tensor_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: tensor(:, :, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      call check(error, all(is_equal(tensor, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_dp

   subroutine test_fill_vector_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call pic_fill(vector, alpha, .true.)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int32_threaded

   subroutine test_fill_vector_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call pic_fill(vector, alpha, .true.)

      call check(error, all(vector == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_int64_threaded

   subroutine test_fill_vector_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call pic_fill(vector, alpha, .true.)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_sp_threaded

   subroutine test_fill_vector_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      call pic_fill(vector, alpha, .true.)

      call check(error, all(is_equal(vector, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_vector_dp_threaded

   subroutine test_fill_matrix_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: matrix(:, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha, .true.)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int32_threaded

   subroutine test_fill_matrix_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: matrix(:, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha, .true.)

      call check(error, all(matrix == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_int64_threaded

   subroutine test_fill_matrix_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: matrix(:, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha, .true.)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_sp_threaded

   subroutine test_fill_matrix_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: matrix(:, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 10

      allocate (matrix(n, n))
      call pic_fill(matrix, alpha, .true.)

      call check(error, all(is_equal(matrix, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_matrix_dp_threaded

   subroutine test_fill_3d_tensor_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: tensor(:, :, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha, .true.)

      call check(error, all(tensor == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_int32_threaded

   subroutine test_fill_3d_tensor_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: tensor(:, :, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha, .true.)

      call check(error, all(tensor == alpha), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_int64_threaded

   subroutine test_fill_3d_tensor_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: tensor(:, :, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha, .true.)

      call check(error, all(is_equal(tensor, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_sp_threaded

   subroutine test_fill_3d_tensor_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: tensor(:, :, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha, .true.)

      call check(error, all(is_equal(tensor, alpha)), .true., "fill should set all elements to the value of alpha")
      if (allocated(error)) return

   end subroutine test_fill_3d_tensor_dp_threaded

   subroutine test_copy_vector_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:), vector_copy(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector)

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

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector)

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

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector)

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

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix)

      call check(error, all(is_equal(matrix_copy, alpha)), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_dp

   subroutine test_copy_3d_tensor_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor)

      call check(error, all(tensor_copy == alpha), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_int32

   subroutine test_copy_3d_tensor_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      integer(int64), parameter :: alpha = 42
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor)

      call check(error, all(tensor_copy == alpha), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_int64

   subroutine test_copy_3d_tensor_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor)

      call check(error, all(is_equal(tensor_copy, alpha)), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_sp

   subroutine test_copy_3d_tensor_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor)

      call check(error, all(is_equal(tensor_copy, alpha)), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_dp

   subroutine test_copy_vector_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:), vector_copy(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10

      allocate (vector(n))
      allocate (vector_copy(n))

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector, .true.)

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

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector, .true.)

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

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector, .true.)

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

      call pic_fill(vector, alpha)
      call pic_copy(vector_copy, vector, .true.)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix, .true.)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix, .true.)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix, .true.)

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

      call pic_fill(matrix, alpha)
      call pic_copy(matrix_copy, matrix, .true.)

      call check(error, all(is_equal(matrix_copy, alpha)), .true., "copy should copy all elements from matrix to matrix_copy")
      if (allocated(error)) return

   end subroutine test_copy_matrix_dp_threaded

   subroutine test_copy_3d_tensor_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor, .true.)

      call check(error, all(tensor_copy == alpha), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_int32_threaded

   subroutine test_copy_3d_tensor_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor, .true.)

      call check(error, all(tensor_copy == alpha), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_int64_threaded

   subroutine test_copy_3d_tensor_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor, .true.)

      call check(error, all(is_equal(tensor_copy, alpha)), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_sp_threaded

   subroutine test_copy_3d_tensor_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: tensor(:, :, :), tensor_copy(:, :, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 5

      allocate (tensor(n, n, n))
      allocate (tensor_copy(n, n, n))

      call pic_fill(tensor, alpha)
      call pic_copy(tensor_copy, tensor, .true.)

      call check(error, all(is_equal(tensor_copy, alpha)), .true., "copy should copy all elements from tensor to tensor_copy")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_dp_threaded

   subroutine test_pic_transpose_matrix_int32(error)
      type(error_type), allocatable, intent(out) :: error
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
      call pic_fill(vector, alpha)

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
      call pic_fill(vector, alpha)

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
      call pic_fill(vector, alpha)

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
      call pic_fill(vector, alpha)

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
      call pic_fill(matrix, alpha)

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
      call pic_fill(matrix, alpha)

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
      call pic_fill(matrix, alpha)

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
      call pic_fill(matrix, alpha)

      sum = pic_sum(matrix)

      call check(error, is_equal(sum, n*n*alpha), .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_dp

   subroutine test_pic_sum_3d_tensor_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: tensor(:, :, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 5
      integer(int32) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor)

      call check(error, sum == n*n*n*alpha, .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_int32

   subroutine test_pic_sum_3d_tensor_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: tensor(:, :, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 5
      integer(int64) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor)

      call check(error, sum == n*n*n*alpha, .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_int64

   subroutine test_pic_sum_3d_tensor_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: tensor(:, :, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 5
      real(sp) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor)

      call check(error, is_equal(sum, n*n*n*alpha), .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_sp

   subroutine test_pic_sum_3d_tensor_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: tensor(:, :, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 5
      real(dp) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor)

      call check(error, is_equal(sum, n*n*n*alpha), .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_dp

   subroutine test_pic_sum_vector_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 10
      integer(int32) :: sum

      allocate (vector(n))
      call pic_fill(vector, alpha)

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
      call pic_fill(vector, alpha)

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
      call pic_fill(vector, alpha)

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
      call pic_fill(vector, alpha)

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
      call pic_fill(matrix, alpha)

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
      call pic_fill(matrix, alpha)

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
      call pic_fill(matrix, alpha)

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
      call pic_fill(matrix, alpha)

      sum = pic_sum(matrix, .true.)

      call check(error, is_equal(sum, n*n*alpha), .true., "pic_sum should return the correct sum of the matrix")
      if (allocated(error)) return

   end subroutine test_pic_sum_matrix_dp_threaded

   subroutine test_pic_sum_3d_tensor_int32_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: tensor(:, :, :)
      integer(int32), parameter :: alpha = 42
      integer(default_int), parameter :: n = 5
      integer(int32) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor, .true.)

      call check(error, sum == n*n*n*alpha, .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_int32_threaded

   subroutine test_pic_sum_3d_tensor_int64_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: tensor(:, :, :)
      integer(int64), parameter :: alpha = 42_int64
      integer(default_int), parameter :: n = 5
      integer(int64) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor, .true.)

      call check(error, sum == n*n*n*alpha, .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_int64_threaded

   subroutine test_pic_sum_3d_tensor_sp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: tensor(:, :, :)
      real(sp), parameter :: alpha = 42.0_sp
      integer(default_int), parameter :: n = 5
      real(sp) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor, .true.)

      call check(error, is_equal(sum, n*n*n*alpha), .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_sp_threaded

   subroutine test_pic_sum_3d_tensor_dp_threaded(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: tensor(:, :, :)
      real(dp), parameter :: alpha = 42.0_dp
      integer(default_int), parameter :: n = 5
      real(dp) :: sum

      allocate (tensor(n, n, n))
      call pic_fill(tensor, alpha)

      sum = pic_sum(tensor, .true.)

      call check(error, is_equal(sum, n*n*n*alpha), .true., "pic_sum should return the correct sum of the tensor")
      if (allocated(error)) return

   end subroutine test_pic_sum_3d_tensor_dp_threaded

   subroutine test_pic_is_sorted_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32), allocatable :: vector(:)
      integer(int32) :: i
      integer(int32), parameter :: length = 20
      logical :: sorted

      allocate (vector(length))
      vector = 0_int32

      vector = [(i, i=1, size(vector))]

      sorted = is_sorted(vector, ASCENDING)
      call check(error, sorted, .true., "Array should be sorted!")
      if (allocated(error)) return

      sorted = is_sorted(vector, DESCENDING)
      call check(error, sorted, .false., "Array is sorted ascendigly, not descendingly")
      if (allocated(error)) return

   end subroutine test_pic_is_sorted_int32

   subroutine test_pic_is_sorted_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), allocatable :: vector(:)
      integer(int64) :: i
      integer(int64), parameter :: length = 20
      logical :: sorted

      allocate (vector(length))
      vector = 0_int64

      vector = [(i, i=1, size(vector))]

      sorted = is_sorted(vector, ASCENDING)
      call check(error, sorted, .true., "Array should be sorted!")
      if (allocated(error)) return

      sorted = is_sorted(vector, DESCENDING)
      call check(error, sorted, .false., "Array is sorted ascendigly, not descendingly")
      if (allocated(error)) return

   end subroutine test_pic_is_sorted_int64

   subroutine test_pic_is_sorted_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp), allocatable :: vector(:)
      integer(default_int) :: i
      integer(default_int), parameter :: length = 20
      logical :: sorted

      allocate (vector(length))
      vector = 0_sp

      vector = [(real(i, sp), i=1, size(vector))]

      sorted = is_sorted(vector, ASCENDING)
      call check(error, sorted, .true., "Array should be sorted!")
      if (allocated(error)) return

      sorted = is_sorted(vector, DESCENDING)
      call check(error, sorted, .false., "Array is sorted ascendigly, not descendingly")
      if (allocated(error)) return

   end subroutine test_pic_is_sorted_sp

   subroutine test_pic_is_sorted_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp), allocatable :: vector(:)
      integer(default_int) :: i
      integer(default_int), parameter :: length = 20
      logical :: sorted

      allocate (vector(length))
      vector = 0_dp

      vector = [(real(i, dp), i=1, size(vector))]

      sorted = is_sorted(vector, ASCENDING)
      call check(error, sorted, .true., "Array should be sorted!")
      if (allocated(error)) return

      sorted = is_sorted(vector, DESCENDING)
      call check(error, sorted, .false., "Array is sorted ascendigly, not descendingly")
      if (allocated(error)) return

   end subroutine test_pic_is_sorted_dp

   subroutine test_pic_is_sorted_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=10) :: array(5)
      logical :: sorted

      array = ["alpha     ", "beta      ", "delta     ", "epsilon   ", "gamma     "]

      sorted = is_sorted(array, ASCENDING)
      call check(error, sorted, .true., "Array should be sorted!")
      if (allocated(error)) return

      sorted = is_sorted(array, DESCENDING)
      call check(error, sorted, .false., "Array is sorted ascendigly, not descendingly")
      if (allocated(error)) return

   end subroutine test_pic_is_sorted_char

   subroutine test_pic_scramble_array_int32(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: int32_arr(10)
      integer(int32) :: arr_before(10)
      integer :: sum_before
      int32_arr = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32, 7_int32, 8_int32, 9_int32, 10_int32]
      arr_before = int32_arr
      call random_seed()
      sum_before = 0_int32

      sum_before = sum(int32_arr)

      call pic_scramble_array(int32_arr)

      call check(error, sum(int32_arr) == sum_before, .true., "The arrays need to produce the same overall sum!")
      if (allocated(error)) return

      call check(error, all(int32_arr == arr_before), .false., "The arrays should not be equal!")
      if (allocated(error)) return

   end subroutine test_pic_scramble_array_int32

   subroutine test_pic_scramble_array_int64(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: int64_arr(10)
      integer(int64) :: arr_before(10)
      integer(int64) :: sum_before
      int64_arr = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64, 7_int64, 8_int64, 9_int64, 10_int64]
      arr_before = int64_arr
      call random_seed()
      sum_before = 0_int64

      sum_before = sum(int64_arr)

      call pic_scramble_array(int64_arr)

      call check(error, sum(int64_arr) == sum_before, .true., "The arrays need to produce the same overall sum!")
      if (allocated(error)) return

      call check(error, all(int64_arr == arr_before), .false., "The arrays should not be equal!")
      if (allocated(error)) return

   end subroutine test_pic_scramble_array_int64

   subroutine test_pic_scramble_array_sp(error)
      type(error_type), allocatable, intent(out) :: error
      real(sp) :: sp_arr(10)
      real(sp) :: arr_before(10)
      real(sp) :: sum_before
      sp_arr = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp, 6.0_sp, 7.0_sp, 8.0_sp, 9.0_sp, 10.0_sp]
      arr_before = sp_arr
      call random_seed()
      sum_before = 0.0_sp

      sum_before = sum(sp_arr)

      call pic_scramble_array(sp_arr)

      call check(error, is_equal(sum(sp_arr), sum_before), .true., "The arrays need to produce the same overall sum!")
      if (allocated(error)) return

      call check(error, all(is_equal(sp_arr, arr_before)), .false., "The arrays should not be equal!")
      if (allocated(error)) return

   end subroutine test_pic_scramble_array_sp

   subroutine test_pic_scramble_array_dp(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: dp_arr(10)
      real(dp) :: arr_before(10)
      real(dp) :: sum_before
      dp_arr = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp, 9.0_dp, 10.0_dp]
      arr_before = dp_arr
      call random_seed()
      sum_before = 0.0_dp

      sum_before = sum(dp_arr)

      call pic_scramble_array(dp_arr)

      call check(error, is_equal(sum(dp_arr), sum_before), .true., "The arrays need to produce the same overall sum!")
      if (allocated(error)) return

      call check(error, all(is_equal(dp_arr, arr_before)), .false., "The arrays should not be equal!")
      if (allocated(error)) return

   end subroutine test_pic_scramble_array_dp

   subroutine test_pic_scramble_array_char(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=1) :: char_arr(11)
      character(len=1) :: arr_before(11)
      char_arr = [character(len=1) :: "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"]
      arr_before = char_arr
      call random_seed()

      call pic_scramble_array(char_arr)

      call check(error, all(char_arr == arr_before), .false., "The arrays should not be equal!")
      if (allocated(error)) return

   end subroutine test_pic_scramble_array_char

   ! ------------------------------------------------------------------
   ! error_t reporting for pic_copy / pic_transpose
   !
   ! pic_array reports a shape mismatch through the optional err argument
   ! when one is supplied, and keeps the historical `error stop` abort when
   ! it is not. The abort path cannot be exercised from inside a test
   ! process, so these tests cover the err path plus the guarantee that the
   ! err-less calls still behave exactly as they always did on valid input.
   ! ------------------------------------------------------------------

   subroutine check_validation(error, err, what)
      !! Shared assertion for a reported shape mismatch: the error is set,
      !! its code is ERROR_VALIDATION and it carries a non-empty message.
      type(error_type), allocatable, intent(out) :: error
      type(error_t), intent(in) :: err
      character(len=*), intent(in) :: what

      call check(error, err%has_error(), "err must be set by "//what)
      if (allocated(error)) return

      call check(error, err%get_code(), ERROR_VALIDATION, "code must be ERROR_VALIDATION for "//what)
      if (allocated(error)) return

      call check(error, err%is(ERROR_VALIDATION), "err%is(ERROR_VALIDATION) must hold for "//what)
      if (allocated(error)) return

      call check(error, len_trim(err%get_message()) > 0, "message must not be empty for "//what)
      if (allocated(error)) return

   end subroutine check_validation

   subroutine test_copy_vector_err_mismatch(error)
      !! A vector pic_copy whose destination and source differ in length
      !! reports ERROR_VALIDATION and leaves the destination exactly as the
      !! caller left it, for every supported element type.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: dest_int32(3), source_int32(4)
      integer(int64) :: dest_int64(3), source_int64(4)
      real(sp) :: dest_sp(3), source_sp(4)
      real(dp) :: dest_dp(3), source_dp(4)

      dest_int32 = 7_int32
      source_int32 = 1_int32
      call pic_copy(dest_int32, source_int32, err=err)
      call check_validation(error, err, "pic_copy on int32 vectors")
      if (allocated(error)) return
      call check(error, all(dest_int32 == 7_int32), "int32 destination must be unchanged on mismatch")
      if (allocated(error)) return

      dest_int64 = 7_int64
      source_int64 = 1_int64
      call pic_copy(dest_int64, source_int64, err=err)
      call check_validation(error, err, "pic_copy on int64 vectors")
      if (allocated(error)) return
      call check(error, all(dest_int64 == 7_int64), "int64 destination must be unchanged on mismatch")
      if (allocated(error)) return

      dest_sp = 7.0_sp
      source_sp = 1.0_sp
      call pic_copy(dest_sp, source_sp, err=err)
      call check_validation(error, err, "pic_copy on sp vectors")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_sp, 7.0_sp)), "sp destination must be unchanged on mismatch")
      if (allocated(error)) return

      dest_dp = 7.0_dp
      source_dp = 1.0_dp
      call pic_copy(dest_dp, source_dp, err=err)
      call check_validation(error, err, "pic_copy on dp vectors")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_dp, 7.0_dp)), "dp destination must be unchanged on mismatch")
      if (allocated(error)) return

   end subroutine test_copy_vector_err_mismatch

   subroutine test_copy_matrix_err_mismatch(error)
      !! A matrix pic_copy reports ERROR_VALIDATION for a mismatch in either
      !! extent (rows for int32/sp, columns for int64/dp below) and leaves
      !! the destination unchanged.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: dest_int32(2, 3), source_int32(3, 3)
      integer(int64) :: dest_int64(2, 3), source_int64(2, 4)
      real(sp) :: dest_sp(2, 3), source_sp(3, 3)
      real(dp) :: dest_dp(2, 3), source_dp(2, 4)

      dest_int32 = 7_int32
      source_int32 = 1_int32
      call pic_copy(dest_int32, source_int32, err=err)
      call check_validation(error, err, "pic_copy on int32 matrices")
      if (allocated(error)) return
      call check(error, all(dest_int32 == 7_int32), "int32 destination must be unchanged on row mismatch")
      if (allocated(error)) return

      dest_int64 = 7_int64
      source_int64 = 1_int64
      call pic_copy(dest_int64, source_int64, err=err)
      call check_validation(error, err, "pic_copy on int64 matrices")
      if (allocated(error)) return
      call check(error, all(dest_int64 == 7_int64), "int64 destination must be unchanged on column mismatch")
      if (allocated(error)) return

      dest_sp = 7.0_sp
      source_sp = 1.0_sp
      call pic_copy(dest_sp, source_sp, err=err)
      call check_validation(error, err, "pic_copy on sp matrices")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_sp, 7.0_sp)), "sp destination must be unchanged on row mismatch")
      if (allocated(error)) return

      dest_dp = 7.0_dp
      source_dp = 1.0_dp
      call pic_copy(dest_dp, source_dp, err=err)
      call check_validation(error, err, "pic_copy on dp matrices")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_dp, 7.0_dp)), "dp destination must be unchanged on column mismatch")
      if (allocated(error)) return

   end subroutine test_copy_matrix_err_mismatch

   subroutine test_copy_3d_tensor_err_mismatch(error)
      !! A 3d pic_copy reports ERROR_VALIDATION for a mismatch in any of the
      !! three extents and leaves the destination unchanged. A different
      !! extent is perturbed per type so all three size checks are exercised.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: dest_int32(2, 2, 2), source_int32(3, 2, 2)
      integer(int64) :: dest_int64(2, 2, 2), source_int64(2, 3, 2)
      real(sp) :: dest_sp(2, 2, 2), source_sp(2, 2, 3)
      real(dp) :: dest_dp(2, 2, 2), source_dp(3, 3, 3)

      dest_int32 = 7_int32
      source_int32 = 1_int32
      call pic_copy(dest_int32, source_int32, err=err)
      call check_validation(error, err, "pic_copy on int32 tensors")
      if (allocated(error)) return
      call check(error, all(dest_int32 == 7_int32), "int32 destination must be unchanged on extent-1 mismatch")
      if (allocated(error)) return

      dest_int64 = 7_int64
      source_int64 = 1_int64
      call pic_copy(dest_int64, source_int64, err=err)
      call check_validation(error, err, "pic_copy on int64 tensors")
      if (allocated(error)) return
      call check(error, all(dest_int64 == 7_int64), "int64 destination must be unchanged on extent-2 mismatch")
      if (allocated(error)) return

      dest_sp = 7.0_sp
      source_sp = 1.0_sp
      call pic_copy(dest_sp, source_sp, err=err)
      call check_validation(error, err, "pic_copy on sp tensors")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_sp, 7.0_sp)), "sp destination must be unchanged on extent-3 mismatch")
      if (allocated(error)) return

      dest_dp = 7.0_dp
      source_dp = 1.0_dp
      call pic_copy(dest_dp, source_dp, err=err)
      call check_validation(error, err, "pic_copy on dp tensors")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_dp, 7.0_dp)), "dp destination must be unchanged on all-extent mismatch")
      if (allocated(error)) return

   end subroutine test_copy_3d_tensor_err_mismatch

   subroutine test_transpose_err_mismatch(error)
      !! pic_transpose reports ERROR_VALIDATION when the result is not shaped
      !! (cols, rows). The result is an intent(out) dummy, so on that path it
      !! is undefined and deliberately not inspected here; what is guaranteed
      !! is that the input matrix is untouched and that the call returns
      !! instead of aborting.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: a_int32(2, 3), b_int32(2, 3)
      integer(int64) :: a_int64(2, 3), b_int64_bad(2, 3), b_int64_good(3, 2)
      real(sp) :: a_sp(2, 3), b_sp(2, 3)
      real(dp) :: a_dp(2, 3), b_dp(3, 4)

      a_int32 = 5_int32
      call pic_transpose(a_int32, b_int32, err=err)
      call check_validation(error, err, "pic_transpose on int32 matrices")
      if (allocated(error)) return
      call check(error, all(a_int32 == 5_int32), "int32 input must be unchanged on mismatch")
      if (allocated(error)) return

      a_int64 = 5_int64
      call pic_transpose(a_int64, b_int64_bad, err=err)
      call check_validation(error, err, "pic_transpose on int64 matrices")
      if (allocated(error)) return
      call check(error, all(a_int64 == 5_int64), "int64 input must be unchanged on mismatch")
      if (allocated(error)) return

      ! a successful call never writes err, so the caller clears it first
      call err%clear()
      call pic_transpose(a_int64, b_int64_good, err=err)
      call check(error,.not. err%has_error(), "a (cols, rows) result must be accepted for int64")
      if (allocated(error)) return
      call check(error, all(b_int64_good == 5_int64), "int64 transpose must still copy the values through")
      if (allocated(error)) return

      a_sp = 5.0_sp
      call pic_transpose(a_sp, b_sp, err=err)
      call check_validation(error, err, "pic_transpose on sp matrices")
      if (allocated(error)) return
      call check(error, all(is_equal(a_sp, 5.0_sp)), "sp input must be unchanged on mismatch")
      if (allocated(error)) return

      a_dp = 5.0_dp
      call pic_transpose(a_dp, b_dp, err=err)
      call check_validation(error, err, "pic_transpose on dp matrices")
      if (allocated(error)) return
      call check(error, all(is_equal(a_dp, 5.0_dp)), "dp input must be unchanged on mismatch")
      if (allocated(error)) return

   end subroutine test_transpose_err_mismatch

   subroutine test_array_err_untouched_on_success(error)
      !! On valid input the routines never write to err. A caller-supplied,
      !! already-clear err therefore stays clear, and an err that already
      !! carried an unrelated error is deliberately left alone rather than
      !! cleared behind the caller's back.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: dest_vector(4), source_vector(4)
      real(dp) :: dest_matrix(2, 3), source_matrix(2, 3)
      real(dp) :: dest_tensor(2, 2, 2), source_tensor(2, 2, 2)
      real(dp) :: transposed(3, 2)

      source_vector = 3_int32
      dest_vector = 0_int32
      call pic_copy(dest_vector, source_vector, err=err)
      call check(error,.not. err%has_error(), "a valid vector copy must not set err")
      if (allocated(error)) return
      call check(error, err%get_code(), SUCCESS, "a valid vector copy must leave err at SUCCESS")
      if (allocated(error)) return
      call check(error, all(dest_vector == 3_int32), "a valid vector copy must still copy")
      if (allocated(error)) return

      source_matrix = 2.5_dp
      dest_matrix = 0.0_dp
      call pic_copy(dest_matrix, source_matrix, err=err)
      call check(error,.not. err%has_error(), "a valid matrix copy must not set err")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_matrix, 2.5_dp)), "a valid matrix copy must still copy")
      if (allocated(error)) return

      source_tensor = 1.5_dp
      dest_tensor = 0.0_dp
      call pic_copy(dest_tensor, source_tensor, err=err)
      call check(error,.not. err%has_error(), "a valid tensor copy must not set err")
      if (allocated(error)) return
      call check(error, all(is_equal(dest_tensor, 1.5_dp)), "a valid tensor copy must still copy")
      if (allocated(error)) return

      call pic_transpose(source_matrix, transposed, err=err)
      call check(error,.not. err%has_error(), "a valid transpose must not set err")
      if (allocated(error)) return
      call check(error, all(is_equal(transposed, 2.5_dp)), "a valid transpose must still transpose")
      if (allocated(error)) return

      call err%set(ERROR_IO, "unrelated failure the caller has not handled yet")
      call pic_copy(dest_vector, source_vector, err=err)
      call check(error, err%is(ERROR_IO), "a successful copy must not clear a pre-existing error")
      if (allocated(error)) return

   end subroutine test_array_err_untouched_on_success

   subroutine test_array_no_err_valid_input(error)
      !! Backward compatibility: the same calls made without err behave
      !! exactly as they did before err existed, threaded paths included.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: dest_vector(4), source_vector(4)
      real(dp) :: dest_matrix(2, 3), source_matrix(2, 3)
      real(dp) :: dest_tensor(2, 2, 2), source_tensor(2, 2, 2)
      real(dp) :: transposed(3, 2)

      source_vector = 3_int32
      dest_vector = 0_int32
      call pic_copy(dest_vector, source_vector)
      call check(error, all(dest_vector == 3_int32), "err-less vector copy must still copy")
      if (allocated(error)) return

      dest_vector = 0_int32
      call pic_copy(dest_vector, source_vector, .true.)
      call check(error, all(dest_vector == 3_int32), "err-less threaded vector copy must still copy")
      if (allocated(error)) return

      source_matrix = 2.5_dp
      dest_matrix = 0.0_dp
      call pic_copy(dest_matrix, source_matrix, .true.)
      call check(error, all(is_equal(dest_matrix, 2.5_dp)), "err-less threaded matrix copy must still copy")
      if (allocated(error)) return

      source_tensor = 1.5_dp
      dest_tensor = 0.0_dp
      call pic_copy(dest_tensor, source_tensor, .true.)
      call check(error, all(is_equal(dest_tensor, 1.5_dp)), "err-less threaded tensor copy must still copy")
      if (allocated(error)) return

      call pic_transpose(source_matrix, transposed, .true.)
      call check(error, all(is_equal(transposed, 2.5_dp)), "err-less threaded transpose must still transpose")
      if (allocated(error)) return

   end subroutine test_array_no_err_valid_input

   pure function pure_is_sorted_guard(array) result(sorted)
      !! Compile-time purity guard. is_sorted is pure, and downstream code is
      !! allowed to call it from its own pure procedures. If pic_array ever
      !! loses that purity this wrapper stops compiling, which is the point.
      integer(int32), intent(in) :: array(:)
      logical :: sorted

      sorted = is_sorted(array, ASCENDING)

   end function pure_is_sorted_guard

   subroutine test_array_pure_guard(error)
      !! Exercises the pure wrapper so the guard is linked, not just compiled.
      type(error_type), allocatable, intent(out) :: error

      call check(error, pure_is_sorted_guard([1_int32, 2_int32, 3_int32]), &
                 "the pure wrapper must report an ascending array as sorted")
      if (allocated(error)) return

      call check(error,.not. pure_is_sorted_guard([3_int32, 2_int32, 1_int32]), &
                 "the pure wrapper must report a descending array as unsorted")
      if (allocated(error)) return

   end subroutine test_array_pure_guard

   subroutine test_print_bad_format_err(error)
      !! An unrecognised format string is reported through err as
      !! ERROR_VALIDATION by every pic_print_array family, and the message
      !! names the offending string. The array is still printed with NumPy
      !! brackets on that path, which is visible on stdout rather than
      !! assertable from here, so err is all the caller has to go on.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: vector_int32(3)
      integer(int64) :: vector_int64(3)
      real(sp) :: vector_sp(3)
      real(dp) :: vector_dp(3)
      integer(int32) :: matrix_int32(2, 2)
      real(dp) :: matrix_dp(2, 2)
      real(sp) :: packed_sp(3)
      real(dp) :: tensor_dp(2, 2, 2)

      vector_int32 = 1_int32
      call pic_print_array(vector_int32, "NOT_A_FORMAT", err=err)
      call check_validation(error, err, "pic_print_array on an int32 vector with a bad format")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "NOT_A_FORMAT") > 0, &
                 "the message must name the unsupported format string")
      if (allocated(error)) return

      call err%clear()
      vector_int64 = 1_int64
      call pic_print_array(vector_int64, "numpyish", err=err)
      call check_validation(error, err, "pic_print_array on an int64 vector with a bad format")
      if (allocated(error)) return

      call err%clear()
      vector_sp = 1.0_sp
      call pic_print_array(vector_sp, "BOGUS", err=err)
      call check_validation(error, err, "pic_print_array on an sp vector with a bad format")
      if (allocated(error)) return

      call err%clear()
      vector_dp = 1.0_dp
      call pic_print_array(vector_dp, "BOGUS", err=err)
      call check_validation(error, err, "pic_print_array on a dp vector with a bad format")
      if (allocated(error)) return

      call err%clear()
      matrix_int32 = 2_int32
      call pic_print_array(matrix_int32, "BOGUS", err=err)
      call check_validation(error, err, "pic_print_array on an int32 matrix with a bad format")
      if (allocated(error)) return

      call err%clear()
      matrix_dp = 2.0_dp
      call pic_print_array(matrix_dp, "BOGUS", err=err)
      call check_validation(error, err, "pic_print_array on a dp matrix with a bad format")
      if (allocated(error)) return

      call err%clear()
      packed_sp = 1.0_sp
      call pic_print_array(packed_sp, 3_default_int, "BOGUS", err=err)
      call check_validation(error, err, "pic_print_array on an sp packed matrix with a bad format")
      if (allocated(error)) return

      call err%clear()
      tensor_dp = 3.0_dp
      call pic_print_array(tensor_dp, "BOGUS", err=err)
      call check_validation(error, err, "pic_print_array on a dp tensor with a bad format")
      if (allocated(error)) return

   end subroutine test_print_bad_format_err

   subroutine test_print_packed_bad_size_err(error)
      !! An n_elements that is not n*(n + 1)/2 for any n is reported as
      !! ERROR_VALIDATION and nothing is printed. Checked for all four packed
      !! specialisations, since each carries its own copy of the test.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: packed_int32(4)
      integer(int64) :: packed_int64(4)
      real(sp) :: packed_sp(4)
      real(dp) :: packed_dp(4)

      packed_int32 = 1_int32
      call pic_print_array(packed_int32, 4_default_int, err=err)
      call check_validation(error, err, "an int32 packed matrix of 4 elements")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "packed triangle") > 0, &
                 "the message must say the packed triangle size is wrong")
      if (allocated(error)) return

      call err%clear()
      packed_int64 = 1_int64
      call pic_print_array(packed_int64, 4_default_int, "PLAIN", err=err)
      call check_validation(error, err, "an int64 packed matrix of 4 elements")
      if (allocated(error)) return

      call err%clear()
      packed_sp = 1.0_sp
      call pic_print_array(packed_sp, 2_default_int, "MATHEMATICA", err=err)
      call check_validation(error, err, "an sp packed matrix of 2 elements")
      if (allocated(error)) return

      call err%clear()
      packed_dp = 1.0_dp
      call pic_print_array(packed_dp, 4_default_int, "NUMPY", err=err)
      call check_validation(error, err, "a dp packed matrix of 4 elements")
      if (allocated(error)) return

      call check(error, all(is_equal(packed_dp, 1.0_dp)), "the packed input must be left alone")
      if (allocated(error)) return

   end subroutine test_print_packed_bad_size_err

   subroutine test_print_bad_input_without_err(error)
      !! Documented err-less behaviour, unchanged from before err existed:
      !! both failures report to stdout and return, and neither aborts. These
      !! calls have no inspectable effect by design, so reaching the assertion
      !! below is what is being tested - a regression that turned either site
      !! into an error stop would take the whole test binary down here.
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: vector_int32(3)
      real(dp) :: matrix_dp(2, 2)
      real(dp) :: tensor_dp(2, 2, 2)
      integer(int32) :: packed_int32(4)
      integer(int64) :: packed_int64(4)
      real(sp) :: packed_sp(4)
      real(dp) :: packed_dp(4)
      integer(default_int) :: calls_survived

      calls_survived = 0

      vector_int32 = 1_int32
      call pic_print_array(vector_int32, "BOGUS")
      calls_survived = calls_survived + 1

      matrix_dp = 2.0_dp
      call pic_print_array(matrix_dp, "BOGUS")
      calls_survived = calls_survived + 1

      tensor_dp = 3.0_dp
      call pic_print_array(tensor_dp, "BOGUS")
      calls_survived = calls_survived + 1

      packed_int32 = 1_int32
      call pic_print_array(packed_int32, 4_default_int)
      calls_survived = calls_survived + 1

      packed_int64 = 1_int64
      call pic_print_array(packed_int64, 4_default_int)
      calls_survived = calls_survived + 1

      packed_sp = 1.0_sp
      call pic_print_array(packed_sp, 4_default_int)
      calls_survived = calls_survived + 1

      packed_dp = 1.0_dp
      call pic_print_array(packed_dp, 4_default_int)
      calls_survived = calls_survived + 1

      call check(error, calls_survived, 7_default_int, &
                 "every err-less bad-input print must return instead of aborting")
      if (allocated(error)) return

   end subroutine test_print_bad_input_without_err

   subroutine test_print_valid_input_err_clear(error)
      !! Valid input still prints and never writes err, with err present or
      !! absent. A packed size that is a genuine triangle is accepted, and a
      !! pre-existing unrelated error is not cleared behind the caller's back.
      type(error_type), allocatable, intent(out) :: error
      type(error_t) :: err
      integer(int32) :: vector_int32(3)
      real(dp) :: matrix_dp(2, 2)
      real(dp) :: tensor_dp(2, 2, 2)
      real(dp) :: packed_dp(6)

      vector_int32 = 1_int32
      call pic_print_array(vector_int32, "PLAIN", err=err)
      call check(error,.not. err%has_error(), "a supported format must not set err for a vector")
      if (allocated(error)) return
      call check(error, err%get_code(), SUCCESS, "err must stay at SUCCESS for a supported format")
      if (allocated(error)) return

      matrix_dp = 2.0_dp
      call pic_print_array(matrix_dp, "MATHEMATICA", err=err)
      call check(error,.not. err%has_error(), "a supported format must not set err for a matrix")
      if (allocated(error)) return

      tensor_dp = 3.0_dp
      call pic_print_array(tensor_dp, "NUMPY", err=err)
      call check(error,.not. err%has_error(), "a supported format must not set err for a tensor")
      if (allocated(error)) return

      packed_dp = 1.0_dp
      call pic_print_array(packed_dp, 6_default_int, "NUMPY", err=err)
      call check(error,.not. err%has_error(), "a valid packed triangle size must not set err")
      if (allocated(error)) return

      call err%set(ERROR_IO, "unrelated failure the caller has not handled yet")
      call pic_print_array(packed_dp, 6_default_int, err=err)
      call check(error, err%is(ERROR_IO), "a successful print must not clear a pre-existing error")
      if (allocated(error)) return

      ! the same calls without err, which is how every existing caller makes them
      call pic_print_array(vector_int32, "PLAIN")
      call pic_print_array(matrix_dp)
      call pic_print_array(tensor_dp, "NUMPY")
      call pic_print_array(packed_dp, 6_default_int)

      call check(error, all(is_equal(packed_dp, 1.0_dp)), "printing must not modify the array")
      if (allocated(error)) return

   end subroutine test_print_valid_input_err_clear

   subroutine test_is_sorted_rejects_unsorted(error)
      !! is_sorted must return .false. for every supported kind when the
      !! requested ordering is violated, for both orderings
      type(error_type), allocatable, intent(out) :: error
      integer(int32) :: a_int32(4)
      integer(int64) :: a_int64(4)
      real(sp) :: a_sp(4)
      real(dp) :: a_dp(4)
      character(len=3) :: a_char(4)

      a_int32 = [1_int32, 2_int32, 9_int32, 3_int32]
      a_int64 = [1_int64, 2_int64, 9_int64, 3_int64]
      a_sp = [1.0_sp, 2.0_sp, 9.0_sp, 3.0_sp]
      a_dp = [1.0_dp, 2.0_dp, 9.0_dp, 3.0_dp]
      a_char = ["aaa", "bbb", "zzz", "ccc"]

      ! Ascending is requested but the last step decreases.
      call check(error,.not. is_sorted(a_int32), "int32 descent must not count as ascending")
      if (allocated(error)) return

      call check(error,.not. is_sorted(a_int64), "int64 descent must not count as ascending")
      if (allocated(error)) return

      call check(error,.not. is_sorted(a_sp), "sp descent must not count as ascending")
      if (allocated(error)) return

      call check(error,.not. is_sorted(a_dp), "dp descent must not count as ascending")
      if (allocated(error)) return

      call check(error,.not. is_sorted(a_char), "character descent must not count as ascending")
      if (allocated(error)) return

      ! The explicit ASCENDING spelling must behave identically.
      call check(error,.not. is_sorted(a_int32, ASCENDING), "explicit ASCENDING must reject int32 descent")
      if (allocated(error)) return

      call check(error,.not. is_sorted(a_char, ASCENDING), "explicit ASCENDING must reject character descent")
      if (allocated(error)) return

      ! Reversing the data makes it ascending and not descending.
      a_int32 = [1_int32, 2_int32, 3_int32, 9_int32]
      a_char = ["aaa", "bbb", "ccc", "zzz"]

      call check(error, is_sorted(a_int32), "int32 ascending run must be accepted")
      if (allocated(error)) return

      call check(error,.not. is_sorted(a_int32, DESCENDING), "ascending int32 must not count as descending")
      if (allocated(error)) return

      call check(error, is_sorted(a_char), "character ascending run must be accepted")
      if (allocated(error)) return

      call check(error,.not. is_sorted(a_char, DESCENDING), "ascending characters must not count as descending")
      if (allocated(error)) return
   end subroutine test_is_sorted_rejects_unsorted

   subroutine test_print_packed_rejects_bad_size(error)
      !! A packed-triangle print with an element count that is not a
      !! triangular number must bail out instead of reading past the array.
      !! The arrays below are sized exactly to the (invalid) element count,
      !! so an unguarded routine would run off their ends.
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), parameter :: five = 5
      integer(int32) :: v_int32(5)
      integer(int64) :: v_int64(5)
      real(sp) :: v_sp(5)
      real(dp) :: v_dp(5)

      v_int32 = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32]
      v_int64 = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64]
      v_sp = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]
      v_dp = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]

      ! 5 is not of the form n*(n+1)/2, so all four of these must return early.
      call pic_print_array(v_int32, five, "PLAIN")
      call pic_print_array(v_int64, five, "PLAIN")
      call pic_print_array(v_sp, five, "PLAIN")
      call pic_print_array(v_dp, five, "PLAIN")

      call check(error, all(v_int32 == [1_int32, 2_int32, 3_int32, 4_int32, 5_int32]), &
                 "Rejected packed print must leave the int32 input untouched")
      if (allocated(error)) return

      call check(error, all(v_int64 == [1_int64, 2_int64, 3_int64, 4_int64, 5_int64]), &
                 "Rejected packed print must leave the int64 input untouched")
      if (allocated(error)) return

      call check(error, all(abs(v_sp - [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]) <= 0.0_sp), &
                 "Rejected packed print must leave the sp input untouched")
      if (allocated(error)) return

      call check(error, all(abs(v_dp - [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]) <= 0.0_dp), &
                 "Rejected packed print must leave the dp input untouched")
      if (allocated(error)) return
   end subroutine test_print_packed_rejects_bad_size

   subroutine test_print_unknown_format_falls_back(error)
      !! An unrecognised bracket style must fall back to the NumPy style
      !! instead of failing; the fallback is exercised on a valid triangle.
      type(error_type), allocatable, intent(out) :: error
      integer(default_int), parameter :: six = 6
      integer(int32) :: v_int32(6)

      v_int32 = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]

      call pic_print_array(v_int32, six, "NOT_A_REAL_FORMAT")

      call check(error, all(v_int32 == [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]), &
                 "Printing with an unknown format must not modify the input")
      if (allocated(error)) return
   end subroutine test_print_unknown_format_falls_back

end module test_pic_array
