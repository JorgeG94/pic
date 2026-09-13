module test_pic_csr
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, dp
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_csr, only: csr_t
   implicit none
   private

   public :: collect_pic_csr_tests

   real(dp), parameter :: TOL = 1.0e-12_dp

contains

   subroutine collect_pic_csr_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("coo_round_trip", test_coo_round_trip), &
                  new_unittest("unsorted_input", test_unsorted_input), &
                  new_unittest("duplicates_are_summed", test_duplicates_are_summed), &
                  new_unittest("empty_matrix", test_empty_matrix), &
                  new_unittest("single_element", test_single_element), &
                  new_unittest("empty_rows", test_empty_rows), &
                  new_unittest("rectangular", test_rectangular), &
                  new_unittest("matvec_dense_reference", test_matvec_dense_reference), &
                  new_unittest("matvec_dimension_mismatch", test_matvec_dimension_mismatch), &
                  new_unittest("matvec_invalid_matrix", test_matvec_invalid_matrix), &
                  new_unittest("transpose_twice", test_transpose_twice), &
                  new_unittest("transpose_rectangular", test_transpose_rectangular), &
                  new_unittest("transpose_invalid_matrix", test_transpose_invalid_matrix), &
                  new_unittest("build_errors", test_build_errors), &
                  new_unittest("row_slice_errors", test_row_slice_errors), &
                  new_unittest("is_valid_rejects", test_is_valid_rejects), &
                  new_unittest("destroy_resets", test_destroy_resets), &
                  new_unittest("optional_error_absent", test_optional_error_absent) &
                  ]
   end subroutine collect_pic_csr_tests

   subroutine build_reference(a, err)
      !! 3x3 matrix
      !!   [ 1  0  2 ]
      !!   [ 0  0  0 ]
      !!   [ 3  4  0 ]
      type(csr_t), intent(inout) :: a
      type(error_t), intent(out) :: err

      call a%build_from_coo(3_default_int, 3_default_int, &
                            [1_default_int, 1_default_int, 3_default_int, 3_default_int], &
                            [1_default_int, 3_default_int, 1_default_int, 2_default_int], &
                            [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], err)
   end subroutine build_reference

   subroutine test_coo_round_trip(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      integer(default_int), allocatable :: cols(:)
      real(dp), allocatable :: vals(:)

      call build_reference(a, err)
      call check(error,.not. err%has_error(), "build_from_coo must succeed")
      if (allocated(error)) return

      call check(error, a%n_rows() == 3, "n_rows")
      if (allocated(error)) return
      call check(error, a%n_cols() == 3, "n_cols")
      if (allocated(error)) return
      call check(error, a%nnz() == 4, "nnz")
      if (allocated(error)) return
      call check(error, a%is_valid(), "matrix must be valid")
      if (allocated(error)) return

      call check(error, all(a%row_ptr == [1, 3, 3, 5]), "row_ptr")
      if (allocated(error)) return

      call a%row_slice(1_default_int, cols, vals, err)
      call check(error, size(cols) == 2, "row 1 has two entries")
      if (allocated(error)) return
      call check(error, all(cols == [1, 3]), "row 1 columns")
      if (allocated(error)) return
      call check(error, abs(vals(1) - 1.0_dp) < TOL .and. abs(vals(2) - 2.0_dp) < TOL, "row 1 values")
      if (allocated(error)) return

      call a%row_slice(3_default_int, cols, vals, err)
      call check(error, all(cols == [1, 2]), "row 3 columns")
      if (allocated(error)) return
      call check(error, abs(vals(1) - 3.0_dp) < TOL .and. abs(vals(2) - 4.0_dp) < TOL, "row 3 values")
      if (allocated(error)) return
   end subroutine test_coo_round_trip

   subroutine test_unsorted_input(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err

      ! same matrix as build_reference but the triplets are scrambled
      call a%build_from_coo(3_default_int, 3_default_int, &
                            [3_default_int, 1_default_int, 3_default_int, 1_default_int], &
                            [2_default_int, 3_default_int, 1_default_int, 1_default_int], &
                            [4.0_dp, 2.0_dp, 3.0_dp, 1.0_dp], err)
      call check(error,.not. err%has_error(), "scrambled build must succeed")
      if (allocated(error)) return

      call check(error, all(a%row_ptr == [1, 3, 3, 5]), "row_ptr from scrambled input")
      if (allocated(error)) return
      call check(error, all(a%col_idx == [1, 3, 1, 2]), "col_idx sorted within rows")
      if (allocated(error)) return
      call check(error, maxval(abs(a%values - [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp])) < TOL, "values follow their columns")
      if (allocated(error)) return
   end subroutine test_unsorted_input

   subroutine test_duplicates_are_summed(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err

      ! (1,1) appears three times and (1,2) twice, in scrambled order
      call a%build_from_coo(2_default_int, 2_default_int, &
                            [1_default_int, 1_default_int, 2_default_int, 1_default_int, 1_default_int], &
                            [2_default_int, 1_default_int, 2_default_int, 1_default_int, 2_default_int], &
                            [10.0_dp, 1.0_dp, 7.0_dp, 2.0_dp, 20.0_dp], err)
      call check(error,.not. err%has_error(), "duplicate build must succeed")
      if (allocated(error)) return

      call check(error, a%nnz() == 3, "duplicates collapse to three stored entries")
      if (allocated(error)) return
      call check(error, all(a%row_ptr == [1, 3, 4]), "row_ptr after compression")
      if (allocated(error)) return
      call check(error, all(a%col_idx == [1, 2, 2]), "col_idx after compression")
      if (allocated(error)) return
      call check(error, abs(a%values(1) - 3.0_dp) < TOL, "(1,1) sums to 3")
      if (allocated(error)) return
      call check(error, abs(a%values(2) - 30.0_dp) < TOL, "(1,2) sums to 30")
      if (allocated(error)) return
      call check(error, abs(a%values(3) - 7.0_dp) < TOL, "(2,2) untouched")
      if (allocated(error)) return
   end subroutine test_duplicates_are_summed

   subroutine test_empty_matrix(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a, at
      type(error_t) :: err
      integer(default_int) :: no_index(0)
      real(dp) :: no_value(0)
      real(dp) :: y(0)

      call a%build_from_coo(0_default_int, 0_default_int, no_index, no_index, no_value, err)
      call check(error,.not. err%has_error(), "0x0 build must succeed")
      if (allocated(error)) return
      call check(error, a%n_rows() == 0 .and. a%n_cols() == 0 .and. a%nnz() == 0, "0x0 shape")
      if (allocated(error)) return
      call check(error, a%is_valid(), "0x0 matrix is valid")
      if (allocated(error)) return
      call check(error, size(a%row_ptr) == 1 .and. a%row_ptr(1) == 1, "0x0 row_ptr is [1]")
      if (allocated(error)) return

      call a%matvec(no_value, y, err)
      call check(error,.not. err%has_error(), "matvec on a 0x0 matrix is a no-op")
      if (allocated(error)) return

      call a%transpose(at, err)
      call check(error,.not. err%has_error(), "transpose of 0x0 is valid")
      if (allocated(error)) return
      call check(error, at%is_valid(), "transpose of 0x0 is valid")
      if (allocated(error)) return

      ! a 3x0 matrix: three rows, all necessarily empty
      call a%build_from_coo(3_default_int, 0_default_int, no_index, no_index, no_value, err)
      call check(error,.not. err%has_error(), "3x0 build")
      if (allocated(error)) return
      call check(error, a%is_valid(), "3x0 build")
      if (allocated(error)) return
      call check(error, all(a%row_ptr == [1, 1, 1, 1]), "3x0 row_ptr")
      if (allocated(error)) return
   end subroutine test_empty_matrix

   subroutine test_single_element(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      real(dp) :: y(1)

      call a%build_from_coo(1_default_int, 1_default_int, [1_default_int], [1_default_int], [2.5_dp], err)
      call check(error,.not. err%has_error(), "1x1 build")
      if (allocated(error)) return
      call check(error, a%is_valid(), "1x1 build")
      if (allocated(error)) return
      call check(error, a%nnz() == 1, "1x1 nnz")
      if (allocated(error)) return
      call check(error, all(a%row_ptr == [1, 2]), "1x1 row_ptr")
      if (allocated(error)) return

      call a%matvec([4.0_dp], y, err)
      call check(error, abs(y(1) - 10.0_dp) < TOL, "1x1 matvec")
      if (allocated(error)) return
   end subroutine test_single_element

   subroutine test_empty_rows(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      integer(default_int), allocatable :: cols(:)
      real(dp), allocatable :: vals(:)
      real(dp) :: y(4)

      ! rows 1, 2 and 4 are empty; only row 3 carries anything
      call a%build_from_coo(4_default_int, 4_default_int, &
                            [3_default_int], [2_default_int], [5.0_dp], err)
      call check(error,.not. err%has_error(), "build with empty rows")
      if (allocated(error)) return
      call check(error, a%is_valid(), "build with empty rows")
      if (allocated(error)) return
      call check(error, all(a%row_ptr == [1, 1, 1, 2, 2]), "row_ptr marks empty rows")
      if (allocated(error)) return

      call a%row_slice(1_default_int, cols, vals, err)
      call check(error,.not. err%has_error(), "slicing an empty row is not an error")
      if (allocated(error)) return
      call check(error, size(cols) == 0 .and. size(vals) == 0, "empty row slices to nothing")
      if (allocated(error)) return

      call a%row_slice(4_default_int, cols, vals, err)
      call check(error, size(cols) == 0, "trailing empty row slices to nothing")
      if (allocated(error)) return

      call a%matvec([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], y, err)
      call check(error, maxval(abs(y - [0.0_dp, 0.0_dp, 10.0_dp, 0.0_dp])) < TOL, "matvec zeroes empty rows")
      if (allocated(error)) return
   end subroutine test_empty_rows

   subroutine test_rectangular(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      real(dp) :: y(2)

      ! 2x4:
      !   [ 0  1  0  2 ]
      !   [ 3  0  0  0 ]
      call a%build_from_coo(2_default_int, 4_default_int, &
                            [1_default_int, 1_default_int, 2_default_int], &
                            [4_default_int, 2_default_int, 1_default_int], &
                            [2.0_dp, 1.0_dp, 3.0_dp], err)
      call check(error,.not. err%has_error(), "rectangular build")
      if (allocated(error)) return
      call check(error, a%is_valid(), "rectangular build")
      if (allocated(error)) return
      call check(error, a%n_rows() == 2 .and. a%n_cols() == 4, "rectangular shape")
      if (allocated(error)) return
      call check(error, all(a%col_idx == [2, 4, 1]), "rectangular col_idx")
      if (allocated(error)) return

      call a%matvec([1.0_dp, 10.0_dp, 100.0_dp, 1000.0_dp], y, err)
      call check(error, maxval(abs(y - [2010.0_dp, 3.0_dp])) < TOL, "rectangular matvec")
      if (allocated(error)) return
   end subroutine test_rectangular

   subroutine test_matvec_dense_reference(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      real(dp) :: dense(3, 3), x(3), y(3), expected(3)
      integer(default_int) :: i, j

      call build_reference(a, err)

      dense = 0.0_dp
      dense(1, 1) = 1.0_dp
      dense(1, 3) = 2.0_dp
      dense(3, 1) = 3.0_dp
      dense(3, 2) = 4.0_dp
      x = [7.0_dp, -1.0_dp, 0.5_dp]

      expected = 0.0_dp
      do i = 1, 3
         do j = 1, 3
            expected(i) = expected(i) + dense(i, j)*x(j)
         end do
      end do

      call a%matvec(x, y, err)
      call check(error,.not. err%has_error(), "matvec must succeed")
      if (allocated(error)) return
      call check(error, maxval(abs(y - expected)) < TOL, "sparse matvec matches the dense product")
      if (allocated(error)) return
      call check(error, maxval(abs(y - [8.0_dp, 0.0_dp, 17.0_dp])) < TOL, "matvec matches hand computation")
      if (allocated(error)) return
   end subroutine test_matvec_dense_reference

   subroutine test_matvec_dimension_mismatch(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      real(dp) :: y_good(3), y_bad(2)

      call build_reference(a, err)

      y_good = -1.0_dp
      call a%matvec([1.0_dp, 1.0_dp], y_good, err)
      call check(error, err%has_error(), "short x must be rejected")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_VALIDATION, "short x is a validation error")
      if (allocated(error)) return
      call check(error, all(abs(y_good + 1.0_dp) < TOL), "y is untouched on a dimension error")
      if (allocated(error)) return

      call a%matvec([1.0_dp, 1.0_dp, 1.0_dp], y_bad, err)
      call check(error, err%has_error(), "short y must be rejected")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_VALIDATION, "short y is a validation error")
      if (allocated(error)) return
   end subroutine test_matvec_dimension_mismatch

   subroutine test_matvec_invalid_matrix(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      real(dp) :: y(3)

      call build_reference(a, err)
      a%col_idx(1) = 99          ! now points outside 1:n_cols
      call a%matvec([1.0_dp, 1.0_dp, 1.0_dp], y, err)
      call check(error, err%has_error(), "matvec must refuse a malformed matrix")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "col_idx") > 0, "error names the broken array")
      if (allocated(error)) return
   end subroutine test_matvec_invalid_matrix

   subroutine test_transpose_twice(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a, at, att
      type(error_t) :: err

      call build_reference(a, err)
      call a%transpose(at, err)
      call check(error,.not. err%has_error(), "first transpose")
      if (allocated(error)) return
      call check(error, at%is_valid(), "first transpose")
      if (allocated(error)) return

      ! A^T of the 3x3 reference is [1 0 3; 0 0 4; 2 0 0]
      call check(error, all(at%row_ptr == [1, 3, 4, 5]), "transpose row_ptr")
      if (allocated(error)) return
      call check(error, all(at%col_idx == [1, 3, 3, 1]), "transpose col_idx")
      if (allocated(error)) return
      call check(error, maxval(abs(at%values - [1.0_dp, 3.0_dp, 4.0_dp, 2.0_dp])) < TOL, "transpose values")
      if (allocated(error)) return

      call at%transpose(att, err)
      call check(error,.not. err%has_error(), "second transpose")
      if (allocated(error)) return
      call check(error, att%n_rows() == a%n_rows() .and. att%n_cols() == a%n_cols(), "double transpose shape")
      if (allocated(error)) return
      call check(error, all(att%row_ptr == a%row_ptr), "double transpose row_ptr")
      if (allocated(error)) return
      call check(error, all(att%col_idx == a%col_idx), "double transpose col_idx")
      if (allocated(error)) return
      call check(error, maxval(abs(att%values - a%values)) < TOL, "double transpose values")
      if (allocated(error)) return
   end subroutine test_transpose_twice

   subroutine test_transpose_rectangular(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a, at
      type(error_t) :: err
      real(dp) :: y(4)

      call a%build_from_coo(2_default_int, 4_default_int, &
                            [1_default_int, 2_default_int], &
                            [3_default_int, 1_default_int], &
                            [6.0_dp, 7.0_dp], err)
      call a%transpose(at, err)
      call check(error, at%n_rows() == 4 .and. at%n_cols() == 2, "transpose swaps the shape")
      if (allocated(error)) return
      call check(error, at%is_valid(), "rectangular transpose is valid")
      if (allocated(error)) return

      call at%matvec([1.0_dp, 1.0_dp], y, err)
      call check(error, maxval(abs(y - [7.0_dp, 0.0_dp, 6.0_dp, 0.0_dp])) < TOL, "transposed matvec")
      if (allocated(error)) return
   end subroutine test_transpose_rectangular

   subroutine test_transpose_invalid_matrix(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a, at
      type(error_t) :: err

      call build_reference(a, err)
      a%nrow = -1
      call a%transpose(at, err)
      call check(error, err%has_error(), "transpose must refuse a malformed matrix")
      if (allocated(error)) return
      call check(error, at%n_rows() == 0 .and. at%nnz() == 0, "the target is left empty")
      if (allocated(error)) return
   end subroutine test_transpose_invalid_matrix

   subroutine test_build_errors(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err

      call a%build_from_coo(2_default_int, 2_default_int, &
                            [1_default_int, 2_default_int], [1_default_int], [1.0_dp, 2.0_dp], err)
      call check(error, err%has_error(), "mismatched cols length is rejected")
      if (allocated(error)) return
      call check(error, a%nnz() == 0 .and. a%n_rows() == 0, "a rejected build leaves the matrix empty")
      if (allocated(error)) return

      call a%build_from_coo(2_default_int, 2_default_int, &
                            [1_default_int], [1_default_int], [1.0_dp, 2.0_dp], err)
      call check(error, err%has_error(), "mismatched vals length is rejected")
      if (allocated(error)) return

      call a%build_from_coo(-1_default_int, 2_default_int, &
                            [1_default_int], [1_default_int], [1.0_dp], err)
      call check(error, err%has_error(), "negative n_rows is rejected")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_VALIDATION, "negative n_rows is a validation error")
      if (allocated(error)) return

      call a%build_from_coo(2_default_int, -3_default_int, &
                            [1_default_int], [1_default_int], [1.0_dp], err)
      call check(error, err%has_error(), "negative n_cols is rejected")
      if (allocated(error)) return

      call a%build_from_coo(2_default_int, 2_default_int, &
                            [0_default_int], [1_default_int], [1.0_dp], err)
      call check(error, err%has_error(), "row coordinate 0 is rejected")
      if (allocated(error)) return

      call a%build_from_coo(2_default_int, 2_default_int, &
                            [3_default_int], [1_default_int], [1.0_dp], err)
      call check(error, err%has_error(), "row coordinate past n_rows is rejected")
      if (allocated(error)) return

      call a%build_from_coo(2_default_int, 2_default_int, &
                            [1_default_int], [0_default_int], [1.0_dp], err)
      call check(error, err%has_error(), "column coordinate 0 is rejected")
      if (allocated(error)) return

      call a%build_from_coo(2_default_int, 2_default_int, &
                            [1_default_int], [5_default_int], [1.0_dp], err)
      call check(error, err%has_error(), "column coordinate past n_cols is rejected")
      if (allocated(error)) return
   end subroutine test_build_errors

   subroutine test_row_slice_errors(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err
      integer(default_int), allocatable :: cols(:)
      real(dp), allocatable :: vals(:)

      call build_reference(a, err)

      call a%row_slice(0_default_int, cols, vals, err)
      call check(error, err%has_error(), "row index 0 is rejected")
      if (allocated(error)) return
      call check(error, size(cols) == 0 .and. size(vals) == 0, "rejected slice is zero-size")
      if (allocated(error)) return

      call a%row_slice(4_default_int, cols, vals, err)
      call check(error, err%has_error(), "row index past n_rows is rejected")
      if (allocated(error)) return

      a%nrow = -2
      call a%row_slice(1_default_int, cols, vals, err)
      call check(error, err%has_error(), "row_slice refuses a malformed matrix")
      if (allocated(error)) return
      call check(error, size(cols) == 0, "malformed slice is zero-size")
      if (allocated(error)) return
   end subroutine test_row_slice_errors

   subroutine test_is_valid_rejects(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a, blank
      type(error_t) :: err

      call check(error,.not. blank%is_valid(err), "a default-initialised matrix has no storage")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "unallocated") > 0, "unallocated storage is named")
      if (allocated(error)) return

      call build_reference(a, err)
      a%ncol = -1
      call check(error,.not. a%is_valid(err), "negative n_cols rejected")
      if (allocated(error)) return
      call check(error, err%get_code() == ERROR_VALIDATION, "validation error code")
      if (allocated(error)) return

      call build_reference(a, err)
      deallocate (a%row_ptr)
      allocate (a%row_ptr(2))
      a%row_ptr = [1, 5]
      call check(error,.not. a%is_valid(err), "wrong row_ptr length rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "n_rows + 1") > 0, "row_ptr length is named")
      if (allocated(error)) return

      call build_reference(a, err)
      deallocate (a%values)
      allocate (a%values(2))
      a%values = 0.0_dp
      call check(error,.not. a%is_valid(err), "values shorter than col_idx rejected")
      if (allocated(error)) return

      call build_reference(a, err)
      a%row_ptr(1) = 0
      call check(error,.not. a%is_valid(err), "row_ptr not starting at 1 rejected")
      if (allocated(error)) return

      call build_reference(a, err)
      a%row_ptr(2) = 0
      call check(error,.not. a%is_valid(err), "non-monotone row_ptr rejected")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "monotonic") > 0, "monotonicity is named")
      if (allocated(error)) return

      call build_reference(a, err)
      a%row_ptr(4) = 4
      call check(error,.not. a%is_valid(err), "row_ptr not ending at nnz+1 rejected")
      if (allocated(error)) return

      call build_reference(a, err)
      a%col_idx(2) = 0
      call check(error,.not. a%is_valid(err), "column index 0 rejected")
      if (allocated(error)) return

      call build_reference(a, err)
      a%col_idx(2) = 4
      call check(error,.not. a%is_valid(err), "column index past n_cols rejected")
      if (allocated(error)) return
   end subroutine test_is_valid_rejects

   subroutine test_destroy_resets(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a
      type(error_t) :: err

      call build_reference(a, err)
      call a%destroy()
      call check(error, a%n_rows() == 0 .and. a%n_cols() == 0, "destroy clears the shape")
      if (allocated(error)) return
      call check(error, a%nnz() == 0, "nnz of an unallocated matrix is zero")
      if (allocated(error)) return
      call check(error,.not. allocated(a%row_ptr), "row_ptr released")
      if (allocated(error)) return

      call a%destroy()
      call check(error, a%nnz() == 0, "destroy is idempotent")
      if (allocated(error)) return
   end subroutine test_destroy_resets

   subroutine test_optional_error_absent(error)
      type(error_type), allocatable, intent(out) :: error
      type(csr_t) :: a, at
      integer(default_int), allocatable :: cols(:)
      real(dp), allocatable :: vals(:)
      real(dp) :: y(2)

      ! every entry point must tolerate a caller that does not pass err
      call a%build_from_coo(2_default_int, 2_default_int, [5_default_int], [1_default_int], [1.0_dp])
      call check(error, a%nnz() == 0, "failed build without err leaves an empty matrix")
      if (allocated(error)) return

      call a%build_from_coo(2_default_int, 2_default_int, [1_default_int], [2_default_int], [3.0_dp])
      call a%matvec([1.0_dp, 1.0_dp], y)
      call check(error, abs(y(1) - 3.0_dp) < TOL, "matvec without err")
      if (allocated(error)) return

      call a%transpose(at)
      call check(error, at%is_valid(), "transpose without err")
      if (allocated(error)) return

      call a%row_slice(1_default_int, cols, vals)
      call check(error, size(cols) == 1, "row_slice without err")
      if (allocated(error)) return

      call a%row_slice(9_default_int, cols, vals)
      call check(error, size(cols) == 0, "failed row_slice without err")
      if (allocated(error)) return
   end subroutine test_optional_error_absent

end module test_pic_csr
