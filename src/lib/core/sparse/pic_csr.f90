! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Compressed sparse row (CSR) matrix storage and the core CSR kernels.
module pic_csr
   !! A compressed sparse row matrix: the standard layout for sparse linear
   !! algebra and, identically, for adjacency lists of weighted graphs.
   !! `pic_graph` builds its shortest-path algorithms directly on this type.
   !!
   !! ### Layout
   !!
   !! A `csr_t` holds the usual triple, all of it one-based:
   !!
   !! * `row_ptr(1:nrow+1)` - `row_ptr(i)` is the index in `col_idx`/`values`
   !!   of the first stored entry of row `i`, and `row_ptr(i+1) - 1` the index
   !!   of its last. `row_ptr(1)` is always 1 and `row_ptr(nrow+1)` is
   !!   `nnz + 1`.
   !! * `col_idx(1:nnz)` - column index of each stored entry, sorted
   !!   ascending within every row.
   !! * `values(1:nnz)` - the matching `real(dp)` values.
   !!
   !! An empty row `i` is represented by `row_ptr(i) == row_ptr(i+1)`, which
   !! makes every row loop `do k = row_ptr(i), row_ptr(i+1) - 1` iterate zero
   !! times. That case is explicitly supported and tested.
   !!
   !! ### Duplicate coordinate entries are summed
   !!
   !! `build_from_coo` accepts coordinate triplets in any order. Repeated
   !! `(i, j)` coordinates are **summed**, which is the conventional
   !! finite-element assembly semantics and makes the builder usable directly
   !! as an assembly routine. Duplicates are not an error.
   !!
   !! ### Components are public on purpose
   !!
   !! The three arrays and the shape are public components so that consumers
   !! such as `pic_graph` can walk rows without copying. Writing to them by
   !! hand can produce a structurally broken matrix, which is exactly what
   !! `is_valid` exists to detect; `matvec`, `transpose` and `row_slice` call
   !! it before touching any index, so a malformed matrix produces an
   !! `error_t` rather than an out-of-bounds access.
   !!
   !! ### Usage
   !!
   !!```fortran
   !! type(csr_t) :: a
   !! type(error_t) :: err
   !! real(dp) :: y(2)
   !!
   !! call a%build_from_coo(2_default_int, 2_default_int, &
   !!                       [1_default_int, 2_default_int], &
   !!                       [2_default_int, 1_default_int], &
   !!                       [3.0_dp, 4.0_dp], err)
   !! call a%matvec([1.0_dp, 1.0_dp], y, err)
   !!```
   use pic_types, only: default_int, dp
   use pic_error, only: error_t, ERROR_VALIDATION
   implicit none

   private

   public :: csr_t

   type :: csr_t
      !! Compressed sparse row matrix with `real(dp)` values.
      integer(default_int) :: nrow = 0
         !! Number of rows; prefer the `n_rows` accessor when reading
      integer(default_int) :: ncol = 0
         !! Number of columns; prefer the `n_cols` accessor when reading
      integer(default_int), allocatable :: row_ptr(:)
         !! Row start offsets, size `nrow + 1`, one-based
      integer(default_int), allocatable :: col_idx(:)
         !! Column index of every stored entry, ascending within each row
      real(dp), allocatable :: values(:)
         !! Value of every stored entry, parallel to `col_idx`
   contains
      procedure :: build_from_coo => csr_build_from_coo
      procedure :: n_rows => csr_n_rows
      procedure :: n_cols => csr_n_cols
      procedure :: nnz => csr_nnz
      procedure :: row_slice => csr_row_slice
      procedure :: matvec => csr_matvec
      procedure :: transpose => csr_transpose
      procedure :: is_valid => csr_is_valid
      procedure :: destroy => csr_destroy
   end type csr_t

contains

   subroutine csr_build_from_coo(self, n_rows, n_cols, rows, cols, vals, err)
      !! Build the matrix from coordinate (COO) triplets.
      !!
      !! `rows`, `cols` and `vals` must have the same length and may be given
      !! in any order; entries are bucketed by row and then sorted by column.
      !! **Repeated `(i, j)` coordinates are summed.** Explicitly stored zeros
      !! are kept, so a triplet with value zero still occupies a slot.
      !!
      !! Reports `ERROR_VALIDATION` when the three arrays differ in length,
      !! when `n_rows` or `n_cols` is negative, or when any coordinate falls
      !! outside `1:n_rows` / `1:n_cols`. On any error the matrix is left
      !! empty rather than half built.
      class(csr_t), intent(inout) :: self
      integer(default_int), intent(in) :: n_rows
         !! Number of rows of the assembled matrix
      integer(default_int), intent(in) :: n_cols
         !! Number of columns of the assembled matrix
      integer(default_int), intent(in) :: rows(:)
         !! Row coordinate of each triplet
      integer(default_int), intent(in) :: cols(:)
         !! Column coordinate of each triplet
      real(dp), intent(in) :: vals(:)
         !! Value of each triplet
      type(error_t), intent(out), optional :: err

      integer(default_int) :: n_in, k, i, j, lo, hi, write_pos
      integer(default_int), allocatable :: start(:), next_free(:), tmp_col(:)
      real(dp), allocatable :: tmp_val(:)

      call self%destroy()

      n_in = int(size(rows), default_int)
      if (int(size(cols), default_int) /= n_in .or. int(size(vals), default_int) /= n_in) then
         call fail(err, "pic_csr: build_from_coo requires rows, cols and vals of equal length")
         return
      end if

      if (n_rows < 0 .or. n_cols < 0) then
         call fail(err, "pic_csr: build_from_coo requires non-negative matrix dimensions")
         return
      end if

      do k = 1, n_in
         if (rows(k) < 1 .or. rows(k) > n_rows) then
            call fail(err, "pic_csr: build_from_coo got a row coordinate outside 1:n_rows")
            return
         end if
         if (cols(k) < 1 .or. cols(k) > n_cols) then
            call fail(err, "pic_csr: build_from_coo got a column coordinate outside 1:n_cols")
            return
         end if
      end do

      self%nrow = n_rows
      self%ncol = n_cols

      allocate (self%row_ptr(n_rows + 1))
      allocate (start(n_rows + 1))
      allocate (next_free(n_rows))
      allocate (tmp_col(n_in))
      allocate (tmp_val(n_in))

      ! bucket by row: count, prefix sum, then scatter
      start = 0
      do k = 1, n_in
         start(rows(k) + 1) = start(rows(k) + 1) + 1
      end do
      start(1) = 1
      do i = 1, n_rows
         start(i + 1) = start(i + 1) + start(i)
         next_free(i) = start(i)
      end do
      do k = 1, n_in
         i = rows(k)
         tmp_col(next_free(i)) = cols(k)
         tmp_val(next_free(i)) = vals(k)
         next_free(i) = next_free(i) + 1
      end do

      ! sort each row by column index, then merge duplicates by summing
      write_pos = 0
      self%row_ptr(1) = 1
      do i = 1, n_rows
         lo = start(i)
         hi = start(i + 1) - 1
         call sort_row(tmp_col, tmp_val, lo, hi)
         k = lo
         do while (k <= hi)
            write_pos = write_pos + 1
            tmp_col(write_pos) = tmp_col(k)
            tmp_val(write_pos) = tmp_val(k)
            j = k + 1
            do while (j <= hi)
               if (tmp_col(j) /= tmp_col(write_pos)) exit
               tmp_val(write_pos) = tmp_val(write_pos) + tmp_val(j)
               j = j + 1
            end do
            k = j
         end do
         self%row_ptr(i + 1) = write_pos + 1
      end do

      allocate (self%col_idx(write_pos))
      allocate (self%values(write_pos))
      do k = 1, write_pos
         self%col_idx(k) = tmp_col(k)
         self%values(k) = tmp_val(k)
      end do
   end subroutine csr_build_from_coo

   pure subroutine sort_row(cols, vals, lo, hi)
      !! Insertion-sort one row segment `lo:hi` by column index.
      !!
      !! Insertion sort is deliberate: CSR rows are short in practice, the
      !! segment is already contiguous, and a stable in-place sort keeps
      !! equal-column duplicates in their original relative order so the
      !! subsequent summation is a plain left-to-right sweep.
      integer(default_int), intent(inout) :: cols(:)
      real(dp), intent(inout) :: vals(:)
      integer(default_int), intent(in) :: lo
      integer(default_int), intent(in) :: hi

      integer(default_int) :: i, j, key_col
      real(dp) :: key_val

      do i = lo + 1, hi
         key_col = cols(i)
         key_val = vals(i)
         j = i - 1
         do while (j >= lo)
            if (cols(j) <= key_col) exit
            cols(j + 1) = cols(j)
            vals(j + 1) = vals(j)
            j = j - 1
         end do
         cols(j + 1) = key_col
         vals(j + 1) = key_val
      end do
   end subroutine sort_row

   pure function csr_n_rows(self) result(n)
      !! Number of rows of the matrix.
      class(csr_t), intent(in) :: self
      integer(default_int) :: n

      n = self%nrow
   end function csr_n_rows

   pure function csr_n_cols(self) result(n)
      !! Number of columns of the matrix.
      class(csr_t), intent(in) :: self
      integer(default_int) :: n

      n = self%ncol
   end function csr_n_cols

   pure function csr_nnz(self) result(n)
      !! Number of stored entries, including any explicitly stored zeros.
      class(csr_t), intent(in) :: self
      integer(default_int) :: n

      n = 0
      if (allocated(self%col_idx)) n = int(size(self%col_idx), default_int)
   end function csr_nnz

   subroutine csr_row_slice(self, i, cols, vals, err)
      !! Copy out the column indices and values stored in row `i`.
      !!
      !! Both outputs are allocated to the number of stored entries in that
      !! row, which is zero for an empty row. A row index outside `1:nrow`
      !! reports `ERROR_VALIDATION` and yields zero-size outputs.
      class(csr_t), intent(in) :: self
      integer(default_int), intent(in) :: i
         !! Row to read, in `1:nrow`
      integer(default_int), allocatable, intent(out) :: cols(:)
         !! Column indices of row `i`, ascending
      real(dp), allocatable, intent(out) :: vals(:)
         !! Values of row `i`, parallel to `cols`
      type(error_t), intent(out), optional :: err

      integer(default_int) :: lo, hi, n, k

      if (.not. self%is_valid(err)) then
         allocate (cols(0))
         allocate (vals(0))
         return
      end if

      if (i < 1 .or. i > self%nrow) then
         call fail(err, "pic_csr: row_slice got a row index outside 1:n_rows")
         allocate (cols(0))
         allocate (vals(0))
         return
      end if

      lo = self%row_ptr(i)
      hi = self%row_ptr(i + 1) - 1
      n = hi - lo + 1
      allocate (cols(n))
      allocate (vals(n))
      do k = 1, n
         cols(k) = self%col_idx(lo + k - 1)
         vals(k) = self%values(lo + k - 1)
      end do
   end subroutine csr_row_slice

   subroutine csr_matvec(self, x, y, err)
      !! Sparse matrix-vector product `y = A*x`.
      !!
      !! `x` must have `n_cols` elements and `y` exactly `n_rows`; anything
      !! else reports `ERROR_VALIDATION` and leaves `y` untouched, rather than
      !! reading past the end of either vector. The matrix is checked with
      !! `is_valid` first, so a structurally broken matrix is an error and not
      !! an out-of-bounds access; that check is O(nnz), the same order as the
      !! product itself.
      class(csr_t), intent(in) :: self
      real(dp), intent(in) :: x(:)
         !! Input vector of length `n_cols`
      real(dp), intent(out) :: y(:)
         !! Output vector of length `n_rows`
      type(error_t), intent(out), optional :: err

      integer(default_int) :: i, k
      real(dp) :: acc

      if (.not. self%is_valid(err)) return

      if (int(size(x), default_int) /= self%ncol) then
         call fail(err, "pic_csr: matvec input vector length does not match n_cols")
         return
      end if
      if (int(size(y), default_int) /= self%nrow) then
         call fail(err, "pic_csr: matvec output vector length does not match n_rows")
         return
      end if

      do i = 1, self%nrow
         acc = 0.0_dp
         do k = self%row_ptr(i), self%row_ptr(i + 1) - 1
            acc = acc + self%values(k)*x(self%col_idx(k))
         end do
         y(i) = acc
      end do
   end subroutine csr_matvec

   subroutine csr_transpose(self, at, err)
      !! Build the transpose of this matrix into `at`.
      !!
      !! `at` is an `n_cols` by `n_rows` matrix whose rows are, as always,
      !! sorted ascending by column index. Transposing twice reproduces the
      !! original structure and values exactly. The source is checked with
      !! `is_valid` first; a malformed source reports `ERROR_VALIDATION` and
      !! leaves `at` empty.
      class(csr_t), intent(in) :: self
      type(csr_t), intent(out) :: at
         !! Receives the transpose
      type(error_t), intent(out), optional :: err

      integer(default_int) :: i, k, j, n_entries
      integer(default_int), allocatable :: next_free(:)

      call at%destroy()
      if (.not. self%is_valid(err)) return

      n_entries = self%nnz()
      at%nrow = self%ncol
      at%ncol = self%nrow

      allocate (at%row_ptr(at%nrow + 1))
      allocate (at%col_idx(n_entries))
      allocate (at%values(n_entries))
      allocate (next_free(at%nrow))

      at%row_ptr = 0
      do k = 1, n_entries
         j = self%col_idx(k) + 1
         at%row_ptr(j) = at%row_ptr(j) + 1
      end do
      at%row_ptr(1) = 1
      do i = 1, at%nrow
         at%row_ptr(i + 1) = at%row_ptr(i + 1) + at%row_ptr(i)
         next_free(i) = at%row_ptr(i)
      end do

      ! walking the source in row order leaves every target row ascending
      do i = 1, self%nrow
         do k = self%row_ptr(i), self%row_ptr(i + 1) - 1
            j = self%col_idx(k)
            at%col_idx(next_free(j)) = i
            at%values(next_free(j)) = self%values(k)
            next_free(j) = next_free(j) + 1
         end do
      end do
   end subroutine csr_transpose

   function csr_is_valid(self, err) result(ok)
      !! Check that the stored triple is structurally consistent.
      !!
      !! Verifies non-negative dimensions, that all three arrays are
      !! allocated, that `row_ptr` has `nrow + 1` elements, starts at 1, is
      !! monotonically non-decreasing and ends at `nnz + 1`, that `col_idx`
      !! and `values` have the same length, and that every column index lies
      !! in `1:ncol`. When `err` is present it carries the first violation
      !! found as `ERROR_VALIDATION`.
      class(csr_t), intent(in) :: self
      type(error_t), intent(out), optional :: err
      logical :: ok
         !! `.true.` when the matrix is structurally sound

      integer(default_int) :: i, n_entries

      ok = .false.

      if (self%nrow < 0 .or. self%ncol < 0) then
         call fail(err, "pic_csr: matrix has a negative dimension")
         return
      end if
      if (.not. allocated(self%row_ptr) .or. .not. allocated(self%col_idx) .or. .not. allocated(self%values)) then
         call fail(err, "pic_csr: matrix has unallocated storage arrays")
         return
      end if
      if (int(size(self%row_ptr), default_int) /= self%nrow + 1) then
         call fail(err, "pic_csr: row_ptr must have exactly n_rows + 1 elements")
         return
      end if
      n_entries = int(size(self%col_idx), default_int)
      if (int(size(self%values), default_int) /= n_entries) then
         call fail(err, "pic_csr: col_idx and values must have the same length")
         return
      end if
      if (self%row_ptr(1) /= 1) then
         call fail(err, "pic_csr: row_ptr must start at 1")
         return
      end if
      do i = 1, self%nrow
         if (self%row_ptr(i + 1) < self%row_ptr(i)) then
            call fail(err, "pic_csr: row_ptr must be monotonically non-decreasing")
            return
         end if
      end do
      if (self%row_ptr(self%nrow + 1) /= n_entries + 1) then
         call fail(err, "pic_csr: row_ptr must end at nnz + 1")
         return
      end if
      do i = 1, n_entries
         if (self%col_idx(i) < 1 .or. self%col_idx(i) > self%ncol) then
            call fail(err, "pic_csr: col_idx holds a column index outside 1:n_cols")
            return
         end if
      end do

      ok = .true.
   end function csr_is_valid

   subroutine csr_destroy(self)
      !! Release all storage and reset the matrix to an empty 0 by 0 one.
      class(csr_t), intent(inout) :: self

      if (allocated(self%row_ptr)) deallocate (self%row_ptr)
      if (allocated(self%col_idx)) deallocate (self%col_idx)
      if (allocated(self%values)) deallocate (self%values)
      self%nrow = 0
      self%ncol = 0
   end subroutine csr_destroy

   pure subroutine fail(err, message)
      !! Set `err` to `ERROR_VALIDATION` with `message`, when `err` is present.
      type(error_t), intent(out), optional :: err
      character(len=*), intent(in) :: message
         !! Human-readable description of the violation

      if (present(err)) call err%set(ERROR_VALIDATION, message)
   end subroutine fail

end module pic_csr
