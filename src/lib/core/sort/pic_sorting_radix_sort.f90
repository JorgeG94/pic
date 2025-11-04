submodule(pic_sorting) pic_sorting_radix_sort

   implicit none
!! The generic subroutine implementing the LSD radix sort algorithm to return
!! an input array with its elements sorted in order of (non-)decreasing
!! value. Its use has the syntax:
!!
!!     call radix_sort( array[, work, reverse] )
!!
!! with the arguments:
!!
!! * array: the rank 1 array to be sorted. It is an `intent(inout)`
!!   argument of any of the types `integer(int8)`, `integer(int16)`,
!!   `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`.
!!   If both the type of `array` is real and at least one of the
!!   elements is a `NaN`, then the ordering of the result is undefined.
!!   Otherwise it is defined to be the original elements in
!!   non-decreasing order. Especially, -0.0 is lesser than 0.0.
!!
!! * work (optional): shall be a rank 1 array of the same type as
!!   `array`, and shall have at least `size(array)` elements. It is an
!!   `intent(inout)` argument to be used as buffer. Its value on return is
!!   undefined. If it is not present, `radix_sort` will allocate a
!!   buffer for use, and deallocate it before return. If you do several
!!   similar `radix_sort`s, reusing the `work` array is a good parctice.
!!   This argument is not present for `int8_radix_sort` because it use
!!   counting sort, so no buffer is needed.
!!
!! * `reverse` (optional): shall be a scalar of type default logical. It
!!   is an `intent(in)` argument. If present with a value of `.true.` then
!!   `array` will be sorted in order of non-increasing values in stable
!!   order. Otherwise index will sort `array` in order of non-decreasing
!!   values in stable order.
!!
!!#### Example
!!
!!```fortran
!!    ...
!!    ! Read random data from a file
!!    call read_file( 'dummy_file', array )
!!    ! Sort the random data
!!    call radix_sort( array )
!!    ...
!!```

   integer, parameter :: radix_bits = 8
   integer, parameter :: radix_mask = 255
   integer(kind=int32), parameter :: radix_bits_i32 = 8_int32
   integer(kind=int32), parameter :: radix_mask_i32 = 255_int32
   integer(kind=int64), parameter :: radix_bits_i64 = 8_int64
   integer(kind=int64), parameter :: radix_mask_i64 = 255_int64

contains

   pure subroutine radix_sort_u32_helper(N, arr, buf)
      integer(kind=int_index), intent(in) :: N
      integer(kind=int32), dimension(N), intent(inout) :: arr
      integer(kind=int32), dimension(N), intent(inout) :: buf
      integer(kind=int_index) :: i
      integer :: b, b0, b1, b2, b3
      integer(kind=int_index), dimension(0:radix_mask) :: c0, c1, c2, c3
      c0(:) = 0
      c1(:) = 0
      c2(:) = 0
      c3(:) = 0
      do i = 1, N
         b0 = iand(arr(i), radix_mask_i32)
         b1 = iand(ishft(arr(i), -radix_bits_i32), radix_mask_i32)
         b2 = iand(ishft(arr(i), -2*radix_bits_i32), radix_mask_i32)
         b3 = ishft(arr(i), -3*radix_bits_i32)
         c0(b0) = c0(b0) + 1
         c1(b1) = c1(b1) + 1
         c2(b2) = c2(b2) + 1
         c3(b3) = c3(b3) + 1
      end do
      do b = 1, radix_mask
         c0(b) = c0(b) + c0(b - 1)
         c1(b) = c1(b) + c1(b - 1)
         c2(b) = c2(b) + c2(b - 1)
         c3(b) = c3(b) + c3(b - 1)
      end do
      do i = N, 1, -1
         b0 = iand(arr(i), radix_mask_i32)
         buf(c0(b0)) = arr(i)
         c0(b0) = c0(b0) - 1
      end do
      do i = N, 1, -1
         b1 = iand(ishft(buf(i), -radix_bits_i32), radix_mask_i32)
         arr(c1(b1)) = buf(i)
         c1(b1) = c1(b1) - 1
      end do
      do i = N, 1, -1
         b2 = iand(ishft(arr(i), -2*radix_bits_i32), radix_mask_i32)
         buf(c2(b2)) = arr(i)
         c2(b2) = c2(b2) - 1
      end do
      do i = N, 1, -1
         b3 = ishft(buf(i), -3*radix_bits_i32)
         arr(c3(b3)) = buf(i)
         c3(b3) = c3(b3) - 1
      end do
   end subroutine radix_sort_u32_helper

   pure subroutine radix_sort_u64_helper(N, arr, buffer)
      integer(kind=int_index), intent(in) :: N
      integer(kind=int64), dimension(N), intent(inout) :: arr
      integer(kind=int64), dimension(N), intent(inout) :: buffer
      integer(kind=int_index) :: i
      integer(kind=int64) :: b, b0, b1, b2, b3, b4, b5, b6, b7
      integer(kind=int_index), dimension(0:radix_mask) :: c0, c1, c2, c3, c4, c5, c6, c7
      c0(:) = 0
      c1(:) = 0
      c2(:) = 0
      c3(:) = 0
      c4(:) = 0
      c5(:) = 0
      c6(:) = 0
      c7(:) = 0
      do i = 1, N
         b0 = iand(arr(i), radix_mask_i64)
         b1 = iand(ishft(arr(i), -radix_bits_i64), radix_mask_i64)
         b2 = iand(ishft(arr(i), -2*radix_bits_i64), radix_mask_i64)
         b3 = iand(ishft(arr(i), -3*radix_bits_i64), radix_mask_i64)
         b4 = iand(ishft(arr(i), -4*radix_bits_i64), radix_mask_i64)
         b5 = iand(ishft(arr(i), -5*radix_bits_i64), radix_mask_i64)
         b6 = iand(ishft(arr(i), -6*radix_bits_i64), radix_mask_i64)
         b7 = ishft(arr(i), -7*radix_bits_i64)
         c0(b0) = c0(b0) + 1
         c1(b1) = c1(b1) + 1
         c2(b2) = c2(b2) + 1
         c3(b3) = c3(b3) + 1
         c4(b4) = c4(b4) + 1
         c5(b5) = c5(b5) + 1
         c6(b6) = c6(b6) + 1
         c7(b7) = c7(b7) + 1
      end do
      do b = 1, radix_mask
         c0(b) = c0(b) + c0(b - 1)
         c1(b) = c1(b) + c1(b - 1)
         c2(b) = c2(b) + c2(b - 1)
         c3(b) = c3(b) + c3(b - 1)
         c4(b) = c4(b) + c4(b - 1)
         c5(b) = c5(b) + c5(b - 1)
         c6(b) = c6(b) + c6(b - 1)
         c7(b) = c7(b) + c7(b - 1)
      end do
      do i = N, 1, -1
         b0 = iand(arr(i), radix_mask_i64)
         buffer(c0(b0)) = arr(i)
         c0(b0) = c0(b0) - 1
      end do
      do i = N, 1, -1
         b1 = iand(ishft(buffer(i), -radix_bits_i64), radix_mask_i64)
         arr(c1(b1)) = buffer(i)
         c1(b1) = c1(b1) - 1
      end do
      do i = N, 1, -1
         b2 = iand(ishft(arr(i), -2*radix_bits_i64), radix_mask_i64)
         buffer(c2(b2)) = arr(i)
         c2(b2) = c2(b2) - 1
      end do
      do i = N, 1, -1
         b3 = iand(ishft(buffer(i), -3*radix_bits_i64), radix_mask_i64)
         arr(c3(b3)) = buffer(i)
         c3(b3) = c3(b3) - 1
      end do
      do i = N, 1, -1
         b4 = iand(ishft(arr(i), -4*radix_bits_i64), radix_mask_i64)
         buffer(c4(b4)) = arr(i)
         c4(b4) = c4(b4) - 1
      end do
      do i = N, 1, -1
         b5 = iand(ishft(buffer(i), -5*radix_bits_i64), radix_mask_i64)
         arr(c5(b5)) = buffer(i)
         c5(b5) = c5(b5) - 1
      end do
      do i = N, 1, -1
         b6 = iand(ishft(arr(i), -6*radix_bits_i64), radix_mask_i64)
         buffer(c6(b6)) = arr(i)
         c6(b6) = c6(b6) - 1
      end do
      do i = N, 1, -1
         b7 = ishft(buffer(i), -7*radix_bits_i64)
         arr(c7(b7)) = buffer(i)
         c7(b7) = c7(b7) - 1
      end do
   end subroutine radix_sort_u64_helper

   pure module subroutine int32_radix_sort(array, work, reverse)
      integer(kind=int32), dimension(:), intent(inout) :: array
      integer(kind=int32), dimension(:), intent(inout), target, optional :: work
      logical, intent(in), optional :: reverse
      integer(kind=int_index) :: i, N, start, middle, end
      integer(kind=int32), dimension(:), pointer :: buffer
      integer(kind=int32) :: item
      logical :: use_internal_buffer
      N = size(array, kind=int_index)
      if (present(work)) then
         if (size(work, kind=int_index) < N) then
            error stop "int32_radix_sort: work array is too small."
         end if
         use_internal_buffer = .false.
         buffer => work
      else
         use_internal_buffer = .true.
         allocate (buffer(N))
      end if
      call radix_sort_u32_helper(N, array, buffer)
      if (array(1) >= 0 .and. array(N) < 0) then
         start = 1
         end = N
         middle = (1 + N)/2
         do while (.true.)
            if (array(middle) >= 0) then
               start = middle + 1
            else
               end = middle
            end if
            middle = (start + end)/2
            if (start == end) exit
         end do
         buffer(1:(N - middle + 1)) = array(middle:N)
         buffer(N - middle + 2:N) = array(1:middle - 1)
         array(:) = buffer(:)
      end if
      if (pic_optional(reverse, .false.)) then
         do i = 1, N/2
            item = array(i)
            array(i) = array(N - i + 1)
            array(N - i + 1) = item
         end do
      end if
      if (use_internal_buffer) then
         deallocate (buffer)
      end if
   end subroutine int32_radix_sort

   module subroutine sp_radix_sort(array, work, reverse)
      use iso_c_binding, only: c_loc, c_f_pointer
      real(kind=sp), dimension(:), intent(inout), target :: array
      real(kind=sp), dimension(:), intent(inout), target, optional :: work
      logical, intent(in), optional :: reverse
      integer(kind=int_index) :: i, N, pos, rev_pos
      integer(kind=int32), dimension(:), pointer :: arri32
      integer(kind=int32), dimension(:), pointer :: buffer
      real(kind=sp) :: item
      logical :: use_internal_buffer
      N = size(array, kind=int_index)
      if (present(work)) then
         if (size(work, kind=int_index) < N) then
            error stop "sp_radix_sort: work array is too small."
         end if
         use_internal_buffer = .false.
         call c_f_pointer(c_loc(work), buffer, [N])
      else
         use_internal_buffer = .true.
         allocate (buffer(N))
      end if
      call c_f_pointer(c_loc(array), arri32, [N])
      call radix_sort_u32_helper(N, arri32, buffer)
! After calling `radix_sort_u<width>_helper. The array is sorted as unsigned integers.
! The positive real number is sorted, guaranteed by IEEE-754 standard.
! But the negative real number is sorted in a reversed order, and also in the tail of array.
! Remark that -0.0 is the minimum nagative integer, so using radix sort, -0.0 is naturally lesser than 0.0.
! In IEEE-754 standard, the bit representation of `Inf` is greater than all positive real numbers,
! and the `-Inf` is lesser than all negative real numbers. So the order of `Inf, -Inf` is ok.
! The bit representation of `NaN` may be positive or negative integers in different machine,
! thus if the array contains `NaN`, the result is undefined.
      if (arri32(1) >= 0 .and. arri32(N) < 0) then
         pos = 1
         rev_pos = N
         do while (arri32(rev_pos) < 0)
            buffer(pos) = arri32(rev_pos)
            pos = pos + 1
            rev_pos = rev_pos - 1
         end do
         buffer(pos:N) = arri32(1:rev_pos)
         arri32(:) = buffer(:)
      end if
      if (pic_optional(reverse, .false.)) then
         do i = 1, N/2
            item = array(i)
            array(i) = array(N - i + 1)
            array(N - i + 1) = item
         end do
      end if
      if (use_internal_buffer) then
         deallocate (buffer)
      end if
   end subroutine sp_radix_sort

   pure module subroutine int64_radix_sort(array, work, reverse)
      integer(kind=int64), dimension(:), intent(inout) :: array
      integer(kind=int64), dimension(:), intent(inout), target, optional :: work
      logical, intent(in), optional :: reverse
      integer(kind=int_index) :: i, N, start, middle, end
      integer(kind=int64), dimension(:), pointer :: buffer
      integer(kind=int64) :: item
      logical :: use_internal_buffer
      N = size(array, kind=int_index)
      if (present(work)) then
         if (size(work, kind=int_index) < N) then
            error stop "int64_radix_sort: work array is too small."
         end if
         use_internal_buffer = .false.
         buffer => work
      else
         use_internal_buffer = .true.
         allocate (buffer(N))
      end if
      call radix_sort_u64_helper(N, array, buffer)
      if (array(1) >= 0 .and. array(N) < 0) then
         start = 1
         end = N
         middle = (1 + N)/2
         do while (.true.)
            if (array(middle) >= 0) then
               start = middle + 1
            else
               end = middle
            end if
            middle = (start + end)/2
            if (start == end) exit
         end do
         buffer(1:(N - middle + 1)) = array(middle:N)
         buffer(N - middle + 2:N) = array(1:middle - 1)
         array(:) = buffer(:)
      end if
      if (pic_optional(reverse, .false.)) then
         do i = 1, N/2
            item = array(i)
            array(i) = array(N - i + 1)
            array(N - i + 1) = item
         end do
      end if
      if (use_internal_buffer) then
         deallocate (buffer)
      end if
   end subroutine int64_radix_sort

   module subroutine dp_radix_sort(array, work, reverse)
      use iso_c_binding, only: c_loc, c_f_pointer
      real(kind=dp), dimension(:), intent(inout), target :: array
      real(kind=dp), dimension(:), intent(inout), target, optional :: work
      logical, intent(in), optional :: reverse
      integer(kind=int_index) :: i, N, pos, rev_pos
      integer(kind=int64), dimension(:), pointer :: arri64
      integer(kind=int64), dimension(:), pointer :: buffer
      real(kind=dp) :: item
      logical :: use_internal_buffer
      N = size(array, kind=int_index)
      if (present(work)) then
         if (size(work, kind=int_index) < N) then
            error stop "sp_radix_sort: work array is too small."
         end if
         use_internal_buffer = .false.
         call c_f_pointer(c_loc(work), buffer, [N])
      else
         use_internal_buffer = .true.
         allocate (buffer(N))
      end if
      call c_f_pointer(c_loc(array), arri64, [N])
      call radix_sort_u64_helper(N, arri64, buffer)
      if (arri64(1) >= 0 .and. arri64(N) < 0) then
         pos = 1
         rev_pos = N
         do while (arri64(rev_pos) < 0)
            buffer(pos) = arri64(rev_pos)
            pos = pos + 1
            rev_pos = rev_pos - 1
         end do
         buffer(pos:N) = arri64(1:rev_pos)
         arri64(:) = buffer(:)
      end if
      if (pic_optional(reverse, .false.)) then
         do i = 1, N/2
            item = array(i)
            array(i) = array(N - i + 1)
            array(N - i + 1) = item
         end do
      end if
      if (use_internal_buffer) then
         deallocate (buffer)
      end if
   end subroutine dp_radix_sort
end submodule pic_sorting_radix_sort
