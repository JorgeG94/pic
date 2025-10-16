module pic_string_mod
   use pic_types, only: int64
   implicit none
   private
   public :: pic_string_type

   type :: pic_string_type
      character(len=:), allocatable :: buf
      integer(int64) :: len = 0_int64
      integer(int64) :: cap = 0_int64
   contains
      procedure :: size => pic_string_size
      procedure :: capacity => pic_string_capacity
      procedure :: empty => pic_string_empty
      procedure :: clear => pic_string_clear
      procedure :: reserve => pic_string_reserve
      procedure :: assign => pic_string_assign
      procedure :: append => pic_string_append
      procedure :: push_back => pic_string_push_back
      procedure :: to_char => pic_string_to_char
   end type pic_string_type

   interface ensure_capacity_
      module procedure :: ensure_capacity
   end interface

contains

   subroutine ensure_capacity(self, need)
      class(pic_string_type), intent(inout) :: self
      integer(int64), intent(in)    :: need

      integer(int64) :: target
      character(len=:), allocatable :: tmp

      if (need <= self%cap) return
      if (self%cap <= 0_int64) then
         target = max(need, 64_int64)
      else
         target = self%cap
         do while (target < need)
            target = 2_int64*target
         end do
      end if

      allocate (character(len=target) :: tmp)
      if (self%len > 0 .and. allocated(self%buf)) tmp(1:self%len) = self%buf(1:self%len)
      call move_alloc(tmp, self%buf)
      self%cap = target
   end subroutine ensure_capacity

   pure function pic_string_size(self) result(res)
      class(pic_string_type), intent(in) :: self
      integer(int64) :: res
      res = self%len
   end function pic_string_size

   pure function pic_string_capacity(self) result(res)
      class(pic_string_type), intent(in) :: self
      integer(int64) :: res
      res = self%cap
   end function pic_string_capacity

   pure function pic_string_empty(self) result(res)
      class(pic_string_type), intent(in) :: self
      logical :: res
      res = (self%len == 0_int64)
   end function pic_string_empty

   subroutine pic_string_clear(self)
      class(pic_string_type), intent(inout) :: self
      self%len = 0_int64
   end subroutine pic_string_clear

   subroutine pic_string_reserve(self, n)
      class(pic_string_type), intent(inout) :: self
      integer(int64), intent(in)    :: n
      call ensure_capacity_(self, n)
   end subroutine pic_string_reserve

   subroutine pic_string_assign(self, s)
      class(pic_string_type), intent(inout) :: self
      character(*), intent(in)    :: s
      integer(int64) :: n
      n = int(len(s), int64)
      call ensure_capacity_(self, n)
      if (n > 0) self%buf(1:n) = s
      self%len = n
   end subroutine pic_string_assign

   subroutine pic_string_append(self, s)
      class(pic_string_type), intent(inout) :: self
      character(*), intent(in)    :: s
      integer(int64) :: n, i0, i1
      n = int(len(s), int64)
      i0 = self%len + 1_int64
      i1 = self%len + n
      call ensure_capacity_(self, i1)
      if (n > 0) self%buf(i0:i1) = s
      self%len = i1
   end subroutine pic_string_append

   subroutine pic_string_push_back(self, ch)
      class(pic_string_type), intent(inout) :: self
      character(1), intent(in)    :: ch
      call ensure_capacity_(self, self%len + 1_int64)
      self%len = self%len + 1_int64
      self%buf(self%len:self%len) = ch
   end subroutine pic_string_push_back

   function pic_string_to_char(self) result(out)
      class(pic_string_type), intent(in) :: self
      character(len=self%len) :: out
      if (self%len > 0) out = self%buf(1:self%len)
   end function pic_string_to_char

end module pic_string_mod
