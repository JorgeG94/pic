!! experimental pic string type, it is still in alpha
module pic_string_mod
   !! a simple string type with basic functionalities
   use pic_types, only: int64
   use pic_optional_value, only: pic_optional
   implicit none
   private
   public :: pic_string_type
   public :: operator(==), operator(/=)

   type :: pic_string_type
      !! pic_string_type holds a dynamic string, intends to be similar to std::string in C++
      character(len=:), allocatable :: buf
      integer(int64) :: len = 0_int64
      integer(int64) :: cap = 0_int64
   contains
      ! queries
      procedure :: size => pic_string_size
      procedure :: capacity => pic_string_capacity
      procedure :: empty => pic_string_empty
      procedure :: starts_with => pic_string_starts_with
      procedure :: ends_with => pic_string_ends_with
      ! modifiers
      procedure :: clear => pic_string_clear
      procedure :: reserve => pic_string_reserve
      procedure :: assign => pic_string_assign
      procedure :: append => pic_string_append
      procedure :: push_back => pic_string_push_back
      procedure :: to_char => pic_string_to_char
      ! in type bindings:
      procedure :: get => pic_string_get
      procedure :: set => pic_string_set

      ! trimming
      procedure :: ltrim => pic_string_ltrim
      procedure :: rtrim => pic_string_rtrim
      procedure :: trim => pic_string_trim
      ! search & slicing
      procedure :: find => pic_string_find
      procedure :: substr => pic_string_substr
      procedure :: shrink_to_fit => pic_string_shrink_to_fit
      procedure :: release => pic_string_release
      ! finalizer
      final     :: pic_string_finalize
   end type pic_string_type

   interface ensure_capacity_
      module procedure :: ensure_capacity
   end interface
   interface operator(==)
      module procedure pic_string_eq_string    ! pic_string_type == pic_string_type
      module procedure pic_string_eq_char      ! pic_string_type == character(*)
      module procedure char_eq_pic_string      ! character(*)   == pic_string_type
   end interface

   interface operator(/=)
      module procedure pic_string_ne_string
      module procedure pic_string_ne_char
      module procedure char_ne_pic_string
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

   !----------------- trimming -----------------
   subroutine pic_string_rtrim(self)
      class(pic_string_type), intent(inout) :: self
      integer(int64) :: j
      if (self%len == 0_int64) return
      j = self%len
      do
         if (j < 1_int64) exit
         if (self%buf(j:j) <= ' ') then
            j = j - 1_int64
         else
            exit
         end if
      end do
      self%len = max(0_int64, j)
   end subroutine pic_string_rtrim

   subroutine pic_string_ltrim(self)
      class(pic_string_type), intent(inout) :: self
      integer(int64) :: i, n
      if (self%len == 0_int64) return
      i = 1_int64
      do
         if (i > self%len) exit
         if (self%buf(i:i) <= ' ') then
            i = i + 1_int64
         else
            exit
         end if
      end do
      if (i > 1_int64) then
         n = self%len - (i - 1_int64)
         if (n > 0_int64) self%buf(1:n) = self%buf(i:self%len)
         self%len = max(0_int64, n)
      end if
   end subroutine pic_string_ltrim

   subroutine pic_string_trim(self)
      class(pic_string_type), intent(inout) :: self
      call self%rtrim()
      call self%ltrim()
   end subroutine pic_string_trim

   !----------------- queries -----------------
   pure logical function pic_string_starts_with(self, pat) result(ok)
      class(pic_string_type), intent(in) :: self
      character(*), intent(in) :: pat
      integer :: m
      m = len(pat)
      ok = (self%len >= m .and. (m == 0 .or. self%buf(1:m) == pat))
   end function pic_string_starts_with

   pure logical function pic_string_ends_with(self, pat) result(ok)
      class(pic_string_type), intent(in) :: self
      character(*), intent(in) :: pat
      integer :: m
      m = len(pat)
      ok = (self%len >= m .and. (m == 0 .or. self%buf(self%len - m + 1:self%len) == pat))
   end function pic_string_ends_with

   !----------------- search & slicing -----------------
   pure integer(int64) function pic_string_find(self, pat, from) result(pos)
      class(pic_string_type), intent(in) :: self
      character(*), intent(in) :: pat
      integer(int64), optional, intent(in):: from
      integer(int64) :: i0
      integer        :: k

      if (self%len == 0_int64 .or. len(pat) == 0) then
         pos = 0_int64; return
      end if

      i0 = pic_optional(from, 1_int64)   ! <- clean & safe

      if (i0 < 1_int64 .or. i0 > self%len) then
         pos = 0_int64; return
      end if

      k = index(self%buf(i0:self%len), pat)
      if (k > 0) then
         pos = int(k, int64) + i0 - 1_int64
      else
         pos = 0_int64
      end if
   end function pic_string_find

   function pic_string_substr(self, i, n) result(out)
      class(pic_string_type), intent(in) :: self
      integer(int64), intent(in) :: i      ! 1-based
      integer(int64), intent(in) :: n      ! length requested
      type(pic_string_type) :: out
      integer(int64) :: i2, n2
      if (n <= 0_int64 .or. i > self%len) return
      i2 = max(1_int64, i)
      n2 = min(self%len - i2 + 1_int64, n)
      call out%reserve(n2)
      if (n2 > 0_int64) out%buf(1:n2) = self%buf(i2:i2 + n2 - 1_int64)
      out%len = n2
   end function pic_string_substr

   !----------------- equality operators -----------------
   pure logical function pic_string_eq_string(a, b) result(ok)
      class(pic_string_type), intent(in) :: a
      class(pic_string_type), intent(in) :: b
      ok = (a%len == b%len)
      if (ok .and. a%len > 0_int64) ok = (a%buf(1:a%len) == b%buf(1:b%len))
   end function pic_string_eq_string

   pure logical function pic_string_eq_char(a, c) result(ok)
      class(pic_string_type), intent(in) :: a
      character(*), intent(in) :: c
      ok = (a%len == len(c))
      if (ok .and. a%len > 0_int64) ok = (a%buf(1:a%len) == c)
   end function pic_string_eq_char

   pure logical function char_eq_pic_string(c, a) result(ok)
      character(*), intent(in) :: c
      class(pic_string_type), intent(in) :: a
      ok = (a == c)
   end function char_eq_pic_string

   pure logical function pic_string_ne_string(a, b) result(ok)
      class(pic_string_type), intent(in) :: a
      class(pic_string_type), intent(in) :: b
      ok = .not. (a == b)
   end function pic_string_ne_string

   pure logical function pic_string_ne_char(a, c) result(ok)
      class(pic_string_type), intent(in) :: a
      character(*), intent(in) :: c
      ok = .not. (a == c)
   end function pic_string_ne_char

   pure logical function char_ne_pic_string(c, a) result(ok)
      character(*), intent(in) :: c
      class(pic_string_type), intent(in) :: a
      ok = .not. (a == c)
   end function char_ne_pic_string

   subroutine pic_string_finalize(self)
      type(pic_string_type), intent(inout) :: self
      if (allocated(self%buf)) deallocate (self%buf)
      self%len = 0_int64; self%cap = 0_int64
   end subroutine pic_string_finalize

   subroutine pic_string_shrink_to_fit(self)
      class(pic_string_type), intent(inout) :: self
      character(len=:), allocatable :: tmp
      if (.not. allocated(self%buf)) return
      if (self%len == 0_int64) then
         deallocate (self%buf); self%cap = 0_int64
      else
         allocate (character(len=self%len) :: tmp)
         tmp(1:self%len) = self%buf(1:self%len)
         call move_alloc(tmp, self%buf)
         self%cap = self%len
      end if
   end subroutine pic_string_shrink_to_fit

   subroutine pic_string_release(self)
      class(pic_string_type), intent(inout) :: self
      if (allocated(self%buf)) deallocate (self%buf)
      self%len = 0_int64; self%cap = 0_int64
   end subroutine pic_string_release

   pure character(1) function pic_string_get(self, i) result(ch)
      class(pic_string_type), intent(in) :: self
      integer(int64), intent(in) :: i
      if (i < 1_int64 .or. i > self%len) then
         ch = achar(0)      ! or stop/error; choose your policy
         ! erorr stop "Index out of bounds in pic_string_get"
      else
         ch = self%buf(i:i)
      end if
   end function pic_string_get

   subroutine pic_string_set(self, i, ch)
      class(pic_string_type), intent(inout) :: self
      integer(int64), intent(in)    :: i
      character(1), intent(in)    :: ch
      if (i < 1_int64 .or. i > self%len) then
         !error stop "Index out of bounds in pic_string_set"
         return
      end if
      self%buf(i:i) = ch
   end subroutine pic_string_set

end module pic_string_mod
