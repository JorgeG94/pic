! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Binary heap / priority queue with stable (FIFO) tie-breaking.
module pic_heap
   !! A concrete binary heap keyed by `integer(int64)` and carrying an
   !! `integer(int32)` payload. This is the data structure behind Dijkstra,
   !! A*, k-nearest searches and discrete-event simulation loops.
   !!
   !! ### Key and payload kinds
   !!
   !! The key is a **fixed-width** `integer(int64)` and the payload a
   !! **fixed-width** `integer(int32)`. These are deliberate, algorithmic kind
   !! choices and must not become `default_int`: the key must hold accumulated
   !! distances or costs without overflowing on a 32-bit default integer build,
   !! and the payload is a node index in a CSR-style graph, which is 32-bit by
   !! convention. Sizes, capacities and internal indices do use
   !! `integer(default_int)`.
   !!
   !! ### Ordering
   !!
   !! Min-heap by default: `pop` returns the smallest key. Pass
   !! `max_heap = .true.` to `init` to get a max-heap, where `pop` returns the
   !! largest key. Everything else behaves identically.
   !!
   !! ### Tie-breaking (guaranteed, not incidental)
   !!
   !! Entries with **equal keys pop in FIFO order**: the entry pushed first
   !! pops first. This holds for a max-heap too, and it holds across the
   !! rebalancing that `push` and `pop` perform. It is implemented by carrying
   !! a monotonically increasing insertion counter alongside every entry and
   !! comparing `(key, insertion index)` lexicographically. Since insertion
   !! indices are unique, the heap order is a *total* order and the pop
   !! sequence is fully determined by the sequence of pushes. Downstream
   !! algorithms are therefore reproducible across runs and compilers.
   !!
   !! Entries loaded by `build_from` are treated as if they had been pushed in
   !! array order, so equal keys pop in increasing array-index order.
   !!
   !! ### Growth
   !!
   !! Storage grows **geometrically by a factor of 2** (`GROWTH_FACTOR`),
   !! starting at `DEFAULT_CAPACITY = 16` slots, so `push` is amortized O(1).
   !! Use `reserve` to pre-size the heap and avoid reallocation in hot loops.
   !! Capacity never shrinks; `clear` keeps the storage for reuse and
   !! `destroy` releases it.
   !!
   !! ### Usage
   !!
   !!```fortran
   !! type(heap_t) :: heap
   !! type(error_t) :: err
   !! integer(int64) :: key
   !! integer(int32) :: payload
   !!
   !! call heap%init(1024_default_int)
   !! call heap%push(7_int64, 3_int32)
   !! call heap%push(2_int64, 9_int32)
   !! do while (.not. heap%is_empty())
   !!    call heap%pop(key, payload, err)
   !!    if (err%has_error()) exit
   !! end do
   !!```
   use pic_types, only: default_int, int32, int64
   use pic_error, only: error_t, ERROR_VALIDATION
   implicit none

   private

   public :: heap_t

   integer(default_int), parameter :: DEFAULT_CAPACITY = 16
      !! Number of slots allocated by the first growth of an empty heap
   integer(default_int), parameter :: GROWTH_FACTOR = 2
      !! Capacity multiplier used when the heap runs out of slots

   type :: heap_t
      !! Binary heap of (`integer(int64)` key, `integer(int32)` payload) pairs
      !! with stable FIFO tie-breaking on equal keys.
      integer(int64), allocatable, private :: keys(:)
         !! Heap-ordered keys, valid in `1:n_entries`
      integer(int32), allocatable, private :: payloads(:)
         !! Payload travelling with `keys`, same index
      integer(int64), allocatable, private :: insertion(:)
         !! Insertion index of each entry, used to break key ties FIFO
      integer(default_int), private :: n_entries = 0
         !! Number of entries currently stored
      integer(int64), private :: n_pushed = 0
         !! Monotonic counter handing out insertion indices
      logical, private :: max_heap = .false.
         !! `.true.` selects max-heap ordering, `.false.` min-heap
   contains
      procedure :: init => heap_init
      procedure :: clear => heap_clear
      procedure :: destroy => heap_destroy
      procedure :: size => heap_size
      procedure :: is_empty => heap_is_empty
      procedure :: capacity => heap_capacity
      procedure :: reserve => heap_reserve
      procedure :: push => heap_push
      procedure :: pop => heap_pop
      procedure :: peek => heap_peek
      procedure :: build_from => heap_build_from
   end type heap_t

contains

   subroutine heap_init(self, cap, max_heap)
      !! Initialise the heap, discarding any previous contents.
      !!
      !! `cap` pre-allocates storage for at least that many entries (default
      !! `DEFAULT_CAPACITY`); a non-positive `cap` allocates nothing yet.
      !! `max_heap = .true.` selects max-heap ordering, the default is a
      !! min-heap. Calling `init` on a heap that already holds entries resets
      !! it to empty but keeps any storage it had already acquired.
      class(heap_t), intent(inout) :: self
      integer(default_int), intent(in), optional :: cap
      logical, intent(in), optional :: max_heap

      integer(default_int) :: wanted

      self%n_entries = 0
      self%n_pushed = 0_int64
      self%max_heap = .false.
      if (present(max_heap)) self%max_heap = max_heap

      wanted = DEFAULT_CAPACITY
      if (present(cap)) wanted = cap

      call self%reserve(wanted)
   end subroutine heap_init

   subroutine heap_clear(self)
      !! Remove every entry but keep the allocated storage for reuse.
      !!
      !! The insertion counter is reset as well, so a cleared heap behaves
      !! exactly like a freshly initialised one with the same capacity and the
      !! same min/max ordering.
      class(heap_t), intent(inout) :: self

      self%n_entries = 0
      self%n_pushed = 0_int64
   end subroutine heap_clear

   subroutine heap_destroy(self)
      !! Release the heap storage and reset it to empty.
      class(heap_t), intent(inout) :: self

      if (allocated(self%keys)) deallocate (self%keys)
      if (allocated(self%payloads)) deallocate (self%payloads)
      if (allocated(self%insertion)) deallocate (self%insertion)
      self%n_entries = 0
      self%n_pushed = 0_int64
   end subroutine heap_destroy

   pure function heap_size(self) result(n)
      !! Number of entries currently in the heap.
      class(heap_t), intent(in) :: self
      integer(default_int) :: n

      n = self%n_entries
   end function heap_size

   pure function heap_is_empty(self) result(empty)
      !! `.true.` when the heap holds no entries.
      class(heap_t), intent(in) :: self
      logical :: empty

      empty = (self%n_entries == 0)
   end function heap_is_empty

   pure function heap_capacity(self) result(cap)
      !! Number of entries the heap can hold before it has to grow.
      class(heap_t), intent(in) :: self
      integer(default_int) :: cap

      cap = 0
      if (allocated(self%keys)) cap = int(size(self%keys), default_int)
   end function heap_capacity

   subroutine heap_reserve(self, n)
      !! Ensure room for at least `n` entries without further reallocation.
      !!
      !! Existing entries are preserved. Capacity never shrinks, so a request
      !! smaller than or equal to the current capacity is a no-op.
      class(heap_t), intent(inout) :: self
      integer(default_int), intent(in) :: n

      if (n <= self%capacity()) return

      call grow_int64(self%keys, self%n_entries, n)
      call grow_int32(self%payloads, self%n_entries, n)
      call grow_int64(self%insertion, self%n_entries, n)
   end subroutine heap_reserve

   subroutine heap_push(self, key, payload)
      !! Insert one (key, payload) entry in O(log n).
      !!
      !! Entries pushed earlier pop before equal-key entries pushed later.
      class(heap_t), intent(inout) :: self
      integer(int64), intent(in) :: key
      integer(int32), intent(in) :: payload

      if (self%n_entries >= self%capacity()) then
         call self%reserve(max(DEFAULT_CAPACITY, GROWTH_FACTOR*self%capacity()))
      end if

      self%n_entries = self%n_entries + 1
      self%n_pushed = self%n_pushed + 1_int64
      self%keys(self%n_entries) = key
      self%payloads(self%n_entries) = payload
      self%insertion(self%n_entries) = self%n_pushed

      call sift_up(self, self%n_entries)
   end subroutine heap_push

   subroutine heap_pop(self, key, payload, err)
      !! Remove and return the root entry (minimum, or maximum for a max-heap).
      !!
      !! Popping an empty heap is an error, not a crash: `key` and `payload`
      !! are set to zero and `err` reports `ERROR_VALIDATION`. `err` is
      !! optional; when it is absent an empty pop simply yields zeros.
      class(heap_t), intent(inout) :: self
      integer(int64), intent(out) :: key
      integer(int32), intent(out) :: payload
      type(error_t), intent(out), optional :: err

      if (.not. root_entry(self, key, payload, err)) return

      self%keys(1) = self%keys(self%n_entries)
      self%payloads(1) = self%payloads(self%n_entries)
      self%insertion(1) = self%insertion(self%n_entries)
      self%n_entries = self%n_entries - 1

      if (self%n_entries > 1) call sift_down(self, 1_default_int)
   end subroutine heap_pop

   subroutine heap_peek(self, key, payload, err)
      !! Return the root entry without removing it.
      !!
      !! Behaves exactly like `pop` on an empty heap: `key` and `payload` are
      !! zeroed and `err` reports `ERROR_VALIDATION`.
      class(heap_t), intent(in) :: self
      integer(int64), intent(out) :: key
      integer(int32), intent(out) :: payload
      type(error_t), intent(out), optional :: err

      logical :: ok

      ok = root_entry(self, key, payload, err)
   end subroutine heap_peek

   subroutine heap_build_from(self, keys, payloads, err)
      !! Replace the heap contents with `keys`/`payloads`, heapified in O(n).
      !!
      !! This is Floyd's bottom-up heapify, not `size(keys)` repeated pushes.
      !! The two arrays must have the same length, otherwise `err` reports
      !! `ERROR_VALIDATION` and the heap is left untouched. Equal keys pop in
      !! increasing array-index order, matching what pushing the arrays in
      !! order would have produced.
      class(heap_t), intent(inout) :: self
      integer(int64), intent(in) :: keys(:)
      integer(int32), intent(in) :: payloads(:)
      type(error_t), intent(out), optional :: err

      integer(default_int) :: n, i

      n = int(size(keys), default_int)
      if (int(size(payloads), default_int) /= n) then
         if (present(err)) then
            call err%set(ERROR_VALIDATION, "pic_heap: build_from requires keys and payloads of equal length")
         end if
         return
      end if

      call self%reserve(n)

      do i = 1, n
         self%keys(i) = keys(i)
         self%payloads(i) = payloads(i)
         self%insertion(i) = int(i, int64)
      end do

      self%n_entries = n
      self%n_pushed = int(n, int64)

      do i = n/2, 1, -1
         call sift_down(self, i)
      end do
   end subroutine heap_build_from

   function root_entry(self, key, payload, err) result(ok)
      !! Fetch the root entry, reporting an error when the heap is empty.
      class(heap_t), intent(in) :: self
      integer(int64), intent(out) :: key
      integer(int32), intent(out) :: payload
      type(error_t), intent(out), optional :: err
      logical :: ok

      if (self%n_entries == 0) then
         key = 0_int64
         payload = 0_int32
         ok = .false.
         if (present(err)) then
            call err%set(ERROR_VALIDATION, "pic_heap: cannot read the root of an empty heap")
         end if
         return
      end if

      key = self%keys(1)
      payload = self%payloads(1)
      ok = .true.
   end function root_entry

   pure function precedes(self, left, right) result(first)
      !! `.true.` when entry `left` must pop before entry `right`.
      !!
      !! Compares `(key, insertion index)` lexicographically, which makes the
      !! ordering total and the tie behaviour FIFO.
      class(heap_t), intent(in) :: self
      integer(default_int), intent(in) :: left
      integer(default_int), intent(in) :: right
      logical :: first

      if (self%keys(left) == self%keys(right)) then
         first = (self%insertion(left) < self%insertion(right))
      else if (self%max_heap) then
         first = (self%keys(left) > self%keys(right))
      else
         first = (self%keys(left) < self%keys(right))
      end if
   end function precedes

   pure subroutine swap_entries(self, left, right)
      !! Exchange two heap slots, key, payload and insertion index together.
      class(heap_t), intent(inout) :: self
      integer(default_int), intent(in) :: left
      integer(default_int), intent(in) :: right

      integer(int64) :: tmp_key, tmp_insertion
      integer(int32) :: tmp_payload

      tmp_key = self%keys(left)
      tmp_payload = self%payloads(left)
      tmp_insertion = self%insertion(left)

      self%keys(left) = self%keys(right)
      self%payloads(left) = self%payloads(right)
      self%insertion(left) = self%insertion(right)

      self%keys(right) = tmp_key
      self%payloads(right) = tmp_payload
      self%insertion(right) = tmp_insertion
   end subroutine swap_entries

   pure subroutine sift_up(self, start)
      !! Move entry `start` towards the root until the heap property holds.
      class(heap_t), intent(inout) :: self
      integer(default_int), intent(in) :: start

      integer(default_int) :: child, parent

      child = start
      do while (child > 1)
         parent = child/2
         if (.not. precedes(self, child, parent)) exit
         call swap_entries(self, child, parent)
         child = parent
      end do
   end subroutine sift_up

   pure subroutine sift_down(self, start)
      !! Move entry `start` towards the leaves until the heap property holds.
      class(heap_t), intent(inout) :: self
      integer(default_int), intent(in) :: start

      integer(default_int) :: parent, child, best

      parent = start
      do
         child = 2*parent
         if (child > self%n_entries) exit

         best = child
         if (child < self%n_entries) then
            if (precedes(self, child + 1, best)) best = child + 1
         end if

         if (.not. precedes(self, best, parent)) exit
         call swap_entries(self, parent, best)
         parent = best
      end do
   end subroutine sift_down

   subroutine grow_int64(arr, n_keep, new_cap)
      !! Reallocate `arr` to `new_cap` slots, keeping its first `n_keep` values.
      !!
      !! Deliberately written as explicit allocate/copy/deallocate rather than
      !! `move_alloc` or reallocation-on-assignment: both have historically
      !! been unreliable on nvfortran and LFortran, and this path runs only
      !! O(log n) times over the life of a heap.
      integer(int64), allocatable, intent(inout) :: arr(:)
      integer(default_int), intent(in) :: n_keep
      integer(default_int), intent(in) :: new_cap

      integer(int64), allocatable :: tmp(:)
      integer(default_int) :: i

      if (.not. allocated(arr)) then
         allocate (arr(new_cap))
         return
      end if

      allocate (tmp(n_keep))
      do i = 1, n_keep
         tmp(i) = arr(i)
      end do

      deallocate (arr)
      allocate (arr(new_cap))
      do i = 1, n_keep
         arr(i) = tmp(i)
      end do

      deallocate (tmp)
   end subroutine grow_int64

   subroutine grow_int32(arr, n_keep, new_cap)
      !! `integer(int32)` counterpart of `grow_int64`.
      integer(int32), allocatable, intent(inout) :: arr(:)
      integer(default_int), intent(in) :: n_keep
      integer(default_int), intent(in) :: new_cap

      integer(int32), allocatable :: tmp(:)
      integer(default_int) :: i

      if (.not. allocated(arr)) then
         allocate (arr(new_cap))
         return
      end if

      allocate (tmp(n_keep))
      do i = 1, n_keep
         tmp(i) = arr(i)
      end do

      deallocate (arr)
      allocate (arr(new_cap))
      do i = 1, n_keep
         arr(i) = tmp(i)
      end do

      deallocate (tmp)
   end subroutine grow_int32

end module pic_heap
