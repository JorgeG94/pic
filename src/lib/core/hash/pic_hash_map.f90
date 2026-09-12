! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Insertion-ordered hash map with character keys and 64-bit integer values.
module pic_hash_map
   !! A hash map from `character(len=*)` keys to `integer(int64)` values whose
   !! iteration order is a documented, reproducible guarantee rather than an
   !! implementation accident.
   !!
   !! ## The iteration-order guarantee
   !!
   !! **Iteration is always in insertion order.** `keys()`, `values()` and
   !! `at()` visit the entries in the exact order in which their keys were
   !! first inserted. That order does **not** depend on the bucket count, on
   !! how many times the table has grown and rehashed, on the hash function,
   !! or on which other keys were inserted or removed. Two runs performing
   !! the same sequence of `insert`/`remove` calls observe the same iteration
   !! order, on every supported compiler.
   !!
   !! The rules that make that guarantee precise:
   !!
   !! * `insert` on a key that is **not** present appends the entry at the end
   !!   of the iteration order.
   !! * `insert` on a key that **is** present overwrites the value and leaves
   !!   the key exactly where it was in the iteration order. It is an update,
   !!   not a re-insertion.
   !! * `remove` deletes the entry from the iteration order. Every remaining
   !!   entry keeps its relative position: entries before the removed one are
   !!   untouched, entries after it shift down by one index.
   !! * `remove` followed by `insert` of the same key is a **fresh insertion**:
   !!   the key reappears at the *end* of the iteration order, not at its old
   !!   position.
   !! * Growing the table rehashes the buckets but never reorders entries.
   !!
   !! ## Key identity
   !!
   !! Keys are compared by **exact length and exact characters**, not with the
   !! Fortran `==` operator. Fortran blank-pads the shorter operand when
   !! comparing `character` values, so `"a" == "a "` is `.true.` in the
   !! language; in this map `"a"` and `"a "` are **different keys**. Trailing
   !! blanks are significant and are stored verbatim.
   !!
   !! Beware that a `character(len=8)` variable holding `"a"` is the eight
   !! character key `"a       "`, because that is what the actual argument
   !! contains. Pass `trim(name)` if trailing blanks are not meant to count.
   !!
   !! The zero-length key `""` is a perfectly ordinary key.
   !!
   !! ## Capacity, load factor and growth
   !!
   !! The bucket table is a power of two, at least 8 buckets, addressed by
   !! open addressing with linear probing. It grows by **doubling** whenever a
   !! new entry would push occupancy above a load factor of **0.75**. There
   !! are no tombstones: `remove` compacts the insertion-order table and
   !! rebuilds the bucket table from scratch, so `remove` costs
   !! `O(size() + bucket_count())` while `insert`, `get` and `has_key` are
   !! amortised `O(1)`. This map is built for reproducible iteration and
   !! simple, portable code, not for remove-heavy workloads.
   !!
   !! ## Example
   !!
   !!```fortran
   !! type(hash_map_t) :: counts
   !! character(len=:), allocatable :: k
   !! integer(int64) :: v
   !! integer(default_int) :: i
   !!
   !! call counts%init(16_default_int)
   !! call counts%insert("beta", 2_int64)
   !! call counts%insert("alpha", 1_int64)
   !! call counts%insert("beta", 20_int64)   ! updates, stays at position 1
   !! do i = 1, counts%size()
   !!    call counts%at(i, k, v)             ! "beta" then "alpha"
   !! end do
   !! call counts%destroy()
   !!```
   use pic_types, only: default_int, int32, int64
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_hash_32bit_fnv, only: fnv_1a_hash
   implicit none
   private

   public :: hash_map_t

   integer(default_int), parameter :: BUCKET_EMPTY = 0
      !! Sentinel stored in a bucket that does not hold an entry.
   integer(default_int), parameter :: MIN_BUCKETS = 8
      !! Smallest bucket table this map will ever allocate.
   integer(default_int), parameter :: MIN_ENTRIES = 8
      !! Smallest insertion-order table this map will ever allocate.
   integer(default_int), parameter :: MIN_POOL = 64
      !! Smallest key character pool this map will ever allocate.
   integer(default_int), parameter :: LOAD_NUM = 3
      !! Numerator of the maximum load factor (3/4).
   integer(default_int), parameter :: LOAD_DEN = 4
      !! Denominator of the maximum load factor (3/4).

   type :: hash_map_t
      !! Insertion-ordered hash map: `character(len=*)` keys, `integer(int64)`
      !! values. Default initialisation yields a valid empty map; the first
      !! `insert` allocates with default capacity if `init` was never called.
      private
      integer(default_int) :: n_buckets = 0
         !! Number of buckets, always a power of two, 0 while unallocated.
      integer(default_int), allocatable :: buckets(:)
         !! Bucket table; `BUCKET_EMPTY` or an index into the entry table.
      integer(default_int) :: n_entries = 0
         !! Number of live entries, i.e. the value returned by `size()`.
      integer(default_int) :: entry_cap = 0
         !! Allocated length of the entry arrays.
      integer(default_int), allocatable :: key_start(:)
         !! First character of each key inside `key_pool`, in insertion order.
      integer(default_int), allocatable :: key_size(:)
         !! Length in characters of each key, in insertion order.
      integer(int64), allocatable :: entry_value(:)
         !! Value of each entry, in insertion order.
      character(len=1), allocatable :: key_pool(:)
         !! Packed storage for all key characters.
      integer(default_int) :: pool_used = 0
         !! Number of characters currently used in `key_pool`.
      integer(default_int) :: pool_cap = 0
         !! Allocated length of `key_pool`.
   contains
      procedure :: init => hash_map_init
      procedure :: destroy => hash_map_destroy
      procedure :: clear => hash_map_clear
      procedure :: size => hash_map_size
      procedure :: is_empty => hash_map_is_empty
      procedure :: bucket_count => hash_map_bucket_count
      procedure :: insert => hash_map_insert
      procedure :: get => hash_map_get
      procedure :: has_key => hash_map_has_key
      procedure :: remove => hash_map_remove
      procedure :: keys => hash_map_keys
      procedure :: values => hash_map_values
      procedure :: at => hash_map_at
   end type hash_map_t

contains

   subroutine hash_map_init(this, initial_capacity)
      !! Allocate the map, discarding any entries it already held.
      !!
      !! `initial_capacity` is the number of *entries* the caller expects to
      !! store; the bucket table is sized to the smallest power of two that
      !! keeps that many entries below the 0.75 load factor. Values smaller
      !! than the built-in minimum, including zero and negative values, are
      !! silently raised to that minimum. Calling `init` is optional: the
      !! first `insert` into an unallocated map initialises it with the
      !! default capacity.
      class(hash_map_t), intent(inout) :: this
      integer(default_int), intent(in), optional :: initial_capacity

      integer(default_int) :: wanted, nb

      call this%destroy()

      wanted = MIN_ENTRIES
      if (present(initial_capacity)) then
         if (initial_capacity > wanted) wanted = initial_capacity
      end if

      nb = MIN_BUCKETS
      do while (wanted*LOAD_DEN > nb*LOAD_NUM)
         nb = nb*2
      end do

      allocate (this%buckets(nb))
      this%buckets = BUCKET_EMPTY
      this%n_buckets = nb

      allocate (this%key_start(wanted))
      allocate (this%key_size(wanted))
      allocate (this%entry_value(wanted))
      this%entry_cap = wanted

      allocate (this%key_pool(MIN_POOL))
      this%pool_cap = MIN_POOL
      this%pool_used = 0
      this%n_entries = 0
   end subroutine hash_map_init

   subroutine hash_map_destroy(this)
      !! Release every buffer and return the map to the unallocated state.
      class(hash_map_t), intent(inout) :: this

      if (allocated(this%buckets)) deallocate (this%buckets)
      if (allocated(this%key_start)) deallocate (this%key_start)
      if (allocated(this%key_size)) deallocate (this%key_size)
      if (allocated(this%entry_value)) deallocate (this%entry_value)
      if (allocated(this%key_pool)) deallocate (this%key_pool)
      this%n_buckets = 0
      this%n_entries = 0
      this%entry_cap = 0
      this%pool_used = 0
      this%pool_cap = 0
   end subroutine hash_map_destroy

   subroutine hash_map_clear(this)
      !! Remove every entry but keep the allocated capacity, so that refilling
      !! the map does not have to reallocate. A cleared map iterates as empty
      !! and the next `insert` starts a fresh insertion order at position 1.
      class(hash_map_t), intent(inout) :: this

      this%n_entries = 0
      this%pool_used = 0
      if (allocated(this%buckets)) this%buckets = BUCKET_EMPTY
   end subroutine hash_map_clear

   pure function hash_map_size(this) result(n)
      !! Number of entries currently stored.
      class(hash_map_t), intent(in) :: this
      integer(default_int) :: n

      n = this%n_entries
   end function hash_map_size

   pure function hash_map_is_empty(this) result(empty)
      !! `.true.` when the map holds no entries.
      class(hash_map_t), intent(in) :: this
      logical :: empty

      empty = (this%n_entries == 0)
   end function hash_map_is_empty

   pure function hash_map_bucket_count(this) result(n)
      !! Current number of buckets, 0 while the map is unallocated. Exposed so
      !! that callers and tests can observe growth; it never affects iteration
      !! order.
      class(hash_map_t), intent(in) :: this
      integer(default_int) :: n

      n = this%n_buckets
   end function hash_map_bucket_count

   subroutine hash_map_insert(this, key, value)
      !! Insert `key` with `value`, or overwrite the value of an existing key.
      !!
      !! A key that is not present is appended at the end of the iteration
      !! order. A key that is already present keeps its position in the
      !! iteration order and only its value changes.
      class(hash_map_t), intent(inout) :: this
      character(len=*), intent(in) :: key
         !! Stored verbatim; trailing blanks are part of the key.
      integer(int64), intent(in) :: value

      integer(default_int) :: entry, klen, i

      if (this%n_buckets == 0) call this%init()

      entry = find_entry(this, key)
      if (entry > 0) then
         this%entry_value(entry) = value
         return
      end if

      if ((this%n_entries + 1)*LOAD_DEN > this%n_buckets*LOAD_NUM) then
         call grow_buckets(this)
      end if

      klen = int(len(key), default_int)
      call reserve_entries(this, this%n_entries + 1)
      call reserve_pool(this, this%pool_used + klen)

      this%n_entries = this%n_entries + 1
      this%key_start(this%n_entries) = this%pool_used + 1
      this%key_size(this%n_entries) = klen
      this%entry_value(this%n_entries) = value
      do i = 1, klen
         this%key_pool(this%pool_used + i) = key(i:i)
      end do
      this%pool_used = this%pool_used + klen

      this%buckets(probe_free(this, key)) = this%n_entries
   end subroutine hash_map_insert

   subroutine hash_map_get(this, key, value, found)
      !! Look up `key`. A missing key is an ordinary outcome, not an error:
      !! `found` comes back `.false.` and `value` is set to 0.
      class(hash_map_t), intent(in) :: this
      character(len=*), intent(in) :: key
      integer(int64), intent(out) :: value
      logical, intent(out) :: found

      integer(default_int) :: entry

      entry = find_entry(this, key)
      found = (entry > 0)
      if (found) then
         value = this%entry_value(entry)
      else
         value = 0_int64
      end if
   end subroutine hash_map_get

   function hash_map_has_key(this, key) result(present_in_map)
      !! `.true.` when `key` is stored in the map.
      class(hash_map_t), intent(in) :: this
      character(len=*), intent(in) :: key
      logical :: present_in_map

      present_in_map = (find_entry(this, key) > 0)
   end function hash_map_has_key

   subroutine hash_map_remove(this, key, err)
      !! Remove `key` from the map.
      !!
      !! The entry disappears from the iteration order and every remaining
      !! entry keeps its relative position: entries inserted before it are
      !! untouched, entries inserted after it move down one index. The key
      !! itself is forgotten completely, so inserting it again afterwards is a
      !! fresh insertion that lands at the *end* of the iteration order.
      !!
      !! Removing a key that is not in the map leaves the map unchanged and
      !! reports `ERROR_VALIDATION` in `err`. When `err` is absent the call is
      !! simply a no-op.
      class(hash_map_t), intent(inout) :: this
      character(len=*), intent(in) :: key
      type(error_t), intent(out), optional :: err

      integer(default_int) :: entry, klen, kstart, i

      entry = find_entry(this, key)
      if (entry <= 0) then
         if (present(err)) then
            call err%set(ERROR_VALIDATION, "pic_hash_map: key not found: '"//key//"'")
         end if
         return
      end if

      kstart = this%key_start(entry)
      klen = this%key_size(entry)

      do i = kstart + klen, this%pool_used
         this%key_pool(i - klen) = this%key_pool(i)
      end do
      this%pool_used = this%pool_used - klen

      do i = entry, this%n_entries - 1
         this%key_start(i) = this%key_start(i + 1) - klen
         this%key_size(i) = this%key_size(i + 1)
         this%entry_value(i) = this%entry_value(i + 1)
      end do
      this%n_entries = this%n_entries - 1

      call rebuild_buckets(this)
   end subroutine hash_map_remove

   subroutine hash_map_at(this, position, key, value, err)
      !! Fetch the entry at `position` in the iteration order, counting from 1.
      !! This is the exact-fidelity iteration accessor: `key` comes back with
      !! the key's true length, trailing blanks included.
      !!
      !! An out-of-range `position` yields `key == ""`, `value == 0` and
      !! `ERROR_VALIDATION` in `err` when it is present.
      class(hash_map_t), intent(in) :: this
      integer(default_int), intent(in) :: position
      character(len=:), allocatable, intent(out) :: key
      integer(int64), intent(out) :: value
      type(error_t), intent(out), optional :: err

      if (position < 1 .or. position > this%n_entries) then
         key = ""
         value = 0_int64
         if (present(err)) then
            call err%set(ERROR_VALIDATION, "pic_hash_map: iteration index out of range")
         end if
         return
      end if

      key = entry_key(this, position)
      value = this%entry_value(position)
   end subroutine hash_map_at

   function hash_map_keys(this) result(res)
      !! All keys, in insertion order.
      !!
      !! Fortran arrays are not ragged, so every element has the same length:
      !! keys shorter than the longest one are blank-padded on the right. That
      !! padding is lossy for keys that themselves end in blanks, and a
      !! zero-length key comes back as a blank string. Use `at()` when the
      !! exact key text matters. The result is zero-sized for an empty map.
      class(hash_map_t), intent(in) :: this
      character(len=:), allocatable :: res(:)

      integer(default_int) :: i, longest

      longest = 1
      do i = 1, this%n_entries
         if (this%key_size(i) > longest) longest = this%key_size(i)
      end do

      allocate (character(len=longest) :: res(this%n_entries))
      do i = 1, this%n_entries
         res(i) = entry_key(this, i)
      end do
   end function hash_map_keys

   function hash_map_values(this) result(res)
      !! All values, in insertion order. `values()` and `keys()` are index
      !! aligned: `values(i)` belongs to `keys(i)`. Zero-sized for an empty
      !! map.
      class(hash_map_t), intent(in) :: this
      integer(int64), allocatable :: res(:)

      integer(default_int) :: i

      allocate (res(this%n_entries))
      do i = 1, this%n_entries
         res(i) = this%entry_value(i)
      end do
   end function hash_map_values

   function entry_key(this, entry) result(key)
      !! Rebuild the key of an entry from the packed character pool.
      class(hash_map_t), intent(in) :: this
      integer(default_int), intent(in) :: entry
      character(len=:), allocatable :: key

      integer(default_int) :: i, n

      n = this%key_size(entry)
      allocate (character(len=n) :: key)
      do i = 1, n
         key(i:i) = this%key_pool(this%key_start(entry) + i - 1)
      end do
   end function entry_key

   function key_matches(this, entry, key) result(same)
      !! Exact key comparison: same length and same characters. Deliberately
      !! not the Fortran `==` operator, which blank-pads the shorter operand.
      class(hash_map_t), intent(in) :: this
      integer(default_int), intent(in) :: entry
      character(len=*), intent(in) :: key
      logical :: same

      integer(default_int) :: i

      same = .false.
      if (this%key_size(entry) /= int(len(key), default_int)) return
      do i = 1, this%key_size(entry)
         if (this%key_pool(this%key_start(entry) + i - 1) /= key(i:i)) return
      end do
      same = .true.
   end function key_matches

   function home_bucket(this, key) result(bucket)
      !! Bucket a key hashes to before probing.
      !!
      !! `fnv_1a_hash` returns a fixed-width `int32` hash code; that width is
      !! an algorithmic property of FNV-1a and is intentionally *not*
      !! `default_int`. The code is widened to `int64` and masked with
      !! `n_buckets - 1`, which is valid because the bucket count is always a
      !! power of two, and which discards the sign bits along with the high
      !! bits.
      class(hash_map_t), intent(in) :: this
      character(len=*), intent(in) :: key
      integer(default_int) :: bucket

      integer(int32) :: code

      code = fnv_1a_hash(key)
      bucket = int(iand(int(code, int64), int(this%n_buckets - 1, int64)), default_int) + 1
   end function home_bucket

   function find_entry(this, key) result(entry)
      !! Index of `key` in the insertion-order table, or 0 when absent.
      class(hash_map_t), intent(in) :: this
      character(len=*), intent(in) :: key
      integer(default_int) :: entry

      integer(default_int) :: bucket, probe, candidate

      entry = 0
      if (this%n_buckets == 0) return

      bucket = home_bucket(this, key)
      do probe = 1, this%n_buckets
         candidate = this%buckets(bucket)
         if (candidate == BUCKET_EMPTY) return
         if (key_matches(this, candidate, key)) then
            entry = candidate
            return
         end if
         bucket = bucket + 1
         if (bucket > this%n_buckets) bucket = 1
      end do
   end function find_entry

   function probe_free(this, key) result(bucket)
      !! First empty bucket in the probe sequence of `key`. The load factor
      !! keeps at least one bucket empty at all times, so this always finds
      !! one.
      class(hash_map_t), intent(in) :: this
      character(len=*), intent(in) :: key
      integer(default_int) :: bucket

      integer(default_int) :: probe

      bucket = home_bucket(this, key)
      do probe = 1, this%n_buckets
         if (this%buckets(bucket) == BUCKET_EMPTY) return
         bucket = bucket + 1
         if (bucket > this%n_buckets) bucket = 1
      end do
   end function probe_free

   subroutine rebuild_buckets(this)
      !! Re-index every entry into the bucket table. Touches buckets only, so
      !! the insertion order is untouched by construction.
      class(hash_map_t), intent(inout) :: this

      integer(default_int) :: i

      this%buckets = BUCKET_EMPTY
      do i = 1, this%n_entries
         this%buckets(probe_free(this, entry_key(this, i))) = i
      end do
   end subroutine rebuild_buckets

   subroutine grow_buckets(this)
      !! Double the bucket table and rehash. Entry storage is never moved.
      class(hash_map_t), intent(inout) :: this

      integer(default_int), allocatable :: fresh(:)

      allocate (fresh(this%n_buckets*2))
      deallocate (this%buckets)
      call move_alloc(fresh, this%buckets)
      this%n_buckets = this%n_buckets*2
      call rebuild_buckets(this)
   end subroutine grow_buckets

   subroutine reserve_entries(this, needed)
      !! Ensure the insertion-order arrays hold at least `needed` entries.
      !! Explicit allocate/copy/`move_alloc` rather than
      !! reallocation-on-assignment, which is shaky on some supported
      !! compilers.
      class(hash_map_t), intent(inout) :: this
      integer(default_int), intent(in) :: needed

      integer(default_int), allocatable :: new_start(:), new_size(:)
      integer(int64), allocatable :: new_value(:)
      integer(default_int) :: new_cap, i

      if (needed <= this%entry_cap) return

      new_cap = max(this%entry_cap, MIN_ENTRIES)
      do while (new_cap < needed)
         new_cap = new_cap*2
      end do

      allocate (new_start(new_cap))
      allocate (new_size(new_cap))
      allocate (new_value(new_cap))
      do i = 1, this%n_entries
         new_start(i) = this%key_start(i)
         new_size(i) = this%key_size(i)
         new_value(i) = this%entry_value(i)
      end do

      deallocate (this%key_start)
      deallocate (this%key_size)
      deallocate (this%entry_value)
      call move_alloc(new_start, this%key_start)
      call move_alloc(new_size, this%key_size)
      call move_alloc(new_value, this%entry_value)
      this%entry_cap = new_cap
   end subroutine reserve_entries

   subroutine reserve_pool(this, needed)
      !! Ensure the packed key pool holds at least `needed` characters.
      class(hash_map_t), intent(inout) :: this
      integer(default_int), intent(in) :: needed

      character(len=1), allocatable :: fresh(:)
      integer(default_int) :: new_cap, i

      if (needed <= this%pool_cap) return

      new_cap = max(this%pool_cap, MIN_POOL)
      do while (new_cap < needed)
         new_cap = new_cap*2
      end do

      allocate (fresh(new_cap))
      do i = 1, this%pool_used
         fresh(i) = this%key_pool(i)
      end do

      deallocate (this%key_pool)
      call move_alloc(fresh, this%key_pool)
      this%pool_cap = new_cap
   end subroutine reserve_pool

end module pic_hash_map
