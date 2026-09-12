module test_pic_hash_map
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64
   use pic_error, only: error_t, ERROR_VALIDATION
   use pic_hash_32bit_fnv, only: fnv_1a_hash
   use pic_hash_map, only: hash_map_t
   implicit none
   private
   public :: collect_pic_hash_map_tests

contains

   subroutine collect_pic_hash_map_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("empty_map", test_empty_map), &
                  new_unittest("init_clear_destroy", test_init_clear_destroy), &
                  new_unittest("auto_init_on_insert", test_auto_init_on_insert), &
                  new_unittest("order_survives_growth", test_order_survives_growth), &
                  new_unittest("overwrite_keeps_position", test_overwrite_keeps_position), &
                  new_unittest("remove_front_middle_back", test_remove_front_middle_back), &
                  new_unittest("remove_then_reinsert", test_remove_then_reinsert), &
                  new_unittest("collisions", test_collisions), &
                  new_unittest("missing_key_operations", test_missing_key_operations), &
                  new_unittest("whitespace_keys", test_whitespace_keys), &
                  new_unittest("empty_and_long_keys", test_empty_and_long_keys), &
                  new_unittest("interleaved_remove_insert", test_interleaved_remove_insert) &
                  ]
   end subroutine collect_pic_hash_map_tests

   !> Build a reproducible key name such as "key007".
   function numbered_key(i) result(key)
      integer(default_int), intent(in) :: i
      character(len=6) :: key
      write (key, '(a,i3.3)') "key", i
   end function numbered_key

   !> Replicates the map's internal bucket mapping so the test can craft
   !> keys that collide in the same home bucket.
   function bucket_of(key, n_buckets) result(bucket)
      character(len=*), intent(in) :: key
      integer(default_int), intent(in) :: n_buckets
      integer(default_int) :: bucket
      integer(int32) :: code
      code = fnv_1a_hash(key)
      bucket = int(iand(int(code, int64), int(n_buckets - 1, int64)), default_int) + 1
   end function bucket_of

   subroutine test_empty_map(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      type(error_t) :: err
      character(len=:), allocatable :: key
      character(len=:), allocatable :: all_keys(:)
      integer(int64), allocatable :: all_values(:)
      integer(int64) :: value
      logical :: found

      call check(error, map%size() == 0, "fresh map must be size 0")
      if (allocated(error)) return
      call check(error, map%is_empty(), "fresh map must be empty")
      if (allocated(error)) return
      call check(error, map%bucket_count() == 0, "fresh map must have no buckets")
      if (allocated(error)) return
      call check(error, .not. map%has_key("nope"), "fresh map has no keys")
      if (allocated(error)) return

      call map%get("nope", value, found)
      call check(error, .not. found, "get on empty map must not find")
      if (allocated(error)) return
      call check(error, value == 0_int64, "get on empty map returns 0")
      if (allocated(error)) return

      call map%remove("nope", err)
      call check(error, err%has_error(), "remove of missing key must error")
      if (allocated(error)) return
      call check(error, err%is(ERROR_VALIDATION), "remove error code is ERROR_VALIDATION")
      if (allocated(error)) return

      call map%at(1_default_int, key, value, err)
      call check(error, err%has_error(), "at() out of range must error")
      if (allocated(error)) return
      call check(error, len(key) == 0, "at() out of range yields empty key")
      if (allocated(error)) return
      call check(error, value == 0_int64, "at() out of range yields zero value")
      if (allocated(error)) return

      all_keys = map%keys()
      call check(error, size(all_keys) == 0, "keys() of empty map is zero sized")
      if (allocated(error)) return
      all_values = map%values()
      call check(error, size(all_values) == 0, "values() of empty map is zero sized")
      if (allocated(error)) return

      ! clear() and destroy() on a never-allocated map must be safe
      call map%clear()
      call map%destroy()
      call check(error, map%size() == 0, "clear/destroy on empty map stays empty")
   end subroutine test_empty_map

   subroutine test_init_clear_destroy(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=:), allocatable :: all_keys(:)
      integer(default_int) :: small_buckets, big_buckets
      integer(int64) :: value
      logical :: found

      ! a capacity below the built-in minimum is raised to it
      call map%init(0_default_int)
      small_buckets = map%bucket_count()
      call check(error, small_buckets >= 8, "init(0) still allocates the minimum table")
      if (allocated(error)) return

      ! a large capacity is honoured without any growth on insert
      call map%init(100_default_int)
      big_buckets = map%bucket_count()
      call check(error, big_buckets > small_buckets, "init(100) allocates a bigger table")
      if (allocated(error)) return

      call map%insert("one", 1_int64)
      call map%insert("two", 2_int64)
      call check(error, map%size() == 2, "two entries inserted")
      if (allocated(error)) return
      call check(error, map%bucket_count() == big_buckets, "no growth below the load factor")
      if (allocated(error)) return

      call map%clear()
      call check(error, map%is_empty(), "clear empties the map")
      if (allocated(error)) return
      call check(error, map%bucket_count() == big_buckets, "clear keeps the capacity")
      if (allocated(error)) return
      call check(error, .not. map%has_key("one"), "cleared map has no keys")
      if (allocated(error)) return

      ! refilling a cleared map restarts the insertion order at position 1
      call map%insert("two", 22_int64)
      call map%insert("one", 11_int64)
      call map%get("two", value, found)
      call check(error, found .and. value == 22_int64, "refilled value is readable")
      if (allocated(error)) return
      all_keys = map%keys()
      call check(error, all_keys(1) == "two", "cleared map restarts insertion order")
      if (allocated(error)) return

      call map%destroy()
      call check(error, map%bucket_count() == 0, "destroy releases the table")
      if (allocated(error)) return
      call check(error, map%size() == 0, "destroy empties the map")
   end subroutine test_init_clear_destroy

   subroutine test_auto_init_on_insert(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      integer(int64) :: value
      logical :: found

      call map%insert("alpha", 7_int64)
      call check(error, map%bucket_count() > 0, "insert auto-initialises the map")
      if (allocated(error)) return
      call map%get("alpha", value, found)
      call check(error, found .and. value == 7_int64, "auto-initialised insert is readable")
      if (allocated(error)) return
      call map%destroy()
   end subroutine test_auto_init_on_insert

   subroutine test_order_survives_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=:), allocatable :: key
      character(len=:), allocatable :: all_keys(:)
      integer(int64), allocatable :: all_values(:)
      integer(int64) :: value
      integer(default_int) :: i, start_buckets, growths, prev_buckets
      logical :: found

      integer(default_int), parameter :: N = 200

      call map%init(8_default_int)
      start_buckets = map%bucket_count()
      prev_buckets = start_buckets
      growths = 0

      do i = 1, N
         call map%insert(numbered_key(i), int(i, int64)*10_int64)
         if (map%bucket_count() /= prev_buckets) then
            growths = growths + 1
            prev_buckets = map%bucket_count()
         end if
      end do

      call check(error, growths >= 2, "at least two growth events must have happened")
      if (allocated(error)) return
      call check(error, map%size() == N, "all keys stored")
      if (allocated(error)) return

      all_keys = map%keys()
      all_values = map%values()
      call check(error, size(all_keys) == N, "keys() length matches size()")
      if (allocated(error)) return
      call check(error, size(all_values) == N, "values() length matches size()")
      if (allocated(error)) return

      do i = 1, N
         call check(error, all_keys(i) == numbered_key(i), "keys() still in insertion order")
         if (allocated(error)) return
         call check(error, all_values(i) == int(i, int64)*10_int64, "values() aligned with keys()")
         if (allocated(error)) return
         call map%at(i, key, value)
         call check(error, key == numbered_key(i), "at() still in insertion order")
         if (allocated(error)) return
         call check(error, value == int(i, int64)*10_int64, "at() value matches")
         if (allocated(error)) return
         call map%get(numbered_key(i), value, found)
         call check(error, found, "every key is retrievable after rehashing")
         if (allocated(error)) return
         call check(error, value == int(i, int64)*10_int64, "every value survives rehashing")
         if (allocated(error)) return
      end do

      call map%destroy()
   end subroutine test_order_survives_growth

   subroutine test_overwrite_keeps_position(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=:), allocatable :: all_keys(:)
      integer(int64), allocatable :: all_values(:)
      integer(int64) :: value
      logical :: found

      call map%init()
      call map%insert("alpha", 1_int64)
      call map%insert("bravo", 2_int64)
      call map%insert("charlie", 3_int64)
      call map%insert("delta", 4_int64)

      call map%insert("bravo", 222_int64)
      call check(error, map%size() == 4, "overwrite must not add an entry")
      if (allocated(error)) return

      call map%get("bravo", value, found)
      call check(error, found .and. value == 222_int64, "overwrite updates the value")
      if (allocated(error)) return

      all_keys = map%keys()
      all_values = map%values()
      call check(error, all_keys(1) == "alpha", "position 1 unchanged")
      if (allocated(error)) return
      call check(error, all_keys(2) == "bravo", "overwritten key keeps position 2")
      if (allocated(error)) return
      call check(error, all_keys(3) == "charlie", "position 3 unchanged")
      if (allocated(error)) return
      call check(error, all_keys(4) == "delta", "position 4 unchanged")
      if (allocated(error)) return
      call check(error, all_values(2) == 222_int64, "values() shows the new value at position 2")
      if (allocated(error)) return

      ! overwriting the first and last entries must not move them either
      call map%insert("alpha", 111_int64)
      call map%insert("delta", 444_int64)
      all_keys = map%keys()
      all_values = map%values()
      call check(error, all_keys(1) == "alpha" .and. all_values(1) == 111_int64, "first entry stays first")
      if (allocated(error)) return
      call check(error, all_keys(4) == "delta" .and. all_values(4) == 444_int64, "last entry stays last")
      if (allocated(error)) return

      call map%destroy()
   end subroutine test_overwrite_keeps_position

   subroutine test_remove_front_middle_back(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      type(error_t) :: err
      character(len=:), allocatable :: all_keys(:)
      integer(int64), allocatable :: all_values(:)

      call map%init()
      call map%insert("a", 1_int64)
      call map%insert("b", 2_int64)
      call map%insert("c", 3_int64)
      call map%insert("d", 4_int64)
      call map%insert("e", 5_int64)

      ! remove from the front: b c d e
      call map%remove("a", err)
      call check(error, .not. err%has_error(), "removing a present key succeeds")
      if (allocated(error)) return
      all_keys = map%keys()
      all_values = map%values()
      call check(error, map%size() == 4, "size drops after front removal")
      if (allocated(error)) return
      call check(error, all_keys(1) == "b" .and. all_keys(2) == "c" .and. &
                 all_keys(3) == "d" .and. all_keys(4) == "e", "front removal shifts the rest down")
      if (allocated(error)) return
      call check(error, all_values(1) == 2_int64 .and. all_values(4) == 5_int64, "values shift with keys")
      if (allocated(error)) return
      call check(error, .not. map%has_key("a"), "removed key is gone")
      if (allocated(error)) return

      ! remove from the middle: b d e
      call map%remove("c")
      all_keys = map%keys()
      all_values = map%values()
      call check(error, map%size() == 3, "size drops after middle removal")
      if (allocated(error)) return
      call check(error, all_keys(1) == "b" .and. all_keys(2) == "d" .and. all_keys(3) == "e", &
                 "middle removal preserves relative order")
      if (allocated(error)) return
      call check(error, all_values(1) == 2_int64 .and. all_values(2) == 4_int64 .and. &
                 all_values(3) == 5_int64, "values follow their keys")
      if (allocated(error)) return

      ! remove from the back: b d
      call map%remove("e")
      all_keys = map%keys()
      call check(error, map%size() == 2, "size drops after back removal")
      if (allocated(error)) return
      call check(error, all_keys(1) == "b" .and. all_keys(2) == "d", "back removal leaves the front alone")
      if (allocated(error)) return

      ! drain the map completely
      call map%remove("b")
      call map%remove("d")
      call check(error, map%is_empty(), "draining every key empties the map")
      if (allocated(error)) return
      call check(error, size(map%keys()) == 0, "drained map iterates as empty")
      if (allocated(error)) return

      call map%destroy()
   end subroutine test_remove_front_middle_back

   subroutine test_remove_then_reinsert(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=:), allocatable :: all_keys(:)
      integer(int64), allocatable :: all_values(:)

      call map%init()
      call map%insert("alpha", 1_int64)
      call map%insert("bravo", 2_int64)
      call map%insert("charlie", 3_int64)

      call map%remove("bravo")
      call map%insert("bravo", 99_int64)

      all_keys = map%keys()
      all_values = map%values()
      call check(error, map%size() == 3, "re-inserted key is back")
      if (allocated(error)) return
      call check(error, all_keys(1) == "alpha", "alpha still first")
      if (allocated(error)) return
      call check(error, all_keys(2) == "charlie", "charlie moved up into position 2")
      if (allocated(error)) return
      call check(error, all_keys(3) == "bravo", "re-insertion appends at the end")
      if (allocated(error)) return
      call check(error, all_values(3) == 99_int64, "re-inserted value is the new one")
      if (allocated(error)) return

      call map%destroy()
   end subroutine test_remove_then_reinsert

   subroutine test_collisions(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=10) :: candidate
      character(len=10) :: chosen(6)
      character(len=:), allocatable :: all_keys(:)
      integer(default_int) :: n_buckets, target_bucket, n_found, i
      integer(int64) :: value
      logical :: found

      call map%init(8_default_int)
      n_buckets = map%bucket_count()
      target_bucket = bucket_of("anchor", n_buckets)

      ! brute-force six distinct keys that all hash to the same home bucket
      n_found = 1
      chosen(1) = "anchor"
      i = 0
      do while (n_found < 6 .and. i < 100000)
         i = i + 1
         write (candidate, '(a,i6.6)') "kx", i
         if (bucket_of(trim(candidate), n_buckets) == target_bucket) then
            n_found = n_found + 1
            chosen(n_found) = candidate
         end if
      end do

      call check(error, n_found == 6, "found six colliding keys")
      if (allocated(error)) return

      do i = 1, 6
         call map%insert(trim(chosen(i)), int(i, int64)*1000_int64)
      end do
      call check(error, map%size() == 6, "all colliding keys stored separately")
      if (allocated(error)) return
      call check(error, map%bucket_count() == n_buckets, "no growth during the collision test")
      if (allocated(error)) return

      do i = 1, 6
         call map%get(trim(chosen(i)), value, found)
         call check(error, found, "colliding key "//trim(chosen(i))//" must be retrievable")
         if (allocated(error)) return
         call check(error, value == int(i, int64)*1000_int64, "colliding key keeps its own value")
         if (allocated(error)) return
      end do

      all_keys = map%keys()
      do i = 1, 6
         call check(error, trim(all_keys(i)) == trim(chosen(i)), "collisions do not disturb insertion order")
         if (allocated(error)) return
      end do

      ! removing from the middle of a probe chain must not orphan the rest
      call map%remove(trim(chosen(3)))
      call check(error, .not. map%has_key(trim(chosen(3))), "removed colliding key is gone")
      if (allocated(error)) return
      do i = 1, 6
         if (i == 3) cycle
         call map%get(trim(chosen(i)), value, found)
         call check(error, found, "probe chain survives a mid-chain removal")
         if (allocated(error)) return
         call check(error, value == int(i, int64)*1000_int64, "mid-chain removal keeps other values")
         if (allocated(error)) return
      end do

      call map%destroy()
   end subroutine test_collisions

   subroutine test_missing_key_operations(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      type(error_t) :: err
      character(len=:), allocatable :: key
      integer(int64) :: value
      logical :: found

      call map%init()
      call map%insert("alpha", 1_int64)
      call map%insert("bravo", 2_int64)

      call map%get("charlie", value, found)
      call check(error, .not. found, "missing key is not found")
      if (allocated(error)) return
      call check(error, value == 0_int64, "missing key yields a zero value")
      if (allocated(error)) return
      call check(error, .not. map%has_key("charlie"), "has_key is false for a missing key")
      if (allocated(error)) return

      call map%remove("charlie", err)
      call check(error, err%has_error(), "removing a missing key reports an error")
      if (allocated(error)) return
      call check(error, index(err%get_message(), "charlie") > 0, "error message names the key")
      if (allocated(error)) return
      call check(error, map%size() == 2, "a failed remove changes nothing")
      if (allocated(error)) return

      ! removing a missing key without err must be a silent no-op
      call map%remove("charlie")
      call check(error, map%size() == 2, "err-less remove of a missing key is a no-op")
      if (allocated(error)) return

      ! out-of-range iteration, with and without err
      call map%at(0_default_int, key, value, err)
      call check(error, err%has_error(), "index 0 is out of range")
      if (allocated(error)) return
      call map%at(3_default_int, key, value, err)
      call check(error, err%has_error(), "index size()+1 is out of range")
      if (allocated(error)) return
      call map%at(-5_default_int, key, value)
      call check(error, len(key) == 0 .and. value == 0_int64, "negative index yields the empty entry")
      if (allocated(error)) return

      call map%at(2_default_int, key, value, err)
      call check(error, .not. err%has_error(), "in-range index does not error")
      if (allocated(error)) return
      call check(error, key == "bravo" .and. value == 2_int64, "in-range index returns the entry")
      if (allocated(error)) return

      call map%destroy()
   end subroutine test_missing_key_operations

   subroutine test_whitespace_keys(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=:), allocatable :: key
      integer(int64) :: value
      logical :: found

      call map%init()
      ! Fortran says "a" == "a " is .true.; this map says they are distinct.
      call map%insert("a", 1_int64)
      call map%insert("a ", 2_int64)
      call map%insert("a  ", 3_int64)

      call check(error, map%size() == 3, "trailing blanks make distinct keys")
      if (allocated(error)) return

      call map%get("a", value, found)
      call check(error, found .and. value == 1_int64, "'a' keeps its own value")
      if (allocated(error)) return
      call map%get("a ", value, found)
      call check(error, found .and. value == 2_int64, "'a ' keeps its own value")
      if (allocated(error)) return
      call map%get("a  ", value, found)
      call check(error, found .and. value == 3_int64, "'a  ' keeps its own value")
      if (allocated(error)) return
      call check(error, .not. map%has_key("a   "), "a longer blank run is yet another key")
      if (allocated(error)) return

      ! at() reports the exact key length, unlike the blank-padded keys()
      call map%at(1_default_int, key, value)
      call check(error, len(key) == 1, "at() reports the exact length of 'a'")
      if (allocated(error)) return
      call map%at(2_default_int, key, value)
      call check(error, len(key) == 2, "at() reports the exact length of 'a '")
      if (allocated(error)) return
      call map%at(3_default_int, key, value)
      call check(error, len(key) == 3, "at() reports the exact length of 'a  '")
      if (allocated(error)) return

      ! keys() blank-pads to the longest key, which is documented as lossy here
      call check(error, len(map%keys()) == 3, "keys() pads to the longest key")
      if (allocated(error)) return

      ! removing the middle blank-padded variant leaves the others intact
      call map%remove("a ")
      call check(error, map%size() == 2, "one blank variant removed")
      if (allocated(error)) return
      call check(error, map%has_key("a"), "'a' survives removal of 'a '")
      if (allocated(error)) return
      call check(error, map%has_key("a  "), "'a  ' survives removal of 'a '")
      if (allocated(error)) return

      call map%destroy()
   end subroutine test_whitespace_keys

   subroutine test_empty_and_long_keys(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=512) :: long_key
      character(len=:), allocatable :: key
      character(len=:), allocatable :: all_keys(:)
      integer(int64) :: value
      integer(default_int) :: i
      logical :: found

      do i = 1, 512
         long_key(i:i) = achar(97_default_int + modulo(i, 26_default_int))
      end do

      call map%init()
      call map%insert("", 42_int64)
      call map%insert(long_key, 43_int64)
      call map%insert("mid", 44_int64)

      call check(error, map%size() == 3, "empty and long keys both stored")
      if (allocated(error)) return

      call map%get("", value, found)
      call check(error, found .and. value == 42_int64, "the empty key round-trips")
      if (allocated(error)) return
      call map%get(long_key, value, found)
      call check(error, found .and. value == 43_int64, "a 512 character key round-trips")
      if (allocated(error)) return
      call check(error, map%has_key(""), "has_key finds the empty key")
      if (allocated(error)) return

      call map%at(1_default_int, key, value)
      call check(error, len(key) == 0, "at() returns the empty key at its true length")
      if (allocated(error)) return
      call map%at(2_default_int, key, value)
      call check(error, len(key) == 512 .and. key == long_key, "at() returns the long key intact")
      if (allocated(error)) return

      all_keys = map%keys()
      call check(error, len(all_keys) == 512, "keys() pads everything to the longest key")
      if (allocated(error)) return
      call check(error, all_keys(3) == "mid", "padded keys stay in insertion order")
      if (allocated(error)) return

      ! removing the empty key must not disturb the pool offsets of the rest
      call map%remove("")
      call check(error, map%size() == 2, "empty key removed")
      if (allocated(error)) return
      call map%get(long_key, value, found)
      call check(error, found .and. value == 43_int64, "long key survives removal of the empty key")
      if (allocated(error)) return
      call map%get("mid", value, found)
      call check(error, found .and. value == 44_int64, "short key survives removal of the empty key")
      if (allocated(error)) return

      ! removing the long key must compact the pool correctly too
      call map%remove(long_key)
      call map%get("mid", value, found)
      call check(error, found .and. value == 44_int64, "short key survives removal of the long key")
      if (allocated(error)) return
      all_keys = map%keys()
      call check(error, size(all_keys) == 1 .and. all_keys(1) == "mid", "only the short key is left")
      if (allocated(error)) return

      call map%destroy()
   end subroutine test_empty_and_long_keys

   subroutine test_interleaved_remove_insert(error)
      type(error_type), allocatable, intent(out) :: error
      type(hash_map_t) :: map
      character(len=:), allocatable :: all_keys(:)
      integer(int64), allocatable :: all_values(:)
      integer(int64) :: value
      integer(default_int) :: i, expected
      logical :: found

      integer(default_int), parameter :: N = 60

      call map%init()
      do i = 1, N
         call map%insert(numbered_key(i), int(i, int64))
      end do

      ! remove every even key; the odd ones must keep their relative order
      do i = 2, N, 2
         call map%remove(numbered_key(i))
      end do
      call check(error, map%size() == N/2, "half the entries are gone")
      if (allocated(error)) return

      all_keys = map%keys()
      all_values = map%values()
      do i = 1, N/2
         expected = 2*i - 1
         call check(error, all_keys(i) == numbered_key(expected), "odd keys keep their relative order")
         if (allocated(error)) return
         call check(error, all_values(i) == int(expected, int64), "odd values follow their keys")
         if (allocated(error)) return
      end do

      ! re-inserting the even keys appends them, in the order re-inserted
      do i = 2, N, 2
         call map%insert(numbered_key(i), int(i, int64)*100_int64)
      end do
      call check(error, map%size() == N, "every key is back")
      if (allocated(error)) return

      all_keys = map%keys()
      all_values = map%values()
      do i = 1, N/2
         call check(error, all_keys(i) == numbered_key(2*i - 1), "odd keys still lead the order")
         if (allocated(error)) return
         call check(error, all_keys(N/2 + i) == numbered_key(2*i), "re-inserted even keys are appended in order")
         if (allocated(error)) return
         call check(error, all_values(N/2 + i) == int(2*i, int64)*100_int64, "re-inserted values are the new ones")
         if (allocated(error)) return
      end do

      do i = 1, N
         call map%get(numbered_key(i), value, found)
         call check(error, found, "every key is still retrievable after the churn")
         if (allocated(error)) return
      end do

      call map%destroy()
   end subroutine test_interleaved_remove_insert

end module test_pic_hash_map
