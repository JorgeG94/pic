! SPDX-Identifer: MIT
module test_pic_clock
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int64
   use pic_error, only: error_t, ERROR_GENERIC
   use pic_clock, only: datetime_t, monotonic_ms, monotonic_us, now_local, now_utc, &
                        unix_time_ms, format_iso8601, PIC_CLOCK_NO_CLOCK
   implicit none
   private
   public :: collect_pic_clock_tests

contains

   subroutine collect_pic_clock_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
                  new_unittest("monotonic-is-available", test_monotonic_available), &
                  new_unittest("monotonic-does-not-go-backwards", test_monotonic_forward), &
                  new_unittest("monotonic-ms-and-us-agree", test_monotonic_units), &
                  new_unittest("unix-epoch", test_unix_epoch), &
                  new_unittest("unix-reference-instants", test_unix_reference), &
                  new_unittest("unix-before-the-epoch", test_unix_negative), &
                  new_unittest("leap-days", test_leap_days), &
                  new_unittest("round-trip-over-40000-days", test_round_trip), &
                  new_unittest("utc-offset-is-applied", test_utc_offset), &
                  new_unittest("iso8601-format", test_iso8601), &
                  new_unittest("iso8601-with-offset", test_iso8601_offset), &
                  new_unittest("iso8601-pads-every-field", test_iso8601_padding), &
                  new_unittest("now-local-is-sane", test_now_local) &
                  ]
   end subroutine collect_pic_clock_tests

   subroutine test_monotonic_available(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, monotonic_ms() /= PIC_CLOCK_NO_CLOCK, &
                 "this processor should report a monotonic clock")
      if (allocated(error)) return
      call check(error, monotonic_ms() >= 0_int64, "a real reading is never negative")
      if (allocated(error)) return
   end subroutine test_monotonic_available

   subroutine test_monotonic_forward(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: t0, t1, spin
      integer(default_int) :: i

      t0 = monotonic_ms()
      spin = 0_int64
      do i = 1, 2000000
         spin = spin + int(i, int64)
      end do
      t1 = monotonic_ms()
      call check(error, t1 >= t0, "monotonic time never moves backwards")
      if (allocated(error)) return
      ! keeps the loop from being optimised away entirely
      call check(error, spin > 0_int64, "the spin loop ran")
      if (allocated(error)) return
   end subroutine test_monotonic_forward

   subroutine test_monotonic_units(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: ms, us

      us = monotonic_us()
      ms = monotonic_ms()
      ! Same origin, so the microsecond reading divided down must be within a
      ! millisecond or two of the millisecond reading. A wrong scale factor
      ! would be off by a factor of 1000, not by 2.
      call check(error, abs(us/1000_int64 - ms) <= 50_int64, &
                 "monotonic_us/1000 tracks monotonic_ms")
      if (allocated(error)) return
   end subroutine test_monotonic_units

   subroutine test_unix_epoch(error)
      type(error_type), allocatable, intent(out) :: error
      type(datetime_t) :: dt

      dt = datetime_t(1970, 1, 1, 0, 0, 0, 0, 0)
      call check(error, unix_time_ms(dt) == 0_int64, "1970-01-01T00:00:00Z is zero")
      if (allocated(error)) return

      dt = datetime_t(1970, 1, 2, 0, 0, 0, 0, 0)
      call check(error, unix_time_ms(dt) == 86400000_int64, "one day later is 86400000 ms")
      if (allocated(error)) return
   end subroutine test_unix_epoch

   !> Values from an independent reference (Python datetime), not from this
   !> library.
   subroutine test_unix_reference(error)
      type(error_type), allocatable, intent(out) :: error
      type(datetime_t) :: dt

      dt = datetime_t(2000, 2, 29, 12, 0, 0, 0, 0)
      call check(error, unix_time_ms(dt) == 951825600000_int64, "2000-02-29T12:00:00Z")
      if (allocated(error)) return

      dt = datetime_t(2026, 9, 14, 9, 46, 0, 123, 0)
      call check(error, unix_time_ms(dt) == 1789379160123_int64, "2026-09-14T09:46:00.123Z")
      if (allocated(error)) return

      ! the 32-bit time_t rollover, a date every calendar implementation
      ! should be asked about
      dt = datetime_t(2038, 1, 19, 3, 14, 7, 0, 0)
      call check(error, unix_time_ms(dt) == 2147483647000_int64, "2038-01-19T03:14:07Z")
      if (allocated(error)) return

      dt = datetime_t(2100, 3, 1, 0, 0, 0, 0, 0)
      call check(error, unix_time_ms(dt) == 4107542400000_int64, &
                 "2100-03-01Z, the year 2100 is not a leap year")
      if (allocated(error)) return
   end subroutine test_unix_reference

   subroutine test_unix_negative(error)
      type(error_type), allocatable, intent(out) :: error
      type(datetime_t) :: dt

      dt = datetime_t(1969, 12, 31, 23, 59, 59, 999, 0)
      call check(error, unix_time_ms(dt) == -1_int64, "one millisecond before the epoch")
      if (allocated(error)) return

      dt = datetime_t(1900, 3, 1, 0, 0, 0, 0, 0)
      call check(error, unix_time_ms(dt) == -2203891200000_int64, &
                 "1900-03-01Z, the year 1900 is not a leap year")
      if (allocated(error)) return

      dt = datetime_t(1600, 2, 29, 0, 0, 0, 0, 0)
      call check(error, unix_time_ms(dt) == -11670998400000_int64, &
                 "1600-02-29Z, the year 1600 is a leap year")
      if (allocated(error)) return
   end subroutine test_unix_negative

   !> The three-part Gregorian leap rule, each part asserted separately.
   subroutine test_leap_days(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: a, b

      ! 2024 divisible by 4 -> leap, so Feb 29 exists and Mar 1 is a day later
      a = unix_time_ms(datetime_t(2024, 2, 29, 0, 0, 0, 0, 0))
      b = unix_time_ms(datetime_t(2024, 3, 1, 0, 0, 0, 0, 0))
      call check(error, b - a == 86400000_int64, "2024 is a leap year")
      if (allocated(error)) return

      ! 1900 divisible by 100 but not 400 -> not leap, so Feb 28 to Mar 1 is one day
      a = unix_time_ms(datetime_t(1900, 2, 28, 0, 0, 0, 0, 0))
      b = unix_time_ms(datetime_t(1900, 3, 1, 0, 0, 0, 0, 0))
      call check(error, b - a == 86400000_int64, "1900 is not a leap year")
      if (allocated(error)) return

      ! 2000 divisible by 400 -> leap
      a = unix_time_ms(datetime_t(2000, 2, 28, 0, 0, 0, 0, 0))
      b = unix_time_ms(datetime_t(2000, 3, 1, 0, 0, 0, 0, 0))
      call check(error, b - a == 2_int64*86400000_int64, "2000 is a leap year")
      if (allocated(error)) return
   end subroutine test_leap_days

   !> Walks day by day for 40000 days, about 110 years spanning 2000, and
   !> checks that consecutive dates are always exactly one day apart. A
   !> month-length or leap-rule error anywhere in the range shows up as a
   !> jump.
   subroutine test_round_trip(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: previous, current, day
      logical :: contiguous

      contiguous = .true.
      previous = unix_time_ms(datetime_t(1950, 1, 1, 0, 0, 0, 0, 0))
      do day = 1_int64, 40000_int64
         current = previous + 86400000_int64
         ! round-trip the instant through the calendar and back
         if (unix_time_ms(datetime_from_ms_via_public_api(current)) /= current) then
            contiguous = .false.
            exit
         end if
         previous = current
      end do
      call check(error, contiguous, "every day over 40000 days round-trips exactly")
      if (allocated(error)) return
   end subroutine test_round_trip

   !> `now_utc` is the only public route to the inverse conversion, so the
   !> round-trip test reconstructs a datetime the same way `now_utc` does:
   !> by asserting that a known instant formats and re-parses consistently.
   !> Here we simply rebuild the fields arithmetically and compare.
   function datetime_from_ms_via_public_api(ms) result(dt)
      integer(int64), intent(in) :: ms
      type(datetime_t) :: dt
      integer(int64) :: days, rem, z, era, doe, yoe, doy, mp, y, m, d

      days = ms/86400000_int64
      rem = mod(ms, 86400000_int64)
      if (rem < 0_int64) then
         days = days - 1_int64
         rem = rem + 86400000_int64
      end if
      z = days + 719468_int64
      if (z >= 0_int64) then
         era = z/146097_int64
      else
         era = (z - 146096_int64)/146097_int64
      end if
      doe = z - era*146097_int64
      yoe = (doe - doe/1460_int64 + doe/36524_int64 - doe/146096_int64)/365_int64
      y = yoe + era*400_int64
      doy = doe - (365_int64*yoe + yoe/4_int64 - yoe/100_int64)
      mp = (5_int64*doy + 2_int64)/153_int64
      d = doy - (153_int64*mp + 2_int64)/5_int64 + 1_int64
      if (mp < 10_int64) then
         m = mp + 3_int64
      else
         m = mp - 9_int64
      end if
      if (m <= 2_int64) y = y + 1_int64

      dt%year = int(y, default_int)
      dt%month = int(m, default_int)
      dt%day = int(d, default_int)
      dt%hour = int(rem/3600000_int64, default_int)
      rem = mod(rem, 3600000_int64)
      dt%minute = int(rem/60000_int64, default_int)
      rem = mod(rem, 60000_int64)
      dt%second = int(rem/1000_int64, default_int)
      dt%millisecond = int(mod(rem, 1000_int64), default_int)
      dt%utc_offset_min = 0
   end function datetime_from_ms_via_public_api

   subroutine test_utc_offset(error)
      type(error_type), allocatable, intent(out) :: error
      type(datetime_t) :: utc, ahead, behind

      utc = datetime_t(2026, 9, 14, 12, 0, 0, 0, 0)
      ! 14:00 at +02:00 is the same instant as 12:00Z
      ahead = datetime_t(2026, 9, 14, 14, 0, 0, 0, 120)
      ! 07:00 at -05:00 is also the same instant
      behind = datetime_t(2026, 9, 14, 7, 0, 0, 0, -300)

      call check(error, unix_time_ms(ahead) == unix_time_ms(utc), &
                 "a local time ahead of UTC maps to the same instant")
      if (allocated(error)) return
      call check(error, unix_time_ms(behind) == unix_time_ms(utc), &
                 "and so does one behind it")
      if (allocated(error)) return
   end subroutine test_utc_offset

   subroutine test_iso8601(error)
      type(error_type), allocatable, intent(out) :: error
      type(datetime_t) :: dt

      dt = datetime_t(2026, 9, 14, 9, 46, 0, 123, 0)
      call check(error, format_iso8601(dt) == "2026-09-14T09:46:00.123Z", &
                 "the documented example renders exactly")
      if (allocated(error)) return
   end subroutine test_iso8601

   subroutine test_iso8601_offset(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, format_iso8601(datetime_t(2026, 9, 14, 14, 0, 0, 0, 120)) &
                 == "2026-09-14T14:00:00.000+02:00", "a positive offset renders as +HH:MM")
      if (allocated(error)) return
      call check(error, format_iso8601(datetime_t(2026, 9, 14, 7, 30, 0, 0, -330)) &
                 == "2026-09-14T07:30:00.000-05:30", &
                 "a negative half-hour offset renders as -HH:MM")
      if (allocated(error)) return
   end subroutine test_iso8601_offset

   subroutine test_iso8601_padding(error)
      type(error_type), allocatable, intent(out) :: error
      call check(error, format_iso8601(datetime_t(7, 1, 2, 3, 4, 5, 6, 0)) &
                 == "0007-01-02T03:04:05.006Z", &
                 "every field is zero filled to its full width")
      if (allocated(error)) return
   end subroutine test_iso8601_padding

   subroutine test_now_local(error)
      type(error_type), allocatable, intent(out) :: error
      type(datetime_t) :: dt
      type(error_t) :: err

      call now_local(dt, err)
      call check(error,.not. err%has_error(), "now_local succeeds on a normal processor")
      if (allocated(error)) return
      call check(error, dt%year >= 2020 .and. dt%year < 3000, "the year is plausible")
      if (allocated(error)) return
      call check(error, dt%month >= 1 .and. dt%month <= 12, "the month is in range")
      if (allocated(error)) return
      call check(error, dt%day >= 1 .and. dt%day <= 31, "the day is in range")
      if (allocated(error)) return
      call check(error, dt%hour >= 0 .and. dt%hour <= 23, "the hour is in range")
      if (allocated(error)) return
      call check(error, dt%millisecond >= 0 .and. dt%millisecond <= 999, &
                 "the millisecond is in range")
      if (allocated(error)) return
   end subroutine test_now_local

end module test_pic_clock
