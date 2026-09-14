! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Monotonic elapsed time and wall-clock date and time.
module pic_clock
   !! Two unrelated notions of time, kept apart on purpose.
   !!
   !! **Monotonic** time (`monotonic_ms`, `monotonic_us`) counts from an
   !! unspecified origin and only ever moves forward. Use it to measure how
   !! long something took, or to pace a loop against real time. Differences
   !! are meaningful; the absolute value is not.
   !!
   !! **Wall-clock** time (`now_local`, `now_utc`) is the calendar date and
   !! time. Use it to stamp a log line or name an output file. It can jump
   !! backwards when the system clock is corrected, so it must never be used
   !! to measure a duration.
   !!
   !! ### Relationship to `pic_timer`
   !!
   !! `pic_timer` measures intervals and reports `real(dp)` seconds, which is
   !! what a benchmark wants. This module reports whole milliseconds or
   !! microseconds as `integer(int64)`, which is what a simulation pacing
   !! itself against the wall clock wants: integers compare and accumulate
   !! exactly, so a frame budget does not drift with rounding.
   !!
   !! ### Determinism
   !!
   !! Nothing here is reproducible between runs, by definition. Code whose
   !! results must replay identically from a seed must not call any of it.
   !! `format_iso8601` is the exception: it is a pure function of its
   !! argument, and it builds its text with `zfill` rather than internal I/O
   !! so that the bytes are identical on every compiler.
   use pic_types, only: default_int, int32, int64
   use pic_strings, only: to_string, zfill
   use pic_error, only: error_t, error_raise, ERROR_GENERIC

   implicit none
   private

   public :: datetime_t
   public :: monotonic_ms
   public :: monotonic_us
   public :: now_local
   public :: now_utc
   public :: unix_time_ms
   public :: format_iso8601
   public :: PIC_CLOCK_NO_CLOCK

   integer(int64), parameter :: PIC_CLOCK_NO_CLOCK = -1_int64
      !! Returned by `monotonic_ms` and `monotonic_us` on a processor that has
      !! no clock. Zero is deliberately not used: it is a valid reading, and
      !! `pic_timer` already returns zero elapsed time for the same condition,
      !! which a caller cannot distinguish from "no time passed". A negative
      !! value can never be a real monotonic reading.

   integer(default_int), parameter :: DATE_AND_TIME_VALUES = 8_default_int
      !! Length of the VALUES array `date_and_time` fills: year, month, day,
      !! minutes from UTC, hour, minute, second, millisecond (F2018 16.9.59).

   integer(int64), parameter :: MS_PER_SECOND = 1000_int64
   integer(int64), parameter :: US_PER_SECOND = 1000000_int64
   integer(int64), parameter :: MS_PER_MINUTE = 60000_int64
   integer(int64), parameter :: MS_PER_HOUR = 3600000_int64
   integer(int64), parameter :: MS_PER_DAY = 86400000_int64

   !! Offset between the civil-calendar epoch used by `days_from_civil`
   !! (0000-03-01) and the Unix epoch (1970-01-01), in days.
   integer(int64), parameter :: DAYS_EPOCH_SHIFT = 719468_int64

   type :: datetime_t
      !! A broken-down calendar date and time.
      !!
      !! `utc_offset_min` is the number of minutes local time is **ahead** of
      !! UTC, so it is negative in the Americas. A value of zero means the
      !! instant is expressed in UTC.
      integer(default_int) :: year = 1970
      integer(default_int) :: month = 1
      integer(default_int) :: day = 1
      integer(default_int) :: hour = 0
      integer(default_int) :: minute = 0
      integer(default_int) :: second = 0
      integer(default_int) :: millisecond = 0
      integer(default_int) :: utc_offset_min = 0
   end type datetime_t

contains

   function monotonic_ms() result(t)
      !! Milliseconds from an unspecified origin, or `PIC_CLOCK_NO_CLOCK`.
      !!
      !! `system_clock` is called with `integer(int64)` arguments, which on
      !! every supported compiler selects a finer tick than the default
      !! integer kind does -- often nanoseconds, where the default kind is
      !! only milliseconds or worse.
      integer(int64) :: t

      t = monotonic_scaled(MS_PER_SECOND)
   end function monotonic_ms

   function monotonic_us() result(t)
      !! Microseconds from an unspecified origin, or `PIC_CLOCK_NO_CLOCK`.
      integer(int64) :: t

      t = monotonic_scaled(US_PER_SECOND)
   end function monotonic_us

   function monotonic_scaled(units_per_second) result(t)
      !! Shared body of `monotonic_ms` and `monotonic_us`.
      integer(int64), intent(in) :: units_per_second
      integer(int64) :: t

      integer(int64) :: count, rate

      call system_clock(count, rate)

      ! F2018 16.9.180: a processor with no clock reports a zero count rate.
      if (rate <= 0_int64) then
         t = PIC_CLOCK_NO_CLOCK
         return
      end if

      ! Split rather than `count*units_per_second/rate`: with a nanosecond
      ! tick the product overflows int64 after a few seconds of uptime. The
      ! quotient and remainder together lose nothing.
      t = (count/rate)*units_per_second + (mod(count, rate)*units_per_second)/rate
   end function monotonic_scaled

   subroutine now_local(dt, err)
      !! The current local date and time.
      !!
      !! `ERROR_GENERIC` when the processor cannot supply the date and time;
      !! F2018 16.9.59 has `date_and_time` return `-huge(0)` for any value it
      !! does not have, and a wrong timestamp is worse than a reported failure.
      type(datetime_t), intent(out) :: dt
      type(error_t), intent(inout), optional :: err

      ! Default integer kind, as F2018 16.9.59 requires for this argument;
      ! this is the one place in PIC where a bare `integer` is correct.
      integer :: values(DATE_AND_TIME_VALUES)

      call date_and_time(values=values)

      if (any(values(1:3) == -huge(0)) .or. any(values(5:8) == -huge(0))) then
         call error_raise(err, ERROR_GENERIC, &
                          "pic_clock: the processor does not supply a date and time.")
         return
      end if

      dt%year = int(values(1), default_int)
      dt%month = int(values(2), default_int)
      dt%day = int(values(3), default_int)
      dt%hour = int(values(5), default_int)
      dt%minute = int(values(6), default_int)
      dt%second = int(values(7), default_int)
      dt%millisecond = int(values(8), default_int)

      ! The UTC offset is the field most often unavailable; local time is
      ! still valid without it, so it is reported as zero and only `now_utc`
      ! treats its absence as an error.
      if (values(4) == -huge(0)) then
         dt%utc_offset_min = 0
      else
         dt%utc_offset_min = int(values(4), default_int)
      end if
   end subroutine now_local

   subroutine now_utc(dt, err)
      !! The current date and time in UTC, with `utc_offset_min` zero.
      !!
      !! `ERROR_GENERIC` when the processor cannot supply its offset from UTC,
      !! rather than silently returning local time labelled as UTC.
      type(datetime_t), intent(out) :: dt
      type(error_t), intent(inout), optional :: err

      ! Default integer kind, as F2018 16.9.59 requires for this argument;
      ! this is the one place in PIC where a bare `integer` is correct.
      integer :: values(DATE_AND_TIME_VALUES)
      type(datetime_t) :: local
      integer(int64) :: utc_ms

      call date_and_time(values=values)
      if (values(4) == -huge(0)) then
         call error_raise(err, ERROR_GENERIC, &
                          "pic_clock: the processor does not supply its offset from UTC.")
         return
      end if

      call now_local(local, err)
      if (present(err)) then
         if (err%has_error()) return
      end if

      utc_ms = unix_time_ms(local)
      dt = datetime_from_unix_ms(utc_ms)
   end subroutine now_utc

   pure function unix_time_ms(dt) result(ms)
      !! Milliseconds since 1970-01-01T00:00:00Z.
      !!
      !! Integer arithmetic throughout, via the civil-calendar conversion
      !! below, so the result is exact and identical on every compiler. The
      !! `utc_offset_min` field is subtracted, so a local time and the same
      !! instant expressed in UTC give the same answer.
      type(datetime_t), intent(in) :: dt
      integer(int64) :: ms

      integer(int64) :: days

      days = days_from_civil(int(dt%year, int64), int(dt%month, int64), int(dt%day, int64))
      ms = days*MS_PER_DAY &
           + int(dt%hour, int64)*MS_PER_HOUR &
           + int(dt%minute, int64)*MS_PER_MINUTE &
           + int(dt%second, int64)*MS_PER_SECOND &
           + int(dt%millisecond, int64) &
           - int(dt%utc_offset_min, int64)*MS_PER_MINUTE
   end function unix_time_ms

   pure function datetime_from_unix_ms(ms) result(dt)
      !! Inverse of `unix_time_ms`, producing a UTC `datetime_t`.
      integer(int64), intent(in) :: ms
      type(datetime_t) :: dt

      integer(int64) :: days, rem, y, m, d

      ! Floored division, so that instants before the epoch land on the right
      ! day rather than being truncated towards it.
      days = ms/MS_PER_DAY
      rem = mod(ms, MS_PER_DAY)
      if (rem < 0_int64) then
         days = days - 1_int64
         rem = rem + MS_PER_DAY
      end if

      call civil_from_days(days, y, m, d)

      dt%year = int(y, default_int)
      dt%month = int(m, default_int)
      dt%day = int(d, default_int)
      dt%hour = int(rem/MS_PER_HOUR, default_int)
      rem = mod(rem, MS_PER_HOUR)
      dt%minute = int(rem/MS_PER_MINUTE, default_int)
      rem = mod(rem, MS_PER_MINUTE)
      dt%second = int(rem/MS_PER_SECOND, default_int)
      dt%millisecond = int(mod(rem, MS_PER_SECOND), default_int)
      dt%utc_offset_min = 0
   end function datetime_from_unix_ms

   pure function days_from_civil(y_in, m, d) result(days)
      !! Days from 1970-01-01 to the given proleptic Gregorian date.
      !!
      !! Howard Hinnant's `days_from_civil`, which is exact for every date in
      !! the int64 range and uses only integer operations. The year is shifted
      !! so that the era begins on 1 March, which is what removes the leap-day
      !! special case from the month arithmetic.
      integer(int64), intent(in) :: y_in
      integer(int64), intent(in) :: m
      integer(int64), intent(in) :: d
      integer(int64) :: days

      integer(int64) :: y, era, yoe, doy, doe

      y = y_in
      if (m <= 2_int64) y = y - 1_int64

      if (y >= 0_int64) then
         era = y/400_int64
      else
         era = (y - 399_int64)/400_int64
      end if

      yoe = y - era*400_int64                                  ! [0, 399]
      if (m > 2_int64) then
         doy = (153_int64*(m - 3_int64) + 2_int64)/5_int64 + d - 1_int64
      else
         doy = (153_int64*(m + 9_int64) + 2_int64)/5_int64 + d - 1_int64
      end if
      doe = yoe*365_int64 + yoe/4_int64 - yoe/100_int64 + doy  ! [0, 146096]

      days = era*146097_int64 + doe - DAYS_EPOCH_SHIFT
   end function days_from_civil

   pure subroutine civil_from_days(days, y, m, d)
      !! Inverse of `days_from_civil`.
      integer(int64), intent(in) :: days
      integer(int64), intent(out) :: y
      integer(int64), intent(out) :: m
      integer(int64), intent(out) :: d

      integer(int64) :: z, era, doe, yoe, doy, mp

      z = days + DAYS_EPOCH_SHIFT
      if (z >= 0_int64) then
         era = z/146097_int64
      else
         era = (z - 146096_int64)/146097_int64
      end if

      doe = z - era*146097_int64                                       ! [0, 146096]
      yoe = (doe - doe/1460_int64 + doe/36524_int64 - doe/146096_int64)/365_int64
      y = yoe + era*400_int64
      doy = doe - (365_int64*yoe + yoe/4_int64 - yoe/100_int64)         ! [0, 365]
      mp = (5_int64*doy + 2_int64)/153_int64                            ! [0, 11]
      d = doy - (153_int64*mp + 2_int64)/5_int64 + 1_int64              ! [1, 31]

      if (mp < 10_int64) then
         m = mp + 3_int64
      else
         m = mp - 9_int64
      end if
      if (m <= 2_int64) y = y + 1_int64
   end subroutine civil_from_days

   pure function format_iso8601(dt) result(text)
      !! ISO 8601, for example `2026-09-14T09:46:00.123Z`.
      !!
      !! A zero `utc_offset_min` is rendered as `Z`; any other offset is
      !! rendered as `+HH:MM` or `-HH:MM`, so the text always identifies the
      !! instant rather than quietly presenting local time as UTC.
      !!
      !! Built with `zfill` rather than an internal `write`: the `I0.N` edit
      !! descriptor and list-directed output are processor dependent, and this
      !! text is compared byte for byte by tests and golden files.
      type(datetime_t), intent(in) :: dt
      character(len=:), allocatable :: text

      text = pad(dt%year, 4)//"-"//pad(dt%month, 2)//"-"//pad(dt%day, 2)//"T"// &
             pad(dt%hour, 2)//":"//pad(dt%minute, 2)//":"//pad(dt%second, 2)//"."// &
             pad(dt%millisecond, 3)//offset_text(dt%utc_offset_min)
   end function format_iso8601

   pure function offset_text(offset_min) result(text)
      !! The trailing zone designator of an ISO 8601 timestamp.
      integer(default_int), intent(in) :: offset_min
      character(len=:), allocatable :: text

      integer(default_int) :: magnitude

      if (offset_min == 0) then
         text = "Z"
         return
      end if

      magnitude = abs(offset_min)
      if (offset_min > 0) then
         text = "+"
      else
         text = "-"
      end if
      text = text//pad(magnitude/60_default_int, 2)//":"// &
             pad(mod(magnitude, 60_default_int), 2)
   end function offset_text

   pure function pad(value, width) result(text)
      !! `value` in decimal, zero filled on the left to `width` characters.
      !! A value too wide to fit is not truncated; the field simply grows.
      integer(default_int), intent(in) :: value
      integer, intent(in) :: width
      character(len=:), allocatable :: text

      text = zfill(to_string(value), width)
   end function pad

end module pic_clock
