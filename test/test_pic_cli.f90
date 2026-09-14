module test_pic_cli
   !! Tests for pic_cli.
   !!
   !! Everything goes through `parse_args`, which is the real implementation.
   !! No test needs a real command line, which is the point of splitting
   !! `parse` off as a thin collector.
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use pic_types, only: default_int, int32, int64, dp
   use pic_error, only: error_t, ERROR_PARSE, ERROR_VALIDATION
   use pic_string_type, only: string_type, assignment(=), char
   use pic_cli, only: cli_t
   implicit none
   private

   public :: collect_pic_cli_tests

contains

   subroutine collect_pic_cli_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("long_space_and_equals", test_long_space_and_equals), &
                  new_unittest("short_options", test_short_options), &
                  new_unittest("flags", test_flags), &
                  new_unittest("positionals", test_positionals), &
                  new_unittest("double_dash", test_double_dash), &
                  new_unittest("help_is_reserved", test_help_is_reserved), &
                  new_unittest("defaults", test_defaults), &
                  new_unittest("unknown_option", test_unknown_option), &
                  new_unittest("missing_value", test_missing_value), &
                  new_unittest("missing_required", test_missing_required), &
                  new_unittest("strict_conversion", test_strict_conversion), &
                  new_unittest("typed_get", test_typed_get), &
                  new_unittest("logical_spellings", test_logical_spellings), &
                  new_unittest("repeated_last_wins", test_repeated_last_wins), &
                  new_unittest("negative_numbers", test_negative_numbers), &
                  new_unittest("reparse_resets", test_reparse_resets), &
                  new_unittest("help_text_is_stable", test_help_text_is_stable), &
                  new_unittest("many_options_grow", test_many_options_grow) &
                  ]
   end subroutine collect_pic_cli_tests

   ! ---- helpers -------------------------------------------------------------

   subroutine make_args(words, args)
      !! Build an argument array from a blank-separated line, so the tests read
      !! like the command lines they stand for.
      character(len=*), intent(in) :: words
      type(string_type), allocatable, intent(out) :: args(:)

      integer(default_int) :: i, n, start
      character(len=:), allocatable :: padded

      padded = trim(words)//" "
      n = 0_default_int
      start = 0_default_int
      do i = 1_default_int, len(padded, kind=default_int)
         if (padded(i:i) /= " ") then
            if (start == 0_default_int) start = i
         else if (start /= 0_default_int) then
            n = n + 1_default_int
            start = 0_default_int
         end if
      end do

      allocate (args(n))
      n = 0_default_int
      start = 0_default_int
      do i = 1_default_int, len(padded, kind=default_int)
         if (padded(i:i) /= " ") then
            if (start == 0_default_int) start = i
         else if (start /= 0_default_int) then
            n = n + 1_default_int
            args(n) = padded(start:i - 1_default_int)
            start = 0_default_int
         end if
      end do
   end subroutine make_args

   subroutine sample_cli(cli)
      !! The fairport command line from the design document.
      type(cli_t), intent(out) :: cli

      call cli%set_program("fairport", "Deterministic airport operations simulator")
      call cli%add_positional("scenario", "Scenario script to run", required=.true.)
      call cli%add_option("seed", "Master RNG seed", short="s", default="0")
      call cli%add_option("speed", "Sim speed multiplier or 'instant'", default="1")
      call cli%add_flag("hash", "Print the event-log hash and exit")
   end subroutine sample_cli

   ! ---- grammar -------------------------------------------------------------

   subroutine test_long_space_and_equals(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(int64) :: seed

      call sample_cli(cli)
      call make_args("run.txt --seed 42", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "--name value must parse")
      if (allocated(error)) return
      call cli%get("seed", seed, err)
      call check(error, seed == 42_int64, "--name value must reach get")
      if (allocated(error)) return

      call sample_cli(cli)
      call make_args("run.txt --seed=42", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "--name=value must parse")
      if (allocated(error)) return
      call cli%get("seed", seed, err)
      call check(error, seed == 42_int64, "--name=value must reach get")
      if (allocated(error)) return

      ! a value containing an equals sign survives, because only the first
      ! equals separates
      call sample_cli(cli)
      call make_args("run.txt --speed=a=b", args)
      call cli%parse_args(args, err)
      block
         character(len=:), allocatable :: speed
         call cli%get("speed", speed, err)
         call check(error, speed == "a=b", "only the first equals separates")
      end block
   end subroutine test_long_space_and_equals

   subroutine test_short_options(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(int64) :: seed

      call sample_cli(cli)
      call make_args("run.txt -s 7", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "-s value must parse")
      if (allocated(error)) return
      call cli%get("seed", seed, err)
      call check(error, seed == 7_int64, "-s value must reach get")
      if (allocated(error)) return
      call check(error, cli%is_set("seed"), "is_set must be true after -s")
      if (allocated(error)) return
      ! an option with no short name is not reachable by its first letter
      call sample_cli(cli)
      call make_args("run.txt -p 3", args)
      call cli%parse_args(args, err)
      call check(error, err%has_error(), "a short name that was never declared must fail")
   end subroutine test_short_options

   subroutine test_flags(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      logical :: hash

      call sample_cli(cli)
      call make_args("run.txt", args)
      call cli%parse_args(args, err)
      call check(error,.not. cli%is_set("hash"), "an absent flag is not set")
      if (allocated(error)) return
      call cli%get("hash", hash, err)
      call check(error,.not. hash, "an absent flag reads false")
      if (allocated(error)) return

      call sample_cli(cli)
      call make_args("run.txt --hash", args)
      call cli%parse_args(args, err)
      call check(error, cli%is_set("hash"), "a present flag is set")
      if (allocated(error)) return
      call cli%get("hash", hash, err)
      call check(error, hash, "a present flag reads true")
      if (allocated(error)) return

      ! a flag does not swallow the next word
      call sample_cli(cli)
      call make_args("--hash run.txt", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "a flag before a positional must parse")
      if (allocated(error)) return
      block
         character(len=:), allocatable :: scenario
         call cli%get("scenario", scenario, err)
         call check(error, scenario == "run.txt", "a flag must not consume the next word")
      end block
      if (allocated(error)) return

      ! giving a flag a value is an error rather than a silent drop
      call sample_cli(cli)
      call make_args("run.txt --hash=yes", args)
      call cli%parse_args(args, err)
      call check(error, err%has_error(), "a flag given a value is an error")
   end subroutine test_flags

   subroutine test_positionals(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      character(len=:), allocatable :: first, second

      call cli%set_program("two", "Takes two")
      call cli%add_positional("input", "Input file", required=.true.)
      call cli%add_positional("output", "Output file", required=.false., default="out.bin")
      call cli%add_flag("verbose", "Chatter", short="v")

      call make_args("a.txt b.txt", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "two positionals must parse")
      if (allocated(error)) return
      call cli%get("input", first, err)
      call cli%get("output", second, err)
      call check(error, first == "a.txt" .and. second == "b.txt", &
                 "positionals fill in declaration order")
      if (allocated(error)) return

      ! interleaved with options, order among positionals is preserved
      call make_args("-v a.txt b.txt", args)
      call cli%parse_args(args, err)
      call cli%get("input", first, err)
      call cli%get("output", second, err)
      call check(error, first == "a.txt" .and. second == "b.txt", &
                 "an option before the positionals does not disturb their order")
      if (allocated(error)) return

      ! one too many
      call make_args("a.txt b.txt c.txt", args)
      call cli%parse_args(args, err)
      call check(error, err%has_error(), "an extra positional is an error")
   end subroutine test_positionals

   subroutine test_double_dash(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      character(len=:), allocatable :: scenario

      call sample_cli(cli)
      call make_args("-- --seed", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "-- must end option parsing")
      if (allocated(error)) return
      call cli%get("scenario", scenario, err)
      call check(error, scenario == "--seed", &
                 "after -- an option-looking word is a positional")
      if (allocated(error)) return
      call check(error,.not. cli%is_set("seed"), "and the option itself was not set")
   end subroutine test_double_dash

   subroutine test_help_is_reserved(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)

      ! --help alone, with the required positional missing, must still work:
      ! that is exactly when a user asks for help
      call sample_cli(cli)
      call make_args("--help", args)
      call cli%parse_args(args, err)
      call check(error, cli%help_requested(), "--help must set help_requested")
      if (allocated(error)) return
      call check(error,.not. err%has_error(), &
                 "--help must not fail on a missing required argument")
      if (allocated(error)) return

      call sample_cli(cli)
      call make_args("-h", args)
      call cli%parse_args(args, err)
      call check(error, cli%help_requested(), "-h must set help_requested")
      if (allocated(error)) return

      call sample_cli(cli)
      call make_args("run.txt", args)
      call cli%parse_args(args, err)
      call check(error,.not. cli%help_requested(), &
                 "help_requested is false when help was not asked for")
   end subroutine test_help_is_reserved

   subroutine test_defaults(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(int64) :: seed
      character(len=:), allocatable :: speed

      call sample_cli(cli)
      call make_args("run.txt", args)
      call cli%parse_args(args, err)
      call cli%get("seed", seed, err)
      call check(error, seed == 0_int64, "an absent option falls back to its default")
      if (allocated(error)) return
      call cli%get("speed", speed, err)
      call check(error, speed == "1", "and so does a string option")
      if (allocated(error)) return
      call check(error,.not. cli%is_set("seed"), "a defaulted option is not 'set'")
   end subroutine test_defaults

   ! ---- errors --------------------------------------------------------------

   subroutine test_unknown_option(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)

      call sample_cli(cli)
      call make_args("run.txt --nope 1", args)
      call cli%parse_args(args, err)
      call check(error, err%has_error(), "an unknown option is an error")
      if (allocated(error)) return
      call check(error, err%code == ERROR_PARSE, "an unknown option is ERROR_PARSE")
      if (allocated(error)) return
      call check(error, index(err%message, "nope") > 0, &
                 "the message must name the offending option")
   end subroutine test_unknown_option

   subroutine test_missing_value(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)

      call sample_cli(cli)
      call make_args("run.txt --seed", args)
      call cli%parse_args(args, err)
      call check(error, err%has_error(), "an option without its value is an error")
      if (allocated(error)) return
      call check(error, err%code == ERROR_PARSE, "a missing value is ERROR_PARSE")
      if (allocated(error)) return
      call check(error, index(err%message, "seed") > 0, "the message must name the option")
      if (allocated(error)) return

      call sample_cli(cli)
      call make_args("run.txt -s", args)
      call cli%parse_args(args, err)
      call check(error, err%has_error(), "a short option without its value is an error")
   end subroutine test_missing_value

   subroutine test_missing_required(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)

      call sample_cli(cli)
      call make_args("--seed 1", args)
      call cli%parse_args(args, err)
      call check(error, err%has_error(), "a missing required positional is an error")
      if (allocated(error)) return
      call check(error, err%code == ERROR_VALIDATION, &
                 "a missing required positional is ERROR_VALIDATION")
      if (allocated(error)) return
      call check(error, index(err%message, "scenario") > 0, &
                 "the message must name the missing argument")
   end subroutine test_missing_required

   subroutine test_strict_conversion(error)
      !! The design's example: --seed 42x is an error, not 42.
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(int64) :: seed

      call sample_cli(cli)
      call make_args("run.txt --seed 42x", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "parsing succeeds; the conversion is what fails")
      if (allocated(error)) return
      call cli%get("seed", seed, err)
      call check(error, err%has_error(), "42x must not convert")
      if (allocated(error)) return
      call check(error, err%code == ERROR_PARSE, "a bad conversion is ERROR_PARSE")
      if (allocated(error)) return
      call check(error, seed == 0_int64, "and the value is left at zero")
      if (allocated(error)) return

      ! reading an undeclared name is a usage error, not a silent empty
      call err%clear()
      call cli%get("nothere", seed, err)
      call check(error, err%has_error(), "reading an undeclared name is an error")
      if (allocated(error)) return
      call check(error, err%code == ERROR_VALIDATION, "and it is ERROR_VALIDATION")
   end subroutine test_strict_conversion

   subroutine test_typed_get(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(int32) :: i32
      integer(int64) :: i64
      real(dp) :: rdp
      type(string_type) :: text
      character(len=:), allocatable :: chars

      call cli%set_program("typed", "Every accepted type")
      call cli%add_option("count", "An int32", default="0")
      call cli%add_option("big", "An int64", default="0")
      call cli%add_option("scale", "A real", default="0.0")
      call cli%add_option("label", "A string", default="none")

      call make_args("--count -12 --big 9000000000 --scale 2.5e3 --label hello", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "the typed line must parse")
      if (allocated(error)) return

      call cli%get("count", i32, err)
      call check(error, i32 == -12_int32, "int32 round trip")
      if (allocated(error)) return
      call cli%get("big", i64, err)
      call check(error, i64 == 9000000000_int64, &
                 "int64 round trip, beyond the range of int32")
      if (allocated(error)) return
      call cli%get("scale", rdp, err)
      call check(error, abs(rdp - 2500.0_dp) < 1.0e-9_dp, "real round trip")
      if (allocated(error)) return
      call cli%get("label", text, err)
      call check(error, char(text) == "hello", "string_type round trip")
      if (allocated(error)) return
      call cli%get("label", chars, err)
      call check(error, chars == "hello", "character round trip")
   end subroutine test_typed_get

   subroutine test_logical_spellings(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      logical :: value

      call cli%set_program("bool", "Logical spellings")
      call cli%add_option("on", "A logical", default="false")

      block
         character(len=5), parameter :: TRUES(4) = ["true ", "YES  ", "On   ", "1    "]
         character(len=5), parameter :: FALSES(4) = ["false", "no   ", "OFF  ", "0    "]
         integer(default_int) :: i
         do i = 1_default_int, 4_default_int
            call make_args("--on "//trim(TRUES(i)), args)
            call cli%parse_args(args, err)
            call cli%get("on", value, err)
            call check(error, value, "a true spelling must read true: "//trim(TRUES(i)))
            if (allocated(error)) return
            call make_args("--on "//trim(FALSES(i)), args)
            call cli%parse_args(args, err)
            call cli%get("on", value, err)
            call check(error,.not. value, "a false spelling must read false: "//trim(FALSES(i)))
            if (allocated(error)) return
         end do
      end block

      call make_args("--on maybe", args)
      call cli%parse_args(args, err)
      call err%clear()
      call cli%get("on", value, err)
      call check(error, err%has_error(), "an unrecognised spelling is an error")
   end subroutine test_logical_spellings

   subroutine test_repeated_last_wins(error)
      !! Design question 9.4: a repeat is last-wins, and countable.
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(int64) :: seed

      call sample_cli(cli)
      call make_args("run.txt --seed 1 --seed 2 --seed 3", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "a repeated option is not an error")
      if (allocated(error)) return
      call cli%get("seed", seed, err)
      call check(error, seed == 3_int64, "the last occurrence wins")
      if (allocated(error)) return
      call check(error, cli%occurrences("seed") == 3_default_int, &
                 "occurrences must report all three, so a caller can object")
      if (allocated(error)) return
      call check(error, cli%occurrences("speed") == 0_default_int, &
                 "an option never given has no occurrences")
      if (allocated(error)) return
      call check(error, cli%occurrences("nothere") == 0_default_int, &
                 "an undeclared name has no occurrences")
   end subroutine test_repeated_last_wins

   subroutine test_negative_numbers(error)
      !! A negative number must reach a positional rather than being mistaken
      !! for a short option.
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(int32) :: value
      character(len=:), allocatable :: where

      call cli%set_program("neg", "Negative values")
      call cli%add_positional("offset", "How far", required=.true.)
      call cli%add_option("delta", "A signed amount", short="d", default="0")

      call make_args("-5 --delta -3", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "a negative positional must parse")
      if (allocated(error)) return
      call cli%get("offset", where, err)
      call check(error, where == "-5", "a negative number is a positional, not an option")
      if (allocated(error)) return
      call cli%get("delta", value, err)
      call check(error, value == -3_int32, "a negative option value survives")
   end subroutine test_negative_numbers

   subroutine test_reparse_resets(error)
      !! Parsing twice must not accumulate state from the first pass.
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)

      call sample_cli(cli)
      call make_args("run.txt --hash --seed 5", args)
      call cli%parse_args(args, err)
      call check(error, cli%is_set("hash"), "first parse sets the flag")
      if (allocated(error)) return

      call make_args("run.txt", args)
      call cli%parse_args(args, err)
      call check(error,.not. cli%is_set("hash"), "second parse clears the flag")
      if (allocated(error)) return
      call check(error, cli%occurrences("seed") == 0_default_int, &
                 "second parse clears the occurrence count")
      if (allocated(error)) return
      call check(error,.not. cli%help_requested(), "second parse clears the help request")
   end subroutine test_reparse_resets

   subroutine test_help_text_is_stable(error)
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      character(len=:), allocatable :: first, second

      call sample_cli(cli)
      first = cli%help_text()
      second = cli%help_text()

      call check(error, first == second, "help_text must be a pure function of the declaration")
      if (allocated(error)) return
      call check(error, index(first, "fairport") > 0, "the program name appears")
      if (allocated(error)) return
      call check(error, index(first, "Deterministic airport") > 0, "the summary appears")
      if (allocated(error)) return
      call check(error, index(first, "<scenario>") > 0, &
                 "a required positional is shown in angle brackets")
      if (allocated(error)) return
      call check(error, index(first, "-s, --seed") > 0, "a short name is shown with its long one")
      if (allocated(error)) return
      call check(error, index(first, "--hash") > 0, "a flag appears")
      if (allocated(error)) return
      call check(error, index(first, "-h, --help") > 0, "help documents itself")
      if (allocated(error)) return
      call check(error, index(first, "Master RNG seed") > 0, "descriptions appear")
   end subroutine test_help_text_is_stable

   subroutine test_many_options_grow(error)
      !! More options than the initial capacity, to exercise the doubling.
      type(error_type), allocatable, intent(out) :: error
      type(cli_t) :: cli
      type(error_t) :: err
      type(string_type), allocatable :: args(:)
      integer(default_int) :: i
      integer(int32) :: value
      character(len=2) :: name

      call cli%set_program("many", "Many options")
      do i = 1_default_int, 40_default_int
         write (name, "(a1,i1)") "o", mod(i, 10_default_int)
         call cli%add_option(trim(name)//achar(iachar("a") + int(i/10_default_int)), &
                             "Option", default="0")
      end do

      call check(error, cli%has("o1a"), "the first option survived the growth")
      if (allocated(error)) return
      call check(error, cli%has("o9d"), "and so did a late one")
      if (allocated(error)) return

      call make_args("--o1a 11 --o9d 99", args)
      call cli%parse_args(args, err)
      call check(error,.not. err%has_error(), "options past the initial capacity parse")
      if (allocated(error)) return
      call cli%get("o1a", value, err)
      call check(error, value == 11_int32, "an early option keeps its value after growth")
      if (allocated(error)) return
      call cli%get("o9d", value, err)
      call check(error, value == 99_int32, "and a late one has its own")
   end subroutine test_many_options_grow

end module test_pic_cli
