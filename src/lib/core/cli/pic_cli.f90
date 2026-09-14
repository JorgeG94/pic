! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! Command line parsing: declare the interface, then read it.
module pic_cli
   !! A declarative command line parser.
   !!
   !! Declare what the program accepts, parse once, then read values back by
   !! name. Nothing is printed and nothing is stopped: every failure comes
   !! back as an `error_t`, and even `--help` is reported rather than acted
   !! on, because a library has no business deciding that a process should
   !! end or where its text should go. See "The library never prints" below.
   !!
   !! ### Quick start
   !!
   !! ```fortran
   !! type(cli_t) :: cli
   !! type(error_t) :: err
   !! integer(int64) :: seed
   !!
   !! call cli%set_program("fairport", "Deterministic airport simulator")
   !! call cli%add_positional("scenario", "Scenario script to run", required=.true.)
   !! call cli%add_option("seed", "Master RNG seed", short="s", default="0")
   !! call cli%add_flag("hash", "Print the event-log hash and exit")
   !! call cli%parse(err)
   !! if (.haserror. err) call err%fatal()
   !!
   !! if (cli%help_requested()) then
   !!    write (*, "(a)") cli%help_text()
   !!    stop 0
   !! end if
   !!
   !! call cli%get("seed", seed, err)
   !! if (cli%is_set("hash")) ...
   !! ```
   !!
   !! ### Grammar
   !!
   !! * `--name value` and `--name=value`
   !! * `-s value` for an option given a short name
   !! * `--` ends option parsing; everything after it is a positional, even if
   !!   it starts with a dash
   !! * `-h` and `--help` are reserved: they set `help_requested()` and are
   !!   never passed through as options
   !!
   !! A flag takes no value. `--flag=value` is an error rather than a silently
   !! ignored value.
   !!
   !! Out of scope in this version: subcommands, grouped short flags (`-abc`),
   !! environment variable fallbacks, and range validation.
   !!
   !! ### Repeated options
   !!
   !! The last occurrence wins, which is what a shell user expects when they
   !! edit a long command line by appending to it. The earlier values are not
   !! lost silently, though: `occurrences(name)` reports how many times an
   !! option was given, so a program that wants to reject a repeat, or warn
   !! about one, can.
   !!
   !! ### The library never prints
   !!
   !! `help_text()` returns the text. `help_requested()` reports the request.
   !! Neither writes to a unit, and nothing here calls `stop`. A library that
   !! prints has decided that the program has a terminal, that the text goes
   !! to `output_unit` rather than to a log or a socket, and that English is
   !! wanted. A library that stops has decided that the program has nothing
   !! left to clean up. Those are the caller's decisions.
   !!
   !! ### Testing
   !!
   !! `parse_args` is the real implementation and takes the arguments as an
   !! array. `parse` only collects `get_command_argument` and forwards to it.
   !! Tests therefore never need a real command line, and every case below --
   !! including the ones that are errors -- is exercised without a shell.
   use pic_types, only: default_int, int32, int64, sp, dp
   use pic_error, only: error_t, ERROR_PARSE, ERROR_VALIDATION
   use pic_string_type, only: string_type, char, assignment(=), operator(==)
   use pic_tokenizer, only: parse_int, parse_real
   implicit none
   private

   public :: cli_t

   integer(default_int), parameter :: KIND_OPTION = 1_default_int
      !! An entry that takes a value.
   integer(default_int), parameter :: KIND_FLAG = 2_default_int
      !! An entry that takes no value.
   integer(default_int), parameter :: KIND_POSITIONAL = 3_default_int
      !! An entry filled from the argument order rather than by name.

   integer(default_int), parameter :: INITIAL_CAPACITY = 8_default_int
      !! Entries reserved on the first registration; grown by doubling.

   type :: entry_t
      !! One declared option, flag or positional.
      private
      type(string_type) :: name
         !! Long name, without the leading dashes.
      type(string_type) :: short
         !! Single-character short name, or empty if there is none.
      type(string_type) :: help
         !! One-line description, for `help_text`.
      type(string_type) :: value
         !! Current value: the default until parsing overwrites it.
      integer(default_int) :: kind = KIND_OPTION
         !! One of the KIND_* codes.
      logical :: required = .false.
         !! Whether a missing positional is an error.
      integer(default_int) :: seen = 0_default_int
         !! How many times this entry appeared on the command line.
   end type entry_t

   type :: cli_t
      !! A command line interface: what is accepted, and what was given.
      !!
      !! Declare with `set_program`, `add_option`, `add_flag` and
      !! `add_positional`; fill with `parse` or `parse_args`; read with `get`,
      !! `is_set` and `occurrences`.
      private
      type(string_type) :: program_name
         !! Name used in the usage line.
      type(string_type) :: summary
         !! One-line description of the program.
      type(entry_t), allocatable :: entries(:)
         !! Declared entries, in registration order, so `help_text` is stable.
      integer(default_int) :: count = 0_default_int
         !! Entries in use; `entries` may be longer.
      logical :: wants_help = .false.
         !! Set when `-h` or `--help` was seen.
      logical :: parsed = .false.
         !! Set once a parse has completed, successfully or not.
   contains
      procedure :: set_program => cli_set_program
      procedure :: add_option => cli_add_option
      procedure :: add_flag => cli_add_flag
      procedure :: add_positional => cli_add_positional
      procedure :: parse => cli_parse
      procedure :: parse_args => cli_parse_args
      procedure :: help_requested => cli_help_requested
      procedure :: help_text => cli_help_text
      procedure :: is_set => cli_is_set
      procedure :: occurrences => cli_occurrences
      procedure :: has => cli_has
      generic :: get => get_string, get_char, get_int32, get_int64, &
         get_real_sp, get_real_dp, get_logical

      procedure, private :: get_string => cli_get_string
      procedure, private :: get_char => cli_get_char
      procedure, private :: get_int32 => cli_get_int32
      procedure, private :: get_int64 => cli_get_int64
      procedure, private :: get_real_sp => cli_get_real_sp
      procedure, private :: get_real_dp => cli_get_real_dp
      procedure, private :: get_logical => cli_get_logical
      procedure, private :: find => cli_find
      procedure, private :: reserve => cli_reserve
   end type cli_t

contains

   ! ---- declaration ---------------------------------------------------------

   subroutine cli_reserve(this)
      !! Make room for one more entry, doubling the store when it is full.
      class(cli_t), intent(inout) :: this

      type(entry_t), allocatable :: bigger(:)
      integer(default_int) :: capacity

      if (.not. allocated(this%entries)) then
         allocate (this%entries(INITIAL_CAPACITY))
         return
      end if
      capacity = size(this%entries, kind=default_int)
      if (this%count < capacity) return
      allocate (bigger(2_default_int*capacity))
      bigger(1:this%count) = this%entries(1:this%count)
      call move_alloc(bigger, this%entries)
   end subroutine cli_reserve

   subroutine cli_set_program(this, name, summary)
      !! Name and one-line description, used by `help_text`.
      class(cli_t), intent(inout) :: this
      character(len=*), intent(in) :: name
         !! Program name as it should appear in the usage line.
      character(len=*), intent(in) :: summary
         !! One-line description of what the program does.

      this%program_name = name
      this%summary = summary
   end subroutine cli_set_program

   subroutine cli_add_option(this, name, help, short, default)
      !! Declare an option that takes a value.
      class(cli_t), intent(inout) :: this
      character(len=*), intent(in) :: name
         !! Long name, given on the command line as `--name`.
      character(len=*), intent(in) :: help
         !! One-line description.
      character(len=*), intent(in), optional :: short
         !! Single-character short name, given as `-s`.
      character(len=*), intent(in), optional :: default
         !! Value `get` returns when the option is absent; empty if omitted.

      call this%reserve()
      this%count = this%count + 1_default_int
      this%entries(this%count)%name = name
      this%entries(this%count)%help = help
      this%entries(this%count)%kind = KIND_OPTION
      this%entries(this%count)%required = .false.
      this%entries(this%count)%seen = 0_default_int
      if (present(short)) then
         this%entries(this%count)%short = short
      else
         this%entries(this%count)%short = ""
      end if
      if (present(default)) then
         this%entries(this%count)%value = default
      else
         this%entries(this%count)%value = ""
      end if
   end subroutine cli_add_option

   subroutine cli_add_flag(this, name, help, short)
      !! Declare a flag, which takes no value.
      !!
      !! An absent flag reads back as `"false"`, a present one as `"true"`, so
      !! `get` into a `logical` works without a special case.
      class(cli_t), intent(inout) :: this
      character(len=*), intent(in) :: name
         !! Long name, given on the command line as `--name`.
      character(len=*), intent(in) :: help
         !! One-line description.
      character(len=*), intent(in), optional :: short
         !! Single-character short name.

      call this%reserve()
      this%count = this%count + 1_default_int
      this%entries(this%count)%name = name
      this%entries(this%count)%help = help
      this%entries(this%count)%kind = KIND_FLAG
      this%entries(this%count)%required = .false.
      this%entries(this%count)%seen = 0_default_int
      this%entries(this%count)%value = "false"
      if (present(short)) then
         this%entries(this%count)%short = short
      else
         this%entries(this%count)%short = ""
      end if
   end subroutine cli_add_flag

   subroutine cli_add_positional(this, name, help, required, default)
      !! Declare a positional argument, filled in registration order.
      class(cli_t), intent(inout) :: this
      character(len=*), intent(in) :: name
         !! Name used to read the value back, and in the usage line.
      character(len=*), intent(in) :: help
         !! One-line description.
      logical, intent(in), optional :: required
         !! Whether a parse without it is `ERROR_VALIDATION`; default false.
      character(len=*), intent(in), optional :: default
         !! Value `get` returns when the argument is absent.

      call this%reserve()
      this%count = this%count + 1_default_int
      this%entries(this%count)%name = name
      this%entries(this%count)%help = help
      this%entries(this%count)%short = ""
      this%entries(this%count)%kind = KIND_POSITIONAL
      this%entries(this%count)%seen = 0_default_int
      if (present(required)) then
         this%entries(this%count)%required = required
      else
         this%entries(this%count)%required = .false.
      end if
      if (present(default)) then
         this%entries(this%count)%value = default
      else
         this%entries(this%count)%value = ""
      end if
   end subroutine cli_add_positional

   ! ---- lookup --------------------------------------------------------------

   pure function cli_find(this, name) result(idx)
      !! Index of the entry with this long name, or 0.
      !!
      !! A linear walk over the registration array. A command line has tens of
      !! options, not thousands, and at that size a scan beats hashing while
      !! needing no map to allocate, copy or free.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name to look for, without dashes.
      integer(default_int) :: idx

      integer(default_int) :: i

      idx = 0_default_int
      do i = 1_default_int, this%count
         if (char(this%entries(i)%name) == name) then
            idx = i
            return
         end if
      end do
   end function cli_find

   pure function cli_find_short(this, short) result(idx)
      !! Index of the entry with this short name, or 0. Empty never matches.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: short
      integer(default_int) :: idx

      integer(default_int) :: i

      idx = 0_default_int
      if (len(short) == 0) return
      do i = 1_default_int, this%count
         if (len(char(this%entries(i)%short)) == 0) cycle
         if (char(this%entries(i)%short) == short) then
            idx = i
            return
         end if
      end do
   end function cli_find_short

   pure function cli_has(this, name) result(r)
      !! Whether `name` was declared at all.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name to look for.
      logical :: r

      r = this%find(name) > 0_default_int
   end function cli_has

   pure function cli_is_set(this, name) result(r)
      !! Whether `name` appeared on the command line.
      !!
      !! False for an undeclared name, and false for a declared one that fell
      !! back to its default.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name to look for.
      logical :: r

      integer(default_int) :: idx

      idx = this%find(name)
      r = idx > 0_default_int
      if (r) r = this%entries(idx)%seen > 0_default_int
   end function cli_is_set

   pure function cli_occurrences(this, name) result(n)
      !! How many times `name` appeared on the command line.
      !!
      !! `get` returns the last occurrence. This is how a program that wants
      !! to reject or warn about a repeat can find out about one.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name to look for.
      integer(default_int) :: n

      integer(default_int) :: idx

      n = 0_default_int
      idx = this%find(name)
      if (idx > 0_default_int) n = this%entries(idx)%seen
   end function cli_occurrences

   pure function cli_help_requested(this) result(r)
      !! Whether `-h` or `--help` was given.
      !!
      !! The caller decides what to do about it. See "The library never
      !! prints".
      class(cli_t), intent(in) :: this
      logical :: r

      r = this%wants_help
   end function cli_help_requested

   ! ---- parsing -------------------------------------------------------------

   subroutine cli_parse(this, err)
      !! Parse the real command line.
      !!
      !! Collects `get_command_argument` into an array and forwards to
      !! `parse_args`, which holds the whole of the logic.
      class(cli_t), intent(inout) :: this
      type(error_t), intent(inout), optional :: err
         !! Set on a parse or validation failure.

      type(string_type), allocatable :: args(:)
      integer(default_int) :: i, n
      integer :: arg_index, arg_len
         !! Bare default integer on purpose, and the one place in this module
         !! where that is right: F2018 16.9.82 defines NUMBER, LENGTH and
         !! STATUS of `get_command_argument` as default integer, so these must
         !! not follow `default_int`. Under `PIC_DEFAULT_INT8` they would
         !! otherwise become `int64` and the call would not compile.

      n = int(command_argument_count(), default_int)
      allocate (args(n))
      do i = 1_default_int, n
         arg_index = int(i)
         call get_command_argument(arg_index, length=arg_len)
         block
            character(len=arg_len) :: buffer
            call get_command_argument(arg_index, buffer)
            args(i) = buffer
         end block
      end do
      call this%parse_args(args, err)
   end subroutine cli_parse

   subroutine cli_parse_args(this, args, err)
      !! Parse an explicit argument list.
      !!
      !! This is the real implementation; `parse` is a thin collector over it.
      !! Taking the arguments as data rather than reading them from the
      !! process is what makes every path here testable.
      class(cli_t), intent(inout) :: this
      type(string_type), intent(in) :: args(:)
         !! Arguments, excluding the program name.
      type(error_t), intent(inout), optional :: err
         !! Set on a parse or validation failure.

      integer(default_int) :: i, n, idx, eq, positional
      logical :: options_done
      character(len=:), allocatable :: arg, name, value

      this%wants_help = .false.
      this%parsed = .true.
      do i = 1_default_int, this%count
         this%entries(i)%seen = 0_default_int
      end do

      n = size(args, kind=default_int)
      options_done = .false.
      positional = 0_default_int
      i = 1_default_int
      do while (i <= n)
         arg = char(args(i))

         if (.not. options_done .and. arg == "--") then
            options_done = .true.
            i = i + 1_default_int
            cycle
         end if

         if (.not. options_done .and. is_long(arg)) then
            eq = index(arg, "=")
            if (eq > 0) then
               name = arg(3:eq - 1)
               value = arg(eq + 1:)
            else
               name = arg(3:)
               value = ""
            end if

            if (name == "help") then
               this%wants_help = .true.
               i = i + 1_default_int
               cycle
            end if

            idx = this%find(name)
            if (idx == 0_default_int) then
               call fail(err, ERROR_PARSE, "pic_cli: unknown option --"//name)
               return
            end if
            if (this%entries(idx)%kind == KIND_FLAG) then
               if (eq > 0) then
                  call fail(err, ERROR_PARSE, "pic_cli: flag --"//name//" takes no value")
                  return
               end if
               this%entries(idx)%value = "true"
               this%entries(idx)%seen = this%entries(idx)%seen + 1_default_int
               i = i + 1_default_int
               cycle
            end if
            if (eq == 0) then
               if (i == n) then
                  call fail(err, ERROR_PARSE, "pic_cli: option --"//name//" needs a value")
                  return
               end if
               value = char(args(i + 1_default_int))
               i = i + 1_default_int
            end if
            this%entries(idx)%value = value
            this%entries(idx)%seen = this%entries(idx)%seen + 1_default_int
            i = i + 1_default_int
            cycle
         end if

         if (.not. options_done .and. is_short(arg)) then
            name = arg(2:2)
            if (name == "h") then
               this%wants_help = .true.
               i = i + 1_default_int
               cycle
            end if
            idx = cli_find_short(this, name)
            if (idx == 0_default_int) then
               call fail(err, ERROR_PARSE, "pic_cli: unknown option -"//name)
               return
            end if
            if (this%entries(idx)%kind == KIND_FLAG) then
               this%entries(idx)%value = "true"
               this%entries(idx)%seen = this%entries(idx)%seen + 1_default_int
               i = i + 1_default_int
               cycle
            end if
            if (i == n) then
               call fail(err, ERROR_PARSE, "pic_cli: option -"//name//" needs a value")
               return
            end if
            this%entries(idx)%value = char(args(i + 1_default_int))
            this%entries(idx)%seen = this%entries(idx)%seen + 1_default_int
            i = i + 2_default_int
            cycle
         end if

         ! anything else fills the next declared positional
         call next_positional(this, positional, idx)
         if (idx == 0_default_int) then
            call fail(err, ERROR_PARSE, "pic_cli: unexpected argument "//arg)
            return
         end if
         this%entries(idx)%value = arg
         this%entries(idx)%seen = this%entries(idx)%seen + 1_default_int
         i = i + 1_default_int
      end do

      ! A help request short-circuits validation: `--help` alone must work
      ! even though the required positionals are missing, which is exactly
      ! when a user reaches for it.
      if (this%wants_help) return

      do i = 1_default_int, this%count
         if (this%entries(i)%kind /= KIND_POSITIONAL) cycle
         if (.not. this%entries(i)%required) cycle
         if (this%entries(i)%seen > 0_default_int) cycle
         call fail(err, ERROR_VALIDATION, &
                   "pic_cli: missing required argument "//char(this%entries(i)%name))
         return
      end do
   end subroutine cli_parse_args

   pure function is_long(arg) result(r)
      !! Whether `arg` is a `--name` form. A bare `--` is not.
      character(len=*), intent(in) :: arg
      logical :: r

      r = .false.
      if (len(arg) <= 2) return
      r = arg(1:2) == "--"
   end function is_long

   pure function is_short(arg) result(r)
      !! Whether `arg` is a `-s` form. A bare `-` is not, and neither is a
      !! negative number, which must be able to reach a positional.
      character(len=*), intent(in) :: arg
      logical :: r

      r = .false.
      if (len(arg) /= 2) return
      if (arg(1:1) /= "-") return
      r = .not. (arg(2:2) >= "0" .and. arg(2:2) <= "9")
   end function is_short

   pure subroutine next_positional(this, used, idx)
      !! Index of the next unfilled positional, advancing `used`.
      !!
      !! A subroutine rather than a function because it advances the counter,
      !! and a pure function may not have an `intent(inout)` argument.
      class(cli_t), intent(in) :: this
      integer(default_int), intent(inout) :: used
         !! Positionals consumed so far; incremented when one is taken.
      integer(default_int), intent(out) :: idx
         !! Index of the positional taken, or 0 when they are all filled.

      integer(default_int) :: i, seen

      idx = 0_default_int
      seen = 0_default_int
      do i = 1_default_int, this%count
         if (this%entries(i)%kind /= KIND_POSITIONAL) cycle
         seen = seen + 1_default_int
         if (seen > used) then
            used = seen
            idx = i
            return
         end if
      end do
   end subroutine next_positional

   subroutine fail(err, code, message)
      !! Set `err` if it is present, and do nothing if it is not.
      type(error_t), intent(inout), optional :: err
      integer(default_int), intent(in) :: code
      character(len=*), intent(in) :: message

      if (present(err)) call err%set(code, message)
   end subroutine fail

   ! ---- reading values ------------------------------------------------------

   subroutine cli_get_string(this, name, value, err)
      !! Read a value as a `string_type`.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name of a declared entry.
      type(string_type), intent(out) :: value
         !! The last value given, or the declared default.
      type(error_t), intent(inout), optional :: err
         !! Set to `ERROR_VALIDATION` when `name` was never declared.

      integer(default_int) :: idx

      value = ""
      idx = this%find(name)
      if (idx == 0_default_int) then
         call fail(err, ERROR_VALIDATION, "pic_cli: no such option "//name)
         return
      end if
      value = this%entries(idx)%value
   end subroutine cli_get_string

   subroutine cli_get_char(this, name, value, err)
      !! Read a value into a deferred-length character.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name of a declared entry.
      character(len=:), allocatable, intent(out) :: value
         !! The last value given, or the declared default.
      type(error_t), intent(inout), optional :: err
         !! Set to `ERROR_VALIDATION` when `name` was never declared.

      type(string_type) :: text

      call this%get_string(name, text, err)
      value = char(text)
   end subroutine cli_get_char

   subroutine cli_get_int32(this, name, value, err)
      !! Read a value as `integer(int32)`, strictly.
      !!
      !! Conversion goes through `pic_tokenizer`'s `parse_int`, so `42x` is
      !! `ERROR_PARSE` rather than 42.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name of a declared entry.
      integer(int32), intent(out) :: value
         !! Parsed value; zero when the conversion failed.
      type(error_t), intent(inout), optional :: err
         !! Set on an undeclared name or an unparsable value.

      type(string_type) :: text
      type(error_t) :: local

      value = 0_int32
      call this%get_string(name, text, err)
      if (present(err)) then
         if (err%has_error()) return
      else
         if (this%find(name) == 0_default_int) return
      end if
      call parse_int(char(text), value, local)
      if (local%has_error()) then
         call fail(err, ERROR_PARSE, "pic_cli: "//name//" is not an integer: "//char(text))
      end if
   end subroutine cli_get_int32

   subroutine cli_get_int64(this, name, value, err)
      !! Read a value as `integer(int64)`, strictly.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name of a declared entry.
      integer(int64), intent(out) :: value
         !! Parsed value; zero when the conversion failed.
      type(error_t), intent(inout), optional :: err
         !! Set on an undeclared name or an unparsable value.

      type(string_type) :: text
      type(error_t) :: local

      value = 0_int64
      call this%get_string(name, text, err)
      if (present(err)) then
         if (err%has_error()) return
      else
         if (this%find(name) == 0_default_int) return
      end if
      call parse_int(char(text), value, local)
      if (local%has_error()) then
         call fail(err, ERROR_PARSE, "pic_cli: "//name//" is not an integer: "//char(text))
      end if
   end subroutine cli_get_int64

   subroutine cli_get_real_sp(this, name, value, err)
      !! Read a value as `real(sp)`, strictly.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name of a declared entry.
      real(sp), intent(out) :: value
         !! Parsed value; zero when the conversion failed.
      type(error_t), intent(inout), optional :: err
         !! Set on an undeclared name or an unparsable value.

      type(string_type) :: text
      type(error_t) :: local

      value = 0.0_sp
      call this%get_string(name, text, err)
      if (present(err)) then
         if (err%has_error()) return
      else
         if (this%find(name) == 0_default_int) return
      end if
      call parse_real(char(text), value, local)
      if (local%has_error()) then
         call fail(err, ERROR_PARSE, "pic_cli: "//name//" is not a real: "//char(text))
      end if
   end subroutine cli_get_real_sp

   subroutine cli_get_real_dp(this, name, value, err)
      !! Read a value as `real(dp)`, strictly.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name of a declared entry.
      real(dp), intent(out) :: value
         !! Parsed value; zero when the conversion failed.
      type(error_t), intent(inout), optional :: err
         !! Set on an undeclared name or an unparsable value.

      type(string_type) :: text
      type(error_t) :: local

      value = 0.0_dp
      call this%get_string(name, text, err)
      if (present(err)) then
         if (err%has_error()) return
      else
         if (this%find(name) == 0_default_int) return
      end if
      call parse_real(char(text), value, local)
      if (local%has_error()) then
         call fail(err, ERROR_PARSE, "pic_cli: "//name//" is not a real: "//char(text))
      end if
   end subroutine cli_get_real_dp

   subroutine cli_get_logical(this, name, value, err)
      !! Read a value as `logical`.
      !!
      !! Accepts `true`, `false`, `yes`, `no`, `on`, `off`, `1` and `0` in any
      !! mixture of cases. A flag always reads back cleanly, since its stored
      !! value is one of the first two.
      class(cli_t), intent(in) :: this
      character(len=*), intent(in) :: name
         !! Long name of a declared entry.
      logical, intent(out) :: value
         !! Parsed value; false when the conversion failed.
      type(error_t), intent(inout), optional :: err
         !! Set on an undeclared name or an unrecognised spelling.

      type(string_type) :: text
      character(len=:), allocatable :: lowered
      integer(default_int) :: i

      value = .false.
      call this%get_string(name, text, err)
      if (present(err)) then
         if (err%has_error()) return
      else
         if (this%find(name) == 0_default_int) return
      end if

      lowered = char(text)
      do i = 1_default_int, len(lowered, kind=default_int)
         if (lowered(i:i) >= "A" .and. lowered(i:i) <= "Z") then
            lowered(i:i) = achar(iachar(lowered(i:i)) + 32)
         end if
      end do

      select case (lowered)
      case ("true", "yes", "on", "1")
         value = .true.
      case ("false", "no", "off", "0", "")
         value = .false.
      case default
         call fail(err, ERROR_PARSE, "pic_cli: "//name//" is not a logical: "//lowered)
      end select
   end subroutine cli_get_logical

   ! ---- help ----------------------------------------------------------------

   function cli_help_text(this) result(text)
      !! The help text, as a string. Nothing is printed; see the module note.
      !!
      !! Entries appear in registration order, so the text is stable across
      !! runs and across compilers.
      class(cli_t), intent(in) :: this
      character(len=:), allocatable :: text

      integer(default_int) :: i, width, this_width
      character(len=:), allocatable :: label

      text = "Usage: "//char(this%program_name)
      do i = 1_default_int, this%count
         if (this%entries(i)%kind /= KIND_OPTION .and. &
             this%entries(i)%kind /= KIND_FLAG) cycle
         text = text//" [options]"
         exit
      end do
      do i = 1_default_int, this%count
         if (this%entries(i)%kind /= KIND_POSITIONAL) cycle
         if (this%entries(i)%required) then
            text = text//" <"//char(this%entries(i)%name)//">"
         else
            text = text//" ["//char(this%entries(i)%name)//"]"
         end if
      end do
      text = text//new_line("a")

      if (len(char(this%summary)) > 0) then
         text = text//new_line("a")//char(this%summary)//new_line("a")
      end if

      ! One pass to find the column, one to write it, so the descriptions line
      ! up whatever the names are.
      width = 0_default_int
      do i = 1_default_int, this%count
         this_width = len(entry_label(this%entries(i)), kind=default_int)
         width = max(width, this_width)
      end do

      if (any_of_kind(this, KIND_POSITIONAL)) then
         text = text//new_line("a")//"Arguments:"//new_line("a")
         do i = 1_default_int, this%count
            if (this%entries(i)%kind /= KIND_POSITIONAL) cycle
            label = entry_label(this%entries(i))
            text = text//"  "//label//repeat(" ", width - len(label))// &
                   "  "//char(this%entries(i)%help)//new_line("a")
         end do
      end if

      text = text//new_line("a")//"Options:"//new_line("a")
      do i = 1_default_int, this%count
         if (this%entries(i)%kind == KIND_POSITIONAL) cycle
         label = entry_label(this%entries(i))
         text = text//"  "//label//repeat(" ", width - len(label))// &
                "  "//char(this%entries(i)%help)//new_line("a")
      end do
      text = text//"  "//"-h, --help"//repeat(" ", max(0_default_int, width - 10_default_int))// &
             "  Show this help"//new_line("a")
   end function cli_help_text

   pure function entry_label(entry) result(label)
      !! The left-hand column of one help line.
      type(entry_t), intent(in) :: entry
      character(len=:), allocatable :: label

      if (entry%kind == KIND_POSITIONAL) then
         label = char(entry%name)
      else if (len(char(entry%short)) > 0) then
         label = "-"//char(entry%short)//", --"//char(entry%name)
      else
         label = "    --"//char(entry%name)
      end if
   end function entry_label

   pure function any_of_kind(this, kind) result(r)
      !! Whether any declared entry has this kind.
      class(cli_t), intent(in) :: this
      integer(default_int), intent(in) :: kind
      logical :: r

      integer(default_int) :: i

      r = .false.
      do i = 1_default_int, this%count
         if (this%entries(i)%kind == kind) then
            r = .true.
            return
         end if
      end do
   end function any_of_kind

end module pic_cli
