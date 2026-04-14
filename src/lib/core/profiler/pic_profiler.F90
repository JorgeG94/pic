! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo

!! Portable profiler with NVTX range support
!!
!! Provides named profiling regions with wall-clock timers and optional
!! NVTX annotations for NVIDIA Nsight Systems timeline visualisation.
!!
!! Portability:
!! - Compile with -DPIC_USE_NVTX to enable NVTX (requires nvtx module from NVIDIA HPC SDK)
!! - Without PIC_USE_NVTX, NVTX calls become no-ops but timers still work
!! - Compile with -DPIC_DISABLE_PROFILER to disable everything (zero overhead)
!!
!! Usage:
!!   use pic_profiler
!!
!!   call profiler_init()
!!   call profiler_start("solver_step")
!!   call profiler_start("inner_loop")
!!   ...
!!   call profiler_stop()  ! stops "inner_loop" (stack-based)
!!   call profiler_stop()  ! stops "solver_step"
!!   call profiler_report()
!!   call profiler_finalize()
!!
!! You can also use explicit names: call profiler_stop("solver_step")
module pic_profiler
   !! Lightweight profiler for named code regions with optional NVTX support
   use pic_types, only: default_int, dp, int_index
   use pic_logger, only: logger => global_logger
   use pic_sorting, only: sort_index
   use pic_timer, only: timer_type
#ifdef PIC_USE_NVTX
   use nvtx, only: nvtxStartRange, nvtxEndRange
#endif
   implicit none
   private

   integer(default_int), parameter :: MAX_REGIONS = 256
      !! Maximum number of distinct profiling regions
   integer(default_int), parameter :: MAX_NAME_LEN = 64
      !! Maximum length of region names

   type :: tracked_region
      !! Internal type for tracking a single profiling region
      character(len=MAX_NAME_LEN) :: name = ''
      type(timer_type) :: timer
         !! PIC timer for this region
      real(dp) :: total_time = 0.0_dp
      integer(default_int) :: call_count = 0
      logical :: active = .false.
      logical :: nvtx_only = .false.
         !! If true, only shows in NVTX timeline, not in text report
   end type tracked_region

   type :: profiler_state
      !! Internal type for global profiler state
      logical :: initialized = .false.
      logical :: enabled = .true.
      integer(default_int) :: num_regions = 0
      type(tracked_region) :: regions(MAX_REGIONS)
      integer(default_int) :: stack_depth = 0
         !! Current depth of the active region stack
      integer(default_int) :: stack(MAX_REGIONS)
         !! Stack of active region indices (for stack-based stop)
   end type profiler_state

   type(profiler_state), save :: state
      !! Global profiler state (module-level singleton)

   public :: profiler_init, profiler_finalize
   public :: profiler_start, profiler_stop
   public :: profiler_enable, profiler_disable
   public :: profiler_report, profiler_reset
   public :: profiler_get_time

contains

   subroutine nvtx_range_push(name)
      !! Push an NVTX range (no-op if PIC_USE_NVTX not defined)
      character(len=*), intent(in) :: name
#ifdef PIC_USE_NVTX
      call nvtxStartRange(name)
#else
      ! Silence unused argument warning
      if (len(name) < 0) continue
#endif
   end subroutine nvtx_range_push

   subroutine nvtx_range_pop()
      !! Pop an NVTX range (no-op if PIC_USE_NVTX not defined)
#ifdef PIC_USE_NVTX
      call nvtxEndRange()
#endif
   end subroutine nvtx_range_pop

   subroutine profiler_init(enabled)
      !! Initialize the profiler
      !!
      !! Must be called before any profiling regions are started.
      !! Optionally pass enabled=.false. to start with profiling disabled.
      logical, intent(in), optional :: enabled

#ifdef PIC_DISABLE_PROFILER
      return
#endif

      state%initialized = .true.
      state%enabled = .true.
      if (present(enabled)) state%enabled = enabled
      state%num_regions = 0
      state%stack_depth = 0
   end subroutine profiler_init

   subroutine profiler_finalize()
      !! Finalize the profiler and release resources
      !!
      !! Should be called at the end of the program after profiler_report().
#ifdef PIC_DISABLE_PROFILER
      return
#endif

      state%initialized = .false.
      state%num_regions = 0
      state%stack_depth = 0
   end subroutine profiler_finalize

   subroutine profiler_enable()
      !! Enable profiling (if it was disabled)
#ifdef PIC_DISABLE_PROFILER
      return
#endif
      state%enabled = .true.
   end subroutine profiler_enable

   subroutine profiler_disable()
      !! Disable profiling temporarily
      !!
      !! Regions started while disabled will not be tracked.
      !! Use profiler_enable() to re-enable.
#ifdef PIC_DISABLE_PROFILER
      return
#endif
      state%enabled = .false.
   end subroutine profiler_disable

   function find_or_create_region(name) result(idx)
      !! Find an existing region by name or create a new one
      !! Returns -1 if MAX_REGIONS is exceeded
      character(len=*), intent(in) :: name
      integer(default_int) :: idx
      integer(default_int) :: i

      do i = 1, state%num_regions
         if (trim(state%regions(i)%name) == trim(name)) then
            idx = i
            return
         end if
      end do

      if (state%num_regions < MAX_REGIONS) then
         state%num_regions = state%num_regions + 1
         idx = state%num_regions
         state%regions(idx)%name = trim(name)
         state%regions(idx)%total_time = 0.0_dp
         state%regions(idx)%call_count = 0
         state%regions(idx)%active = .false.
      else
         idx = -1
      end if
   end function find_or_create_region

   subroutine profiler_start(name, nvtx_only)
      !! Start a named profiling region
      !!
      !! If nvtx_only is true, the region only appears in NVTX timeline,
      !! not in the text report from profiler_report().
      character(len=*), intent(in) :: name
      logical, intent(in), optional :: nvtx_only
      integer(default_int) :: idx

#ifdef PIC_DISABLE_PROFILER
      return
#endif

      if (.not. state%enabled) return

      idx = find_or_create_region(name)
      if (idx < 0) return

      ! Don't start if already active (nested calls to same region)
      if (state%regions(idx)%active) return

      if (present(nvtx_only) .and. state%regions(idx)%call_count == 0) then
         state%regions(idx)%nvtx_only = nvtx_only
      end if

      state%regions(idx)%active = .true.
      call nvtx_range_push(name)
      call state%regions(idx)%timer%start()

      ! Push onto stack for stack-based stop
      state%stack_depth = state%stack_depth + 1
      state%stack(state%stack_depth) = idx
   end subroutine profiler_start

   subroutine profiler_stop(name)
      !! Stop a profiling region
      !!
      !! If name is omitted, stops the most recently started region (stack-based).
      !! If name is provided, stops that specific region (for explicit control).
      !! Time is accumulated across multiple start/stop pairs.
      character(len=*), intent(in), optional :: name
      integer(default_int) :: idx

#ifdef PIC_DISABLE_PROFILER
      return
#endif

      if (.not. state%enabled) return

      if (present(name)) then
         ! Find the region by name
         do idx = 1, state%num_regions
            if (trim(state%regions(idx)%name) == trim(name)) exit
         end do
         if (idx > state%num_regions) return
         if (.not. state%regions(idx)%active) return

         ! Remove from stack (may not be at top if using explicit names)
         call remove_from_stack(idx)
      else
         ! Stack-based: pop the most recent region
         if (state%stack_depth == 0) return
         idx = state%stack(state%stack_depth)
         state%stack_depth = state%stack_depth - 1
         if (.not. state%regions(idx)%active) return
      end if

      call state%regions(idx)%timer%stop()
      call nvtx_range_pop()

      state%regions(idx)%total_time = state%regions(idx)%total_time + &
                                      state%regions(idx)%timer%get_elapsed_time()
      state%regions(idx)%call_count = state%regions(idx)%call_count + 1
      state%regions(idx)%active = .false.
   end subroutine profiler_stop

   subroutine remove_from_stack(idx)
      !! Remove a region index from the stack (helper for explicit stop)
      integer(default_int), intent(in) :: idx
      integer(default_int) :: i, j

      do i = state%stack_depth, 1, -1
         if (state%stack(i) == idx) then
            ! Shift remaining elements down
            do j = i, state%stack_depth - 1
               state%stack(j) = state%stack(j + 1)
            end do
            state%stack_depth = state%stack_depth - 1
            return
         end if
      end do
   end subroutine remove_from_stack

   function profiler_get_time(name) result(t)
      !! Get accumulated time for a named region
      !!
      !! Returns 0.0 if the region does not exist.
      character(len=*), intent(in) :: name
      real(dp) :: t
      integer(default_int) :: idx

      t = 0.0_dp

#ifdef PIC_DISABLE_PROFILER
      return
#endif

      do idx = 1, state%num_regions
         if (trim(state%regions(idx)%name) == trim(name)) then
            t = state%regions(idx)%total_time
            return
         end if
      end do
   end function profiler_get_time

   subroutine profiler_reset()
      !! Reset all timing data while keeping region names
      !!
      !! Useful for discarding warm-up iterations.
      integer(default_int) :: i

#ifdef PIC_DISABLE_PROFILER
      return
#endif

      do i = 1, state%num_regions
         state%regions(i)%total_time = 0.0_dp
         state%regions(i)%call_count = 0
         state%regions(i)%active = .false.
      end do
      state%stack_depth = 0
   end subroutine profiler_reset

   subroutine profiler_report(title, root_region)
      !! Print profiling report sorted by time
      !!
      !! If root_region is specified, percentages are relative to that region's
      !! total time. Otherwise, percentages are relative to the sum of all regions.
      character(len=*), intent(in), optional :: title
      character(len=*), intent(in), optional :: root_region
      integer(default_int) :: i, j, root_idx, n_print
      integer(default_int) :: region_map(MAX_REGIONS)
      ! 0-based arrays for sort_index
      real(dp) :: times_to_sort(0:MAX_REGIONS - 1)
      integer(int_index) :: sort_indices(0:MAX_REGIONS - 1)
      real(dp) :: total_time, pct
      character(len=256) :: line

#ifdef PIC_DISABLE_PROFILER
      return
#endif

      if (state%num_regions == 0) return

      ! Find root region if specified
      root_idx = -1
      if (present(root_region)) then
         do i = 1, state%num_regions
            if (trim(state%regions(i)%name) == trim(root_region)) then
               root_idx = i
               exit
            end if
         end do
      end if

      ! Calculate total time
      if (root_idx > 0) then
         total_time = state%regions(root_idx)%total_time
      else
         total_time = 0.0_dp
         do i = 1, state%num_regions
            total_time = total_time + state%regions(i)%total_time
         end do
      end if

      ! Build list of regions to print (excluding root and nvtx_only regions)
      ! and prepare arrays for sorting
      n_print = 0
      do i = 1, state%num_regions
         if (i /= root_idx .and. .not. state%regions(i)%nvtx_only) then
            region_map(n_print + 1) = i
            times_to_sort(n_print) = state%regions(i)%total_time
            n_print = n_print + 1
         end if
      end do

      ! Sort by time descending using PIC's sort_index
      if (n_print > 0) then
         call sort_index(times_to_sort(0:n_print - 1), sort_indices(0:n_print - 1), reverse=.true.)
      end if

      ! Print report
      call logger%info('')
      call logger%info('============================================================')
      if (present(title)) then
         call logger%info('Profiler Report: '//trim(title))
      else
         call logger%info('Profiler Report')
      end if
      call logger%info('============================================================')
      call logger%info('  Region                          Time (s)    Calls    %    ')
      call logger%info('------------------------------------------------------------')

      do i = 0, n_print - 1
         ! sort_indices(i) is the 0-based index into times_to_sort
         ! region_map(sort_indices(i) + 1) gives us the actual region index
         j = region_map(sort_indices(i) + 1)
         if (total_time > 0.0_dp) then
            pct = 100.0_dp*state%regions(j)%total_time/total_time
         else
            pct = 0.0_dp
         end if
         write (line, '(A,A32,F12.6,I8,F8.1)') '  ', &
            state%regions(j)%name, &
            state%regions(j)%total_time, &
            state%regions(j)%call_count, &
            pct
         call logger%info(trim(line))
      end do

      call logger%info('------------------------------------------------------------')
      write (line, '(A,F12.6)') '  Total:                        ', total_time
      call logger%info(trim(line))
      call logger%info('============================================================')

#ifdef PIC_USE_NVTX
      call logger%info('  (NVTX enabled - use Nsight Systems for GPU timeline)')
#else
      call logger%info('  (NVTX disabled - compile with -DPIC_USE_NVTX to enable)')
#endif
   end subroutine profiler_report

end module pic_profiler
