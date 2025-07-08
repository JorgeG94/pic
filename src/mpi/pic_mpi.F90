!! The pic_mpi.F90 file is the main interface to the MPI implementation, it should
!! be done in such a way that we can compile the library with and without MPI
!! it will be a pain in general, but meh, let's see what we can do

module pic_mpi
  !! General MPI interface module
   use pic_types, only: default_int, int64, dp
#ifdef USE_MPI
   use mpi_f08, only: MPI_COMM, MPI_COMM_WORLD, MPI_Init, MPI_Finalize, &
                      MPI_Comm_size, MPI_Comm_rank, MPI_OP, MPI_INTEGER8, &
                      MPI_DOUBLE_PRECISION, MPI_Barrier, MPI_Reduce, MPI_SUM, &
                      MPI_MAX, MPI_MIN, MPI_COMM_NULL
#endif
   implicit none
   private
   public :: pic_comm
   ! Module-level world communicator
#ifdef USE_MPI
   public :: MPI_SUM, MPI_MAX
#endif
   type :: pic_comm
      private
#ifdef USE_MPI
      type(MPI_COMM), public :: comm
#else
      integer(default_int), public :: comm
#endif
      integer(default_int), public :: m_rank
      integer(default_int), public :: m_size
      integer(default_int), public :: m_ierr
      logical, private :: m_is_world
   contains
      procedure :: init => pic_comm_init
      procedure :: finalize => pic_comm_finalize
      procedure :: is_world
      procedure :: get => pic_comm_get
      procedure :: world => pic_get_world
      ! procedure :: dup => pic_comm_dup
      procedure :: barrier => pic_comm_barrier
      !procedure :: reduce => pic_comm_reduce
      procedure, pass :: reduce_int8 => pic_comm_reduce_int8
      procedure, pass :: reduce_dp => pic_comm_reduce_dp
      generic :: reduce => reduce_int8, reduce_dp
      ! procedure :: send => pic_comm_send
      ! procedure :: recv => pic_comm_recv
   end type pic_comm
   type(pic_comm), public :: pic_comm_world

contains

   function is_world(self) result(is_comm_world)
      !! Check if this communicator is the world communicator
      class(pic_comm), intent(in) :: self
      logical :: is_comm_world
#ifdef USE_MPI
      is_comm_world = self%m_is_world
#else
      is_comm_world = .false.
#endif

   end function is_world

   subroutine pic_comm_init(self, from_world)
      !! Initialize communicator - either as world or duplicate from world
      class(pic_comm), intent(inout) :: self
      logical, intent(in), optional :: from_world
      logical :: use_world
      integer(default_int) :: ierr, rank, size

      use_world = .false.
      if (present(from_world)) use_world = from_world

#ifdef USE_MPI

      if (use_world) then
         ! Initialize MPI and set up world communicator
         call MPI_Init(ierr)
         self%m_ierr = ierr
         self%comm = MPI_COMM_WORLD
         self%m_is_world = .true.
      else
         call MPI_Comm_dup(pic_comm_world%comm, self%comm, ierr)
         self%m_ierr = ierr
      end if

      call MPI_Comm_size(self%comm, size, ierr)
      call MPI_Comm_rank(self%comm, rank, ierr)
      self%m_size = size
      self%m_rank = rank
#else
      self%m_size = 1
      self%m_rank = 0
      self%comm = 0
      self%m_ierr = 0
#endif
   end subroutine pic_comm_init

   subroutine pic_comm_finalize(self)
      !! Finalize communicator
      class(pic_comm), intent(inout) :: self
      integer(default_int) :: ierr

#ifdef USE_MPI
      if (.not. self%is_world()) then
         call MPI_Comm_free(self%comm, ierr)
      else
         ! If this is the world communicator, finalize MPI
         call MPI_Finalize(ierr)
      end if

#endif
   end subroutine pic_comm_finalize

   function pic_comm_dup(self) result(new_comm)
      !! Create a duplicate of this communicator
      class(pic_comm), intent(in) :: self
      type(pic_comm) :: new_comm

#ifdef USE_MPI
      integer(default_int) :: ierr, rank, size
      call MPI_Comm_dup(self%comm, new_comm%comm, ierr)
      new_comm%m_ierr = ierr
      call MPI_Comm_size(new_comm%comm, size, ierr)
      call MPI_Comm_rank(new_comm%comm, rank, ierr)
      new_comm%m_size = size
      new_comm%m_rank = rank
#else
      new_comm%m_size = 1
      new_comm%m_rank = 0
      new_comm%comm = 0
      new_comm%m_ierr = 0
#endif
   end function pic_comm_dup

   subroutine pic_comm_barrier(self)
      !! Barrier synchronization
      class(pic_comm), intent(inout) :: self

#ifdef USE_MPI
      integer(default_int) :: ierr
      call MPI_Barrier(self%comm, ierr)
      self%m_ierr = ierr
#endif
   end subroutine pic_comm_barrier

   function pic_comm_get(self) result(raw_comm)
   !! Returns the raw communicator
      class(pic_comm), intent(in) :: self
#ifdef USE_MPI
      type(MPI_COMM) :: raw_comm
      raw_comm = self%comm
#else
      integer(default_int) :: raw_comm
      raw_comm = self%comm
#endif
   end function pic_comm_get

   function pic_get_world(self) result(comm)
      class(pic_comm), intent(in) :: self
      type(pic_comm) :: comm
      comm = pic_comm_world
   end function pic_get_world

   subroutine pic_comm_reduce_int8(self, sendbuf, recvbuf, count, op, root)
   !! Reduce operation
      class(pic_comm), intent(inout) :: self
      integer(int64), intent(in) :: sendbuf
      integer(int64), intent(inout) :: recvbuf
      integer(default_int), intent(in) :: count
      type(MPI_OP), intent(in) :: op        ! MPI operation
      integer(default_int), intent(in) :: root      ! root rank
      integer(default_int) :: ierr

#ifdef USE_MPI
      call MPI_Reduce(sendbuf, recvbuf, count, MPI_INTEGER8, op, root, self%comm, ierr)
      self%m_ierr = ierr
#else
      recvbuf = sendbuf
      self%m_ierr = 0
#endif
   end subroutine pic_comm_reduce_int8

   subroutine pic_comm_reduce_dp(self, sendbuf, recvbuf, count, op, root)
   !! Reduce operation
      class(pic_comm), intent(inout) :: self
      real(dp), intent(in) :: sendbuf
      real(dp), intent(inout) :: recvbuf
      integer(default_int), intent(in) :: count
      type(MPI_OP), intent(in) :: op        ! MPI operation
      integer(default_int), intent(in) :: root      ! root rank
      integer(default_int) :: ierr

#ifdef USE_MPI
      call MPI_Reduce(sendbuf, recvbuf, count, MPI_DOUBLE_PRECISION, op, root, self%comm, ierr)
      self%m_ierr = ierr
#else
      recvbuf = sendbuf
      self%m_ierr = 0
#endif
   end subroutine pic_comm_reduce_dp

end module pic_mpi
