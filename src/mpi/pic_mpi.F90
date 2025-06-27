module pic_comms
   use pic_types, only: default_int
#ifdef USE_MPI
   use mpi_f08
#endif
   implicit none

   type :: pic_comm

      private

#ifdef USE_MPI
      type(MPI_COMM), public :: comm
#else
      integer(default_int), public :: dummy_comm
#endif
      integer(default_int), public :: m_rank
      integer(default_int), public :: m_size
   contains
      procedure :: init => pic_comm_init
      procedure :: finalize => pic_comm_finalize
   end type pic_comm

   type(pic_comm), public :: world_comm

contains

   subroutine pic_comm_init(self)
      class(pic_comm), intent(inout) :: self
      integer(default_int) :: ierr, rank, size
#ifdef USE_MPI
      call MPI_Init(ierr)
      self%comm = MPI_COMM_WORLD
      call MPI_Comm_size(self%comm, size, ierr)
      call MPI_Comm_rank(self%comm, rank, ierr)
      self%m_size = size
      self%m_rank = rank
#else
      self%m_size = 1
      self%m_rank = 0
#endif
   end subroutine pic_comm_init

   subroutine pic_comm_finalize(self)
      class(pic_comm), intent(inout) :: self
#ifdef USE_MPI
      integer(default_int) :: ierr
      call MPI_Finalize(ierr)
#endif
   end subroutine pic_comm_finalize

end module pic_comms
