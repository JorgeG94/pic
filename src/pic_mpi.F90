#ifdef PIC_HAVE_MPI
!! The pic_mpi.F90 file is the main interface to the MPI implementation, it should
!! be done in such a way that we can compile the library with and without MPI
!! it will be a pain in general, but meh, let's see what we can do

module pic_mpi
  !! General MPI interface module
   use pic_types, only: int32
   use mpi_f08, only: MPI_COMM, MPI_COMM_WORLD, MPI_Init, MPI_Finalize, &
                      MPI_Comm_size, MPI_Comm_rank
   implicit none
   private
   public :: pic_comm_type

   type :: pic_comm_type
    !! custom data type that holds the MPI communicators

      private

      type(MPI_COMM), public :: comm
        !! use the MPI_COMM from the mpi_f08 module interface
      integer(int32), public :: m_rank
        !! MPI rank
      integer(int32), public :: m_size
        !! MPI size
      integer(int32), public :: m_ierr
   contains
      procedure :: init => pic_comm_init
      procedure :: finalize => pic_comm_finalize
   end type pic_comm_type

   type(pic_comm_type), public :: world_comm
    !! our world comm

contains

   subroutine pic_comm_init(self)
    !! initilalize the MPI library and get the size and rank variables
      class(pic_comm_type), intent(inout) :: self
      integer(int32) :: ierr, rank, size
      call MPI_Init(ierr)
      self%m_ierr = ierr
      self%comm = MPI_COMM_WORLD
      call MPI_Comm_size(self%comm, size, ierr)
      call MPI_Comm_rank(self%comm, rank, ierr)
      self%m_size = size
      self%m_rank = rank
   end subroutine pic_comm_init

   subroutine pic_comm_finalize(self)
    !! finalize the MPI library
      class(pic_comm_type), intent(inout) :: self
      integer(int32) :: ierr
      call MPI_Finalize(ierr)
      self%m_ierr = ierr
   end subroutine pic_comm_finalize

end module pic_mpi
#endif
