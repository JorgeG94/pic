module pic_solver_provider
   use pic_types
   implicit none
contains
   SUBROUTINE sym_solve(A, X, IPVT, N, LDA, JOB, IERR)
      USE omp_lib
      IMPLICIT NONE
      external dlansy
      double precision dlansy
      INTEGER(kind=int64), INTENT(IN) :: N, LDA, JOB
      DOUBLE PRECISION, INTENT(INOUT) :: A(LDA, N), X(N)
      INTEGER(kind=int64), INTENT(OUT) :: IPVT(N)
      double precision, dimension(:), allocatable :: work
      integer, dimension(:), allocatable ::iwork
      integer :: lwork
      INTEGER(kind=int64) :: IERR
      DOUBLE PRECISION, PARAMETER :: tol = 1.0d-14
      DOUBLE PRECISION :: rcond, anorm
      INTEGER(kind=int64) :: INFO

      IERR = 0
      INFO = 0
      if (JOB .ne. 0) then
         call gen_solve(A, X, IPVT, N, LDA, JOB, IERR)
      end if

      lwork = -1
      allocate (work(1))
      call dsytrf('L', N, A, LDA, IPVT, work, lwork, INFO)
      lwork = int(work(1))
      deallocate (work)
      allocate (work(lwork))
      anorm = dlansy('1', 'U', N, A, LDA, work)
!      Perform the symmetric factorization of A
      CALL DSYTRF('U', N, A, LDA, IPVT, work, lwork, INFO)
      IF (INFO .NE. 0) THEN
         IERR = INFO
         PRINT *, "ERROR: DSYTRF failed with INFO =", INFO
         RETURN
      END IF
      allocate (iwork(N))
      call dsycon('U', N, A, LDA, IPVT, ANORM, rcond, work, iwork, INFO)
      IF (INFO .NE. 0) THEN
         PRINT *, "ERROR: DSYCON failed with INFO =", INFO
         IERR = INFO
         RETURN
      END IF
      deallocate (iwork)
      IF (rcond < tol) THEN
         PRINT *, "WARNING: Matrix may be ill-conditioned, rcond =", rcond
      END IF
      CALL DSYTRS('U', N, 1, A, LDA, IPVT, X, LDA, INFO)
      IF (INFO .NE. 0) THEN
         IERR = INFO
         PRINT *, "ERROR: DSYTRS failed with INFO =", INFO
         RETURN
      END IF
      RETURN
   END SUBROUTINE sym_solve

   SUBROUTINE gen_solve(A, X, IPVT, N, LDA, JOB, IERR)
      USE omp_lib
      IMPLICIT NONE
      INTEGER(kind=int64), INTENT(IN) :: N, LDA, JOB
      DOUBLE PRECISION, INTENT(INOUT) :: A(LDA, N), X(N)
      INTEGER(kind=int64), INTENT(OUT) :: IPVT(N)
      character(len=1) :: OP
      INTEGER(kind=int64), intent(inout) :: ierr
      OP = 'N'
      ierr = 0
      if (JOB .ne. 0) then
         OP = 'T'
      end if
      call DGETRF(N, N, A, LDA, IPVT, ierr)
      call DGETRS(OP, N, 1, A, LDA, IPVT, X, LDA, ierr)

      RETURN
   END SUBROUTINE gen_solve
end module pic_solver_provider
