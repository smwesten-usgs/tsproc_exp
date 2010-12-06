module tsp_data_structures

  implicit none

  integer, parameter    :: DATETEXTLENGTH = 15
  integer, parameter    :: MAXNAMELENGTH = 18
  integer, parameter    :: MAXEQUATIONLENGTH = 1024
  integer, parameter    :: MAXARGLENGTH = 256
  integer, parameter    :: MAXBLOCKLENGTH = 10000

  ! Define the sizes of base types used in the model
  integer*2, public, parameter :: T_LOGICAL = 4
!  integer*2, public, parameter :: T_LOGICAL = 3
  integer*2, public, parameter :: T_INT = 4
!  integer*2, public, parameter :: T_INT = 3
  integer*2, public, parameter :: T_SHORT = 2
  integer*2, public, parameter :: T_BYTE = 1

! Define machine-independent sizes for base types
  integer*2, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=6,r=37)
!  integer*2, public, parameter :: T_SGL = 1
!  integer*2, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=13,r=307)
!  integer*2, public, parameter :: T_INT = SELECTED_INT_KIND(37)
!  integer*2, public, parameter :: T_DBL = 2
  integer*2, public, parameter :: T_DBL = SELECTED_REAL_KIND(p=13,r=200)
!  integer*2, public, parameter :: T_DBL = 8
  integer*2, public, parameter :: T_CPLX_SGL = KIND((T_SGL, T_SGL))
  integer*2, public, parameter :: T_CPLX_DBL = KIND((T_DBL, T_DBL))

  ! Some useful typed constants (to ensure accurate computations)
  real (kind=T_SGL), public, parameter :: rZERO = 0.0_T_SGL
  real (kind=T_DBL), public, parameter :: rD_ZERO = 0.0_T_DBL
  real (kind=T_DBL), public, parameter :: rD_HALF = 0.5_T_DBL
  real (kind=T_SGL), public, parameter :: rNEAR_ZERO = 1E-8_T_SGL
  logical (kind=T_LOGICAL), public, parameter :: lTRUE = .true._T_LOGICAL
  logical (kind=T_LOGICAL), public, parameter :: lFALSE = .false._T_LOGICAL
  real (kind=T_SGL), public, parameter :: rNEAR_TINY = -huge(rZERO) + 1.
  real (kind=T_SGL), public, parameter :: rNODATA = -99999.0_T_SGL
  real (kind=T_INT), public, parameter :: iNODATA = -99999_T_INT

  logical (kind=T_LOGICAL) :: lAssertAlwaysFatal = lTRUE

  character (len=1) :: cTAB = ACHAR(9)
  character (len=1) :: cRETURN = ACHAR(13)

  integer LU_TSPROC_CONTROL,LU_OUT

  integer (kind=T_INT) :: iIMAGE_NUM = 0

  ! define constants for use in processing C table entries
  integer (kind=T_INT), parameter :: iNUM_C_TABLE_STATS = 8
  enum, bind(c)
    enumerator :: BIAS = 1
    enumerator STANDARD_ERROR, PERCENT_BIAS, RELATIVE_BIAS, RELATIVE_STD_ERROR, &
               NASH_SUTCLIFFE, COEFFICIENT_OF_EFFICIENCY, INDEX_OF_AGREEMENT
  end enum

! global parameter for rec file, std output unit
   integer, public :: LU_REC = 12
   integer, public :: LU_DATA
   integer, public :: LU_STD_OUT = 6

   character (len=256), public :: sMostRecentListOutputFile = "none"
   character (len=256), public :: sMostRecentInstructionFile = "none"

end module tsp_data_structures
