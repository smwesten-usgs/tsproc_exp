!     Last change:  J     9 Sep 2004    5:38 pm
module tsp_data_structures

  implicit none

!   integer, parameter    :: MAXSERIES=10000
!   integer, parameter    :: MAXSERIESREAD=10000
!   integer, parameter    :: MAXSTABLE=500
!   integer, parameter    :: MAXGTABLE=500
!   integer, parameter    :: MAXCTABLE=500
!   integer, parameter    :: MAXCONTEXT=5
!   integer, parameter    :: MAXVTABLE=500
!   integer, parameter    :: MAXDTABLE=100
!   integer, parameter    :: MAXTEMPDURFLOW=30
!   integer, parameter    :: MAXTEMPFILE=200
!   integer, parameter    :: MAXPAR=5000
!   character, parameter  :: OBSCHAR='_'

  integer, parameter    :: DATETEXTLENGTH = 15
  integer, parameter    :: MAXNAMELENGTH = 18
  integer, parameter    :: MAXEQUATIONLENGTH = 1024

  ! Define the sizes of base types used in the model
  integer*2, public, parameter :: T_LOGICAL = 4
  integer*2, public, parameter :: T_INT = 4
  integer*2, public, parameter :: T_SHORT = 2
  integer*2, public, parameter :: T_BYTE = 1

! Define machine-independent sizes for base types
  integer*2, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=6,r=37)
!  integer*2, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=13,r=307)
  integer*2, public, parameter :: T_DBL = SELECTED_REAL_KIND(p=13,r=200)
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
  real (kind=T_SGL), public, parameter :: rNODATA = -99999_T_SGL
  real (kind=T_INT), public, parameter :: iNODATA = -99999_T_INT

  logical (kind=T_LOGICAL) :: lAssertAlwaysFatal = lTRUE

  character (len=1) :: cTAB = ACHAR(9)
  character (len=1) :: cRETURN = ACHAR(13)

  integer LU_TSPROC_CONTROL,LU_OUT
!   integer NumProcBloc_g,ILine_g,IProcSetting_g
!   character*25 Context_g
!   character*40 CurrentBlock_g
!   character*120 sInfile_g,sRecfile_g,sOutfile_g,sString_g
!
! ! -- The following variables are global because they are used to exchange information
! !    between the LIST_OUTPUT block and the WRITE_PEST_FILES block.
!
!   integer iMseries_g
!   integer iMstable_g
!   integer iMctable_g
!   integer iMvtable_g
!   integer iMdtable_g
!   integer iOutseries_g(MAXSERIES),iOutStable_g(MAXSTABLE),iOutVtable_g(MAXVTABLE), &
!           iOutDtable_g(MAXDTABLE),iOutCtable_g(MAXCTABLE)
!   character*10 sSeriesFormat_g
!   character*120 sListOutputFile_g
!
! ! -- Following are some parameter definitions related to equations.
!
! ! -- Maximum terms in any mathematical expression:-
!   integer MAXTERM
!   parameter(MAXTERM=200)
! ! -- Maximum number of function types:-
!   integer NFUNCT
!   parameter(NFUNCT=16)
! ! -- Maximum number of operators:-
!   integer NOPER
!   parameter(NOPER=7)
! ! -- Maximum number of series_g names in a series_g equation:-
!   integer MAXEQNSER
!   parameter (MAXEQNSER=25)
!
!   integer iorder(MAXTERM)
!   character*1   operat(7)
!   character*6   funct(NFUNCT)
!   character*28  aterm(MAXTERM),bterm(MAXTERM),cterm(MAXTERM)
!   double precision rterm(MAXTERM), qterm(MAXTERM)
!   data funct /'abs   ','acos  ','asin  ','atan  ','cos   ','cosh  ',  &
!     'exp   ','log   ','log10 ','sin   ','sinh  ','sqrt  ','tan   ',   &
!     'tanh  ','neg   ','pos   '/
!   data operat /'^','/','*','-','+','(',')'/
!
! ! -- The following pertain to WDM files.
!
!   integer MAXWDOPN
!   parameter (MAXWDOPN=10)  ! Number of WDM files that can be open.
!   integer iwdopn
!   integer wdmun(MAXWDOPN)
!   character*120 wdmfil(MAXWDOPN)
!
! !****************************************************************************
! ! defined types
! !****************************************************************************
!
! 	type modelgrid
! 	  integer                         :: nrow,ncol
! 	  double precision                :: east_corner,north_corner,rotation
! 	  real                            :: cosang,sinang
! 	  real, dimension(:), pointer     :: delr,delc
! 	  integer                         :: specunit,specline
! 	  character (len=80)              :: specfile
! 	end type modelgrid
!
! !****************************************************************************
! !global variables
! !****************************************************************************
!
! !variables for reading a file ------->
!
! 	integer, parameter              	:: NUM_WORD_DIM=100
! 	integer, dimension(NUM_WORD_DIM)        :: left_word,right_word
! 	character (len=400)             	:: cline
!
! !variables for writing a message ------->
!
! 	integer                 :: imessage=0
! 	character (len=500)     :: amessage= ' '
! 	character (len=200)     :: initial_message=' '
!
! !escape variables ------->
!
! 	integer                 :: escset=0
! 	character (len=5)       :: eschar = 'E ~e '
!
! !variables in bore data manipulation ------->
!
! 	integer                         :: num_bore_coord, num_bore_list
! 	character (len=120)             :: bore_coord_file, bore_list_file
! 	integer, dimension(:), pointer			:: bore_coord_layer
! 	double precision, dimension(:), pointer         :: bore_coord_east, &
! 							   bore_coord_north
! 	character (len=10), dimension(:), pointer       :: bore_coord_id, &
!                                                            bore_list_id
!
! !variables recording data settings ------->
!
! 	integer				:: datespec

! global parameter for rec file, std output unit
   integer, public :: LU_REC = 12
   integer, public :: LU_DATA
   integer, public :: LU_STD_OUT = 6


!  type T_USGS_NWIS_DAILY
!    integer (kind=T_INT) :: iWaterYear
!    type (T_DATETIME) :: tDT
!    real (kind=T_SGL) :: rMeanDischarge
!    character (len=10) :: sDataFlag
!  end type T_USGS_NWIS_DAILY
!
!  type T_USGS_NWIS_GAGE
!    character (len=256) :: sAgencyCode
!    character (len=256) :: sSiteNumber
!    character (len=256) :: sDescription
!    type (T_USGS_NWIS_DAILY), dimension(:), allocatable :: pGageData
!  end type T_USGS_NWIS_GAGE


contains


end module tsp_data_structures
