!     Last change:  J     9 Sep 2004    5:38 pm
module tsp_data_structures

  implicit none

  integer, parameter    :: MAXSERIES=10000
  integer, parameter    :: MAXSERIESREAD=10000
  integer, parameter    :: MAXSTABLE=500
  integer, parameter    :: MAXGTABLE=500
  integer, parameter    :: MAXCTABLE=500
  integer, parameter    :: MAXCONTEXT=5
  integer, parameter    :: MAXVTABLE=500
  integer, parameter    :: MAXDTABLE=100
  integer, parameter    :: MAXTEMPDURFLOW=30
  integer, parameter    :: MAXTEMPFILE=200
  integer, parameter    :: MAXPAR=5000
  character, parameter  :: OBSCHAR='_'
  integer, parameter    :: DATETEXTLENGTH = 15
  integer, parameter    :: MAXNAMELENGTH = 18

  ! Define the sizes of base types used in the model
  integer*2, public, parameter :: T_LOGICAL = 4
  integer*2, public, parameter :: T_INT = 4
  integer*2, public, parameter :: T_SHORT = 2
  integer*2, public, parameter :: T_REAL = SELECTED_REAL_KIND(p=6,r=37)

! Define machine-independent sizes for base types
  integer*2, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=6,r=37)
!  integer*2, public, parameter :: T_SGL = SELECTED_REAL_KIND(p=13,r=307)
  integer*2, public, parameter :: T_DBL = SELECTED_REAL_KIND(p=13,r=200)
  integer*2, public, parameter :: T_CPLX_SGL = KIND((T_SGL, T_SGL))
  integer*2, public, parameter :: T_CPLX_DBL = KIND((T_DBL, T_DBL))

  ! Some useful typed constants (to ensure accurate computations)
  real (kind=T_SGL), public, parameter :: rZERO = 0.0_T_SGL
  real (kind=T_DBL), public, parameter :: rD_ZERO = 0.0_T_DBL
  real (kind=T_SGL), public, parameter :: rNEAR_ZERO = 1E-8_T_SGL
  logical (kind=T_LOGICAL), public, parameter :: lTRUE = .true._T_LOGICAL
  logical (kind=T_LOGICAL), public, parameter :: lFALSE = .false._T_LOGICAL
  real (kind=T_SGL), public, parameter :: rNODATA = -99999_T_SGL
  real (kind=T_INT), public, parameter :: iNODATA = -99999_T_INT

  logical (kind=T_LOGICAL) :: lAssertAlwaysFatal = lTRUE

  character (len=1) :: cTAB = ACHAR(9)
  character (len=1) :: cRETURN = ACHAR(13)

  type T_MONTH
    character (len=3) :: sAbbreviation
    character (len=12) :: sName
  end type T_MONTH

  type time_series
    logical active
    integer nterm
    character*2 type
    character (len=MAXNAMELENGTH) :: name
    integer, dimension(:), pointer :: days
    integer, dimension(:), pointer :: secs
    real,    dimension(:), pointer :: val
  end type time_series

  ! general-purpose table for arbitrary stats output
  type g_table
    logical active
    character (len=MAXNAMELENGTH) :: name
    character (len=MAXNAMELENGTH) :: series_name
    character (len=80), dimension(:), pointer    :: sDescription
    real, dimension(:), pointer    :: rValue
  end type g_table

  ! STATISTICS TABLE
  type s_table
    logical active
    character (len=MAXNAMELENGTH) :: name
    character (len=MAXNAMELENGTH) :: series_name
    real     maximum
    real     minimum
    real     range
    real     mean
    real     stddev
    real     total
    real     minmean
    real     maxmean
    real     rec_power
    integer  rec_icount
    integer  rec_itrans
    integer  rec_begdays
    integer  rec_begsecs
    integer  rec_enddays
    integer  rec_endsecs
    integer  avetime
  end type s_table

  ! COMPARE SERIES table
  type c_table
    logical active
    character (len=MAXNAMELENGTH) :: name
    character (len=MAXNAMELENGTH) :: series_name_obs
    character (len=MAXNAMELENGTH) :: series_name_sim
    real     bias
    real     se
    real     rbias
    real     rse
    real     ns
    real     ce
    real     ia
    integer  rec_icount
    integer  rec_begdays
    integer  rec_begsecs
    integer  rec_enddays
    integer  rec_endsecs
  end type c_table

  ! VOLUME table
  type v_table
    logical active
    character (len=MAXNAMELENGTH) :: name
    character (len=MAXNAMELENGTH) :: series_name
    integer nterm
    integer, dimension(:), pointer :: days1
    integer, dimension(:), pointer :: secs1
    integer, dimension(:), pointer :: days2
    integer, dimension(:), pointer :: secs2
    real, dimension(:), pointer    :: vol
  end type v_table

  !FLOW-DURATION table (exceedance table)
  type d_table
    logical active
    character (len=MAXNAMELENGTH) :: name
    character (len=MAXNAMELENGTH) :: series_name
    character*7 time_units
    integer under_over
    integer nterm
    real total_time
    real, dimension(:), pointer     :: flow
    real, dimension(:), pointer     :: tdelay
    real, dimension(:), pointer     :: time
  end type d_table

  type (time_series) tempseries_g
  type (time_series) series_g(MAXSERIES)
  type (s_table) stable_g(MAXSTABLE)
  type (g_table) gtable_g(MAXGTABLE)
  type (c_table) ctable_g(MAXCTABLE)
  type (v_table) vtable_g(MAXVTABLE)
  type (d_table) tempdtable_g
  type (d_table) dtable_g(MAXDTABLE)

  integer LU_TSPROC_CONTROL,LU_OUT
  integer NumProcBloc_g,ILine_g,IProcSetting_g
  character*25 Context_g
  character*40 CurrentBlock_g
  character*120 sInfile_g,sRecfile_g,sOutfile_g,sString_g

! -- The following variables are global because they are used to exchange information
!    between the LIST_OUTPUT block and the WRITE_PEST_FILES block.

  integer iMseries_g
  integer iMstable_g
  integer iMctable_g
  integer iMvtable_g
  integer iMdtable_g
  integer iOutseries_g(MAXSERIES),iOutStable_g(MAXSTABLE),iOutVtable_g(MAXVTABLE), &
          iOutDtable_g(MAXDTABLE),iOutCtable_g(MAXCTABLE)
  character*10 sSeriesFormat_g
  character*120 sListOutputFile_g

! -- Following are some parameter definitions related to equations.

! -- Maximum terms in any mathematical expression:-
  integer MAXTERM
  parameter(MAXTERM=200)
! -- Maximum number of function types:-
  integer NFUNCT
  parameter(NFUNCT=16)
! -- Maximum number of operators:-
  integer NOPER
  parameter(NOPER=7)
! -- Maximum number of series_g names in a series_g equation:-
  integer MAXEQNSER
  parameter (MAXEQNSER=25)

  integer iorder(MAXTERM)
  character*1   operat(7)
  character*6   funct(NFUNCT)
  character*28  aterm(MAXTERM),bterm(MAXTERM),cterm(MAXTERM)
  double precision rterm(MAXTERM), qterm(MAXTERM)
  data funct /'abs   ','acos  ','asin  ','atan  ','cos   ','cosh  ',  &
    'exp   ','log   ','log10 ','sin   ','sinh  ','sqrt  ','tan   ',   &
    'tanh  ','neg   ','pos   '/
  data operat /'^','/','*','-','+','(',')'/

! -- The following pertain to WDM files.

  integer MAXWDOPN
  parameter (MAXWDOPN=10)  ! Number of WDM files that can be open.
  integer iwdopn
  integer wdmun(MAXWDOPN)
  character*120 wdmfil(MAXWDOPN)

!****************************************************************************
! defined types
!****************************************************************************

	type modelgrid
	  integer                         :: nrow,ncol
	  double precision                :: east_corner,north_corner,rotation
	  real                            :: cosang,sinang
	  real, dimension(:), pointer     :: delr,delc
	  integer                         :: specunit,specline
	  character (len=80)              :: specfile
	end type modelgrid

!****************************************************************************
!global variables
!****************************************************************************

!variables for reading a file ------->

	integer, parameter              	:: NUM_WORD_DIM=100
	integer, dimension(NUM_WORD_DIM)        :: left_word,right_word
	character (len=400)             	:: cline

!variables for writing a message ------->

	integer                 :: imessage=0
	character (len=500)     :: amessage= ' '
	character (len=200)     :: initial_message=' '

!escape variables ------->

	integer                 :: escset=0
	character (len=5)       :: eschar = 'E ~e '

!variables in bore data manipulation ------->

	integer                         :: num_bore_coord, num_bore_list
	character (len=120)             :: bore_coord_file, bore_list_file
	integer, dimension(:), pointer			:: bore_coord_layer
	double precision, dimension(:), pointer         :: bore_coord_east, &
							   bore_coord_north
	character (len=10), dimension(:), pointer       :: bore_coord_id, &
                                                           bore_list_id

!variables recording data settings ------->

	integer				:: datespec

! parameters defining valid program options

   integer, parameter :: iGET_WDM_SERIES               = 101
   integer, parameter :: iGET_SSF_SERIES               = 102
   integer, parameter :: iGET_PLT_SERIES               = 103
   integer, parameter :: iGET_MUL_SERIES_TETRAD        = 104
   integer, parameter :: iGET_MUL_SERIES_SSF           = 105
   integer, parameter :: iGET_UFORE_SERIES             = 106
   integer, parameter :: iGET_MUL_SERIES_GSFLOW_GAGE   = 107
   integer, parameter :: iGET_MUL_SERIES_STATVAR       = 108

   integer, parameter :: iWRITE_LIST_OUTPUT            = 201

   integer, parameter :: iERASE_ENTITY                 = 301
   integer, parameter :: iREDUCE_SPAN                  = 302
   integer, parameter :: iSERIES_STATISTICS            = 303
   integer, parameter :: iSERIES_COMPARE               = 304
   integer, parameter :: iNEW_TIME_BASE                = 305
   integer, parameter :: iVOLUME_CALCULATION           = 306
   integer, parameter :: iEXCEEDANCE_TIME              = 308
   integer, parameter :: iSERIES_EQUATION              = 310
   integer, parameter :: iSERIES_DISPLACE              = 311
   integer, parameter :: iSERIES_CLEAN                 = 312
   integer, parameter :: iDIGITAL_FILTER               = 313
   integer, parameter :: iSERIES_BASE_LEVEL            = 314
   integer, parameter :: iVOL_TABLE_TO_SERIES          = 315
   integer, parameter :: iMOVING_MINIMUM               = 316
   integer, parameter :: iNEW_SERIES_UNIFORM           = 317
   integer, parameter :: iSERIES_DIFFERENCE            = 318
   integer, parameter :: iPERIOD_STATISTICS            = 319
   integer, parameter :: iHYDRO_PEAKS                  = 320
   integer, parameter :: iUSGS_HYSEP                   = 321
   integer, parameter :: iHYDROLOGIC_INDICES           = 322

   integer, parameter :: iWRITE_PEST_FILES             = 401

   integer, parameter :: iGET_SETTINGS                 = 1

! global parameter for rec file, std output unit
   integer, public :: LU_REC
   integer, public :: LU_DATA
   integer, public :: LU_STD_OUT = 6

  type T_DATETIME
    integer (kind=T_SHORT) :: iMonth = 1
    integer (kind=T_SHORT) :: iDay = 1
    integer (kind=T_SHORT) :: iYear = 1
    integer (kind=T_SHORT) :: iHour = 0
    integer (kind=T_SHORT) :: iMinute = 0
    integer (kind=T_SHORT) :: iSecond = 0
    integer (kind=T_SHORT) :: iWaterYear
!    real (kind=T_DBL)    :: rJulianDay
    integer (kind=T_INT) :: iJulianDay
    real (kind=T_SGL) :: rFractionOfDay

  contains

    procedure :: calc_julian_day_sub
    procedure :: populate_julian_day_sub
    generic, public :: calcJulianDay => calc_julian_day_sub, &
                                           populate_julian_day_sub
    procedure, public :: calcWaterYear => calc_water_year_sub
    procedure, public :: parseDate => parse_text_to_date_sub
    procedure, public :: parseTime => parse_text_to_time_sub
!    generic :: operator(+) => add_datetime_to_datetime_fn
!    generic :: operator(-) => subtract_datetime_from_datetime_fn
    procedure :: is_date_greater_than
    generic :: operator(>) => is_date_greater_than
    procedure :: is_date_less_than
    generic :: operator(<) => is_date_less_than

    procedure :: is_date_GT_or_equal_to
    generic :: operator(>=) => is_date_GT_or_equal_to
    procedure :: is_date_LT_or_equal_to
    generic :: operator(<=) => is_date_LT_or_equal_to

    procedure, public :: writelog => print_date_time_object_sub
    procedure, public :: prettydate => write_pretty_date_fn
    procedure, public :: listdatetime => write_list_datetime_fn
    procedure, public :: listdate => write_list_date_fn
    procedure, public :: listtime => write_list_time_fn
    procedure :: is_date_equal_to
    generic :: operator(==) => is_date_equal_to
    procedure :: date_subtract_fn
    generic :: operator(-) => date_subtract_fn

  end type T_DATETIME

   type (T_MONTH),dimension(12) :: MONTH = (/ &
     T_MONTH('JAN', 'January'), &
     T_MONTH('FEB', 'February'), &
     T_MONTH('MAR', 'March'), &
     T_MONTH('APR', 'April'), &
     T_MONTH('MAY', 'May'), &
     T_MONTH('JUN', 'June'), &
     T_MONTH('JUL', 'July'), &
     T_MONTH('AUG', 'August'), &
     T_MONTH('SEP', 'September'), &
     T_MONTH('OCT', 'October'), &
     T_MONTH('NOV', 'November'), &
     T_MONTH('DEC', 'December') &
     /)

!  type (T_DATETIME) :: YEAR_ZERO = (/ &
!    T_DATETIME( iJulianDay = 0, rFractionOfDay = 0.) /)

!  type (T_DATETIME) :: YEAR_3000 = (/ &
!    T_DATETIME( iJulianDay = 2816788, rFractionOfDay = 0.) /)

  type T_USGS_NWIS_DAILY
    integer (kind=T_INT) :: iWaterYear
    type (T_DATETIME) :: tDT
    real (kind=T_SGL) :: rMeanDischarge
    character (len=10) :: sDataFlag
  end type T_USGS_NWIS_DAILY

  type T_USGS_NWIS_GAGE
    character (len=256) :: sAgencyCode
    character (len=256) :: sSiteNumber
    character (len=256) :: sDescription
    type (T_USGS_NWIS_DAILY), dimension(:), allocatable :: pGageData
  end type T_USGS_NWIS_GAGE

  interface uppercase
    module procedure uppercase_sub
    module procedure uppercase_fn
  end interface

  interface equals
    module procedure equals_int
    module procedure equals_real
    module procedure equals_dbl
  end interface

contains

!--------------------------------------------------------------------------

subroutine parse_text_to_date_sub(this, sString, sDateFormat)

  class (T_DATETIME), intent(inout) :: this
  character (len=*), intent(in) :: sString
  character (len=*), intent(in), optional :: sDateFormat

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iMonth
  integer (kind=T_INT) :: iDay
  integer (kind=T_INT) :: iYear
  integer (kind=T_INT) :: iMonthOffset, iDayOffset
  character (len=DATETEXTLENGTH) :: sDateFmt
  character (len=DATETEXTLENGTH) :: sStr
  character (len=256) :: sMonth, sDay, sYear, sBuf

  integer (kind=T_INT) :: iScanMM1, iScanMM2
  integer (kind=T_INT) :: iScanDD1, iScanDD2
  integer (kind=T_INT) :: iScanYYYY1, iScanYYYY2

  if(present(sDateFormat)) then
    sDateFmt = uppercase( trim(adjustl(sDateFormat)) )
  else
    sDateFmt = "MM/DD/YYYY"
  endif

  ! these offset amounts have value of 1 if the program detects a single-digit date value
  iMonthOffset = 0; iDayOffset = 0

  sStr = trim(adjustl(sString))

  iScanMM1 = scan(string=sDateFmt,set="M")
  iScanMM2 = scan(string=sDateFmt,set="M", back=lTRUE )
  sMonth = sStr(iScanMM1 : iScanMM2 )
  sBuf = clean(sMonth)
  if(len_trim(sBuf) /= len_trim(sMonth)) then   ! we have a case where there is no leading zero
    iMonthOffset = 1
    sMonth = trim(sBuf)
  endif
  read(sMonth,fmt=*, iostat = iStat) iMonth
  call Assert(iStat==0, "Error parsing month value from text file - got "//trim(sBuf)//";"// &
  " date format: "//trim(sDateFmt)//" | date text: "//trim(sStr), &
    TRIM(__FILE__),__LINE__)

  iScanDD1 = scan(string=sDateFmt,set="D") - iMonthOffset
  iScanDD2 = scan(string=sDateFmt,set="D", back=lTRUE ) - iMonthOffset
  sDay = sStr( iScanDD1 : iScanDD2 )
  sBuf = clean(sDay)
  if(len_trim(sBuf) /= len_trim(sDay)) then   ! we have a case where there is no leading zero
    iDayOffset = 1
    sDay = trim(sBuf)
  endif
  read(sDay, fmt=*, iostat = iStat) iDay
  call Assert(iStat==0, "Error parsing day value from text file - got "//trim(sBuf)//";"// &
  " date format: "//trim(sDateFmt)//" | date text: "//trim(sStr), &
    TRIM(__FILE__),__LINE__)

  iScanYYYY1 = scan(string=sDateFmt,set="Y") - iMonthOffset - iDayOffset
  iScanYYYY2 = scan(string=sDateFmt,set="Y", back=lTRUE ) - iMonthOffset - iDayOffset
  sBuf = sStr( iScanYYYY1 : iScanYYYY2 )
  read(sBuf,fmt=*, iostat = iStat) iYear
  call Assert(iStat==0, "Error parsing year value from text file - got "//trim(sBuf)//";"// &
  " date format: "//trim(sDateFmt)//" | date text: "//trim(sStr), &
    TRIM(__FILE__),__LINE__)

  if(iYear <= 99 ) iYear = iYear + 1900    ! this might be a lethal assumption

  this%iMonth = iMonth
  this%iYear = iYear
  this%iDay = iDay

end subroutine parse_text_to_date_sub


subroutine parse_text_to_time_sub(this, sString, sTimeFormat)

  class (T_DATETIME), intent(inout) :: this
  character (len=*), intent(in) :: sString
  character (len=*), intent(in), optional :: sTimeFormat

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iHour
  integer (kind=T_INT) :: iMinute
  integer (kind=T_INT) :: iSecond
  integer (kind=T_INT) :: iOffset

  character (len=DATETEXTLENGTH) :: sHour, sMinute, sSecond


  character (len=DATETEXTLENGTH) :: sTimeFmt
  character (len=DATETEXTLENGTH) :: sStr
  character (len=256) :: sBuf

  iOffset = 0

  if(present(sTimeFormat)) then
    sTimeFmt = uppercase( trim(adjustl(sTimeFormat)) )
  else
    sTimeFmt = "HH:MM:SS"
  endif

  sStr = trim(adjustl(sString))

  sHour =   sStr(scan(string=sTimeFmt,set="H") : scan(string=sTimeFmt,set="H", back=lTRUE ) )

  sBuf = clean(sHour)
  if(len_trim(sBuf) /= len_trim(sHour)) then   ! we have a case where there is no leading zero
    iOffset = 1
    sHour = trim(sBuf)
  endif

  read(sHour,fmt=*, iostat = iStat) iHour
  call Assert(iStat==0, "Error parsing hours value from format string: '"//trim(sTimeFmt)//"'", &
    TRIM(__FILE__),__LINE__)

  sMinute = sStr(scan(string=sTimeFmt,set="M")-iOffset : scan(string=sTimeFmt,set="M", back=lTRUE )-iOffset )
  read(sMinute,fmt=*, iostat = iStat) iMinute
  call Assert(iStat==0, "Error parsing minutes value from format string: '"//trim(sTimeFmt)//"'", &
    TRIM(__FILE__),__LINE__)

  if(scan(string=sTimeFmt,set="S") /= 0) then
    sSecond = sStr(scan(string=sTimeFmt,set="S")-iOffset : scan(string=sTimeFmt,set="S", back=lTRUE )-iOffset )
    read(sSecond,fmt=*, iostat = iStat) iSecond
    call Assert(iStat==0, "Error parsing seconds value from format string: '"//trim(sTimeFmt)//"'", &
      TRIM(__FILE__),__LINE__)
  else
    iSecond = 0
  endif

  this%iHour = iHour
  this%iMinute = iMinute
  this%iSecond = iSecond

end subroutine parse_text_to_time_sub

!--------------------------------------------------------------------------

subroutine calc_water_year_sub(this)

  class (T_DATETIME) :: this

    if(this%iMonth > 9) then
       this%iWaterYear = this%iYear + 1
    else
       this%iWaterYear = &
       this%iYear
    end if


end subroutine calc_water_year_sub

!--------------------------------------------------------------------------

subroutine populate_julian_day_sub(this, iMonth, iDay, iYear, &
                                iHour, iMinute, iSecond)

  class (T_DATETIME) :: this
  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iDay
  integer (kind=T_INT), intent(in) :: iYear
  integer (kind=T_INT), intent(in) :: iHour
  integer (kind=T_INT), intent(in) :: iMinute
  integer (kind=T_INT), intent(in) :: iSecond

  ! [LOCALS]
!  integer (kind=T_INT) :: iJulianDay
!  real (kind=T_DBL) :: rFractionOfDay

  this%iMonth = iMonth
  this%iDay = iDay
  this%iYear = iYear
  this%iHour = iHour
  this%iMinute = iMinute
  this%iSecond = iSecond

!  this%iJulianDay = julian_day( this%iYear, this%iMonth, this%iDay)
  this%iJulianDay = julian_day( int(this%iYear, kind=T_INT), &
                                int(this%iMonth, kind=T_INT), &
                                int(this%iDay, kind=T_INT))

  this%rFractionOfDay = real(this%iHour, kind=T_DBL) / 24_T_DBL + &
                   real(this%iMinute, kind=T_DBL) / 1440_T_DBL + &
                   real(this%iSecond, kind=T_DBL) / 86400_T_DBL

!  this%rJulianDay = real(iJulianDay, kind=T_DBL) + &
!                            rFractionOfDay

end subroutine populate_julian_day_sub

!--------------------------------------------------------------------------

subroutine calc_julian_day_sub(this)

  class (T_DATETIME) :: this

  ! [LOCALS]
!  integer (kind=T_INT) :: iJulianDay
!  real (kind=T_DBL) :: rFractionOfDay

  this%iJulianDay = julian_day( int(this%iYear, kind=T_INT), &
                                int(this%iMonth, kind=T_INT), &
                                int(this%iDay, kind=T_INT))
  this%rFractionOfDay = real(this%iHour, kind=T_DBL) / 24_T_DBL + &
                   real(this%iMinute, kind=T_DBL) / 1440_T_DBL + &
                   real(this%iSecond, kind=T_DBL) / 86400_T_DBL

!  this%rJulianDay = real(iJulianDay, kind=T_DBL) + &
!                            rFractionOfDay - 2400000.5_T_DBL

end subroutine calc_julian_day_sub

!!***

!--------------------------------------------------------------------------
!!****f* types/gregorian_date
! NAME
!   gregorian_date - Convert from a Julian day number to a Gregorian date.
!
! SYNOPSIS
!   Conversion to a Gregorian calendar date from a Julian date.
!   Valid for any Gregorian calendar date producing a Julian day number
!   greater than zero.
!
! INPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
! OUTPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! NOTES
!   Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
!   Communications of the ACM, Vol. 11, No. 10 (October, 1968).
!   Modified from code found at:
!       http://aa.usno.navy.mil/faq/docs/JD_Formula.html
!
! SOURCE

subroutine gregorian_date(iJD, iYear, iMonth, iDay, iOrigin)

!! COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY)
!! GIVEN THE JULIAN DATE (JD).

  ! [ ARGUMENTS ]
  integer (kind=T_INT) :: iJD
  integer (kind=T_INT), intent(inout) :: iYear, iMonth, iDay
  integer (kind=T_INT), optional :: iOrigin
  ! [ LOCALS ]
  integer (kind=T_INT) iI,iJ,iK,iL,iN
  integer (kind=T_INT) :: iOffset

  if(present(iOrigin)) then
    iOffset = iOrigin
  else
    iOffset = 0
  endif

  ! allow for an alternate "origin" to be specified... technically,
  ! this is no longer a "Julian" day, but alas... This modification
  ! was required in order to process the "time" variables from global
  ! climate models, which seem to be defined as something like this:
  ! time:units = "days since 1960-01-01 00:00:00"
  !
  ! for the above example, JD = 2436935 on the first day; the NetCDF "time"
  ! variable will be equal to 0.  Thus, in order to get the conversion
  ! right, we must add 0 + 2436935 to yield a true Julian Day.

  iJD = iJD + iOffset

  iL= iJD + 68569_T_INT
  iN= 4*iL / 146097_T_INT
  iL= iL - (146097_T_INT * iN + 3_T_INT)/4_T_INT
  iI= 4000_T_INT * (iL + 1_T_INT) / 1461001_T_INT
  iL= iL - 1461_T_INT * iI / 4_T_INT + 31_T_INT
  iJ= 80_T_INT * iL / 2447_T_INT
  iK= iL - 2447_T_INT * iJ / 80_T_INT
  iL= iJ / 11_T_INT
  iJ= iJ + 2_T_INT - 12_T_INT * iL
  iI= 100_T_INT * (iN - 49_T_INT) + iI + iL

  iYear = iI
  iMonth = iJ
  iDay = iK

  return

end subroutine gregorian_date

subroutine uppercase_sub ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s
  ! LOCALS
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT) :: LOWER_TO_UPPER = ichar( "A" ) - ichar( "a" )

  do i=1,len_trim(s)
      if ( ichar(s(i:i) ) >= ichar("a") .and. ichar(s) <= ichar("z") ) then
          s(i:i) = char( ichar( s(i:i) ) + LOWER_TO_UPPER )
      end if
  end do

  return
end subroutine uppercase_sub

elemental function uppercase_fn ( s )                               result(sOut)

  ! ARGUMENTS
  character (len=*), intent(in) :: s
  character(len=len(s)) :: sOut
  ! LOCALS
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT) :: LOWER_TO_UPPER

  LOWER_TO_UPPER = ichar( "A" ) - ichar( "a" )

  sOut = s

  do i=1,len_trim(sOut)
      if ( ichar(sOut(i:i) ) >= ichar("a") .and. ichar(sOut) <= ichar("z") ) then
          sOut(i:i) = char( ichar( sOut(i:i) ) + LOWER_TO_UPPER )
      end if
  end do

  return
end function uppercase_fn


!--------------------------------------------------------------------------
!!****s* types/Assert
! NAME
!   Assert - General-purpose error-checking routine.
!
! SYNOPSIS
!   General-purpose error-checking routine. If lCondition is .false.,
!   prints the error message and stops!
!
! INPUTS
!   lCondition - statement that evaluates to a logical .true. or .false value.
!   sErrorMessage - accompanying error message to print if lCondition is .false.
!   sFilename - name of the offending file; populate with the C compiler
!                 preprocessor macro __FILE__
!   sLineNo - line number of error; populate with preprocessor macro __LINE__
!
! OUTPUTS
!   NONE
!
! SOURCE

subroutine Assert(lCondition,sErrorMessage,sFilename,iLineNo)

  ! ARGUMENTS
  logical (kind=T_LOGICAL), intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage
  character (len=*), optional :: sFilename
  integer (kind=T_INT), optional :: iLineNo

  logical :: lFileOpen

  if ( .not. lCondition ) then

    if(lAssertAlwaysFatal) then
      print *,'FATAL ERROR - HALTING TSPROC'
      print *,trim(sErrorMessage)
      print *, " "
      if(present(sFilename)) print *,"module: ", trim(sFilename)
      if(present(iLineNo)) print *,"line no.: ",iLineNo
    else
      print *, "TSPROC error:"
      print *,trim(sErrorMessage)
    endif

      ! echo error condition to the log file ONLY if it is open!
      !
      ! ACHTUNG!! inquire is poorly supported under GFORTRAN - need to look at alternatives
      !
      inquire (unit=LU_REC, opened=lFileOpen)

      if(lFileOpen) then

        if(lAssertAlwaysFatal) then
          write(UNIT=LU_REC,FMT="(a)") 'FATAL ERROR - HALTING TSPROC'
          write(UNIT=LU_REC,FMT="(a,/)") trim(sErrorMessage)
          if(present(sFilename)) write(UNIT=LU_REC,FMT=*) "module: ", &
             trim(sFilename)
          if(present(iLineNo)) write(UNIT=LU_REC,FMT=*) "line no.: ",iLineNo
        else
          write(UNIT=LU_REC,FMT="(a)") 'TSPROC error'
          write(UNIT=LU_REC,FMT="(a,/)") trim(sErrorMessage)
        endif

      end if

    if(lAssertAlwaysFatal) stop

  end if

end subroutine Assert

!--------------------------------------------------------------------------

subroutine Warn(lCondition,sWarningMessage,sFilename,iLineNo)

  ! ARGUMENTS
  logical (kind=T_LOGICAL), intent(in) :: lCondition
  character (len=*), intent(in) :: sWarningMessage
  character (len=*), optional :: sFilename
  integer (kind=T_INT), optional :: iLineNo
  logical :: lFileOpen

  if ( .not. lCondition ) then
      print *,' *** WARNING *** WARNING ***'
      print *,trim(sWarningMessage)
      print *, " "
      if(present(sFilename)) print *,"filename: ", trim(sFilename)
      if(present(iLineNo)) print *,"line no.: ",iLineNo

      ! echo error condition to the log file ONLY if it is open!
      inquire (unit=LU_REC, opened=lFileOpen)
      if(lFileOpen) then

        write(UNIT=LU_REC,FMT=*) ' *** WARNING *** WARNING ***'
        write(UNIT=LU_REC,FMT=*) trim(sWarningMessage)
        write(UNIT=LU_REC,FMT=*) " "
        if(present(sFilename)) write(UNIT=LU_REC,FMT=*) "filename: ", &
           trim(sFilename)
        if(present(iLineNo)) write(UNIT=LU_REC,FMT=*) "line no.: ",iLineNo

      end if
  end if

  return
end subroutine Warn

!--------------------------------------------------------------------------
!!****f* types/julian_day
! NAME
!   julian_day - Convert from a Gregorian calendar date to a Julian day number.
!
! SYNOPSIS
!   Conversion from a Gregorian calendar date to a Julian day number.
!   Valid for any Gregorian calendar date producing a Julian day
!   greater than zero.
!
! INPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! OUTPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
!
! SOURCE

function julian_day ( iYear, iMonth, iDay, iOrigin ) result(iJD)

  ! [ ARGUMENTS ]
  integer (kind=T_INT), intent(in) :: iYear, iMonth, iDay
  integer (kind=T_INT), optional :: iOrigin

  ! [ LOCALS ]
  integer (kind=T_INT) i,j,k
  integer (kind=T_INT) :: iOffset

  ! [ RETURN VALUE ]
  integer (kind=T_INT) :: iJD

  i= iYear
  j= iMonth
  k= iDay

  call Assert(iMonth >= 1 .and. iMonth <= 12, "Illegal month value given", &
     TRIM(__FILE__), __LINE__)
  call Assert(iDay >= 1 .and. iDay <= 31, "Illegal day value given", &
     TRIM(__FILE__), __LINE__)

  if(present(iOrigin)) then
    iOffset = iOrigin
  else
    iOffset = 0
  endif

  iJD= ( k-32075_T_INT + 1461_T_INT * (i + 4800_T_INT + (j - 14_T_INT) / 12_T_INT) &
        /4_T_INT + 367_T_INT * (j - 2_T_INT - (j - 14_T_INT)/ 12_T_INT * 12_T_INT) &
        /12_T_INT - 3_T_INT *((i + 4900_T_INT + (j - 14_T_INT) &
        /12_T_INT)/100_T_INT)/4_T_INT ) - iOffset

  return

end function julian_day

!------------------------------------------------------------------------------

function clean(sRecord,sDelimiters)                       result(sItem)

  ! ARGUMENTS
  character (len=*), intent(inout)           :: sRecord
  character (len=*), intent(in), optional    :: sDelimiters

  ! LOCALS
  character (len=256) :: sItem
  integer (kind=T_INT) :: iR                 ! Index in sRecord
  integer (kind=T_INT) :: i, j

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)
  sItem = ""
  j = 0

  do i = 1,len_trim(sRecord)

    if(present(sDelimiters)) then
      iR = SCAN(sRecord(i:i),sDelimiters)
    else
      iR = SCAN(sRecord(i:i),":/;,")
    endif

    if(iR==0) then
      j = j + 1
      sItem(j:j) = sRecord(i:i)
    end if

  enddo

end function clean

function is_date_greater_than(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if(date2%iJulianDay == date1%iJulianDay &
     .and. date1%rFractionOfDay > date2%rFractionOfDay) then
     lResult = lTRUE
  elseif(date1%iJulianDay > date2%iJulianDay) then
    lResult = lTRUE
  endif

end function is_date_greater_than

!------------------------------------------------------------------------------

function is_date_less_than(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if(date1%iJulianDay == date2%iJulianDay &
     .and. date1%rFractionOfDay < date2%rFractionOfDay) then
     lResult = lTRUE
  elseif(date1%iJulianDay < date2%iJulianDay) then
    lResult = lTRUE
  endif

end function is_date_less_than

!------------------------------------------------------------------------------

function is_date_LT_or_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult
  real (kind=T_SGL) :: rFractionOfDay1
  real (kind=T_SGL) :: rFractionOfDay2

  rFractionOfDay1 = date1%rFractionOfDay
  rFractionOfDay2 = date2%rFractionOfDay

  lResult = lFALSE

  if(date1%iJulianDay == date2%iJulianDay &
     .and. equals(rFractionOfDay1, rFractionOfDay2)) then
     lResult = lTRUE
  elseif(date1%iJulianDay == date2%iJulianDay &
     .and. date1%rFractionOfDay < date2%rFractionOfDay) then
     lResult = lTRUE
  elseif(date1%iJulianDay < date2%iJulianDay) then
    lResult = lTRUE
  endif

end function is_date_LT_or_equal_to

!------------------------------------------------------------------------------

function is_date_GT_or_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult
  real (kind=T_SGL) :: rFractionOfDay1
  real (kind=T_SGL) :: rFractionOfDay2

  rFractionOfDay1 = date1%rFractionOfDay
  rFractionOfDay2 = date2%rFractionOfDay

  lResult = lFALSE

  if(date1%iJulianDay == date2%iJulianDay &
     .and. equals(rFractionOfDay1, rFractionOfDay2)) then
     lResult = lTRUE
  elseif(date1%iJulianDay == date2%iJulianDay &
     .and. date1%rFractionOfDay > date2%rFractionOfDay) then
     lResult = lTRUE
  elseif(date1%iJulianDay > date2%iJulianDay) then
    lResult = lTRUE
  endif

end function is_date_GT_or_equal_to

!------------------------------------------------------------------------------

function is_date_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if(date1%iJulianDay == date2%iJulianDay .and. &
             date1%iHour == date2%iHour .and. &
             date1%iMinute == date2%iMinute .and. &
             date1%iSecond == date2%iSecond) then

     lResult = lTRUE

  endif

end function is_date_equal_to

!------------------------------------------------------------------------------

function date_subtract_fn(date1, date2)  result(rDelta)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  real (kind=T_DBL) :: rDelta

  rDelta = real(date1%iJulianDay, kind=T_DBL) - real(date2%iJulianDay, kind=T_DBL) &
           + real(date1%rFractionOfDay, kind=T_DBL) &
           - real(date2%rFractionOfDay, kind=T_DBL)

end function date_subtract_fn

!------------------------------------------------------------------------------

subroutine openlog(sFilename)

  character(len=*), intent(in), optional :: sFilename

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat
  character(len=256) :: sDateStr, sDateStrPretty

  LU_REC = getNextLogicalUnit()

  if(present(sFilename)) then
    open(unit=LU_REC, file=trim(sFilename),status='REPLACE',iostat=iStat)
  else
    call GetSysTimeDate(sDateStr,sDateStrPretty)
    open(unit=LU_REC, file="tsproc_logfile_"//trim(sDateStr)//".txt", &
          status='REPLACE',iostat=iStat)
  end if

  call Assert(iStat==0, "Problem opening TSPROC logfile", trim(__FILE__), __LINE__)

end subroutine openlog

!------------------------------------------------------------------------------

subroutine writelog(sMessage, sFormat)

  character(len=*), intent(in)             :: sMessage
  character(len=*), intent(in), optional   :: sFormat

  ! [ LOCALS ]
  character (len=256) :: sFormatString = ""

  if(present(sFormat)) then
    sFormatString = '('//trim(sFormat)//')'
  else
    sFormatString = '(a)'
  endif

  write(unit=LU_STD_OUT, fmt=trim(sFormatString)) sMessage
  write(unit=LU_REC, fmt=trim(sFormatString)) sMessage

end subroutine writelog

!------------------------------------------------------------------------------

subroutine closelog()

  close(unit=LU_REC)

end subroutine closelog

!------------------------------------------------------------------------------

subroutine GetSysTimeDate(sDateStr,sDateStrPretty)

  character(len=256), intent(out) :: sDateStr, sDateStrPretty

  character (len=256) :: sRecord
  character (len=256) :: sItem
  character (len=256) :: sDay
  character (len=256) :: sMon
  character (len=256) :: sDD
  character (len=8) :: sHH
  character (len=8) :: sMM
  character (len=8) :: sSS
  character (len=256) :: sTime
  character (len=256) :: sYear

  sRecord = FDATE()

  call chomp(sRecord,sDay)
  call chomp(sRecord,sMon)
  call chomp(sRecord,sDD)
  call chomp(sRecord,sTime)
  call chomp(sRecord,sYear)

  sHH = sTime(1:2)
  sMM = sTime(4:5)
  sSS = sTime(7:8)

  sDateStr = TRIM(sDD)//"_"//TRIM(sMon)//"_"//TRIM(sYear)//"__"//&
    TRIM(sHH)//"_"//TRIM(sMM)
  sDateStrPretty = &
    TRIM(sDay)//" "//TRIM(sMon)//" "//TRIM(sDD)//" "//TRIM(sYear)//" " &
     //TRIM(sHH)//":"//TRIM(sMM)

  return

end subroutine GetSysTimeDate

!------------------------------------------------------------------------------

subroutine Chomp(sRecord, sItem, sDelimiters)

  ! ARGUMENTS
  character (len=*), intent(inout)           :: sRecord
  character (len=256), intent(out)           :: sItem
  character (len=*), intent(in), optional    :: sDelimiters
  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)

  if(present(sDelimiters)) then
    iR = SCAN(sRecord,sDelimiters)
  else
    iR = SCAN(sRecord," ")
  endif

  if(iR==0) then
    sItem = trim(sRecord)   ! no delimiters found; return entirety of sRecord
    sRecord = ""            ! as sItem
  else
    sItem = trim(sRecord(1:iR-1))
    sRecord = trim(adjustl(sRecord(iR+1:)))
  end if

  return
end subroutine Chomp

!------------------------------------------------------------------------------

integer function getNextLogicalUnit()

! -- Function nextunit determines the lowest unit number available for
! -- opening.

! -- Revision history:-
!       June-November, 1995: version 1.

	logical::lopen
   integer :: i

	do i=10,200
	  inquire(unit=i,opened=lopen)
	  if(.not.lopen) return
	end do
	write(6,10)
10      format(' *** No more unit numbers to open files ***')
	stop

end function getNextLogicalUnit

subroutine print_date_time_object_sub(this)

  class(T_DATETIME) :: this
  character (len=256) :: sDateText

  write(sDateText, fmt="(i2.2,'/',i2.2,'/',i4.4,1x,i2.2,':',i2.2,':',i2.2)") &
    this%iMonth, this%iDay, this%iYear, this%iHour, this%iMinute, this%iSecond

  call writelog(sDateText)

end subroutine print_date_time_object_sub

!------------------------------------------------------------------------------

function write_pretty_date_fn(this)     result(sDateText)

  class(T_DATETIME) :: this
  character (len=10) :: sDateText

  write(sDateText, fmt="(i2.2,'/',i2.2,'/',i4.4)") &
    this%iMonth, this%iDay, this%iYear

end function write_pretty_date_fn

!------------------------------------------------------------------------------

function write_list_date_fn(this)                     result(sDateText)

  class(T_DATETIME) :: this
  character (len=10) :: sDateText

  sDateText = this%listdatetime(sDateFormat="MM/DD/YYYY", lDateOnly = lTRUE)

end function write_list_date_fn

!------------------------------------------------------------------------------

function write_list_time_fn(this)                     result(sTimeText)

  class(T_DATETIME) :: this
  character (len=8) :: sTimeText

  write(sTimeText,fmt="(i2.2,':',i2.2':',i2.2)") this%iHour, this%iMinute, this%iSecond

end function write_list_time_fn

!------------------------------------------------------------------------------

function write_list_datetime_fn(this, sDateFormat, lDateOnly)    result(sDatetimeText)

  class(T_DATETIME) :: this
  character(len=*), optional :: sDateFormat
  logical (kind=T_LOGICAL), optional :: lDateOnly
  character (len=25) :: sDatetimeText

  ! [ LOCALS ]
  character(len=25) sDateFmt
  integer (kind=T_INT) :: iScanMM1, iScanMM2
  integer (kind=T_INT) :: iScanDD1, iScanDD2
  integer (kind=T_INT) :: iScanYYYY1, iScanYYYY2
  integer (kind=T_INT) :: iScanDelim1, iScanDelim2
  character (len=32) :: sBuf
  character (len=6), parameter :: DELIMITERS = "/-_\. "
  integer (kind=T_INT), dimension(5) :: iStat
  logical (kind=T_LOGICAL) lListTime

  sDateTimeText = repeat(" ",25)

  if(present(sDateFormat)) then
    sDateFmt = uppercase(trim(adjustl(sDateFormat)))
  else
    sDateFmt = "MM/DD/YYYY"
  endif

  if(present(lDateOnly)) then
    lListTime = .not. lDateOnly
  else
    lListTime = lTRUE
  endif

  iScanMM1 = scan(string=sDateFmt,set="M")
  iScanMM2 = scan(string=sDateFmt,set="M", back=lTRUE )

  iScanDD1 = scan(string=sDateFmt,set="D")
  iScanDD2 = scan(string=sDateFmt,set="D", back=lTRUE )

  iScanYYYY1 = scan(string=sDateFmt,set="Y")
  iScanYYYY2 = scan(string=sDateFmt,set="Y", back=lTRUE )

  iScanDelim1 = scan(string=trim(sDateFmt), set=DELIMITERS)
  iScanDelim2 = scan(string=trim(sDateFmt), set=DELIMITERS, back=lTRUE)

  write(sDateTimeText(iScanMM1:iScanMM2),fmt="(i2.2)", iostat=iStat(1)) this%iMonth
  write(sDateTimeText(iScanDD1:iScanDD2),fmt="(i2.2)", iostat=iStat(2)) this%iDay
  write(sDateTimeText(iScanYYYY1:iScanYYYY2),fmt="(i4.4)",iostat=iStat(3)) this%iYear
  if(iScanDelim1 > 0) write(sDateTimeText(iScanDelim1:iScanDelim1), &
     fmt="(a1)",iostat=iStat(4)) &
     sDateFmt(iScanDelim1:iScanDelim1)
  if(iScanDelim2 > 0) write(sDateTimeText(iScanDelim2:iScanDelim2), &
     fmt="(a1)",iostat=iStat(5)) &
     sDateFmt(iScanDelim2:iScanDelim2)

  call Assert(all(iStat==0),"Problem parsing the date format '"// &
     trim(sDateFmt)//"' for output", &
    trim(__FILE__), __LINE__)

  write(sBuf,fmt="(3x,i2.2,':',i2.2':',i2.2)") this%iHour, this%iMinute, this%iSecond

  if(lListTime) then
    sDateTimeText = trim(sDateTimeText) // trim(sBuf)
  else
    sDateTimeText = trim(sDateTimeText)
  endif

end function write_list_datetime_fn

function equals_int(i1,i2)              result(lBool)

  integer (kind=T_INT), intent(in)   :: i1
  integer (kind=T_INT), intent(in)   :: i2
  logical (kind=T_LOGICAL) :: lBool

  lBool = (i1 == i2)

end function equals_int

!------------------------------------------------------------------------------

function equals_real(r1,r2)             result(lBool)

  real (kind=T_SGL) , intent(in)      :: r1
  real (kind=T_SGL) , intent(in)      :: r2
  logical (kind=T_LOGICAL) :: lBool

  ! [ LOCALS ]
  real (kind=T_SGL) :: rtemp

  ! intrinsic function 'spacing' returns the absolute spacing
  ! of the numbers near X in the model used to represent real numbers.
  ! therefore, if the two numbers are within a factor of three of the
  ! smallest representable numbers near X in real number space, conclude the
  ! numbers are equal
  rtemp=abs( 3.0 * spacing(r1) )
  if( abs( r1 - r2 ) < rtemp )then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function equals_real

!------------------------------------------------------------------------------

function equals_dbl(r1,r2)             result(lBool)

  real (kind=T_DBL) , intent(in)      :: r1
  real (kind=T_DBL) , intent(in)      :: r2
  logical (kind=T_LOGICAL) :: lBool

  ! [ LOCALS ]
  real (kind=T_DBL) :: rtemp

  ! intrinsic function 'spacing' returns the absolute spacing
  ! of the numbers near X in the model used to represent real numbers.
  ! therefore, if the two numbers are within a factor of three of the
  ! smallest representable numbers near X in real number space, conclude the
  ! numbers are equal
  rtemp=abs( 3.0 * spacing(r1) )
  if( abs( r1 - r2 ) < rtemp )then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function equals_dbl


end module tsp_data_structures
