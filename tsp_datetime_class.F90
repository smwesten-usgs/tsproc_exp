module tsp_datetime_class

  use tsp_data_structures
  use tsp_utilities
  implicit none

  type T_DATETIME

    integer (kind=T_BYTE) :: iMonth = 1
    integer (kind=T_BYTE) :: iDay = 1
    integer (kind=T_SHORT) :: iYear = 1
    integer (kind=T_BYTE) :: iHour = 0
    integer (kind=T_BYTE) :: iMinute = 0
    integer (kind=T_BYTE) :: iSecond = 0
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
    procedure :: is_date_greater_than

    ! define operators that will work with datetime objects
    generic :: operator(>) => is_date_greater_than
    procedure :: is_date_less_than
    generic :: operator(<) => is_date_less_than
    procedure :: is_date_GT_or_equal_to
    generic :: operator(>=) => is_date_GT_or_equal_to
    procedure :: is_date_LT_or_equal_to
    generic :: operator(<=) => is_date_LT_or_equal_to
    procedure :: is_date_equal_to
    generic :: operator(==) => is_date_equal_to
    procedure :: date_subtract_fn
    generic :: operator(-) => date_subtract_fn

    procedure, public :: prettydate => write_pretty_date_fn
    procedure, public :: listdatetime => write_list_datetime_fn
    procedure, public :: listdate => write_list_date_fn
    procedure, public :: listtime => write_list_time_fn
    procedure, public :: systime => system_time_to_date_sub
    procedure, public :: getJulianDay => get_julian_day_float_fn

  end type T_DATETIME


  type T_DATERANGE

    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate

  contains

    procedure, public :: new_daterange_fm_text_sub, new_daterange_fm_datetime_sub
    generic :: new => new_daterange_fm_text_sub, new_daterange_fm_datetime_sub

  end type T_DATERANGE


  type T_MONTH
    character (len=3) :: sAbbreviation
    character (len=12) :: sName
  end type T_MONTH

  ! constants for converting month numbers to month names
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

contains

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

!------------------------------------------------------------------------------

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

!  this%rJulianDay = real(iJulianDay, kind=T_DBL) + rFractionOfDay !&
!                                     - 2400000.5_T_DBL

  ! 2400000.5 is subtracted to yield one definition of a "MODIFIED JUILAN DAY"

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

!  this%rJulianDay = real(iJulianDay, kind=T_DBL) + rFractionOfDay ! - 2400000.5_T_DBL

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
  character (len=256) :: sBuf

  ! [ RETURN VALUE ]
  integer (kind=T_INT) :: iJD

  i= iYear
  j= iMonth
  k= iDay

  if(.not. (iMonth >= 1 .and. iMonth <= 12)) then
    write(sBuf,fmt="('Illegal month value given: ',i4)") iMonth
    call Assert( lFALSE, trim(sBuf), TRIM(__FILE__), __LINE__)
  elseif(.not. (iDay >= 1 .and. iDay <= 31)) then
    write(sBuf,fmt="('Illegal day value given: ',i4)") iDay
    call Assert( lFALSE, trim(sBuf), TRIM(__FILE__), __LINE__)
  endif

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

function is_date_greater_than(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

!  if(date2%iJulianDay == date1%iJulianDay &
!     .and. date1%rFractionOfDay > date2%rFractionOfDay) then
!     lResult = lTRUE
!  elseif(date1%iJulianDay > date2%iJulianDay) then
!    lResult = lTRUE
!  endif

  if( date1%getJulianDay() > date2%getJulianDay() )  lResult = lTRUE

end function is_date_greater_than

!------------------------------------------------------------------------------

function is_date_less_than(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

!  if(date1%iJulianDay == date2%iJulianDay &
!     .and. date1%rFractionOfDay < date2%rFractionOfDay) then
!     lResult = lTRUE
!  elseif(date1%iJulianDay < date2%iJulianDay) then
!    lResult = lTRUE
!  endif

  if( date1%getJulianDay() < date2%getJulianDay() )  lResult = lTRUE

end function is_date_less_than

!------------------------------------------------------------------------------

function is_date_LT_or_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if( date1%getJulianDay() <= date2%getJulianDay() )  lResult = lTRUE

end function is_date_LT_or_equal_to

!------------------------------------------------------------------------------

function is_date_GT_or_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if( date1%getJulianDay() >= date2%getJulianDay() )  lResult = lTRUE

end function is_date_GT_or_equal_to

!------------------------------------------------------------------------------

function is_date_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if( date1%iJulianDay == date2%iJulianDay .and. &
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

!  rDelta = ( real(date1%iJulianDay, kind=T_DBL) + real(date1%rFractionOfDay, kind=T_DBL) ) &
!     - ( real(date2%iJulianDay, kind=T_DBL) + real(date2%rFractionOfDay, kind=T_DBL) )

  rDelta = date1%getJulianDay() - date2%getJulianDay()

end function date_subtract_fn

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

  write(sBuf,fmt="(1x,i2.2,':',i2.2':',i2.2)") this%iHour, this%iMinute, this%iSecond

  if(lListTime) then
    sDateTimeText = trim(sDateTimeText) // trim(sBuf)
  else
    sDateTimeText = trim(sDateTimeText)
  endif

end function write_list_datetime_fn

!------------------------------------------------------------------------------

subroutine system_time_to_date_sub(this)

  class (T_DATETIME) :: this

  ! [ LOCALS ]
  character (len=16) :: sDateText
  character (len=16) :: sTimeText
  integer (kind=T_INT), dimension(8) :: iValues

  call DATE_AND_TIME(sDateText, sTimeText)
  call DATE_AND_TIME(VALUES = iValues)

  call this%parseDate(sDateText, "YYYYMMDD")
  call this%parseTime(sTimeText, "HHMMSS")
  call this%calcJulianDay()
  this%rFractionOfDay = this%rFractionOfDay + &
      (real(iValues(8), kind=T_DBL) / 86400_T_DBL / 1000_T_DBL) ! milliseconds

end subroutine system_time_to_date_sub

!------------------------------------------------------------------------------

subroutine new_daterange_fm_text_sub(this, sStartDate, sStartTime, sEndDate, sEndTime, &
     sDateFormat)

  class (T_DATERANGE) :: this
  character (len=*), intent(in) :: sStartDate
  character (len=*), intent(in) :: sStartTime
  character (len=*), intent(in) :: sEndDate
  character (len=*), intent(in) :: sEndTime
  character (len=*), intent(in), optional :: sDateFormat

  ! [ LOCALS ]
  character (len=14) :: sDateFormatText

  if(present(sDateFormat)) then
    sDateFormatText = trim(sDateFormat)
  else
    sDateFormatText = "MM/DD/YYYY"
  endif

  call this%tStartDate%parseDate(sStartDate, sDateFormatText)
  call this%tStartDate%parseTime(sStartTime)
  call this%tStartDate%calcJulianDay()

  call this%tEndDate%parseDate(sEndDate, sDateFormatText)
  call this%tEndDate%parseTime(sEndTime)
  call this%tEndDate%calcJulianDay()

end subroutine new_daterange_fm_text_sub

!--------------------------------------------------------------------------

subroutine new_daterange_fm_datetime_sub(this, tStartDate, tEndDate )

  class (T_DATERANGE) :: this
  type (T_DATETIME), intent(in) :: tStartDate
  type (T_DATETIME), intent(in) :: tEndDate

  ! [ LOCALS ]

  this%tStartDate = tStartDate
  this%tEndDate = tEndDate

end subroutine new_daterange_fm_datetime_sub

!--------------------------------------------------------------------------
!!****f* types/num_days_in_year
! NAME
!   num_days_in_year - Return the number of days in the given year.
!
! SYNOPSIS
!   This function simply returns the number of days given the current year.
!
! INPUTS
!   iYear   4-digit year
!
! OUTPUTS
!   iNumDaysInYear - integer number of days that have elapsed between
!                    January 1 and December 31 of the current year.
!
! SOURCE

function num_days_in_year(iYear) result(iNumDaysInYear)

  integer (kind=T_INT), intent(in) :: iYear

  ! [ LOCALS ]
  integer (kind=T_INT) :: iFirstDay, iLastDay, iNumDaysInYear

  iFirstDay = julian_day ( iYear, 1, 1 )
  iLastDay = julian_day ( iYear, 12, 31 )
  iNumDaysInYear = iLastDay - iFirstDay + 1

end function num_days_in_year

!--------------------------------------------------------------------------

function read_dates_file(sFilename)                           result(pDateRange)

  implicit none

  character (len=*), intent(in) :: sFilename
  type (T_DATERANGE), dimension(:), pointer :: pDateRange

  ! [ LOCALS ]
  integer (kind=T_INT) :: LU_DATES
  integer (kind=T_INT) :: iStat, iSize, i
  character (len=256) :: sRecord, sItem
  character (len=256) :: sStartDate, sStartTime, sEndDate, sEndTime
  character (len=2) :: sSet

  sSet = cTAB//" "   ! inform 'Chomp' that tabs *or* spaces are the delimiters

  open(newunit=LU_DATES, file=trim(sFilename),status='OLD',iostat=iStat)
  call Assert(iStat == 0, "Problem opening dates file "//trim(sFilename), &
     trim(__FILE__), __LINE__)

  i=0
  do
    read(LU_DATES,fmt=*, iostat=iStat) sRecord
    if(iStat /=0) exit
    i = i + 1
  enddo

  iSize = i
  allocate(pDateRange(iSize), stat = iStat)
  call Assert(iStat == 0, "Problem allocating memory for date range data structure", &
     trim(__FILE__), __LINE__)

  rewind(LU_DATES, iostat = iStat)
  call Assert(iStat == 0, "Problem rewinding dates file "//trim(sFilename), &
     trim(__FILE__), __LINE__)

  i=0
  do
    read(LU_DATES,fmt="(a)", iostat=iStat) sRecord
    if(iStat /=0) exit
    i = i + 1

    call Chomp(sRecord, sStartDate, sSet)
    call Chomp(sRecord, sStartTime, sSet)
    call Chomp(sRecord, sEndDate, sSet)
    sEndTime = trim(sRecord)

    call Assert(i <= iSize, "Attempt to access pointer beyond allocated range", &
      trim(__FILE__), __LINE__)
    call pDateRange(i)%new(sStartDate, sStartTime, sEndDate, sEndTime)

  enddo

  close(unit=LU_DATES)

end function read_dates_file

!--------------------------------------------------------------------------

!> \brief Return the number of days in the given year.
!!
!! This function simply returns the number of days given the current year.
function day_of_year(iJulianDay) result(iDOY)

  integer (kind=T_INT), intent(in) :: iJulianDay

  ! [ LOCALS ]
  integer (kind=T_INT) :: iFirstDay, iCurrDay, iDOY
  integer (kind=T_INT) :: iYear, iMonth, iDay

  ! first get the value for the current year
  call gregorian_date(iJulianDay, iYear, iMonth, iDay)

  ! now calculate the Julian day for the first of the year
  iFirstDay = julian_day ( iYear, 1, 1 )

  ! return the current day of the year
  iDOY = iJulianDay - iFirstDay + 1

  return

end function day_of_year

!--------------------------------------------------------------------------
!!****f* types/solstice
! NAME
!   solstice - Returns 0 normally, or a value >0 during solstice or equinox.
!
! SYNOPSIS
!    Returns the following:
!      0: non-solstice and non-equinox day
!      1: Vernal equinox
!      2: Summer Solstice
!      3: Autumnal equinox
!      4: Winter solstice
!
! INPUTS
!   iJD     Julian day value
!
! OUTPUTS
!   iSol    Code as described above
!
! SOURCE

function solstice (iJD)  result (iSol)

  ! [ ARGUMENTS ]
  integer (kind=T_INT), intent(in) :: iJD

  ! [ LOCALS ]
  integer (kind=T_INT) iMonth, iDay, iYear


  ! [ RETURN VALUE ]
  integer (kind=T_INT) :: iSol

  call gregorian_date(iJD, iYear, iMonth, iDay)

  if(iMonth==3 .and. iDay == 20) then
    iSol = 1
  elseif(iMonth==6 .and. iDay == 21) then
    iSol = 2
  elseif(iMonth==9 .and. iDay == 22) then
    iSol = 3
  elseif(iMonth==12 .and. iDay == 21) then
    iSol = 4
  else
    iSol = 0
  endif

  return

end function solstice

!------------------------------------------------------------------------------

function get_julian_day_float_fn(this)                   result(rJulianDay)

  class(T_DATETIME) :: this
  real (kind=T_DBL) :: rJulianDay

  rJulianDay = real(this%iJulianDay, kind=T_DBL) + real(this%rFractionOfDay,kind=T_DBL)

end function get_julian_day_float_fn

!------------------------------------------------------------------------------

end module tsp_datetime_class
