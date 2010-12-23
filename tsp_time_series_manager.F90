module tsp_time_series_manager

  use tsp_data_structures
  use tsp_utilities
  use tsp_statistics
  use tsp_datetime_class
  use tsp_control_file_ops
  implicit none

  ! define some useful named constants
  integer (kind=T_INT), parameter, public :: iDAILY_SERIES = 0
  integer (kind=T_INT), parameter, public :: iMONTHLY_SERIES = 1
  integer (kind=T_INT), parameter, public :: iMONTHLY_SUMMARY = 2
  integer (kind=T_INT), parameter, public :: iANNUAL_SUMMARY = 3

  integer (kind=T_INT), parameter, private :: iSUM = 0
  integer (kind=T_INT), parameter, private :: iMINIMUM = 1
  integer (kind=T_INT), parameter, private :: iMAXIMUM = 2
  integer (kind=T_INT), parameter, private :: iMEAN = 3
  integer (kind=T_INT), parameter, private :: iRANGE = 4
  integer (kind=T_INT), parameter, private :: iSTD_DEV = 5

  integer (kind=T_INT), parameter, private :: iCALENDAR = 0
  integer (kind=T_INT), parameter, private :: iWATER_YEAR_HIGH = 1
  integer (kind=T_INT), parameter, private :: iWATER_YEAR_LOW = 2

  character (len=15), dimension(0:3), parameter, public :: sSERIES_TYPE = &
    ["Daily series   ", "Monthly series ", "Monthly summary", "Annual summary "]

  character (len=9), dimension(0:5), parameter, private :: sSTATISTIC = &
    ["Sum      ","Minimum  ","Maximum  ","Mean     ","Range    ","Std. dev."]
  character (len=17), dimension(0:2), parameter, private :: sYEAR_TYPE = &
    ["(calendar year)  ", "(water year high)","(water year low) "]

  integer (kind=T_INT), parameter, private :: iSTART = 0
  integer (kind=T_INT), parameter, private :: iCENTER = 1
  integer (kind=T_INT), parameter, private :: iEND = 2

  ! define a derived type to hold series data
  type, public :: T_TIME_SERIES_DATA
!    integer (kind=T_INT) :: iWaterYear     = 0
    type (T_DATETIME) :: tDT
    real (kind=T_SGL)    :: rValue         = -HUGE(rZERO)
    character (len=10)   :: sDataFlag      = ""
    logical (kind=T_LOGICAL) :: lSelect = lTRUE   ! use this flag to select subset of active range
    logical (kind=T_LOGICAL) :: lInclude = lTRUE  ! use this flag to preselect by date range
    logical (kind=T_LOGICAL) :: lValid = lTRUE    ! use this flag indicate faulty or questionable values

  contains
!    procedure :: calcWaterYear => calc_water_year_sub
!    procedure :: textToDate => text_to_date_sub

  end type T_TIME_SERIES_DATA

  ! define a derived type to hold metadata concerning a time series
  type, public :: T_TIME_SERIES
    type (T_TIME_SERIES), pointer :: pNext => null()
    type (T_TIME_SERIES), pointer :: pPrevious => null()
    character (len=MAXNAMELENGTH) :: sSiteName = ""
    character (len=MAXNAMELENGTH) :: sSeriesName = ""
    character (len=256) :: sDescription = ""
    character (len=DATETEXTLENGTH) :: sDateFormat
    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate
    type (T_DATETIME) :: tSelectionStartDate
    type (T_DATETIME) :: tSelectionEndDate
    integer (kind=T_INT) :: iDataType = iDAILY_SERIES
    integer (kind=T_INT) :: iCurrentRecord = 0
    integer (kind=T_INT) :: iListOutputPosition = -999
    type (T_TIME_SERIES_DATA), dimension(:), allocatable :: tData
    procedure (stat_func_interface), pointer :: stat_func_ptr
    procedure (select_func_interface), pointer :: select_func_ptr

  contains

    procedure :: includeByDate => include_by_date_fn
    procedure :: selectByDate => select_by_date_fn
    procedure :: selectByValue => select_by_value_fn
    procedure :: selectByMonth => select_by_month_fn
    procedure :: selectByMonthAndYear => select_by_month_and_year_fn
    procedure :: selectByYear => select_by_year_fn
    procedure :: selectByWaterYearHigh => select_by_water_year_high_fn
    procedure :: selectByWaterYearLow => select_by_water_year_high_fn

    procedure :: statsCreateObject => ts_create_stats_object_fn
    procedure :: statByPeriod => ts_calc_stat_by_period_fn
    procedure :: statsByMonth => ts_calc_stats_by_month_fn
    procedure :: statsByYear => ts_calc_stats_by_year_fn
    procedure :: statsByMonthAndYear => ts_calc_stats_by_month_and_year_fn

    procedure :: selectNone => select_none_sub
    procedure :: selectAll => select_all_sub
    procedure :: list => list_output_sub
    procedure :: writeInstructions => list_output_instructions_sub
    procedure :: calcPeriodStatistics => calculate_period_statistics_fn

    procedure :: mean => ts_mean_fn
    procedure :: min => ts_min_fn
    procedure :: max => ts_max_fn
    procedure :: sum => ts_sum_fn

    procedure :: new_time_series_fm_txt_sub
    procedure :: new_time_series_fm_NWIS_sub
    procedure :: new_time_series_fm_values_sub
    procedure :: new_time_series_fm_DT_sub
!    generic :: new => new_time_series_fm_txt_sub, &
!                                new_time_series_fm_NWIS_sub, &
!                                new_time_series_fm_values_sub, &
!                                new_time_series_fm_DT_sub

    procedure :: newFmText => new_time_series_fm_txt_sub
    procedure :: newFmNWIS => new_time_series_fm_NWIS_sub
    procedure :: newFmValues => new_time_series_fm_values_sub
    procedure :: newFmDT => new_time_series_fm_DT_sub

    procedure :: newTemp => new_empty_temp_series_sub
    procedure :: addTemp => add_record_to_temp_series_sub
    procedure :: resizeTemp => right_size_temp_series_sub

    procedure :: findDateMinAndMax => find_min_and_max_date_sub
    procedure :: findDataGaps => find_data_gaps_fn
    procedure :: findHydroEvents => find_hydro_events_fn
    procedure :: printDateRange => print_date_range_fn
    procedure :: integrate => integrate_fn

!    procedure :: reducetimespan => reduce_time_span_sub

  end type T_TIME_SERIES


  type T_MINI_COLLECTION
    type (T_TIME_SERIES), pointer :: pTS
  end type T_MINI_COLLECTION

  type (T_MINI_COLLECTION), dimension(:), pointer ::  pTSCOL

  abstract interface

    function stat_func_interface(ts)  result(rValue)
      import
      class (T_TIME_SERIES) :: ts
      real (kind=T_SGL) :: rValue
    end function stat_func_interface

    function select_func_interface(ts, iYear, lKeep_)  result(iCount)
      import
      class (T_TIME_SERIES) :: ts
      integer (kind=T_INT), intent(in) :: iYear
      logical (kind=T_LOGICAL), intent(in), optional :: lKeep_
      integer (kind=T_INT) :: iCount
    end function select_func_interface

  end interface

contains

!------------------------------------------------------------------------------

  subroutine new_empty_temp_series_sub(this, iInitSize_)

    class(T_TIME_SERIES) :: this
    integer (kind=T_INT), intent(in), optional :: iInitSize_

    ! [ LOCALS ]
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: iSize

    if(present(iInitSize_) ) then
      iSize = iInitSize_
    else
      iSize = 365 * 15
    endif

    allocate(this%tData(iSize), stat=iStat)
    call assert(iStat==0, "Problem allocating memory for temporary time series" , &
      trim(__FILE__), __LINE__)

  end subroutine new_empty_temp_series_sub

!------------------------------------------------------------------------------

  subroutine add_record_to_temp_series_sub(this, rValue, tDate, sDataFlag)

    class(T_TIME_SERIES) :: this
    real(kind=T_SGL) :: rValue
    type(T_DATETIME) :: tDate
    character(len=*) :: sDataFlag

    ! [ LOCALS ]
    type(T_TIME_SERIES_DATA), dimension(:), allocatable :: tTempData
    real (kind=T_SGL), parameter :: rIncreaseByFactor = 2.0
    integer (kind=T_INT) :: iStat

    if(this%iCurrentRecord + 1 > size(this%tData) ) then  ! need to increase array size
      allocate(tTempData(size(this%tData)), stat=iStat)
      call assert(iStat==0,"Memory allocation error", &
        trim(__FILE__),__LINE__)
      tTempData = this%tData
      deallocate(this%tData)
      allocate(this%tData(int(this%iCurrentRecord * rIncreaseByFactor)), stat=iStat)
      call assert(iStat==0,"Memory allocation error", &
        trim(__FILE__),__LINE__)
      this%tData(1:this%iCurrentRecord) = tTempData(1:this%iCurrentRecord)
      deallocate(tTempData)

    endif

    this%iCurrentRecord = this%iCurrentRecord + 1
    this%tData(this%iCurrentRecord)%rValue = rValue
    this%tData(this%iCurrentRecord)%tDT = tDate
    this%tData(this%iCurrentRecord)%sDataFlag = trim(sDataFlag)

  end subroutine add_record_to_temp_series_sub

  subroutine right_size_temp_series_sub(this)

    class(T_TIME_SERIES) :: this

    ! [ LOCALS ]
    type(T_TIME_SERIES_DATA), dimension(:), allocatable :: tTempData
    integer (kind=T_INT) :: iStat

    allocate(tTempData(count(this%tData%rValue > rNEAR_ZERO) ), stat = iStat)
      call assert(iStat==0,"Memory allocation error", &
        trim(__FILE__),__LINE__)
    tTempData = pack(this%tData,this%tData%rValue > rNEAR_ZERO)
    deallocate(this%tData)
    allocate(this%tData(size(tTempData)), stat=iStat)
      call assert(iStat==0,"Memory allocation error", &
        trim(__FILE__),__LINE__)
    this%tData = tTempData
    deallocate(tTempData)

  end subroutine right_size_temp_series_sub

!------------------------------------------------------------------------------

  subroutine new_time_series_fm_txt_sub(this, sSeriesName, sDescription, sDateTxt, &
     sTimeTxt, sValTxt, sDateFormat, sTimeFormat, iDataType)

    class(T_TIME_SERIES) :: this
    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in) :: sDescription
    character(len=*), dimension(:), intent(in) :: sDateTxt
    character(len=*), dimension(:), intent(in) :: sTimeTxt
    character(len=*), dimension(:), intent(in) :: sValTxt
    character(len=*), intent(in), optional :: sDateFormat
    character(len=*), intent(in), optional :: sTimeFormat
    integer (kind=T_INT), intent(in), optional :: iDataType

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    character(len=256) ::  sRecord, sItem

    iCount = size(sDateTxt)
    call Assert(iCount == size(sValTxt), "Date and value vectors of unequal length", &
        TRIM(__FILE__), __LINE__)

    this%sSeriesName = TRIM(sSeriesName)
    call Warn(len_trim(sSeriesName) > MAXNAMELENGTH, &
       "Series name exceeds "//asChar(MAXNAMELENGTH)//" characters and will be truncated")

    this%sDescription = TRIM(sDescription)

    if(allocated(this%tData)) deallocate(this%tData, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    allocate(this%tData(iCount), stat=iStat)
    call Assert(iStat==0, "Unable to allocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    do i=1,iCount

       call this%tData(i)%tDT%parseDate(sDateTxt(i))
       call this%tData(i)%tDT%parseTime(sTimeTxt(i))
       read(sValTxt(i),*) this%tData(i)%rValue

    enddo

    call this%findDateMinAndMax()

  end subroutine new_time_series_fm_txt_sub

!------------------------------------------------------------------------------

  subroutine new_time_series_fm_values_sub(this, sSeriesName, sDescription, &
    iMonth, iDay, iYear, iHour, iMinute, iSecond, rValue)

    class(T_TIME_SERIES) :: this
    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in) :: sDescription
    integer (kind=T_INT), dimension(:), intent(in) :: iMonth
    integer (kind=T_INT), dimension(:), intent(in) :: iDay
    integer (kind=T_INT), dimension(:), intent(in) :: iYear
    integer (kind=T_INT), dimension(:), intent(in) :: iHour
    integer (kind=T_INT), dimension(:), intent(in) :: iMinute
    integer (kind=T_INT), dimension(:), intent(in) :: iSecond
    real (kind=T_SGL), dimension(:), intent(in) :: rValue

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    character(len=256) ::  sRecord, sItem

    iCount = size(iMonth)
    call Assert(iCount == size(rValue), "Date and/or value vectors of unequal length", &
        TRIM(__FILE__), __LINE__)

    this%sSeriesName = TRIM(sSeriesName)
    call Warn(len_trim(sSeriesName) <= MAXNAMELENGTH, &
       "Series name exceeds "//asChar(MAXNAMELENGTH)//" characters and will be truncated")

    this%sDescription = TRIM(sDescription)

    if(allocated(this%tData)) deallocate(this%tData, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    allocate(this%tData(iCount), stat=iStat)
    call Assert(iStat==0, "Unable to allocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    do i=1,iCount

       call this%tData(i)%tDT%calcJulianDay(iMonth(i), iDay(i), iYear(i), &
             iHour(i), iMinute(i), iSecond(i))
       this%tData(i)%rValue = rValue(i)

    enddo

    call this%findDateMinAndMax()

  end subroutine new_time_series_fm_values_sub

!------------------------------------------------------------------------------

  subroutine new_time_series_fm_DT_sub(this, sSeriesName, sDescription, &
    tDT, rValue)

    class(T_TIME_SERIES) :: this
    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in) :: sDescription
    type (T_DATETIME), dimension(:) :: tDT
    real (kind=T_SGL), dimension(:), intent(in) :: rValue

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    character(len=256) ::  sRecord, sItem

    iCount = size(rValue)
    call Assert(iCount == size(tDT), "Date and value vectors of unequal length", &
        TRIM(__FILE__), __LINE__)

    this%sSeriesName = TRIM(sSeriesName)

    call Warn(len_trim(sSeriesName) <= MAXNAMELENGTH, &
       "Series name exceeds "//asChar(MAXNAMELENGTH)//" characters and will be truncated")

    this%sDescription = TRIM(sDescription)

    if(allocated(this%tData)) deallocate(this%tData, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    allocate(this%tData(iCount), stat=iStat)
    call Assert(iStat==0, "Unable to allocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)
    this%tData%tDT = tDT
    this%tData%rValue = rValue

    call this%findDateMinAndMax()

  end subroutine new_time_series_fm_DT_sub

!------------------------------------------------------------------------------

  subroutine new_time_series_fm_NWIS_sub(this, tTS, sNewSeriesName)

    class(T_TIME_SERIES) :: this
    type (T_TIME_SERIES) :: tTS
    character(len=256), optional :: sNewSeriesName

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    character(len=256) ::  sRecord, sItem

    iCount = size(tTS%tData)

    this%sSiteName = TRIM(tTS%sSiteName)

    if(present(sNewSeriesName)) then
      this%sSeriesName = TRIM(sNewSeriesName)
      call Warn(len_trim(sNewSeriesName) <= MAXNAMELENGTH, &
        "Series name exceeds "//asChar(MAXNAMELENGTH)//" characters and will be truncated")
    else
      this%sSeriesName = TRIM(tTS%sSiteName)
      call Warn(len_trim(tTS%sSiteName) <= MAXNAMELENGTH, &
        "Series name exceeds "//asChar(MAXNAMELENGTH)//" characters and will be truncated")
    end if

    this%sDescription = TRIM(tTS%sDescription)

    if(allocated(this%tData)) deallocate(this%tData, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    allocate(this%tData(iCount), stat=iStat)
    call Assert(iStat==0, "Unable to allocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

!    this%tData%iWaterYear = tTS%tData%iWaterYear
    this%tData%tDT = tTS%tData%tDT
    this%tData%rValue = tTS%tData%rValue
    this%tData%sDataFlag = tTS%tData%sDataFlag

    call this%findDateMinAndMax()

  end subroutine new_time_series_fm_NWIS_sub

!------------------------------------------------------------------------------

  function integrate_fn(this, GE, LE, rConversionFactor)   result(rVolume)

    class(T_TIME_SERIES) :: this
    type (T_DATETIME), intent(in), optional :: GE
    type (T_DATETIME),intent(in),optional   :: LE
    real (kind=T_SGL), intent(in), optional :: rConversionFactor

   ! [ LOCALS ]
   integer (kind=T_INT) :: i
   real (kind=T_DBL) :: rVolume
   real (kind=T_SGL), dimension(:), allocatable :: rX1, rX2, rY1, rY2
   type (T_DATETIME) :: tGE_DT
   type (T_DATETIME) :: tLE_DT
   logical (kind=T_LOGICAL) :: lKeep
   integer (kind=T_INT) :: iSize, iCount
   real (kind=T_DBL) :: rOffset
   real (kind=T_SGL) :: rFactor
   real (kind=T_DBL) :: rDelta

   integer (kind=T_INT), parameter :: iAkimaResolution = 2

   ! if no arguments supplied, all time series elements will be selected
   if(present(GE)) then
     tGE_DT = GE
   else
     tGE_DT = this%tStartDate
   endif

   if(present(LE)) then
     tLE_DT = LE
   else
     tLE_DT = this%tEndDate
   endif

   ! if not supplied, factor of one is assumed
   if(present(rConversionFactor)) then
     rFactor = rConversionFactor
   else
     rFactor = 1_T_SGL
   endif

   rVolume = rD_ZERO

   iCount = this%includeByDate(tGE_DT, tLE_DT)

   call Assert(iCount > 2, "Not enough data to perform integration ("// &
     trim(this%sSeriesname)//", start:end = " &
     //trim(tGE_DT%prettydate())//":"//trim(tLE_DT%prettydate())//")", &
     trim(__FILE__), __LINE__)

   ! we're subtracting this (large) value so that we can get away with using
   ! single precision real values to represent the datetime values
   rOffset = tGE_DT%getJulianDay()

   allocate(rX1(iCount))
   allocate(rY1(iCount))

   allocate(rX2(iCount * iAkimaResolution ))
   allocate(rY2(iCount * iAkimaResolution ))

   rDelta = ( tLE_DT - tGE_DT ) / real(iCount, kind=T_SGL) &
                                  / real(iAkimaResolution, kind=T_SGL)

   rX1 = real( pack(this%tData%tDT%iJulianDay, this%tData%lSelect), kind=T_DBL) - rOffset &
           + real( pack(this%tData%tDT%rFractionOfDay, this%tData%lSelect), kind=T_DBL)

   rY1 = pack(this%tData%rValue, this%tData%lSelect)

   ! create synthetic X values to allow volume calculation to be made with higher-
   ! resolution time series values as estimated with the Akima 1D method
   do i=1, iCount * iAkimaResolution

     rX2(i) = tGE_DT%getJulianDay() - rOffset + rDelta * real(i-1, kind=T_DBL)

   enddo

   call interp_1d( rX1, rY1, rX2, rY2)

   do i=2, iCount * iAkimaResolution

     rVolume = rVolume + ( (rX2(i) - rX2(i-1)) * ( (rY2(i) + rY2(i-1)) * rD_HALF) ) &
                            * rFactor
   enddo

   deallocate(rX1, rY1, rX2, rY2)

  end function integrate_fn

!------------------------------------------------------------------------------

  function print_date_range_fn(this)    result(sDateRange)

    class(T_TIME_SERIES) :: this
    character(len=26) :: sDateRange

    write(sDateRange,fmt="('(',i2.2,'/',i2.2,'/',i4.4,' to ',i2.2,'/',i2.2,'/',i4.4,')')") &
      this%tStartDate%iMonth, this%tStartDate%iDay, this%tStartDate%iYear, &
      this%tEndDate%iMonth, this%tEndDate%iDay, this%tEndDate%iYear

  end function print_date_range_fn

!------------------------------------------------------------------------------

  subroutine find_min_and_max_date_sub(this)

    class(T_TIME_SERIES) :: this

    ! [ LOCALS ]
    type (T_DATETIME) :: tMINDATE
    type (T_DATETIME) :: tMAXDATE
    type (T_DATETIME) :: tMINDATE_selection
    type (T_DATETIME) :: tMAXDATE_selection
    integer (kind=T_INT) :: i

    call tMINDATE%calcJulianDay( 1, 1, 3000, 0, 0, 0)
    call tMAXDATE%calcJulianDay( 1, 1, 1, 0, 0, 0)
    tMINDATE_selection = tMINDATE
    tMAXDATE_selection = tMAXDATE

    do i=1,size(this%tData)
      ! determine min and max date for selected values only
      if(this%tData(i)%tDT < tMINDATE_selection .and. this%tData(i)%lSelect) &
          tMINDATE_selection = this%tData(i)%tDT
      if(this%tData(i)%tDT > tMAXDATE_selection .and. this%tData(i)%lSelect) &
          tMAXDATE_selection = this%tData(i)%tDT

      ! determine the min and max date for entire series
      if(this%tData(i)%tDT < tMINDATE) &
          tMINDATE = this%tData(i)%tDT
      if(this%tData(i)%tDT > tMAXDATE) &
          tMAXDATE = this%tData(i)%tDT
    enddo

    this%tStartDate = tMINDATE
    this%tEndDate = tMAXDATE

    this%tSelectionStartDate = tMINDATE_selection
    this%tSelectionEndDate = tMAXDATE_selection

  end subroutine find_min_and_max_date_sub

!------------------------------------------------------------------------------

  function find_data_gaps_fn(this)                      result(pDateRange)

    class(T_TIME_SERIES) :: this

!    ! [ LOCALS ]
    type (T_DATERANGE), dimension(:), pointer :: pDateRange
    type (T_DATETIME) :: tStartDate
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iSize
    integer (kind=T_INT) :: iDelta
    integer (kind=T_INT) :: iNumGaps, iNumGaps1

    iNumGaps1 = 0

    iSize = size(this%tData)

    write(LU_STD_OUT, fmt="(/,'Continuous date intervals found for time series ',a,':')") &
         trim(this%sSeriesname)

    do i=2,iSize
      iDelta = this%tData(i)%tDT%iJulianDay - this%tData(i-1)%tDT%iJulianDay
      if(iDelta /= 1) iNumGaps1 = iNumGaps1 + 1
    enddo

    if(iNumGaps1 > 0) then

      allocate(pDateRange(iNumGaps1 + 1) )   ! we really care about the intervals, not the gaps!

      ! set gap counter
      iNumGaps = 0
      tStartDate = this%tData(1)%tDT

      do i=2,iSize

        iDelta = this%tData(i)%tDT%iJulianDay - this%tData(i-1)%tDT%iJulianDay
        if(iDelta /= 1) then

          iNumGaps = iNumGaps + 1
          call pDateRange(iNumGaps)%newFmDT(tStartDate, this%tData(i-1)%tDT)
          tStartDate = this%tData(i)%tDT

          write(LU_STD_OUT, fmt="(i3,') ',a,' to ',a)") iNumGaps, &
                  pDateRange(iNumGaps)%tStartDate%listdate(), &
                  pDateRange(iNumGaps)%tEndDate%listdate()

        endif

      enddo

      ! need to write out info on the *last* gapless interval
      iNumGaps = iNumGaps + 1
      call pDateRange(iNumGaps)%newFmDT(tStartDate, this%tData(iSize)%tDT)
      write(LU_STD_OUT, fmt="(i3,') ',a,' to ',a)") iNumGaps, &
             pDateRange(iNumGaps)%tStartDate%listdate(), &
             pDateRange(iNumGaps)%tEndDate%listdate()

    else   ! no gaps found; return a single date range encompassing all data

      allocate(pDateRange(1) )
      call pDateRange(1)%newFmDT(this%tData(1)%tDT, this%tData(iSize)%tDT)
      write(LU_STD_OUT,fmt="(/,'  ** NO GAPS **',/)")
      write(LU_STD_OUT, fmt="(i3,') ',a,' to ',a)") 1, &
                pDateRange(1)%tStartDate%listdate(), &
                pDateRange(1)%tEndDate%listdate()

    endif

  end function find_data_gaps_fn

!------------------------------------------------------------------------------

  subroutine select_none_sub(this)

    class(T_TIME_SERIES) :: this

    this%tData%lSelect = lFALSE

  end subroutine select_none_sub

!------------------------------------------------------------------------------

  subroutine select_all_sub(this)

    class(T_TIME_SERIES) :: this

    this%tData%lSelect = lTRUE

  end subroutine select_all_sub

!------------------------------------------------------------------------------

  function include_by_date_fn(this, GE, LE)  result(iCount)

    class(T_TIME_SERIES) :: this
    type (T_DATETIME), intent(in), optional :: GE
    type (T_DATETIME),intent(in),optional   :: LE
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    type (T_DATETIME) :: tGE_DT
    type (T_DATETIME) :: tLE_DT
    integer (kind=T_INT) :: i

    ! if no arguments supplied, all time series elements will be selected
    if(present(GE)) then
      tGE_DT = GE
    else
      tGE_DT = this%tStartDate
    endif

    if(present(LE)) then
      tLE_DT = LE
    else
      tLE_DT = this%tEndDate
    endif

    do i=1,size(this%tData)
      if(this%tData(i)%tDT >= tGE_DT .and. this%tData(i)%tDT <= tLE_DT) then
        this%tData(i)%lInclude = lTRUE
      else
        this%tData(i)%lInclude = lFALSE
      endif
    enddo

    iCount = count(this%tData%lInclude)

  end function include_by_date_fn

!------------------------------------------------------------------------------

  function select_by_date_fn(this, GE, LE, lKeep_)  result(iCount)

    class(T_TIME_SERIES) :: this
    type (T_DATETIME), intent(in), optional :: GE
    type (T_DATETIME),intent(in),optional   :: LE
    logical (kind=T_LOGICAL), intent(in), optional :: lKeep_
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    type (T_DATETIME) :: tGE_DT
    type (T_DATETIME) :: tLE_DT
    integer (kind=T_INT) :: i
    logical (kind=T_LOGICAL) :: lKeep

    ! if no arguments supplied, all time series elements will be selected
    if(present(GE)) then
      tGE_DT = GE
    else
      tGE_DT = this%tStartDate
    endif

    if(present(LE)) then
      tLE_DT = LE
    else
      tLE_DT = this%tEndDate
    endif

    if(present(lKeep_)) then
      lKeep = lKeep_
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      do i=1,size(this%tData)

        if(this%tData(i)%tDT >= tGE_DT .and. this%tData(i)%tDT <= tLE_DT &
          .and. this%tData(i)%lInclude) &
            this%tData(i)%lSelect = lTRUE

      enddo

    else

      do i=1,size(this%tData)

        if(this%tData(i)%tDT >= tGE_DT .and. this%tData(i)%tDT <= tLE_DT &
          .and. this%tData(i)%lInclude) then

          this%tData(i)%lSelect = lTRUE

        else
          this%tData(i)%lSelect = lFALSE
        endif

      enddo

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_date_fn

!------------------------------------------------------------------------------

  function select_by_month_fn(this, iMonth, lKeep_)  result(iCount)

    class(T_TIME_SERIES) :: this
    integer (kind=T_INT), intent(in) :: iMonth
    logical (kind=T_LOGICAL), intent(in), optional :: lKeep_
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(lKeep_)) then
      lKeep = lKeep_
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      where (this%tData%tDT%iMonth == iMonth .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      endwhere

    else

      where (this%tData%tDT%iMonth == iMonth .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      elsewhere
        this%tData%lSelect = lFALSE
      endwhere

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_month_fn

!------------------------------------------------------------------------------

  function select_by_month_and_year_fn(this, iMonth, iYear, lKeep_)  result(iCount)

    class(T_TIME_SERIES) :: this
    integer (kind=T_INT), intent(in) :: iMonth
    integer (kind=T_INT), intent(in) :: iYear
    logical (kind=T_LOGICAL), intent(in), optional :: lKeep_
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(lKeep_)) then
      lKeep = lKeep_
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      where (this%tData%tDT%iMonth == iMonth .and. this%tData%tDT%iYear == iYear &
           .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      endwhere

    else

      where (this%tData%tDT%iMonth == iMonth .and. this%tData%tDT%iYear == iYear &
           .and. this%tData%lInclude)
        this%tData%lSelect = lTRUE
      elsewhere
        this%tData%lSelect = lFALSE
      endwhere

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_month_and_year_fn

!------------------------------------------------------------------------------

  function select_by_year_fn(this, iYear, lKeep_)  result(iCount)

    class(T_TIME_SERIES) :: this
    integer (kind=T_INT), intent(in) :: iYear
    logical (kind=T_LOGICAL), intent(in), optional :: lKeep_
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(lKeep_)) then
      lKeep = lKeep_
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      where (this%tData%tDT%iYear == iYear  .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      endwhere

    else

      where (this%tData%tDT%iYear == iYear  .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      elsewhere
        this%tData%lSelect = lFALSE
      endwhere

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_year_fn

!------------------------------------------------------------------------------

  function select_by_water_year_high_fn(this, iYear, lKeep_)  result(iCount)

    class(T_TIME_SERIES) :: this
    integer (kind=T_INT), intent(in) :: iYear
    logical (kind=T_LOGICAL), intent(in), optional :: lKeep_
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(lKeep_)) then
      lKeep = lKeep_
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      where (this%tData%tDT%iWaterYearHigh == iYear  .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      endwhere

    else

      where (this%tData%tDT%iWaterYearHigh == iYear  .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      elsewhere
        this%tData%lSelect = lFALSE
      endwhere

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_water_year_high_fn

!------------------------------------------------------------------------------

  function select_by_water_year_low_fn(this, iYear, lKeep_)  result(iCount)

    class(T_TIME_SERIES) :: this
    integer (kind=T_INT), intent(in) :: iYear
    logical (kind=T_LOGICAL), intent(in), optional :: lKeep_
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(lKeep_)) then
      lKeep = lKeep_
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      where (this%tData%tDT%iWaterYearLow == iYear  .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      endwhere

    else

      where (this%tData%tDT%iWaterYearLow == iYear  .and. this%tData%lInclude )
        this%tData%lSelect = lTRUE
      elsewhere
        this%tData%lSelect = lFALSE
      endwhere

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_water_year_low_fn

!------------------------------------------------------------------------------

  function select_by_change_rate_fn(this, rMinIncrease_, rMinDecrease_, keep)  result(iCount)

    class(T_TIME_SERIES) :: this
    real (kind=T_SGL), optional :: rMinIncrease_
    real (kind=T_SGL), optional :: rMinDecrease_
    logical (kind=T_LOGICAL), intent(in), optional :: keep
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    real (kind=T_SGL) :: rMinIncrease
    real (kind=T_SGL) :: rMinDecrease
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i
    real (kind=T_SGL) :: rDelta

    ! if no arguments supplied, assume rise rate of 10% of median flow represents
    if(present(rMinIncrease_)) then
      rMinIncrease = rMinIncrease_
    else
      rMinIncrease = median(this%tData%rValue) * 0.10
    endif

    if(present(rMinDecrease_)) then
      rMinDecrease = rMinDecrease_
    else
      rMinDecrease = -rMinIncrease
    endif

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(keep)) then
      lKeep = keep
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      do i=2,size(this%tData)
        if( ( rDelta >= rMinIncrease .or. rDelta <= rMinDecrease ) &
           .and. this%tData(i)%lInclude ) then
          this%tData(i)%lSelect = lTRUE
        endif
      enddo

    else

      do i=2,size(this%tData)
        if( (rDelta >= rMinIncrease .or. rDelta <= rMinDecrease) &
             .and. this%tData(i)%lInclude )  then
          this%tData(i)%lSelect = lTRUE
        else
          this%tData(i)%lSelect = lFALSE
        endif
      enddo

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_change_rate_fn

!------------------------------------------------------------------------------

  function select_by_value_fn(this, GE, LE, keep)  result(iCount)

    class(T_TIME_SERIES) :: this
    real (kind=T_SGL), intent(in), optional :: GE
    real (kind=T_SGL),intent(in),optional   :: LE
    logical (kind=T_LOGICAL), intent(in), optional :: keep
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    real (kind=T_SGL) :: rGE_Val
    real (kind=T_SGL) :: rLE_Val
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i

    ! if no arguments supplied, all time series elements will be selected
    if(present(GE)) then
      rGE_Val = GE
    else
      rGE_Val = -huge(rGE_Val)
    endif

    if(present(LE)) then
      rLE_Val = LE
    else
      rLE_Val = huge(rLE_Val)
    endif

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(keep)) then
      lKeep = keep
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      where ( this%tData%rValue >= rGE_Val &
                 .and. this%tData%rValue <= rLE_Val &
                 .and. this%tData%lInclude)

        this%tData%lSelect = lTRUE

      endwhere

    else

      where ( this%tData%rValue >= rGE_Val &
                 .and. this%tData%rValue <= rLE_Val &
                 .and. this%tData%lInclude)

        this%tData%lSelect = lTRUE

      elsewhere

        this%tData%lSelect = lFALSE

      endwhere

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_value_fn

!------------------------------------------------------------------------------

  subroutine list_output_sub(this, iLU_, iOutputFormat_)

    class(T_TIME_SERIES) :: this
!    character(len=*), intent(in), optional :: sDateFormat
    integer (kind=T_INT), intent(in), optional :: iLU_
    integer (kind=T_INT), intent(in), optional :: iOutputFormat_

    ! [ LOCALS ]
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iLU
    integer (kind=T_INT) :: iOutputFormat
!    character(len=20) :: sDateFmt

    if(present(iOutputFormat_) ) then
      iOutputFormat = iOutputFormat_
    else
      iOutputFormat = iOUTPUT_LONG_FORMAT
    endif

    if(present(iLU_)) then
      iLU = iLU_
    else
      iLU = LU_STD_OUT
    endif

!    if(present(sDateFormat)) then
!      sDateFmt = trim(sDateFormat)
!    else
!      sDateFmt = "MM/DD/YYYY"
!    endif

    if(iOutputFormat /= iOUTPUT_SSF_FORMAT) &
        write(iLU,fmt="(/,' TIME_SERIES ',a,' ---->')") quote(this%sSeriesName)

    select case(iOutputFormat)

      case(iOUTPUT_LONG_FORMAT, iOUTPUT_SSF_FORMAT)

        do i=1,size(this%tData)
          write(iLU,fmt="(a18,t22,a20,t44,g16.8)") &
            trim(this%sSeriesName), this%tData(i)%tDT%listdatetime(), &
              this%tData(i)%rValue
        enddo

      case(iOUTPUT_SHORT_FORMAT)

        do i=1,size(this%tData)
          write(iLU,fmt="(t4,g16.8)") this%tData(i)%rValue
        enddo

      case(iOUTPUT_EXTENDED_FORMAT)

        select case(this%iDataType)

          case(iDAILY_SERIES)

            do i=1,size(this%tData)
              write(iLU,fmt="(a18,t22,a20,t44,g16.8)") &
                trim(this%sSeriesName), this%tData(i)%tDT%listdatetime(), &
                  this%tData(i)%rValue
            enddo

          case(iMONTHLY_SERIES)

            do i=1,size(this%tData)

               write(iLU,fmt="(a18,t22,a8,t35,g16.8)") &
                  this%sSeriesName, trim(MONTH(this%tData(i)%tDT%iMonth)%sName), &
                    this%tData(i)%rValue

            enddo

          case(iMONTHLY_SUMMARY)

            do i=1,size(this%tData)

              write(iLU,fmt="(1x,a18,t22,a8,1x,i4,t38,g16.8)") &
                this%sSeriesName, trim(MONTH(this%tData(i)%tDT%iMonth)%sName), &
                  this%tData(i)%tDT%iYear, this%tData(i)%rValue

            enddo

          case(iANNUAL_SUMMARY)

            do i=1,size(this%tData)

               write(iLU,fmt="(1x,a18,t25,i4,t35,g16.8)") &
                 this%sSeriesName, this%tData(i)%tDT%iYear, this%tData(i)%rValue

            enddo

        end select  ! iDataType

      end select  ! iOutputFormat

  end subroutine list_output_sub

!------------------------------------------------------------------------------

  subroutine list_output_instructions_sub(this, iLU)

    class(T_TIME_SERIES) :: this
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: LU
    integer (kind=T_INT) :: iInitialCharacter

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    ! write the instruction to move the file marker up to the beginning
    ! of the next time series
    write(LU,fmt="('$TIME_SERIES$')" )
    iInitialCharacter = 2 + MAXNAMELENGTH + 3 + 20

    do i=1,size(this%tData)

       write(LU,fmt="(a)") "l1 ["//trim(this%sSeriesName)//"_"//trim(asChar(i) )//"]" &
         //trim(asChar(iInitialCharacter) )//":"//trim(asChar(iInitialCharacter + 20) )

    enddo

  end subroutine list_output_instructions_sub

!------------------------------------------------------------------------------

  function find_hydro_events_fn(this, pBlock)    result(pTempSeries)

      class(T_TIME_SERIES) :: this
      type (T_BLOCK), pointer, optional :: pBlock
      type (T_TIME_SERIES), pointer :: pTempSeries

      ! [ LOCALS ]
      integer (kind=T_INT) :: iYear, iMonth, iFirstYear
      integer (kind=T_INT) :: iStat
      integer (kind=T_INT) :: i
      integer (kind=T_INT) :: iCount
      real (kind=T_SGL) :: iSlope1, iSlope2
      character (len=256) :: sBuf
      type (T_DATETIME) :: tStartDate, tEndDate
      type (T_DATETIME) :: tLagStartDate, tLagEndDate
      type (T_DATETIME) :: tCurrentDate
      type (T_STATS_COLLECTION), pointer :: pStats
      character (len=MAXNAMELENGTH) :: sNewSeriesName
      logical (kind=T_LOGICAL) :: lAddSuffix
      logical (kind=T_LOGICAL), dimension(:), allocatable :: lSelect
      type (T_DATETIME) :: tLastDate
!      type (T_DATETIME)
      real (kind=T_SGL) :: rLastValue
      integer (kind=T_INT) :: iLastIndex

      character (len=MAXARGLENGTH), dimension(:), pointer :: &
         pNEW_SERIES_NAME
      real (kind=T_SGL), dimension(:), pointer :: &
         pWINDOW, pMIN_PEAK, pRISE_LAG, pFALL_LAG
      real (kind=T_SGL) :: rWINDOW, rMIN_PEAK, rRISE_LAG, rFALL_LAG

      pWINDOW => null(); pMIN_PEAK => null(); pRISE_LAG => null()
      pFALL_LAG => null(); pNEW_SERIES_NAME => null()

      tStartDate = this%tStartDate
      tEndDate = this%tEndDate

      if(present(pBlock) ) then

        ! Process the standard "DATE_1", "DATE_2", "TIME_1", and "TIME_2" entries
        ! tStartDate and tEndDate are return values
        call processUserSuppliedDateTime(pBlock, tStartDate, tEndDate)

        ! clip the desired date window to the range of actual data
        if(tStartDate < this%tStartDate) tStartDate = this%tStartDate
        if(tEndDate > this%tEndDate) tEndDate = this%tEndDate

        pWINDOW => pBlock%getReal("WINDOW")
        call assert(pWINDOW(1) > rNEAR_TINY, &
          "No value was supplied for WINDOW in block starting at line" &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)
        call assert(pWINDOW(1) > rZERO, &
          "Value supplied for WINDOW must be greater than zero in block starting at line " &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)

        rWINDOW = pWINDOW(1)

        pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
        call assert(.not. str_compare(pNEW_SERIES_NAME(1),"NA"), &
          "No value was supplied for NEW_SERIES_NAME in block starting at line " &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)

        sNewSeriesName = trim(pNEW_SERIES_NAME(1) )

        pMIN_PEAK => pBlock%getReal("MIN_PEAK")
        call assert(pMIN_PEAK(1) > rNEAR_TINY, &
          "No value was supplied for MIN_PEAK in block starting at line" &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)
        call assert(pMIN_PEAK(1) > rZERO, &
          "Value supplied for MIN_PEAK must be greater than zero in block starting at line " &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)

        rMIN_PEAK = pMIN_PEAK(1)

        pRISE_LAG => pBlock%getReal("RISE_LAG")
        call assert(pRISE_LAG(1) > rNEAR_TINY, &
          "No value was supplied for RISE_LAG in block starting at line" &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)
        call assert(pRISE_LAG(1) > rZERO, &
          "Value supplied for RISE_LAG must be greater than zero in block starting at line " &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)

        rRISE_LAG = pRISE_LAG(1)

        pFALL_LAG => pBlock%getReal("FALL_LAG")
        call assert(pFALL_LAG(1) > rNEAR_TINY, &
          "No value was supplied for FALL_LAG in block starting at line" &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)
        call assert(pFALL_LAG(1) > rZERO, &
          "Value supplied for FALL_LAG must be greater than zero in block starting at line " &
            //trim(asChar(pBlock%iStartingLineNumber) ), &
            trim(__FILE__), __LINE__)

        rFALL_LAG = pFALL_LAG(1)

        deallocate(pWINDOW, pMIN_PEAK, pRISE_LAG, pFALL_LAG, pNEW_SERIES_NAME)

      else

        rWINDOW = 7.
        sNewSeriesName = trim(this%sSeriesName)//"_hp"
        rMIN_PEAK = median(this%tData%rValue) * 3.0
        rRISE_LAG = 3.
        rFALL_LAG = 5.

      endif

      allocate(lSelect(size(this%tData)), stat=iStat )
      call assert(iStat == 0, "Problem allocating memory for series in while " &
         //"processing HYDRO_PEAKS block", trim(__FILE__), __LINE__)

      ! pare down current data set based on user inputs DATE_1 and DATE_2
      iCount = this%includeByDate(tStartDate, tEndDate)
      lSelect = lFALSE

      ! initialize variables needed to perform comparisons
      iLastIndex = 1
      rLastValue = this%tData(1)%rValue
      tLastDate = this%tData(1)%tDT

      do i=1, size(this%tData)

        ! if this element is not selected (i.e. within selected date range)
        ! jump to next iteration of loop
        if( .not. this%tData(i)%lInclude ) cycle

!        ! OK, now reset lSelect in preparation for peak identification
!        this%tData(i)%lSelect = lFALSE

        ! cannot evaluate slopes at either end of the time series
        if(i == 1) cycle
        if(i == size(this%tData) ) cycle

        ! if we're dealing with a peak, iSlope1 will be positive while
        ! iSlope2 is negative
        iSlope1 = ( this%tData(i)%rValue - this%tData(i-1)%rValue ) &
                     / ( this%tData(i)%tDT - this%tData(i-1)%tDT )
        iSlope2 = ( this%tData(i+1)%rValue - this%tData(i)%rValue ) &
                     / ( this%tData(i+1)%tDT - this%tData(i)%tDT )
        if( iSlope1 > rZERO ) then
          if(iSlope2 < rZERO) then
            ! we're found a potential peak; are we outside the time window
            ! required in order to identify a new peak?
            if((this%tData(i)%tDT - tLastDate ) > rWINDOW ) then
              ! OK, the timing seems right; is the value of the discharge
              ! sufficient to qualify as a peak?
              if( this%tData(i)%rValue > rMIN_PEAK ) then
                ! identify this point as a peak
                lSelect(i) = lTRUE
                tLastDate = this%tData(i)%tDT
                rLastValue = this%tData(i)%rValue
                iLastIndex = i
              endif
            else
              ! we're within the time window (shadow?) of a previously identified peak
              ! is the peak we identified still the largest value?
              if( this%tData(i)%rValue > rLastValue ) then
                ! we've met all the criteria above, but THIS is an EVEN BIGGER
                ! peak value; we'll now redefine our definition of the peak value
                ! first we reset the old peak value
                this%tData(iLastIndex)%lSelect = lFALSE
                iLastIndex = i
                ! set the new peak value parameters
                lSelect(i) = lTRUE
                tLastDate = this%tData(i)%tDT
                rLastValue = this%tData(i)%rValue
              endif
            endif
          endif
        endif
      enddo

      iCount = count(lSelect)
      call echolog("Identified "//trim(asChar(iCount) )//" peaks in series " &
        //quote(this%sSeriesName) )

      ! clear the selection flags in preparation for identification of the
      ! values preceding and following the peak values
      call this%selectNone()

      do i=2, size(this%tData) - 1
        ! if this element is not selected, jump to next iteration of loop

        if( .not. lSelect(i)) cycle

        ! if we identified a peak in the previous step, we proceed
        tCurrentDate = this%tData(i)%tDT

        ! for each identified date, we select the date range associated with
        ! the rise lag and fall lag, marking the elements for inclusion
        ! in the final output series
        iCount = this%selectByDate(GE = tCurrentDate%decrement(rRISE_LAG), &
                                   LE = tCurrentDate%increment(rFALL_LAG), &
                                   lKeep_ = lTRUE)
     enddo

     if(associated( pTempSeries) ) nullify(pTempSeries)
     allocate(pTempSeries, stat=iStat )
     call assert(iStat == 0, "Problem allocating memory for series in while " &
       //"processing HYDRO_PEAKS block", trim(__FILE__), __LINE__)

     allocate(pTempSeries%tData( count(this%tData%lSelect) ), stat=iStat )
     call assert(iStat == 0, "Problem allocating memory for series in while " &
       //"processing HYDRO_PEAKS block", trim(__FILE__), __LINE__)

     ! now assign just those values associated with the peaks to the new series
     pTempSeries%tData%rValue = pack(this%tData%rValue, this%tData%lSelect)
     pTempSeries%tData%tDT = pack(this%tData%tDT, this%tData%lSelect)
     pTempSeries%sDescription = trim(this%sDescription)//"; processed by HYDRO_PEAKS"
!     pTempSeries%tStartDate = this%tStartDate
!     pTempSeries%tEndDate = this%tEndDate
     pTempSeries%sSeriesName = trim(sNewSeriesName)
     call pTempSeries%findDateMinAndMax()

  end function find_hydro_events_fn

!------------------------------------------------------------------------------

  function calculate_period_statistics_fn(this, pBlock)  result(pTSCOL)

      class(T_TIME_SERIES) :: this
      type (T_BLOCK), pointer :: pBlock
      type (T_MINI_COLLECTION), dimension(:), pointer ::  pTSCOL

      ! [ LOCALS ]
      integer (kind=T_INT) :: iSize, iStatSize
      integer (kind=T_INT) :: i, j, k
      integer (kind=T_INT) :: iYear, iMonth, iFirstYear
      integer (kind=T_INT) :: iStat
      integer (kind=T_INT) :: iCount
      integer (kind=T_INT) :: iStatistic
      integer (kind=T_INT) :: iPeriod
      integer (kind=T_INT) :: iTimeAbscissa
      integer (kind=T_INT) :: iYearType
      character (len=256) :: sBuf
      character (len=12) :: sSuffix
      type (T_DATETIME) :: tStartDate, tEndDate
      character (len=MAXNAMELENGTH) :: sSeriesName
      logical (kind=T_LOGICAL) :: lAddSuffix
!      type (T_STATS_COLLECTION), pointer, save :: pStats
      type (T_TIME_SERIES), dimension(:), pointer :: pTempSeries
      type (T_TIME_SERIES), pointer :: pTS
      type (T_TIME_SERIES_DATA), pointer :: pTempData

      character (len=MAXARGLENGTH), dimension(:), pointer :: &
         pNEW_SERIES_NAME, pSTATISTIC, pPERIOD, pTIME_ABSCISSA, &
         pYEAR_TYPE

      integer (kind=T_INT), dimension(:), pointer :: pMINIMUM_VALID_DAYS

      pNEW_SERIES_NAME => null(); pSTATISTIC => null(); pPERIOD => null()
      pTIME_ABSCISSA => null(); pTS => null(); pTempData => null()
      pTSCOL => null()

      call processUserSuppliedDateTime(pBlock, tStartDate, tEndDate)

      if(tStartDate < this%tStartDate) tStartDate = this%tStartDate
      if(tEndDate > this%tEndDate) tEndDate = this%tEndDate

      ! do *NOT* tack on a descriptive suffix to the output series name
      lAddSuffix = lFALSE
      sSeriesName = this%sSeriesName

      ! restrict data to specified date range, if desired
      iCount =  this%includeByDate(tStartDate, tEndDate)
      if(iCount == 0) &
        call Assert(lFALSE, "Problem calculating PERIOD_STATISTICs: ~ no time" &
          //" series data within " &
          //"specified date range ("//tStartDate%prettydate()//" to " &
          //tEndDate%prettydate()//")", trim(__FILE__), __LINE__)

      ! find out what statistics have been requested; if none, assume "mean"
      pSTATISTIC => pBlock%getString("STATISTIC")
      if(str_compare(pSTATISTIC(1), "NA")) pSTATISTIC(1) = "mean"

      ! find out what type of annual statistic is desired
      pYEAR_TYPE => pBlock%getString("YEAR_TYPE")
      if(str_compare(pYEAR_TYPE(1), "NA")) pYEAR_TYPE(1) = "calendar"

      ! find out what time period been requested; if none, assume "year"
      pPERIOD => pBlock%getString("PERIOD")
      if(str_compare(pPERIOD(1), "NA")) pPERIOD(1) = "year"

      pTIME_ABSCISSA => pBlock%getString("TIME_ABSCISSA")
      if(str_compare(pTIME_ABSCISSA(1), "NA")) pTIME_ABSCISSA(1) = "center"

      ! if only one period is given, assume it applies to all statistics
      if(size(pPERIOD) == 1 .and. size(pSTATISTIC) > 1) then
        sBuf = pPERIOD(1)
        deallocate(pPERIOD)
        allocate(pPERIOD(size(pSTATISTIC)) )
        pPERIOD = trim(sBUF)
      endif

      ! if only one time_abscissa is given, assume it applies to all statistics
      if(size(pTIME_ABSCISSA) == 1 .and. size(pSTATISTIC) > 1) then
        sBuf = pTIME_ABSCISSA(1)
        deallocate(pTIME_ABSCISSA)
        allocate(pTIME_ABSCISSA(size(pSTATISTIC)) )
        pTIME_ABSCISSA = trim(sBUF)
      endif

      ! assume that period and statistic arguments are in order; there
      ! must be an equal number of both arguments
      call Assert(size(pPERIOD) == size(pSTATISTIC), &
        "An averaging period must be specified for each statistic", trim(__FILE__), __LINE__)

      ! how many statistics are we calculating?
      iSize = size(pSTATISTIC)

!      allocate(pTempSeries(iSize), stat=iStat)
      allocate(pTSCOL(iSize), stat=iStat)
      call assert(iStat == 0, "Memory allocation error", trim(__FILE__), __LINE__)

      ! determine what the new series name(s) should be....
       pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
!       if( str_compare(pNEW_SERIES_NAME(1), "NA") ) then
!         pTempSeries%sSeriesName = trim(sSeriesName)//"_"
!         lAddSuffix = lTRUE
!       else
!         do i=1,iSize
!           pTSCOL(i)%pTS%sSeriesName = pNEW_SERIES_NAME(i)
!         enddo
!       endif

      do i=1,iSize

        ! determine the YEAR TYPE
        if(str_compare(pYEAR_TYPE(i),"calendar") ) then
          iYearType = iCALENDAR
        elseif(str_compare(pYEAR_TYPE(i),"water_high") ) then
          iYearType = iWATER_YEAR_HIGH
        elseif(str_compare(pYEAR_TYPE(i),"water_low") ) then
          iYearType = iWATER_YEAR_LOW
        else
          call assert(lFALSE, "Unknown YEAR_TYPE: "//quote(pYEAR_TYPE(i)), &
              trim(__FILE__), __LINE__, pBlock%sBlockname, pBlock%lineNumber("YEAR_TYPE"))
        endif

        ! determine the time period for calculation
        if(str_compare(pPERIOD(i),"month_one") ) then
          iPeriod = iMONTHLY_SUMMARY
          sSuffix = "m"
        elseif(str_compare(pPERIOD(i),"month_many") ) then
          iPeriod = iMONTHLY_SERIES
          sSuffix = "ms"
        elseif(str_compare(pPERIOD(i),"year") ) then
          iPeriod = iANNUAL_SUMMARY
          sSuffix = "a"
        else
          call assert(lFALSE, "Unknown PERIOD: "//quote(pPERIOD(i)), &
              trim(__FILE__), __LINE__)
        endif

        ! determine which statistic should be calculated
        if(str_compare(pSTATISTIC(i), "sum") ) then
          iStatistic = iSUM
          sSuffix = trim(sSuffix)//"_sm"
        elseif(str_compare(pSTATISTIC(i), "mean") ) then
          iStatistic = iMEAN
          sSuffix = trim(sSuffix)//"_mn"
        elseif(str_compare(pSTATISTIC(i), "minimum") ) then
          iStatistic = iMINIMUM
          sSuffix = trim(sSuffix)//"_mi"
        elseif(str_compare(pSTATISTIC(i), "maximum") ) then
          iStatistic = iMAXIMUM
          sSuffix = trim(sSuffix)//"_ma"
        elseif(str_compare(pSTATISTIC(i), "range") ) then
          iStatistic = iRANGE
          sSuffix = trim(sSuffix)//"_ra"
        elseif(str_compare(pSTATISTIC(i), "std_dev") ) then
          iStatistic = iSTD_DEV
          sSuffix = trim(sSuffix)//"_sd"
        else
          call assert(lFALSE, "Unknown STATISTIC: "//quote(pSTATISTIC(i)), &
              trim(__FILE__), __LINE__)
        endif

        if(str_compare(pTIME_ABSCISSA(i), "start") ) then
          iTimeAbscissa = iSTART
        elseif(str_compare(pTIME_ABSCISSA(i), "center") &
           .or. str_compare(pTIME_ABSCISSA(i), "centre") ) then
          iTimeAbscissa = iCENTER
        elseif(str_compare(pTIME_ABSCISSA(i), "end") ) then
          iTimeAbscissa = iEND
        else
          call assert(lFALSE, "Unknown TIME_ABSCISSA value: "//quote(pTIME_ABSCISSA(i)), &
              trim(__FILE__), __LINE__)
        endif

!        allocate(pTempSeries(i)%tData(iStatSize), stat = iStat )
!

        pTSCOL(i)%pTS => this%statByPeriod(iPeriod, iStatistic, iTimeAbscissa, iYearType)
        pTSCOL(i)%pTS%iDataType = iPeriod

        if( str_compare(pNEW_SERIES_NAME(1), "NA") ) then
          pTSCOL(i)%pTS%sSeriesName = trim(sSeriesName)//"_"//trim(sSuffix)
        else
          pTSCOL(i)%pTS%sSeriesName = trim(adjustl(pNEW_SERIES_NAME(i) ) )
        endif

      enddo

      if (associated(pSTATISTIC) ) deallocate(pSTATISTIC)
      if (associated(pPERIOD) ) deallocate(pPERIOD)
      if (associated(pTIME_ABSCISSA) ) deallocate(pTIME_ABSCISSA)
      if (associated(pYEAR_TYPE) ) deallocate(pYEAR_TYPE)

  end function calculate_period_statistics_fn

!------------------------------------------------------------------------------

function ts_create_stats_object_fn(this, sSeriesName)  result(pStats)

   class(T_TIME_SERIES) :: this
   character (len=*), intent(in) :: sSeriesName
   type (T_STATS_COLLECTION), pointer :: pStats

   ! [ LOCALS ]
   integer (kind=T_INT) :: iNumRecs
   integer (kind=T_INT) :: iStat
   integer (kind=T_INT) :: iFirstYear, iLastYear
   integer (kind=T_INT) :: i, j
   integer (kind=T_INT) :: iCount

   iFirstYear = MINVAL(this%tData%tDT%iYear)
   iLastYear = MAXVAL(this%tData%tDT%iYear)

   allocate(pStats, stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for statistics collections data object")

   pStats%sSeriesName = trim(sSeriesName)
   pStats%pByYearAndMonth => this%statsByMonthAndYear()
   pStats%pByYear => this%statsByYear()

   ! select all data elements in time series object
   call this%selectAll()
   iCount = size(this%tData)

   allocate(pStats%pAllRecords)

   ! calculate base statistics for entire data period of record
   pStats%pAllRecords = calc_base_stats(this%tData%rValue, this%tData%tDT%iJulianDay)

   return

end function ts_create_stats_object_fn

!------------------------------------------------------------------------------

function ts_calc_stats_by_month_fn(this)   result(pStatsByMonth)

   class(T_TIME_SERIES) :: this
   type (T_STATS), dimension(:), pointer :: pStatsByMonth

   ! [ LOCALS ]
   integer (kind=T_INT) :: i, j
   integer (kind=T_INT) :: iStat
   integer (kind=T_INT) :: iCount
   integer (kind=T_INT) :: iMonth

   allocate(pStatsByMonth(12), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'pStatsByMonth' statistics data object", &
     trim(__FILE__), __LINE__)

   ! calculate base statistics by month over all years
   do iMonth=1,12

     ! select a subset of the data for a given month
     iCount = this%selectByMonth(iMonth)

     if(iCount < 26) then
       pStatsByMonth(iMonth)%lValid = lFALSE
     else
       pStatsByMonth(iMonth) = &
             calc_base_stats(pack(this%tData%rValue, this%tData%tDT%iMonth == iMonth), &
             pack(this%tData%tDT%iJulianDay, this%tData%tDT%iMonth == iMonth) )
     endif

   enddo

end function ts_calc_stats_by_month_fn

!------------------------------------------------------------------------------

function ts_calc_stats_by_month_and_year_fn(this) result(pStatsByYearAndMonth)

   class(T_TIME_SERIES) :: this
   type (T_STATS), dimension(:,:), pointer :: pStatsByYearAndMonth

   ! [ LOCALS ]
   integer (kind=T_INT) :: iFirstYear, iLastYear
   integer (kind=T_INT) :: iMonth, iYear
   integer (kind=T_INT) :: iStat
   integer (kind=T_INT) :: iCount

   iFirstYear = MINVAL(this%tData%tDT%iYear)
   iLastYear = MAXVAL(this%tData%tDT%iYear)

   allocate(pStatsByYearAndMonth(iFirstYear:iLastYear,12), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'pStatsByYearAndMonth' statistics data object", &
     trim(__FILE__),__LINE__)

   do iYear=iFirstYear,iLastYear

     ! calculate monthly statistics for the current year
     do iMonth=1,12

       iCount = this%selectByMonthAndYear(iMonth, iYear)

       if(iCount < 26) then
         pStatsByYearAndMonth(iYear,iMonth)%lValid = lFALSE
       else

         pStatsByYearAndMonth(iYear,iMonth) = &
             calc_base_stats(pack(this%tData%rValue, this%tData%tDT%iYear == iYear &
                .and. this%tData%tDT%iMonth == iMonth ), &
           pack(this%tData%tDT%iJulianDay, this%tData%tDT%iYear == iYear &
                .and. this%tData%tDT%iMonth == iMonth ))

       endif

     enddo
   enddo

end function ts_calc_stats_by_month_and_year_fn

!------------------------------------------------------------------------------

function ts_mean_fn(this)  result(rMean)

   class(T_TIME_SERIES) :: this
   real (kind=T_SGL) :: rMean

   ! [ LOCALS ]
   integer (kind=T_INT) :: iCount

   rMean = 0_T_SGL

   iCount = count( this%tData%lSelect )
   if(iCount > 0) rMean = sum( this%tData%rValue, this%tData%lSelect ) / iCount

end function ts_mean_fn

!------------------------------------------------------------------------------

function ts_variance_fn(this)  result(rVariance)

   class(T_TIME_SERIES) :: this
   real (kind=T_SGL) :: rVariance

   ! [ LOCALS ]



end function ts_variance_fn

!------------------------------------------------------------------------------

function ts_range_fn(this)  result(rRange)

   class(T_TIME_SERIES) :: this
   real (kind=T_SGL) :: rRange

   rRange = this%max() - this%min()

end function ts_range_fn

!------------------------------------------------------------------------------

function ts_sum_fn(this)  result(rSum)

   class(T_TIME_SERIES) :: this
   real (kind=T_SGL) :: rSum

   rSum = sum( this%tData%rValue, this%tData%lSelect )

end function ts_sum_fn

!------------------------------------------------------------------------------

function ts_min_fn(this)  result(rMin)

   class(T_TIME_SERIES) :: this
   real (kind=T_SGL) :: rMin

   rMin = minval( this%tData%rValue, this%tData%lSelect )

end function ts_min_fn

!------------------------------------------------------------------------------

function ts_max_fn(this)  result(rMax)

   class(T_TIME_SERIES) :: this
   real (kind=T_SGL) :: rMax

   rMax = maxval( this%tData%rValue, this%tData%lSelect )

end function ts_max_fn

!------------------------------------------------------------------------------

function ts_calc_stat_by_period_fn(this, iPeriod, iStatistic, iTimeAbscissa, &
     iYearType) result(pTS)

   class(T_TIME_SERIES) :: this
   integer (kind=T_INT), intent(in) :: iPeriod
   integer (kind=T_INT), intent(in) :: iStatistic
   integer (kind=T_INT), intent(in) :: iTimeAbscissa
   integer (kind=T_INT), intent(in) :: iYearType
   type (T_TIME_SERIES), pointer :: pTS
   type (T_TIME_SERIES_DATA), dimension(:), allocatable :: tData

   ! [ LOCALS ]
   integer (kind=T_INT) :: iFirstYear, iLastYear, iCenterYear
   integer (kind=T_INT), dimension(1) :: iFirstDayPos, iLastDayPos
   integer (kind=T_INT) :: iMonth, iYear, n, i
   integer (kind=T_INT) :: iStat
   integer (kind=T_INT) :: iCount
   integer (kind=T_INT) :: iNumValid

   pTS => null()

   ! define the mathematical function to be applied
   select case (iStatistic)

     case( iSUM )
       this%stat_func_ptr => ts_sum_fn

     case( iMEAN )
       this%stat_func_ptr => ts_mean_fn

     case( iMAXIMUM )
       this%stat_func_ptr => ts_max_fn

     case( iMINIMUM )
       this%stat_func_ptr => ts_min_fn

     case( iRANGE )
       this%stat_func_ptr => ts_range_fn

     case default

       call assert(lFALSE, "Logic error in case select structure", trim(__FILE__), &
          __LINE__)

   end select

   allocate(pTS, stat=iStat)
   call assert(iStat == 0, "Memory allocation error", trim(__FILE__), __LINE__)

   iFirstDayPos = minloc(this%tData%tDT%iJulianDay, this%tData%lInclude)
   iLastDayPos = maxloc(this%tData%tDT%iJulianDay, this%tData%lInclude)
   iFirstYear = this%tData(iFirstDayPos(1))%tDT%iYear
   iLastYear = this%tData(iLastDayPos(1))%tDT%iYear


   ! now apply the function to the appropriate time period
   select case(iPeriod)

     case(iMONTHLY_SUMMARY)
       allocate(tData(12), stat=iStat)
       call assert(iStat == 0,"Memory allocation error", trim(__FILE__), __LINE__)

       tData%tDT%iMonth = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
       pTS%sDescription = trim(sSTATISTIC(iStatistic)) &
           //" monthly values derived from series "//quote(this%sSeriesName) &
           //"; ("//trim(asChar(iFirstYear))//" to "//trim(asChar(iLastYear))//")"

       select case (iTimeAbscissa)
         case(iSTART)
           tData%tDT%iDay = 1
           tData%tDT%iYear = 1800
         case(iCENTER)
           tData%tDT%iDay = 15
           tData%tDT%iYear = 1800
         case(iEND)
           tData%tDT%iDay = MONTH%iNumDays
           tData%tDT%iYear = 1800
       end select

       do iMonth = 1,12

         iCount = this%selectByMonth(iMonth)

         call tData(iMonth)%tDT%calcJulianDay()

         if(iCount > 25) then
           tData(iMonth)%rValue = this%stat_func_ptr()
         else
           tData(iMonth)%rValue = -HUGE(rZERO)
           tData(iMonth)%lValid = lFALSE
         endif

       enddo

     case(iMONTHLY_SERIES)      ! month_many

       allocate(tData(12 * (iLastYear - iFirstYear + 1) ), stat=iStat)
       call assert(iStat == 0,"Memory allocation error", trim(__FILE__), __LINE__)
       pTS%sDescription = "Monthly "//trim(lowercase(sSTATISTIC(iStatistic))) &
           //" values derived from series "//quote(this%sSeriesName) &
           //"; ("//trim(asChar(iFirstYear))//" to "//trim(asChar(iLastYear))//")"

       select case (iTimeAbscissa)
         case(iSTART)
           tData%tDT%iDay = 1
         case(iCENTER)
           tData%tDT%iDay = 15
!         case(iEND)
!           pTS%tData%tDT%iDay = MONTH%iNumDays
       end select

       n = 0
       do iYear = iFirstYear, iLastYear

         do iMonth = 1,12

           n = n + 1
           iCount = this%selectByMonthAndYear(iMonth, iYear)

           tData(n)%tDT%iYear = iYear
           tData(n)%tDT%iMonth = iMonth
           if(iTimeAbscissa == iEND) tData(n)%tDT%iDay = MONTH(iMonth)%iNumDays
           call tData(n)%tDT%calcJulianDay()

           if(iCount > 25) then
             tData(n)%rValue = this%stat_func_ptr()
           else
             tData(n)%rValue = -HUGE(rZERO)
             tData(n)%lValid = lFALSE
           endif

         enddo

       enddo

     case(iANNUAL_SUMMARY)

       ! define the year selection function to be applied
       select case (iYearType)
         case( iCALENDAR )
           this%select_func_ptr => select_by_year_fn
         case( iWATER_YEAR_HIGH )
           this%select_func_ptr => select_by_water_year_high_fn
           ! iFirstYear is redefined as the first WATER YEAR HIGH
           if (this%tData(iFirstDayPos(1))%tDT%iMonth >= 10) &
             iFirstYear = this%tData(iFirstDayPos(1))%tDT%iYear + 1
         case( iWATER_YEAR_LOW )
           this%select_func_ptr => select_by_water_year_low_fn
           ! iFirstYear is redefined as the first WATER YEAR LOW
           if (this%tData(iFirstDayPos(1))%tDT%iMonth >= 4) &
             iFirstYear = this%tData(iFirstDayPos(1))%tDT%iYear + 1
         case default
           call assert(lFALSE,"Logic error in case construct", &
             trim(__FILE__), __LINE__)
       end select

       allocate(tData(iLastYear - iFirstYear + 1), stat=iStat)
       call assert(iStat == 0,"Memory allocation error", trim(__FILE__), __LINE__)

       pTS%sDescription = "Annual "//trim(sYEAR_TYPE(iYearType))//" " &
           //trim(lowercase(sSTATISTIC(iStatistic))) &
           //" values derived from series "//quote(this%sSeriesName) &
           //"; ("//trim(asChar(iFirstYear))//" to "//trim(asChar(iLastYear))//")"

       n = 0
       do iYear = iFirstYear, iLastYear

         n = n + 1
         iCount = this%select_func_ptr(iYear)

         ! took these definitions directly from John Walker's
         ! implementation of PERIOD_STATISTICS
         select case (iTimeAbscissa)
           case(iSTART)
             select case(iYearType)
               case(iCALENDAR)
                 tData(n)%tDT%iDay = 1
                 tData(n)%tDT%iMonth = 1
                 tData(n)%tDT%iYear = iYear
               case(iWATER_YEAR_HIGH)
                 tData(n)%tDT%iDay = 1
                 tData(n)%tDT%iMonth = 10
                 tData(n)%tDT%iYear = iYear - 1
               case(iWATER_YEAR_LOW)
                 tData(n)%tDT%iDay = 1
                 tData(n)%tDT%iMonth = 4
                 tData(n)%tDT%iYear = iYear - 1
              end select
           case(iCENTER)
             select case(iYearType)
               case(iCALENDAR)
                 tData(n)%tDT%iDay = 1
                 tData(n)%tDT%iMonth = 7
                 tData(n)%tDT%iYear = iYear
               case(iWATER_YEAR_HIGH)
                 tData(n)%tDT%iDay = 1
                 tData(n)%tDT%iMonth = 4
                 tData(n)%tDT%iYear = iYear
               case(iWATER_YEAR_LOW)
                 tData(n)%tDT%iDay = 1
                 tData(n)%tDT%iMonth = 10
                 tData(n)%tDT%iYear = iYear
              end select
           case(iEND)
             select case(iYearType)
               case(iCALENDAR)
                 tData(n)%tDT%iDay = 31
                 tData(n)%tDT%iMonth = 12
                 tData(n)%tDT%iYear = iYear
               case(iWATER_YEAR_HIGH)
                 tData(n)%tDT%iDay = 30
                 tData(n)%tDT%iMonth = 9
                 tData(n)%tDT%iYear = iYear
               case(iWATER_YEAR_LOW)
                 tData(n)%tDT%iDay = 31
                 tData(n)%tDT%iMonth = 3
                 tData(n)%tDT%iYear = iYear
              end select
         end select

         call tData(n)%tDT%calcJulianDay()
         if(iCount > 350) then
           tData(n)%rValue = this%stat_func_ptr()
         else
           tData(n)%rValue = -HUGE(rZERO)
           tData(n)%lValid = lFALSE
         endif

       enddo

     case default

          call assert(lFALSE, "Logic error in case select structure", trim(__FILE__), &
            __LINE__)

   end select

   pTS%iDataType = iPeriod

   ! if we have time periods that contain invalid statistics, delete them
   ! from the final time series
   iNumValid = count(tData%lValid)
   allocate(pTS%tData(iNumValid) )
   pTS%tData = pack(tData,tData%lValid)

   call pTS%findDateMinAndMax()

end function ts_calc_stat_by_period_fn

!------------------------------------------------------------------------------

function ts_calc_stats_by_year_fn(this)  result(pStatsByYear)

   class(T_TIME_SERIES) :: this
   type (T_STATS), dimension(:), pointer :: pStatsByYear
   ! [ LOCALS ]
   integer (kind=T_INT) :: iFirstYear, iLastYear
   integer (kind=T_INT) :: iMonth, iYear
   integer (kind=T_INT) :: iStat
   integer (kind=T_INT) :: iCount

   iFirstYear = MINVAL(this%tData%tDT%iYear)
   iLastYear = MAXVAL(this%tData%tDT%iYear)

   allocate(pStatsByYear(iFirstYear:iLastYear), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'pStatsByYear' statistics data object", &
     trim(__FILE__),__LINE__)

   do iYear=iFirstYear,iLastYear

     iCount = this%selectByYear( iYear)

     if(iCount < 350) then
       pStatsByYear(iYear)%lValid = lFALSE
     else
       pStatsByYear(iYear) = &
           calc_base_stats(pack(this%tData%rValue, this%tData%tDT%iYear == iYear), &
           pack(this%tData%tDT%iJulianDay, this%tData%tDT%iYear == iYear))
     endif

   enddo

end function ts_calc_stats_by_year_fn

!------------------------------------------------------------------------------

function ts_calc_base_stats(this)   result(pBaseStats)

   class(T_TIME_SERIES) :: this
   type (T_STATS), pointer :: pBaseStats

   ! [ LOCALS ]
   real (kind=T_SGL), dimension(count(this%tData%lSelect)) :: rData
   integer (kind=T_INT), dimension(count(this%tData%lSelect)) :: iOriginalOrder
   integer (kind=T_INT), dimension(count(this%tData%lSelect)) :: iJulianDay
   integer (kind=T_INT) :: i,j,k
   integer (kind=T_INT), dimension(1) :: iLocMin, iLocMax
   real (kind=T_SGL) :: rMean
   integer (kind=T_INT) :: iMaxPeriodIndex
   integer (kind=T_INT) :: iStat, iCount

   real (kind=T_DBL) :: rNewMean, rOldMean
   real (kind=T_DBL) :: rNewVariance, rOldVariance

   rNewMean = 0_T_DBL; rOldMean = 0_T_DBL
   rNewVariance = 0_T_DBL; rOldVariance = 0_T_DBL

   ! transfer data to temp data structure
   ! calculate mean and variance in the same pass
   ! (credit: Donald Knuth's Art of Computer Programming, Vol 2, page 232, 3rd edition)
   j=0
   do i=1,iCount
     if(this%tData(i)%lSelect) then
       j = j + 1
       rData(j) = this%tData(i)%rValue
       iJulianDay(j) = this%tData(i)%tDT%iJulianDay
       rNewMean = rOldMean + ( rData(j) - rOldMean ) / j
       rNewVariance = rOldVariance + ( rData(j) - rOldMean ) * ( rData(j) - rNewMean )
       rOldMean = rNewMean
       rOldVariance = rNewVariance
     endif
   enddo

   iCount = size(rData)

   ! allocate memory for stats object
   allocate(pBaseStats, stat=iStat)
   call assert(iStat == 0, "Memory allocation error", trim(__FILE__), __LINE__)

   pBaseStats%iCount = iCount

   pBaseStats%rSum = sum(rData)

!   tBaseStats%rMedian = median(rData)

   if(iCount > 0) pBaseStats%rMean = rNewMean

   pBaseStats%rVariance = rNewVariance

   pBaseStats%rStddev = sqrt(rNewVariance)
   if(pBaseStats%rMean /= 0.) pBaseStats%rCV = pBaseStats%rStddev / pBaseStats%rMean

!   do i=1,iNUM_QUANTILES
!     tBaseStats%rQuantile(i) = quantile(rSTD_PROBABILITIES(i), rSortedData)
!     tBaseStats%rExceedance(iNUM_QUANTILES - i + 1) = tBaseStats%rQuantile(i)
!   end do

   pBaseStats%rQuantile = quantiles(rSTD_PROBABILITIES, rData)
   pBaseStats%rExceedance = 1.0 - pBaseStats%rQuantile
   pBaseStats%rMedian = pBaseStats%rQuantile(P50)

   pBaseStats%rMin = MINVAL(rData)
   pBaseStats%rMax = MAXVAL(rData)
   pBaseStats%rRange = pBaseStats%rMax - pBaseStats%rMin
   iLocMin = MINLOC(rData)
   iLocMax = MAXLOC(rData)

   pBaseStats%iDayOfYearMin = day_of_year(iJulianDay(iLocMin(1)))
   pBaseStats%iDayOfYearMax = day_of_year(iJulianDay(iLocMax(1)))

   pBaseStats%rPeriodMax = 1.e-20
   pBaseStats%rPeriodMin = 1.e+20

   if(size(rData) >= 365) then
     iMaxPeriodIndex = D90
   else if(size(rData) >= 30) then
     iMaxPeriodIndex = D30
   else
     iMaxPeriodIndex = D1
   endif

   ! iterate over the indices: 1-day, 3-day, 7-day, 30-day, 90-day
   do i=1,iMaxPeriodIndex
     ! now iterate over all of rData, calculating the mean value for
     ! a given averaging period; track the extrema
     do j=1,size(rData) - AVERAGING_PERIOD(i)%iDaysInPeriod + 1
       k = j + AVERAGING_PERIOD(i)%iDaysInPeriod - 1
       rMean = SUM(rData(j:k)) / AVERAGING_PERIOD(i)%iDaysInPeriod
       if (rMean > pBaseStats%rPeriodMax(i)) pBaseStats%rPeriodMax(i) = rMean
       if (rMean < pBaseStats%rPeriodMin(i)) pBaseStats%rPeriodMin(i) = rMean
     end do
   end do

   pBaseStats%lValid = lTRUE

   return

end function ts_calc_base_stats




!------------------------------------------------------------------------------

end module tsp_time_series_manager
