module tsp_time_series_manager

  use tsp_data_structures
  use tsp_utilities
  use tsp_statistics
  use tsp_datetime_class
  use tsp_control_file_ops
  implicit none

  integer (kind=T_INT), parameter :: iDAILY_DATA = 0
  integer (kind=T_INT), parameter :: iSUMMARY_BY_MONTH = 1
  integer (kind=T_INT), parameter :: iMONTHLY_DATA = 2
  integer (kind=T_INT), parameter :: iANNUAL_DATA = 3

  type T_TIME_SERIES_DATA
    integer (kind=T_INT) :: iWaterYear     = 0
    type (T_DATETIME) :: tDT
    real (kind=T_SGL)    :: rValue         = -HUGE(rZERO)
    character (len=10)   :: sDataFlag      = ""
    logical (kind=T_LOGICAL) :: lSelect = lFALSE

  contains
!    procedure :: calcWaterYear => calc_water_year_sub
!    procedure :: textToDate => text_to_date_sub

  end type T_TIME_SERIES_DATA

  type T_TIME_SERIES
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
    integer (kind=T_INT) :: iDataType = iDAILY_DATA
    integer (kind=T_INT) :: iListOutputPosition = -999
    type (T_TIME_SERIES_DATA), dimension(:), allocatable :: tData

  contains

    procedure :: selectByDate => select_by_date_fn
    procedure :: selectByValue => select_by_value_fn

    procedure :: reset => select_reset_sub
    procedure :: list => list_output_sub
    procedure :: writeInstructions => list_output_instructions_sub
    procedure :: calcPeriodStatistics => calculate_period_statistics_fn

    procedure :: new_time_series_fm_txt_sub
    procedure :: new_time_series_fm_NWIS_sub
    procedure :: new_time_series_fm_values_sub
    procedure :: new_time_series_fm_DT_sub
    generic :: new => new_time_series_fm_txt_sub, &
                                new_time_series_fm_NWIS_sub, &
                                new_time_series_fm_values_sub, &
                                new_time_series_fm_DT_sub
    procedure :: findDateMinAndMax => find_min_and_max_date_sub
    procedure :: findDataGaps => find_data_gaps_fn
    procedure :: findHydroEvents => find_hydro_events_fn
    procedure :: integrate => integrate_fn

!    procedure :: reducetimespan => reduce_time_span_sub

  end type T_TIME_SERIES

contains

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

    this%tData%iWaterYear = tTS%tData%iWaterYear
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

   iCount = this%selectByDate(tGE_DT, tLE_DT)

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

          print *, i, iNumGaps, iSize, size(pDateRange)
          iNumGaps = iNumGaps + 1
          call pDateRange(iNumGaps)%new(tStartDate, this%tData(i-1)%tDT)
          tStartDate = this%tData(i+1)%tDT

          write(LU_STD_OUT, fmt="(i3,') ',a,' to ',a)") iNumGaps, &
                  pDateRange(iNumGaps)%tStartDate%listdate(), &
                  pDateRange(iNumGaps)%tEndDate%listdate()

        endif

      enddo

      ! need to write out info on the *last* gapless interval
      iNumGaps = iNumGaps + 1
      call pDateRange(iNumGaps)%new(tStartDate, this%tData(iSize)%tDT)
      write(LU_STD_OUT, fmt="(i3,') ',a,' to ',a)") iNumGaps, &
             pDateRange(iNumGaps)%tStartDate%listdate(), &
             pDateRange(iNumGaps)%tEndDate%listdate()

    else   ! no gaps found; return a single date range encompassing all data

      allocate(pDateRange(1) )
      call pDateRange(1)%new(this%tData(1)%tDT, this%tData(iSize)%tDT)
      write(LU_STD_OUT,fmt="(/,'  ** NO GAPS **',/)")
      write(LU_STD_OUT, fmt="(i3,') ',a,' to ',a)") 1, &
                pDateRange(1)%tStartDate%listdate(), &
                pDateRange(1)%tEndDate%listdate()

    endif

  end function find_data_gaps_fn

!------------------------------------------------------------------------------

  subroutine select_reset_sub(this)

    class(T_TIME_SERIES) :: this

    this%tData%lSelect = lFALSE

  end subroutine select_reset_sub

!------------------------------------------------------------------------------

  function select_by_date_fn(this, GE, LE, keep)  result(iCount)

    class(T_TIME_SERIES) :: this
    type (T_DATETIME), intent(in), optional :: GE
    type (T_DATETIME),intent(in),optional   :: LE
    logical (kind=T_LOGICAL), intent(in), optional :: keep
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    type (T_DATETIME) :: tGE_DT
    type (T_DATETIME) :: tLE_DT
    logical (kind=T_LOGICAL) :: lKeep
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

    ! if this is TRUE, the select does not alter the status for points
    ! outside the current selection range (i.e. if lKeep is false, ALL
    ! points in the series are scanned and assigned TRUE/FALSE status.
    if(present(keep)) then
      lKeep = keep
    else
      lKeep = lFALSE
    endif

    if(lKeep) then

      do i=1,size(this%tData)
        if(this%tData(i)%tDT >= tGE_DT &
            .and. this%tData(i)%tDT <= tLE_DT) then
          this%tData(i)%lSelect = lTRUE
        endif
      enddo

    else

      do i=1,size(this%tData)
        if(this%tData(i)%tDT >= tGE_DT &
              .and. this%tData(i)%tDT <= tLE_DT) then
          this%tData(i)%lSelect = lTRUE
        else
          this%tData(i)%lSelect = lFALSE
        endif
      enddo

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_date_fn

!------------------------------------------------------------------------------

  function select_by_change_rate_fn(this, MinIncrease, MinDecrease, keep)  result(iCount)

    class(T_TIME_SERIES) :: this
    real (kind=T_SGL), optional :: MinIncrease
    real (kind=T_SGL), optional :: MinDecrease
    logical (kind=T_LOGICAL), intent(in), optional :: keep
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    real (kind=T_SGL) :: rMinIncrease_Val
    real (kind=T_SGL) :: rMinDecrease_Val
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i
    real (kind=T_SGL) :: rDelta

    ! if no arguments supplied, assume rise rate of 10% of median flow represents
    if(present(MinIncrease)) then
      rMinIncrease_Val = MinIncrease
    else
      rMinIncrease_Val = median(this%tData%rValue) * 0.10
    endif

    if(present(MinDecrease)) then
      rMinDecrease_Val = MinDecrease
    else
      rMinDecrease_Val = -rMinIncrease_Val
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
        if( ( rDelta >= rMinIncrease_Val .or. rDelta <= rMinDecrease_Val ) ) then
          this%tData(i)%lSelect = lTRUE
        endif
      enddo

    else

      do i=2,size(this%tData)
        if( rDelta >= rMinIncrease_Val .or. rDelta <= rMinDecrease_Val )  then
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

      do i=1,size(this%tData)
        if( this%tData(i)%rValue >= rGE_Val &
            .and. this%tData(i)%rValue <= rLE_Val ) then
          this%tData(i)%lSelect = lTRUE
        endif
      enddo

    else

      do i=1,size(this%tData)
        if( this%tData(i)%rValue >= rGE_Val &
            .and. this%tData(i)%rValue <= rLE_Val ) then
          this%tData(i)%lSelect = lTRUE
        else
          this%tData(i)%lSelect = lFALSE
        endif
      enddo

    endif

    iCount = count(this%tData%lSelect)

  end function select_by_value_fn

!------------------------------------------------------------------------------

  subroutine list_output_sub(this, sDateFormat, iLU)

    class(T_TIME_SERIES) :: this
    character(len=*), intent(in), optional :: sDateFormat
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: LU
    integer (kind=T_INT) :: iYear
    character(len=20) :: sDateFmt
    character (len=256) :: sFormatString

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    if(present(sDateFormat)) then
      sDateFmt = trim(sDateFormat)
    else
      sDateFmt = "MM/DD/YYYY"
    endif

    write(LU,fmt="(/,' TIME_SERIES ',a,' ---->')") quote(this%sSeriesName)

    if(this%iDataType == iDAILY_DATA) then

      sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,a20,3x,g16.8)"

      do i=1,size(this%tData)

         write(LU,fmt=trim(sFormatString)) trim(this%sSeriesName), &
            this%tData(i)%tDT%listdatetime(sDateFmt), this%tData(i)%rValue

      enddo

    elseif (this%iDataType == iSUMMARY_BY_MONTH) then

      sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,a15,3x,g16.8)"

      do i=1,size(this%tData)

         write(LU,fmt=trim(sFormatString)) trim(this%sSeriesName), &
            trim(MONTH(this%tData(i)%tDT%iMonth)%sName), this%tData(i)%rValue

      enddo

    elseif (this%iDataType == iMONTHLY_DATA) then

      sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,a15,3x,g16.8)"

      do i=1,size(this%tData)

         iYear = this%tData(i)%tDT%iYear
         write(LU,fmt=trim(sFormatString)) trim(this%sSeriesName), &
            trim(MONTH(this%tData(i)%tDT%iMonth)%sName)//" " &
            //trim(asChar(iYear)), this%tData(i)%rValue

      enddo

    elseif (this%iDataType == iANNUAL_DATA) then

      sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,a10,3x,g16.8)"

      do i=1,size(this%tData)

         iYear = this%tData(i)%tDT%iYear
         write(LU,fmt=trim(sFormatString)) trim(this%sSeriesName), &
              trim(asChar(iYear)), this%tData(i)%rValue

      enddo

    endif

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
      iCount = this%selectByDate(tStartDate, tEndDate)
      lSelect = lFALSE

      ! initialize variables needed to perform comparisons
      iLastIndex = 1
      rLastValue = this%tData(1)%rValue
      tLastDate = this%tData(1)%tDT

      do i=1, size(this%tData)

        ! if this element is not selected (i.e. within selected date range)
        ! jump to next iteration of loop
        if( .not. this%tData(i)%lSelect ) cycle

        ! OK, now reset lSelect in preparation for peak identification
        this%tData(i)%lSelect = lFALSE

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
      call this%reset()

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
                                   keep = lTRUE)
     enddo

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
     pTempSeries%tStartDate = this%tStartDate
     pTempSeries%tEndDate = this%tEndDate
     pTempSeries%sSeriesName = trim(sNewSeriesName)

  end function find_hydro_events_fn

!------------------------------------------------------------------------------

  function calculate_period_statistics_fn(this, pBlock)    result(pTempSeries)

      class(T_TIME_SERIES) :: this
      type (T_BLOCK), pointer :: pBlock
      type (T_TIME_SERIES), dimension(:), pointer :: pTempSeries

      ! [ LOCALS ]
      integer (kind=T_INT) :: iSize, iStatSize
      integer (kind=T_INT) :: i, j, k
      integer (kind=T_INT) :: iYear, iMonth, iFirstYear
      integer (kind=T_INT) :: iStat
      integer (kind=T_INT) :: iCount
      character (len=256) :: sBuf
      type (T_DATETIME) :: tStartDate, tEndDate
      type (T_STATS_COLLECTION), pointer :: pStats
      character (len=MAXNAMELENGTH) :: sSeriesName
      logical (kind=T_LOGICAL) :: lAddSuffix

      real (kind=T_SGL), dimension(:), allocatable :: rTempValue
      type (T_DATETIME), dimension(:), allocatable :: tTempDateTime

      character (len=MAXARGLENGTH), dimension(:), pointer :: &
         pNEW_SERIES_NAME, pSTATISTIC, pPERIOD

      call processUserSuppliedDateTime(pBlock, tStartDate, tEndDate)

      if(tStartDate < this%tStartDate) tStartDate = this%tStartDate
      if(tEndDate > this%tEndDate) tEndDate = this%tEndDate

      ! do *NOT* tack on a descriptive suffix to the output series name
      lAddSuffix = lFALSE
      sSeriesName = this%sSeriesName

      ! restrict data to specified date range, if desired
      iCount =  this%selectByDate(tStartDate, tEndDate)
      call Assert(iCount > 0, "Problem calculating PERIOD_STATISTICs: no time series data within " &
        //"specified date range", trim(__FILE__), __LINE__)
      ! allocate memory for temporary data array
      allocate(rTempValue(iCount))
      allocate(tTempDateTime(iCount))
      rTempValue = pack(this%tData%rValue, this%tData%lSelect)
      tTempDateTime = pack(this%tData%tDT, this%tData%lSelect)

      pSTATISTIC => pBlock%getString("STATISTIC")
      if(str_compare(pSTATISTIC(1), "NA")) pSTATISTIC(1) = "mean"

      pPERIOD => pBlock%getString("PERIOD")
      if(str_compare(pPERIOD(1), "NA")) pPERIOD(1) = "year"

      ! if only one period is given, assume it applies to all statistics
      if(size(pPeriod) == 1 .and. size(pStatistic) > 1) then
        sBuf = pPERIOD(1)
        deallocate(pPERIOD)
        allocate(pPERIOD(size(pSTATISTIC)) )
        pPERIOD = trim(sBUF)
      endif

      call Assert(size(pPERIOD) == size(pSTATISTIC), &
        "An averaging period must be specified for each statistic", trim(__FILE__), __LINE__)

      iSize = size(pSTATISTIC)

      allocate(pTempSeries(iSize), stat=iStat )

      ! determine what the new series name(s) should be....
      pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
      if( str_compare(pNEW_SERIES_NAME(1), "NA") ) then
        pTempSeries%sSeriesName = trim(sSeriesName)//"_"
        lAddSuffix = lTRUE
      else
        do i=1,iSize
          pTempSeries%sSeriesName = pNEW_SERIES_NAME(i)
        enddo
      endif

      ! all kinds of period statistics are calculated by invoking
      ! "create_stats_object"...now we have to copy the appropriate
      ! results to time series data structures
      pStats => create_stats_object(rTempValue, tTempDateTime%iMonth, &
           tTempDateTime%iYear, tTempDateTime%iJulianDay)

      do i=1,iSize
        if(str_compare(pPERIOD(i),"month_one") ) then

          iStatSize = 12

          allocate(pTempSeries(i)%tData(iStatSize), stat = iStat )
          pTempSeries(i)%iDataType = iSUMMARY_BY_MONTH
          pTempSeries(i)%tStartDate = tStartDate
          pTempSeries(i)%tEndDate = tEndDate
          do j=1,iStatSize
            pTempSeries(i)%tData(j)%tDT%iMonth = j
          enddo

          if(str_compare(pSTATISTIC(i), "sum") ) then

            pTempSeries(i)%tData%rValue = pStats%pByMonth%rSum
            pTempSeries(i)%sDescription = &
              "Monthly sum for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"msm"

          elseif(str_compare(pSTATISTIC(i), "mean") ) then

            pTempSeries(i)%tData%rValue = pStats%pByMonth%rMean
            pTempSeries(i)%sDescription = &
              "Monthly mean for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"mav"

          elseif(str_compare(pSTATISTIC(i), "std_dev") ) then

            pTempSeries(i)%tData%rValue = pStats%pByMonth%rStddev
            pTempSeries(i)%sDescription = &
              "Monthly standard deviation for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"msd"

          elseif(str_compare(pSTATISTIC(i), "maximum") ) then

            pTempSeries(i)%tData%rValue = pStats%pByMonth%rMax
            pTempSeries(i)%sDescription = &
              "Monthly maximum for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"mmx"

          elseif(str_compare(pSTATISTIC(i), "minimum") ) then

            pTempSeries(i)%tData%rValue = pStats%pByMonth%rMin
            pTempSeries(i)%sDescription = &
              "Monthly sum minimum time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"mmn"

          elseif(str_compare(pSTATISTIC(i), "range") ) then

            pTempSeries(i)%tData%rValue = pStats%pByMonth%rRange
            pTempSeries(i)%sDescription = &
              "Monthly range for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"mrg"

          else

            call Assert(lFALSE, "Unknown STATISTIC "//quote(pSTATISTIC(i) ), &
               trim(__FILE__),__LINE__)

          endif

        elseif(str_compare(pPERIOD(i),"month_many") ) then

          iStatSize = count(pStats%pByYearAndMonth%lValid)

          allocate(pTempSeries(i)%tData(iStatSize), stat = iStat )
          pTempSeries(i)%iDataType = iMONTHLY_DATA
          pTempSeries(i)%tStartDate = tStartDate
          pTempSeries(i)%tEndDate = tEndDate

          j=0
          do iYear = lbound(pStats%pByYearAndMonth,1), ubound(pStats%pByYearAndMonth,1)
            do iMonth = 1,12
              if(pStats%pByYearAndMonth(iYear, iMonth)%lValid) then
                j=j+1
                pTempSeries(i)%tData(j)%tDT%iMonth = iMonth
                pTempSeries(i)%tData(j)%tDT%iYear = iYear
              endif
            enddo
          enddo

          if(str_compare(pSTATISTIC(i), "sum") ) then

            j=0
            do iYear = lbound(pStats%pByYearAndMonth,1), ubound(pStats%pByYearAndMonth,1)
              do iMonth = 1,12
                if(pStats%pByYearAndMonth(iYear, iMonth)%lValid) then
                  j=j+1
                  pTempSeries(i)%tData(j)%rValue = pStats%pByYearAndMonth(iYear, iMonth)%rSum
                endif
              enddo
            enddo
            pTempSeries(i)%sDescription = &
              "Sum of monthly values for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"mssm"

          elseif(str_compare(pSTATISTIC(i), "mean") ) then

            j=0
            do iYear = lbound(pStats%pByYearAndMonth,1), ubound(pStats%pByYearAndMonth,1)
              do iMonth = 1,12
                if(pStats%pByYearAndMonth(iYear, iMonth)%lValid) then
                  j=j+1
                  pTempSeries(i)%tData(j)%rValue = pStats%pByYearAndMonth(iYear, iMonth)%rMean
                endif
              enddo
            enddo
            pTempSeries(i)%sDescription = &
              "Mean monthly values for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"msav"

          elseif(str_compare(pSTATISTIC(i), "std_dev") ) then

            j=0
            do iYear = lbound(pStats%pByYearAndMonth,1), ubound(pStats%pByYearAndMonth,1)
              do iMonth = 1,12
                if(pStats%pByYearAndMonth(iYear, iMonth)%lValid) then
                  j=j+1
                  pTempSeries(i)%tData(j)%rValue = pStats%pByYearAndMonth(iYear, iMonth)%rStddev
                endif
              enddo
            enddo
            pTempSeries(i)%sDescription = &
              "Standard deviation monthly values for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"mssd"

          elseif(str_compare(pSTATISTIC(i), "maximum") ) then

            j=0
            do iYear = lbound(pStats%pByYearAndMonth,1), ubound(pStats%pByYearAndMonth,1)
              do iMonth = 1,12
                if(pStats%pByYearAndMonth(iYear, iMonth)%lValid) then
                  j=j+1
                  pTempSeries(i)%tData(j)%rValue = pStats%pByYearAndMonth(iYear, iMonth)%rMax
                endif
              enddo
            enddo
            pTempSeries(i)%sDescription = &
              "Maximum monthly values for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"msmx"

          elseif(str_compare(pSTATISTIC(i), "minimum") ) then

            j=0
            do iYear = lbound(pStats%pByYearAndMonth,1), ubound(pStats%pByYearAndMonth,1)
              do iMonth = 1,12
                if(pStats%pByYearAndMonth(iYear, iMonth)%lValid) then
                  j=j+1
                  pTempSeries(i)%tData(j)%rValue = pStats%pByYearAndMonth(iYear, iMonth)%rMin
                endif
              enddo
            enddo
            pTempSeries(i)%sDescription = &
              "Minimum monthly values for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"msmn"

          elseif(str_compare(pSTATISTIC(i), "range") ) then

            j=0
            do iYear = lbound(pStats%pByYearAndMonth,1), ubound(pStats%pByYearAndMonth,1)
              do iMonth = 1,12
                if(pStats%pByYearAndMonth(iYear, iMonth)%lValid) then
                  j=j+1
                  pTempSeries(i)%tData(j)%rValue = pStats%pByYearAndMonth(iYear, iMonth)%rRange
                endif
              enddo
            enddo
            pTempSeries(i)%sDescription = &
              "Range of monthly values for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"msrg"

          else

            call Assert(lFALSE, "Unknown STATISTIC "//quote(pSTATISTIC(i) ), &
               trim(__FILE__),__LINE__)

          endif

        elseif(str_compare(pPERIOD(i),"year") ) then

          iStatSize = count(pStats%pByYear%lValid)

          allocate(pTempSeries(i)%tData(iStatSize), stat = iStat )
          pTempSeries(i)%iDataType = iANNUAL_DATA
          pTempSeries(i)%tStartDate = tStartDate
          pTempSeries(i)%tEndDate = tEndDate
          iFirstYear =  lbound(pStats%pByYear,1)
          k = 0
          do j = 1, size(pStats%pByYear)
            if(pStats%pByYear(j - 1 + iFirstYear)%lValid) then
              k = k + 1
              pTempSeries(i)%tData(k)%tDT%iYear = (j-1) + iFirstYear
            endif
          enddo

          if(str_compare(pSTATISTIC(i), "sum") ) then

            pTempSeries(i)%tData%rValue = &
              pack(pStats%pByYear%rSum, pStats%pByYear%lValid)
            pTempSeries(i)%sDescription = &
              "Annual sum for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"asm"

          elseif(str_compare(pSTATISTIC(i), "mean") ) then

            pTempSeries(i)%tData%rValue = &
              pack(pStats%pByYear%rMean, pStats%pByYear%lValid)
            pTempSeries(i)%sDescription = &
              "Annual mean for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"aav"

          elseif(str_compare(pSTATISTIC(i), "std_dev") ) then

            pTempSeries(i)%tData%rValue = &
              pack(pStats%pByYear%rStddev, pStats%pByYear%lValid)
            pTempSeries(i)%sDescription = &
              "Annual standard deviation for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"asd"

          elseif(str_compare(pSTATISTIC(i), "maximum") ) then

            pTempSeries(i)%tData%rValue = &
              pack(pStats%pByYear%rMax, pStats%pByYear%lValid)
            pTempSeries(i)%sDescription = &
              "Annual maximum for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"amx"

          elseif(str_compare(pSTATISTIC(i), "minimum") ) then

            pTempSeries(i)%tData%rValue = &
              pack(pStats%pByYear%rMin, pStats%pByYear%lValid)
            pTempSeries(i)%sDescription = &
              "Annual minimum for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"amn"

          elseif(str_compare(pSTATISTIC(i), "range") ) then

            pTempSeries(i)%tData%rValue = &
              pack(pStats%pByYear%rRange, pStats%pByYear%lValid)
            pTempSeries(i)%sDescription = &
              "Annual range for time series "//quote(sSeriesName)
            if(lAddSuffix) pTempSeries(i)%sSeriesName = &
               trim(pTempSeries(i)%sSeriesName)//"arg"

          else

            call Assert(lFALSE, "Unknown STATISTIC "//quote(pSTATISTIC(i) ), &
               trim(__FILE__),__LINE__)

          endif

        endif

      enddo

  end function calculate_period_statistics_fn

!------------------------------------------------------------------------------

end module tsp_time_series_manager
