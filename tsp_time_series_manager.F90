module tsp_time_series_manager

  use tsp_data_structures
  use tsp_utilities

  implicit none

  integer (kind=T_INT), parameter :: iDAILY_DATA = 0
  integer (kind=T_INT), parameter :: iMONTHLY_DATA = 1
  integer (kind=T_INT), parameter :: iANNUAL_DATA = 2

  type T_TIME_SERIES_DATA
    integer (kind=T_INT) :: iWaterYear     = 0
    type (T_DATETIME) :: tDT
    real (kind=T_SGL)    :: rValue         = 0.0
    character (len=10)   :: sDataFlag      = ""
    logical (kind=T_LOGICAL) :: lSelect = lFALSE

  contains
!    procedure :: calcWaterYear => calc_water_year_sub
!    procedure :: textToDate => text_to_date_sub
!    procedure :: new => new_time_series_sub

  end type T_TIME_SERIES_DATA

  type T_TIME_SERIES
    character (len=MAXNAMELENGTH) :: sSiteName = ""
    character (len=MAXNAMELENGTH) :: sSeriesName = ""
    character (len=256) :: sDescription = ""
    character (len=DATETEXTLENGTH) :: sDateFormat
    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate
    integer (kind=T_INT) :: iDataType = iDAILY_DATA
    type (T_TIME_SERIES_DATA), dimension(:), allocatable :: pData

  contains

    procedure :: selectByDate => select_by_date_fn
!    procedure :: selectByValue => select_by_value_sub
    procedure :: selectReset => select_reset_sub

    procedure :: new_time_series_fm_txt_sub
    procedure :: new_time_series_fm_NWIS_sub
    procedure :: new_time_series_fm_values_sub
    generic :: new => new_time_series_fm_txt_sub, &
                                new_time_series_fm_NWIS_sub, &
                                new_time_series_fm_values_sub
    procedure :: findDateMinAndMax => find_min_and_max_date_sub

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
       "Series name exceeds "//int2char(MAXNAMELENGTH)//" characters and will be truncated")

    this%sDescription = TRIM(sDescription)

    if(allocated(this%pData)) deallocate(this%pData, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    allocate(this%pData(iCount), stat=iStat)
    call Assert(iStat==0, "Unable to allocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    do i=1,iCount

       call this%pData(i)%tDT%parseDate(sDateTxt(i))
       call this%pData(i)%tDT%parseTime(sTimeTxt(i))
       read(sValTxt(i),*) this%pData(i)%rValue

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
!    call Assert(iCount == size(rValue), "Date and/or value vectors of unequal length", &
!        TRIM(__FILE__), __LINE__)

    this%sSeriesName = TRIM(sSeriesName)
    call Warn(len_trim(sSeriesName) <= MAXNAMELENGTH, &
       "Series name exceeds "//int2char(MAXNAMELENGTH)//" characters and will be truncated")

    this%sDescription = TRIM(sDescription)

    if(allocated(this%pData)) deallocate(this%pData, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    allocate(this%pData(iCount), stat=iStat)
    call Assert(iStat==0, "Unable to allocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    do i=1,iCount

       call this%pData(i)%tDT%calcJulianDay(iMonth(i), iDay(i), iYear(i), &
             iHour(i), iMinute(i), iSecond(i))
       this%pData(i)%rValue = rValue(i)

    enddo

    call this%findDateMinAndMax()

  end subroutine new_time_series_fm_values_sub

!------------------------------------------------------------------------------

  subroutine new_time_series_fm_NWIS_sub(this, pGage, sNewSeriesName)

    class(T_TIME_SERIES) :: this
    type (T_USGS_NWIS_GAGE) :: pGage
    character(len=256), optional :: sNewSeriesName

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    character(len=256) ::  sRecord, sItem

    iCount = size(pGage%pGageData)

    this%sSiteName = TRIM(pGage%sSiteNumber)

    if(present(sNewSeriesName)) then
      this%sSeriesName = TRIM(sNewSeriesName)
      call Warn(len_trim(sNewSeriesName) <= MAXNAMELENGTH, &
        "Series name exceeds "//int2char(MAXNAMELENGTH)//" characters and will be truncated")
    else
      this%sSeriesName = TRIM(pGage%sSiteNumber)
      call Warn(len_trim(pGage%sSiteNumber) <= MAXNAMELENGTH, &
        "Series name exceeds "//int2char(MAXNAMELENGTH)//" characters and will be truncated")
    end if

    this%sDescription = TRIM(pGage%sDescription)

    if(allocated(this%pData)) deallocate(this%pData, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    allocate(this%pData(iCount), stat=iStat)
    call Assert(iStat==0, "Unable to allocate memory to create new time series", &
       TRIM(__FILE__), __LINE__)

    this%pData%iWaterYear = pGage%pGageData%iWaterYear
    this%pData%tDT = pGage%pGageData%tDT
    this%pData%rValue = pGage%pGageData%rMeanDischarge
    this%pData%sDataFlag = pGage%pGageData%sDataFlag

    call this%findDateMinAndMax()

  end subroutine new_time_series_fm_NWIS_sub

!------------------------------------------------------------------------------

  subroutine find_min_and_max_date_sub(this)

    class(T_TIME_SERIES) :: this

    ! [ LOCALS ]
    type (T_DATETIME) :: tMINDATE
    type (T_DATETIME) :: tMAXDATE
    integer (kind=T_INT) :: i

    call tMINDATE%calcJulianDay( 1, 1, 3000, 0, 0, 0)
    call tMAXDATE%calcJulianDay( 1, 1, 1, 0, 0, 0)

    do i=1,size(this%pData)
      if(this%pData(i)%tDT < tMINDATE) tMINDATE = this%pData(i)%tDT
      if(this%pData(i)%tDT > tMAXDATE) tMAXDATE = this%pData(i)%tDT
!      print *, this%pData(i)%tDT%prettyDate(), " ",tMINDATE%prettyDate()," ", tMAXDATE%prettyDate()
    enddo

    this%tStartDate = tMINDATE
    this%tEndDate = tMAXDATE

  end subroutine find_min_and_max_date_sub

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

  subroutine select_reset_sub(this)

    class(T_TIME_SERIES) :: this

    this%pData%lSelect = lFALSE

  end subroutine select_reset_sub

!------------------------------------------------------------------------------

  function select_by_date_fn(this, tGE_Date, tLE_Date, lKeepCurrSelect)  result(iCount)

    class(T_TIME_SERIES) :: this
    type (T_DATETIME), intent(in), optional :: tGE_Date
    type (T_DATETIME),intent(in),optional   :: tLE_Date
    logical (kind=T_LOGICAL), intent(in), optional :: lKeepCurrSelect
    integer (kind=T_INT) :: iCount

    ! [ LOCALS ]
    type (T_DATETIME) :: tGE_DT
    type (T_DATETIME) :: tLE_DT
    logical (kind=T_LOGICAL) :: lKeep
    integer (kind=T_INT) :: i

    ! if no arguments supplied, all time series elements will be selected
    if(present(tGE_Date)) then
      tGE_DT = tGE_Date
    else
      tGE_DT = this%tStartDate
    endif

    if(present(tLE_Date)) then
      tLE_DT = tLE_Date
    else
      tLE_DT = this%tEndDate
    endif

    ! if this is TRUE, the select is only applied to elements that are
    ! already selected (as in a previous step)
    if(present(lKeepCurrSelect)) then
      lKeep = lKeepCurrSelect
    else
      lKeep = lFALSE
    endif

    if(lKeep) then
      do i=1,size(this%pData)
        if(this%pData(i)%lSelect &
              .and. this%pData(i)%tDT >= tGE_DT &
              .and. this%pData(i)%tDT <= tLE_DT) then
          this%pData(i)%lSelect = lTRUE
        endif
      enddo
    else
      do i=1,size(this%pData)
        if(this%pData(i)%tDT >= tGE_DT &
              .and. this%pData(i)%tDT <= tLE_DT) then
          this%pData(i)%lSelect = lTRUE
        else
          this%pData(i)%lSelect = lFALSE
        endif
      enddo
    endif

    iCount = count(this%pData%lSelect)

  end function select_by_date_fn



end module tsp_time_series_manager
