module tsp_table_manager

  use tsp_data_structures
  use tsp_utilities
  use tsp_control_file_ops
  use tsp_time_series_manager
  use tsp_statistics
  use tsp_datetime_class
  implicit none

  integer (kind=T_INT), parameter :: iSTABLE = 1
  integer (kind=T_INT), parameter :: iCTABLE = 2
  integer (kind=T_INT), parameter :: iETABLE = 3
  integer (kind=T_INT), parameter :: iVTABLE = 4
  integer (kind=T_INT), parameter :: iITABLE = 5

  integer (kind=T_INT), parameter :: iOBSERVED = 1
  integer (kind=T_INT), parameter :: iSIMULATED = 2
  integer (kind=T_INT), parameter :: iOTHER = 3

  character (len=7), dimension(5), parameter :: TABLE_TYPE = &
       [ "S_TABLE", "C_TABLE", "E_TABLE", "V_TABLE" , "I_TABLE" ]

  type, public :: T_TABLE_DATA
    character (len=MAXARGLENGTH) :: sKeyword
    character (len=256) :: sDescription = ""
    character (len=18), dimension(3) :: sValue = ""
!    logical (kind=T_LOGICAL) :: lSelect = lFALSE
  end type T_TABLE_DATA

!  type, extends(T_TABLE_DATA) :: T_VTABLE_DATA
!    type (T_DATETIME) :: tStartDate
!    type (T_DATETIME) :: tEndDate
!  end type T_VTABLE_DATA

  type, public :: T_TABLE
    type (T_TABLE), pointer :: pNext => null()
    type (T_TABLE), pointer :: pPrevious => null()
    character (len=MAXNAMELENGTH) :: sSeriesName = ""
    character (len=256) :: sDescription = ""
    character (len=256) :: sHeader = ""
    integer (kind=T_INT) :: iTableType = 0
    integer (kind=T_INT) :: iListOutputPosition = 0
    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate
    type (T_TABLE_DATA), dimension(:), allocatable :: tTableData

  contains

    procedure, public :: calc_s_table => table_calc_s_table
    procedure, public :: calc_e_table => table_calc_e_table
    procedure, public :: calc_c_table => table_calc_c_table
    procedure, public :: calc_v_table => table_calc_v_table
    procedure, public :: calc_i_table => table_calc_i_table
    procedure, public :: list => table_list_output_sub
    procedure, public :: new => new_table_sub
    procedure, public :: writeInstructions => table_list_output_instructions_sub

  end type T_TABLE

!  type, public :: T_VTABLE
!    type (T_DATETIME) :: tStartDate
!    type (T_DATETIME) :: tEndDate
!    type (T_VTABLE_DATA), dimension(:), allocatable :: tVTableData
!  end type T_VTABLE

contains

  subroutine new_table_sub(this, tTableData)

    class (T_TABLE) :: this
    type (T_TABLE_DATA), dimension(:), allocatable :: tTableData

    ! [ LOCALS ]
    integer (kind=T_INT) :: iStat

    allocate( this%tTableData(size(tTableData)), stat = iStat)
    call Assert(iStat == 0, "Problem allocating memory for new table", &
      trim(__FILE__), __LINE__)

    this%tTableData = tTableData
    deallocate(tTableData, stat = iStat)
    call Assert(iStat == 0, "Problem deallocating memory while creating new table", &
      trim(__FILE__), __LINE__)

  end subroutine new_table_sub

!------------------------------------------------------------------------------

  subroutine table_calc_c_table(this, pObservedSeries, pModeledSeries, iOptions_, &
      pBaseSeries_, rExponent_)

    class (T_TABLE) :: this
    type (T_TIME_SERIES), pointer :: pObservedSeries
    type (T_TIME_SERIES), pointer :: pModeledSeries
    integer (kind=T_INT), dimension(:), optional :: iOptions_
    type (T_TIME_SERIES), pointer, optional :: pBaseSeries_
    real (kind=T_SGL), optional :: rExponent_

    ! [ LOCALS ]

    ! locals to hold either default or user-defined values
    integer (kind=T_INT), dimension(:), allocatable :: iOptions
    real (kind=T_SGL) :: rExponent
    integer (kind=T_INT) :: i, j, iCount
    real (kind=T_DBL) :: rCount
    real (kind=T_SGL), dimension(:), allocatable :: rObservedValues
    real (kind=T_SGL), dimension(:), allocatable :: rModeledValues
    real (kind=T_SGL), dimension(:), allocatable :: rBaseValues
    real (kind=T_DBL) :: rSum1, rSum2, rSum3, rSum4, rSum5, rSum6, rSum7, rDelta
    integer (kind=T_INT), parameter :: iNUMBER_OF_DESCRIPTIVE_LINES = 6

    this%iTableType = iCTABLE

    ! zero out accumulators
    rSum1=rD_ZERO; rSum2=rD_ZERO; rSum3=rD_ZERO; rSum4=rD_ZERO
    rSum5=rD_ZERO; rSum6 = rD_ZERO; rSum7 = rD_ZERO; rDelta=rD_ZERO

    if(present(iOptions_) ) then
      allocate(iOptions(size(iOptions_)) )
      iOptions = iOptions_
    else
      allocate(iOptions( iNUM_C_TABLE_STATS) )
        do i=1,iNUM_C_TABLE_STATS
          iOptions(i) = i
        enddo
    endif

    if(present(rExponent_) ) then
      rExponent = rExponent_
    else
      rExponent = 1.0
    endif

    ! time series for which statistics are to be calculated may have different numbers
    ! of elements, although the selected elements should be idential; need to make a copy
    allocate(rObservedValues(count(pObservedSeries%tData%lSelect) ) )
    allocate(rModeledValues(count(pModeledSeries%tData%lSelect) ) )
    rObservedValues = pack(pObservedSeries%tData%rValue, pObservedSeries%tData%lSelect)
    rModeledValues = pack(pModeledSeries%tData%rValue, pModeledSeries%tData%lSelect)

    ! if an argument was supplied for SERIES_BASE_NAME, we allocate space for it
    if(present(pBaseSeries_)) then
      allocate(rBaseValues(count(pBaseSeries_%tData%lSelect) ) )
      rBaseValues = pack(pBaseSeries_%tData%rValue, pBaseSeries_%tData%lSelect)
    endif

    ! allocate memory for TABLE object
    allocate(this%tTableData( size(iOptions) + iNUMBER_OF_DESCRIPTIVE_LINES ) )

    ! first, populate the descriptive lines of the table (series names, dates)
    this%tTableData(1)%sDescription = "Observation time series name: "
    this%tTableData(1)%sValue(1) = pObservedSeries%sSeriesName
    this%tTableData(2)%sDescription = "Modeled time series name: "
    this%tTableData(2)%sValue(1) = pModeledSeries%sSeriesName
    this%tTableData(3)%sDescription = "Starting date of series comparison: "
    this%tTableData(3)%sValue(1) = this%tStartDate%listdate()
    this%tTableData(4)%sDescription = "Starting time of series comparison: "
    this%tTableData(4)%sValue(1) = this%tStartDate%listtime()
    this%tTableData(5)%sDescription = "Ending date of series comparison: "
    this%tTableData(5)%sValue(1) = this%tEndDate%listdate()
    this%tTableData(6)%sDescription = "Ending time of series comparison: "
    this%tTableData(6)%sValue(1) = this%tEndDate%listtime()

    ! we shouldn't really need to check lengths at this point, but better safe than sorry
    call assert(size(rObservedValues) == size(rModeledValues), &
      "Internal programming error - unequal array sizes in C-table calculation", &
      trim(__FILE__), __LINE__)

    iCount = size(rObservedValues)
    rCount = real(iCount, kind=T_DBL)

    ! calculate base statistics from which the selected comparison measures will be derived
    do i=1, iCount
      rDelta = rModeledValues(i) - rObservedValues(i)
      rSum1 = rSum1 + rDelta
      rSum2 = rSum2 + (rDelta * rDelta)
      rSum3 = rSum3 + rObservedValues(i)
      rSum4 = rSum4 + abs( rDelta ) ** rExponent
    enddo

    rSum3 = rSum3 / iCount

    ! this calculation needed for Nash-Sutcliffe
    do i=1, iCount
      rDelta = rObservedValues(i) - rSum3
      rSum5 = rSum5 + ( rDelta * rDelta )   ! note the reuse of rDelta!
    enddo

    if(allocated(rBaseValues)) then
      do i=1,iCount
        rDelta = abs(rObservedValues(i) - rBaseValues(i)) ** rExponent
        rSum6 = rSum6 + rDelta
        rDelta = ( abs(rObservedValues(i) - rBaseValues(i)) &
                  + abs(rModeledValues(i) - rBaseValues(i)) ) ** rExponent
        rSum7 = rSum7 + rDelta
      enddo
    else
      do i=1,iCount
        rDelta = abs(rObservedValues(i) - rSum3) ** rExponent
        rSum6 = rSum6 + rDelta
        rDelta = ( abs(rObservedValues(i) - rSum3) &
                  + abs(rModeledValues(i) - rSum3) ) ** rExponent
        rSum7 = rSum7 + rDelta
      enddo
    endif

    do i=1, size(iOptions)

      j = i + iNUMBER_OF_DESCRIPTIVE_LINES

      select case(iOptions(i) )

        case(BIAS)

          this%tTableData(j)%sDescription = "Bias: "
          this%tTableData(j)%sValue(1) = asChar( rSum1 / rCount )

        case(STANDARD_ERROR)

          this%tTableData(j)%sDescription = "Standard error: "
          this%tTableData(j)%sValue(1) = asChar( sqrt( rSum2 / (rCount - 1.) ) )

        case(PERCENT_BIAS)

          this%tTableData(j)%sDescription = "Percent bias: "
          this%tTableData(j)%sValue(1) = asChar( rSum1 / rCount / rSum3 * 100. )

        case(RELATIVE_BIAS)

          this%tTableData(j)%sDescription = "Relative bias: "
          if(rSum3 < rNEAR_ZERO) then
            this%tTableData(j)%sValue(1) = asChar( -HUGE(rExponent) )
          else
            this%tTableData(j)%sValue(1) = asChar( rSum1 / rCount / rSum3 )
          endif

        case(RELATIVE_STD_ERROR)

          this%tTableData(j)%sDescription = "Relative standard error: "
          this%tTableData(j)%sValue(1) = asChar( sqrt( rSum2 / (rCount - 1.) ) &
              / sqrt( rSum5 / (rCount - 1.) ) )

        case(NASH_SUTCLIFFE)

          this%tTableData(j)%sDescription = "Nash-Sutcliffe coefficient: "
          if(rSum1 > rNEAR_ZERO .or. rSum1 < -rNEAR_ZERO) then
            this%tTableData(j)%sValue(1) = asChar( 1.0 - ( rSum2 / rSum5 ) )
          else
            this%tTableData(j)%sValue(1) = asChar( -HUGE(rExponent) )
          endif

        case(COEFFICIENT_OF_EFFICIENCY)

          this%tTableData(j)%sDescription = "Coefficient of efficiency: "
          if(rSum6 > rNEAR_ZERO .or. rSum6 < -rNEAR_ZERO) then
            this%tTableData(j)%sValue(1) = asChar( 1.0 - ( rSum4 / rSum6 ) )
          else
            this%tTableData(j)%sValue(1) = asChar( -HUGE(rExponent) )
          endif

        case(INDEX_OF_AGREEMENT)

          this%tTableData(j)%sDescription = "Index of agreement: "
          if(rSum7 > rNEAR_ZERO .or. rSum7 < -rNEAR_ZERO) then
            this%tTableData(j)%sValue(1) = asChar( 1.0 - ( rSum4 / rSum7 ) )
          else
            this%tTableData(j)%sValue(1) = asChar( -HUGE(rExponent) )
          endif

        case default

          call assert(lFALSE, "Internal programming error - unhandled select case event", &
            trim(__FILE__), __LINE__)

      end select


    enddo


  end subroutine table_calc_c_table

!------------------------------------------------------------------------------

  subroutine table_calc_i_table(this, pTS, pBlock)

    class (T_TABLE) :: this
    type (T_TIME_SERIES), pointer :: pTS
    type (T_BLOCK), pointer, optional :: pBlock

    ! [ LOCALS ]
    integer (kind=T_INT) :: iSizeMA, iSizeML
    integer (kind=T_INT) :: i, j
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: iCount
    character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME, &
       pNEW_I_TABLE_NAME

    real (kind=T_SGL), dimension(:), allocatable :: rTempValue
    type (T_DATETIME), dimension(:), allocatable :: tTempDateTime

    type (T_STATS_COLLECTION), pointer :: pStats
    type(T_HI), dimension(:), pointer :: MA
    type(T_HI), dimension(:), pointer :: ML

    if(present(pBlock)) then  ! obtain user preferences for the I_TABLE from TSPROC input block

      ! is a "DATE_1" construct present? If so, process user supplied dates
      if(pBlock%select("DATE_1") > 0) then
        call processUserSuppliedDateTime(pBlock, this%tStartDate, this%tEndDate)
      else  ! use all available data
        this%tStartDate = pTS%tStartDate
        this%tEndDate = pTS%tEndDate
      endif

      ! restrict data to specified date range, if desired
      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating I_TABLE: no time series data within " &
        //"specified date range", trim(__FILE__), __LINE__)
      ! allocate memory for temporary data array
      allocate(rTempValue(iCount))
      allocate(tTempDateTime(iCount))
      rTempValue = pack(pTS%tData%rValue, pTS%tData%lSelect)
      tTempDateTime = pack(pTS%tData%tDT, pTS%tData%lSelect)

      ! determine what the new I Table name should be....
      pNEW_I_TABLE_NAME => pBlock%getString("NEW_I_TABLE_NAME")
      if(str_compare(pNEW_I_TABLE_NAME(1), "NA")) then
        this%sSeriesName = trim(pTS%sSeriesName)//"_HI" ! default value if none provided
      else
        this%sSeriesName = trim(pNEW_I_TABLE_NAME(1))
      endif

    else   ! ignore pBlock arguments; use default values in calculating I_TABLE

      this%tStartDate = pTS%tStartDate
      this%tEndDate = pTS%tEndDate

      this%sSeriesName = trim(pTS%sSeriesName)//"_HI" ! default value if none provided

      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating I_TABLE: no time series data within " &
        //"specified date range", trim(__FILE__), __LINE__)

      ! allocate memory for temporary data array
      allocate(rTempValue(iCount))
      allocate(tTempDateTime(iCount))
      rTempValue = pack(pTS%tData%rValue, pTS%tData%lSelect)
      tTempDateTime = pack(pTS%tData%tDT, pTS%tData%lSelect)


    endif

    ! get a pointer to a "stats_object" - this object contains all
    ! calculated statistics on the time series by month, by year, by year and month
    ! and for the overall time series. Once this call is made, we simply have to
    ! pull out the statistics of interest.
    pStats => create_stats_object(rTempValue, tTempDateTime%iMonth, &
                   tTempDateTime%iYear, tTempDateTime%iJulianDay)

    ! get a pointer to the MA block of streamflow statistics
    MA => compute_hyd_indices_MA(pStats)
    ML => compute_hyd_indices_ML(pStats)

    this%iTableType = iITABLE

    iSizeMA = size(MA)
    iSizeML = size(ML)

    ! allocate memory for TABLE object
    allocate(this%tTableData( iSizeMA + iSizeML ) )

    ! transfer items from the "MA" block to a table object
    do i=1,iSizeMA

      this%tTableData(i)%sDescription = "MA"//trim(asChar(i))//": " &
         //trim( MA(i)%sHydrologicIndex)//": "
      this%tTableData(i)%sValue(1) = asChar(MA(i)%rValue)
!      this%tTableData(i)%sKeyword = ""

    enddo

    ! transfer items from the "ML" block to a table object
    do i=1,iSizeML

      this%tTableData(iSizeMA + i)%sDescription = "ML"//trim(asChar(i))//": " &
         //trim( ML(i)%sHydrologicIndex)//": "
      this%tTableData(iSizeMA + i)%sValue(1) = asChar(ML(i)%rValue)

    enddo

     write(this%sHeader,fmt= &
        "(3x,'Hydrologic index name',t75,'Hydrologic index value')")

    deallocate(MA, ML , pStats)
    if(associated(pNEW_I_TABLE_NAME)) deallocate(pNEW_I_TABLE_NAME)
    if(associated(pSERIES_NAME)) deallocate(pSERIES_NAME)

  end subroutine table_calc_i_table

!------------------------------------------------------------------------------

  subroutine table_calc_e_table(this, pTS, pBlock)

    class (T_TABLE) :: this
    type (T_TIME_SERIES), pointer :: pTS
    type (T_BLOCK), pointer, optional :: pBlock

    ! [ LOCALS ]
    integer (kind=T_INT) :: iSize
    integer (kind=T_INT) :: i, j
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: iCount
    character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME, &
       pNEW_E_TABLE_NAME, pEXCEEDENCE_TIME_UNITS, pUNDER_OVER

    logical (kind=T_LOGICAL) :: lACCUMULATE, lACCUMULATING

    real (kind=T_SGL), dimension(:), allocatable :: rTempValue
    type (T_DATETIME), dimension(:), allocatable :: tTempDateTime

    real (kind=T_DBL) :: rDeltaDateTime
    real (kind=T_DBL) :: rTotalTime
    real (kind=T_DBL) :: rAccumulation
    real (kind=T_DBL), dimension(:), allocatable :: rDuration
    real (kind=T_DBL), dimension(:), allocatable :: rFractionOfTime

    character (len=MAXARGLENGTH), dimension(:), pointer :: pOVER_UNDER
    logical (kind=T_LOGICAL) :: lOVER
    real (kind=T_SGL), dimension(:), pointer :: pFLOW, pDELAY
    real (kind=T_SGL), dimension(:), allocatable :: rFLOW, rDELAY
    real (kind=T_SGL), dimension(7), parameter :: DEFAULT_QUANTILES = &
                                         [0.1, 0.2, 0.3, 0.5, 0.75, 0.95, 0.99]

    lOVER = lTRUE

    if(present(pBlock)) then  ! obtain user preferences for the E_TABLE

      ! is a "DATE_1" construct present? If so, process user supplied dates
      if(pBlock%select("DATE_1") > 0) then
        call processUserSuppliedDateTime(pBlock, this%tStartDate, this%tEndDate)
      else  ! use all available data
        this%tStartDate = pTS%tStartDate
        this%tEndDate = pTS%tEndDate
      endif

      ! restrict data to specified date range, if desired
      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating E_TABLE: no time series data within " &
        //"specified date range", trim(__FILE__), __LINE__)
      ! allocate memory for temporary data array
      allocate(rTempValue(iCount))
      allocate(tTempDateTime(iCount))
      rTempValue = pack(pTS%tData%rValue, pTS%tData%lSelect)
      tTempDateTime = pack(pTS%tData%tDT, pTS%tData%lSelect)

      ! determine what the new E Table name should be....
      pNEW_E_TABLE_NAME => pBlock%getString("NEW_E_TABLE_NAME")
      if(str_compare(pNEW_E_TABLE_NAME(1), "NA")) then
        this%sSeriesName = trim(pTS%sSeriesName)//"_EX" ! default value if none provided
      else
        this%sSeriesName = trim(pNEW_E_TABLE_NAME(1))
      endif

      ! get FLOW values; if no FLOW values given, use default values
      pFLOW =>  pBlock%getReal("FLOW")
      if(pFLOW(1) > rNEAR_TINY) then
        allocate(rFLOW( size(pFLOW)) )
        rFLOW = pFLOW
      else
        allocate(rFLOW(size(DEFAULT_QUANTILES)) )
        rFLOW = quantiles(DEFAULT_QUANTILES, rTempValue)
      endif

      ! get DELAY values; if no DELAY values given, use default values
      pDELAY =>  pBlock%getReal("DELAY")
      if(pDELAY(1) > rNEAR_TINY) then
        allocate(rDELAY( size(pDELAY)) )
        rDELAY = pDELAY
      else
        allocate(rDELAY(size(DEFAULT_QUANTILES)) )
        rDELAY = 0.0
      endif

      pOVER_UNDER => pBlock%getString("OVER_UNDER")
      if(str_compare(pOVER_UNDER(1),"under") ) lOVER = lFALSE

      deallocate(pFLOW, pDELAY, pNEW_E_TABLE_NAME, pOVER_UNDER)

    else   ! ignore pBlock arguments; use default values in calculating E_TABLE

      this%tStartDate = pTS%tStartDate
      this%tEndDate = pTS%tEndDate

      this%sSeriesName = trim(pTS%sSeriesName)//"_EX" ! default value if none provided

      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating E_TABLE: no time series data within " &
        //"specified date range", trim(__FILE__), __LINE__)

      ! allocate memory for temporary data array
      allocate(rTempValue(iCount))
      allocate(tTempDateTime(iCount))
      rTempValue = pack(pTS%tData%rValue, pTS%tData%lSelect)
      tTempDateTime = pack(pTS%tData%tDT, pTS%tData%lSelect)

      allocate(rFLOW(size(DEFAULT_QUANTILES)) )
      rFLOW = quantiles(DEFAULT_QUANTILES, rTempValue)

      allocate(rDELAY(size(rFLOW)) )
      rDELAY = 0.0

    endif

    this%iTableType = iETABLE

    lACCUMULATING = lFALSE
    iSize = size(rFLOW)

    rTotalTime = this%tEndDate - this%tStartDate

    ! reserve memory for duration values corresponding to the flow thresholds
    allocate( rDuration(iSize) )
    allocate( rFractionOfTime(iSize) )

    ! allocate memory for TABLE object
    allocate(this%tTableData(iSize))

    ! logic for next block translated from John Doherty's "time_duration" subroutine

    rDuration = rD_ZERO
    ! interate over all specified flow thresholds
    do i=1,size(rFLOW)

      rAccumulation = rD_ZERO

      if(lOVER) then
        ! if initial flow value exceeds the current flow threshold, set lACCUMULATE flag to true
        if(rTempValue(1) > rFLOW(i) ) then
          lACCUMULATE = lTRUE
        else
          lACCUMULATE = lFALSE
        endif
      else
        ! if initial flow value is less than the current flow threshold, set lACCUMULATE flag to true
        if(rTempValue(1) < rFLOW(i) ) then
          lACCUMULATE = lTRUE
        else
          lACCUMULATE = lFALSE
        endif
      endif

      ! begin iterating over all values
      do j=2, size(rTempValue)
        lACCUMULATING = lACCUMULATE

        if(lOVER) then
          ! if initial flow value exceeds the current flow threshold, set lACCUMULATE flag to true
          if(rTempValue(j) > rFLOW(i) ) then
            lACCUMULATE = lTRUE
          else
            lACCUMULATE = lFALSE
          endif
        else
          ! if initial flow value is less than the current flow threshold, set lACCUMULATE flag to true
          if(rTempValue(j) < rFLOW(i) ) then
            lACCUMULATE = lTRUE
          else
            lACCUMULATE = lFALSE
          endif
        endif

        ! calculate the difference between current date and previous date (in fractional days)
        rDeltaDateTime = tTempDateTime(j) - tTempDateTime(j-1)

        if( lACCUMULATE .and. lACCUMULATING ) then                  ! keep on accumulating!

          rAccumulation = rAccumulation + rDeltaDateTime

        elseif( lACCUMULATE .and. ( .not. lACCUMULATING ) ) then    ! begin accumulating

          rAccumulation = rDeltaDateTime * ( rTempValue(j) - rFlow(i) ) &
                                 / ( rTempValue(j) - rTempValue(j-1) )

          call Assert( rAccumulation >= 0, "LOGIC ERROR", trim(__FILE__), __LINE__)

        elseif( lACCUMULATING .and. ( .not. lACCUMULATE ) ) then    ! stop accumulating

          rAccumulation = rAccumulation + rDeltaDateTime * ( rTempValue(j-1) - rFlow(i) ) &
                                 / ( rTempValue(j-1) - rTempValue(j) )

          call Assert( rAccumulation >= 0, "LOGIC ERROR", trim(__FILE__), __LINE__)

          rDuration(i) = rDuration(i) + max(0.0, rAccumulation - rDELAY(i) )

          rAccumulation = 0.

        endif

      enddo

      rDuration(i) = rDuration(i) + max(0.0, rAccumulation - rDELAY(i) )
      rFractionOfTime(i) = rDuration(i) / rTotalTime

!      write(this%tTableData(i)%sValue, fmt="(f14.3, t18, f14.5)") &
!          rDuration(i), rFractionOfTime(i)
!      write(this%tTableData(i)%sDescription, fmt="(f14.3, t18, f14.3)") &
!          rFLOW(i), rDELAY(i)



!      write(this%tTableData(i)%sValue, fmt="(a14,t27,a14)") &
!        asChar(rDuration(i), 14, 8), asChar(rFractionOfTime(i), 14, 8)

      this%tTableData(i)%sDescription = asChar(rFLOW(i), 14, 8)
      this%tTableData(i)%sValue(1) = asChar(rDELAY(i), 14, 8)
      this%tTableData(i)%sValue(2) = asChar(rDuration(i), 14, 8)
      this%tTableData(i)%sValue(3) = asChar(rFractionOfTime(i), 14, 8)

!      write(this%tTableData(i)%sDescription, fmt="(a14,t16,a14)") &
!        asChar(rFLOW(i), 14, 8), asChar(rDELAY(i), 14, 8)

!      this%tTableData(i)%sKeyword = ""

    enddo

    if(lOVER) then
       write(this%sHeader,fmt= &
          "(7x,'Flow',7x,'Time delay (days)',4x,'Time above (days)',5x," &
            //"'Time above (fractional)')")
    else
       write(this%sHeader,fmt= &
          "(7x,'Flow',7x,'Time delay (days)',4x,'Time below (days)',5x," &
            //"'Time below (fractional)')")
    endif

    deallocate( rDuration, rFractionOfTime)
    if(associated(pFLOW)) deallocate( pFLOW )
    if(associated(pDELAY)) deallocate( pDELAY )

  end subroutine table_calc_e_table

!------------------------------------------------------------------------------

  subroutine table_calc_s_table(this, pTS, pBlock)

    class (T_TABLE) :: this
    type (T_TIME_SERIES), pointer :: pTS
    type (T_BLOCK), pointer, optional :: pBlock

    ! [ LOCALS ]
    integer (kind=T_INT) :: iSize
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: iCount
    integer (kind=T_SHORT), parameter :: iSUM = 1
    integer (kind=T_SHORT), parameter :: iMEAN = 2
    integer (kind=T_SHORT), parameter :: iMEDIAN = 3
    integer (kind=T_SHORT), parameter :: iMINMEAN = 4
    integer (kind=T_SHORT), parameter :: iMAXMEAN = 5
    integer (kind=T_SHORT), parameter :: iSTD_DEV = 6
    integer (kind=T_SHORT), parameter :: iMINIMUM = 7
    integer (kind=T_SHORT), parameter :: iMAXIMUM = 8
    integer (kind=T_SHORT), parameter :: iRANGE = 9

    ! minimum number of entries ANY table must contain
    integer ( kind=T_SHORT), parameter :: iFIX = 8

    integer (kind=T_INT) :: iPERIOD
    integer (kind=T_INT) :: iIndex, iResult
    integer (kind=T_INT) :: iBegin, iEnd

    real (kind=T_SGL), dimension(:), allocatable :: rTempValue

    real (kind=T_SGL), dimension(9) :: rValue
    character (len=MAXARGLENGTH), dimension(:), pointer :: pMINIMUM, pMAXIMUM, &
       pRANGE, pSUM, pMINMEAN, pMAXMEAN, pSTD_DEV, pPOWER, pLOG, &
       pMEDIAN, pMEAN, pNEW_S_TABLE_NAME

    real (kind=T_SGL) :: rPOWER
    character (len=3) :: sLOG

    ! initialize to the smallest representable number for current machine
    rValue =  -huge(rValue)

    ! define default values
    rPOWER = 1_T_SGL
    sLOG = "no"
    iPERIOD = 5

    if(present(pBlock)) then  ! obtain user preferences for the S_TABLE

      ! determine what the new S Table name should be....
      pNEW_S_TABLE_NAME => pBlock%getString("NEW_S_TABLE_NAME")

      if(str_compare(pNEW_S_TABLE_NAME(1), "NA")) then
        this%sSeriesName = trim(pTS%sSeriesName)//"_STBL" ! default value if none provided
      else
        this%sSeriesName = trim(pNEW_S_TABLE_NAME(1))
      endif

      ! is a "DATE_1" construct present? If so, process user suplied dates
      if(pBlock%select("DATE_1") > 0) then
        call processUserSuppliedDateTime(pBlock, this%tStartDate, this%tEndDate)
      else  ! use all available data
        this%tStartDate = pTS%tStartDate
        this%tEndDate = pTS%tEndDate
      endif

      ! select only the data within the given date range
      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating S_TABLE: no time series data within " &
        //"specified date range for block starting at line " &
        //trim(asChar(pBlock%iStartingLineNumber)), trim(__FILE__), __LINE__)

      ! allocate memory for temporary data array; reset counter
      allocate(rTempValue(iCount)); iCount = 0

      ! is this data supposed to be log-transformed?
      pPOWER => pBlock%getString("POWER")
      if(.not. str_compare(pPOWER(1), "NA")) then
        read(pPOWER(1), fmt=*, iostat=iStat) rPOWER
        call Assert(iStat==0, "Problem reading power value in SERIES_STATISTICS block " &
           //"starting at line "//trim(asChar(pBlock%iStartingLineNumber)) )
        if(rPOWER < 1.0) &
          call Assert(all(pack(pTS%tData%rValue, pTS%tData%lSelect)>=0), &
           "Cannot transform a series that contains negative values with a power less than one.", &
           trim(__FILE__), __LINE__)
      endif

      ! is this data supposed to be log-transformed?
      pLOG => pBlock%getString("LOG")
      sLOG = trim(pLOG(1))
      if(str_compare(sLOG,"yes")) then
        rTempValue = log(pack(pTS%tData%rValue, pTS%tData%lSelect))
      else ! if not, just make a copy, raised to rPOWER
        rTempValue = pack(pTS%tData%rValue, pTS%tData%lSelect) ** rPOWER
      endif

      pMINIMUM => pBlock%getString("MINIMUM")
      if( str_compare(pMINIMUM(1),"yes") ) then
        rValue(iMINIMUM) = MINVAL(rTempValue)
      endif

      pSUM => pBlock%getString("SUM")
      if( str_compare(pSUM(1),"yes") ) then
        rValue(iSUM) = SUM(rTempValue)
      endif

      pMAXIMUM => pBlock%getString("MAXIMUM")
      if( str_compare(pMAXIMUM(1),"yes")) then
        rValue(iMAXIMUM) = MAXVAL(rTempValue)
      endif

      pMEDIAN => pBlock%getString("MEDIAN")
      if( str_compare(pMEDIAN(1),"yes")) then
        rValue(iMEDIAN) = median(rTempValue)
      endif

      pMEAN => pBlock%getString("MEAN")
      if( str_compare(pMEAN(1),"yes")) then
        rValue(iMEAN) = mean(rTempValue)
      endif

      pSTD_DEV => pBlock%getString("STD_DEV")
      if( str_compare(pSTD_DEV(1),"yes")) then
        rValue(iSTD_DEV) = stddev(rTempValue)
      endif

      pMINMEAN => pBlock%getString("MINMEAN")
      if( str_compare(pMINMEAN(1),"yes")) then
        iIndex = pBlock%index("MINMEAN")
        iBegin = scan(string=pBlock%sKeyword(iIndex), set="_", back=lTRUE) + 1
        iEnd = len_trim(pBlock%sKeyword(iIndex))
        read(pBlock%sKeyword(iIndex)(iBegin:iEnd), fmt=*) iPERIOD
        rValue(iMINMEAN) = calc_period_min_mean(rTempValue, iPERIOD)
      endif

      pMAXMEAN => pBlock%getString("MAXMEAN")
      if( str_compare(pMAXMEAN(1),"yes")) then
        iIndex = pBlock%index("MAXMEAN")
        iBegin = scan(string=pBlock%sKeyword(iIndex), set="_", back=lTRUE) + 1
        iEnd = len_trim(pBlock%sKeyword(iIndex))
        read(pBlock%sKeyword(iIndex)(iBegin:iEnd), fmt=*) iPERIOD
        rValue(iMAXMEAN) = calc_period_max_mean(rTempValue, iPERIOD)
      endif

      pRANGE => pBlock%getString("RANGE")
      if( str_compare(pRANGE(1),"yes")) then
        rValue(iRANGE) = rValue(iMAXIMUM) - rValue(iMINIMUM)
      endif

      ! sum the number of statistics the user has selected
      iCount = count(rValue > rNEAR_TINY)
      iSize = iCount + iFIX

      call Assert(iCount > 0, "No statistics selected in " &
        //trim(pBlock%sBlockName)//" block starting at line "// &
        trim(asChar(pBlock%iStartingLineNumber)), trim(__FILE__), __LINE__)

      deallocate(pMINIMUM, pMAXIMUM, pRANGE, pSUM, pMINMEAN, pMAXMEAN, &
        pSTD_DEV, pPOWER, pLOG, pMEDIAN, pMEAN, pNEW_S_TABLE_NAME )

    else ! just calculate all statistics and populate the S_TABLE

      this%tStartDate = pTS%tStartDate
      this%tEndDate = pTS%tEndDate

      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating S_TABLE: no time series data within " &
        //"specified date range", trim(__FILE__), __LINE__)

      ! make a temporary copy of just the selected values
      allocate(rTempValue(iCount), stat=iStat)
      call Assert(iStat==0, "Problem allocating memory for temporary array", &
         trim(__FILE__), __LINE__)
      rTempValue = pack(pTS%tData%rValue, pTS%tData%lSelect)

      ! calculate the statistics
      rValue(iSUM) = SUM(rTempValue)
      rValue(iMINIMUM) = MINVAL(rTempValue)
      rValue(iMAXIMUM) = MAXVAL(pTS%tData%rValue, pTS%tData%lSelect)
      rValue(iRANGE) = rValue(iMAXIMUM) - rValue(iMINIMUM)
      rValue(iMINMEAN) = calc_period_min_mean(rTempValue, iPERIOD)
      rValue(iMAXMEAN) = calc_period_max_mean(rTempValue, iPERIOD)
      rValue(iMEDIAN) = median(rTempValue)
      rValue(iMEAN) = mean(rTempValue)
      rValue(iSTD_DEV) = stddev(rTempValue)

      iCount = count(rValue > rNEAR_TINY)
      iSize = iCount + iFIX

      this%sSeriesName = trim(pTS%sSeriesName)//"_STBL" ! default value if none provided

    endif

    ! if no value given for 'LOG', mark it as a 'no' value
    if(str_compare(sLOG,"NA")) sLOG = "no"

    this%iTableType = iSTABLE

    allocate(this%tTableData(iSize))

    this%tTableData(1:iFIX) = (/ &
      T_TABLE_DATA("SERIES_NAME", &
         "Series for which statistics are calculated:",quote(pTS%sSeriesName)), &
      T_TABLE_DATA("DATE_1", &
         "Beginning date of data accumulation:", this%tStartDate%listdate()), &
      T_TABLE_DATA("TIME_1", &
         "Beginning time of data accumulation:", this%tStartDate%listtime()), &
      T_TABLE_DATA("DATE_2", &
          "Ending date of data accumulation:", this%tEndDate%listdate()), &
      T_TABLE_DATA("TIME_2", &
         "Beginning time of data accumulation:", this%tEndDate%listtime()), &
      T_TABLE_DATA("", &
          "Number of series terms in this interval:",asChar(count(pTS%tData%lSelect))) , &
      T_TABLE_DATA("LOG", &
          "Logarithmic transformation of series?", trim(sLOG)), &
      T_TABLE_DATA("POWER", &
          "Exponent in power transformation:", asChar(rPOWER)) &
    /)

    iCount = iFIX

    if(rValue(iSUM) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iSUM)
      this%tTableData(iCount)%sDescription = "Sum of values:"
      this%tTableData(iCount)%sKeyword = "SUM"
    endif

    if(rValue(iMINIMUM) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iMINIMUM)
      this%tTableData(iCount)%sDescription = "Minimum value:"
      this%tTableData(iCount)%sKeyword = "MINIMUM"
    endif

    if(rValue(iMAXIMUM) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iMAXIMUM)
      this%tTableData(iCount)%sDescription = "Maximum value:"
      this%tTableData(iCount)%sKeyword = "MAXIMUM"
    endif

    if(rValue(iRANGE) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iRANGE)
      this%tTableData(iCount)%sDescription = "Range of values:"
      this%tTableData(iCount)%sKeyword = "RANGE"
    endif

    if(rValue(iMEAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iMEAN)
      this%tTableData(iCount)%sDescription = "Mean of values:"
      this%tTableData(iCount)%sKeyword = "MEAN"
    endif

    if(rValue(iSTD_DEV) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iSTD_DEV)
      this%tTableData(iCount)%sDescription = "Standard deviation of values:"
      this%tTableData(iCount)%sKeyword = "STD_DEV"
    endif

    if(rValue(iMEDIAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iMEDIAN)
      this%tTableData(iCount)%sDescription = "Median of values:"
      this%tTableData(iCount)%sKeyword = "MEDIAN"
    endif

    if(rValue(iMINMEAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iMINMEAN)
      this%tTableData(iCount)%sDescription = &
         "Minimum "//trim(asChar(iPERIOD))//"-day mean value:"
      this%tTableData(iCount)%sKeyword = "MINMEAN"
    endif

    if(rValue(iMAXMEAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue(1), fmt="(g14.8)") rValue(iMAXMEAN)
      this%tTableData(iCount)%sDescription = &
         "Maximum "//trim(asChar(iPERIOD))//"-day mean value:"
      this%tTableData(iCount)%sKeyword = "MAXMEAN"
    endif

  end subroutine table_calc_s_table

!------------------------------------------------------------------------------

  subroutine table_calc_v_table(this, pTS, pBlock)

    class (T_TABLE) :: this
    type (T_TIME_SERIES), pointer :: pTS
    type (T_BLOCK), pointer, optional :: pBlock

    ! [ LOCALS ]
    integer (kind=T_INT) :: i, j, iCount, iStat
    character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME, &
       pNEW_V_TABLE_NAME, pDATE_FILE, pFLOW_TIME_UNITS, pFACTOR, &
       pMONTHLY_VOLUMES
    real (kind=T_DBL) :: rVolume
    real (kind=T_SGL) :: rConversionFactor, rFlowTimeFactor, rFactor
    character (len=256) :: sBuf

    type (T_DATERANGE), dimension(:), pointer :: pDR
    type (T_TABLE_DATA), dimension(:), allocatable :: tTableData
    type (T_TABLE) :: tTempTable

    type (T_DATETIME) :: tMINDATE, tMAXDATE

    character (len=256) :: sNewVTableName

    call tMINDATE%parseDate("01/01/3000")
    call tMINDATE%parseTime("00:00:00")
    call tMINDATE%calcJulianDay()
    call tMAXDATE%parseDate("01/01/0100")
    call tMAXDATE%parseTime("00:00:00")
    call tMAXDATE%calcJulianDay()

    rFlowTimeFactor = 86400.  ! if flows are in cms or cfs, this is correct
    rConversionFactor = 1.    ! default is no conversion

    ! set default date range in case a date file is not specified
    allocate(pDR(1))
    pDR%tStartDate = pTS%tStartDate
    pDR%tEndDate = pTS%tEndDate

    if(present(pBlock)) then  ! obtain user preferences for the V_TABLE

      pSERIES_NAME => pBlock%getString("SERIES_NAME")

      pNEW_V_TABLE_NAME => pBlock%getString("NEW_V_TABLE_NAME")
      if(.not. str_compare(pNEW_V_TABLE_NAME(1), "NA")) then
        sNewVTableName = trim(pNEW_V_TABLE_NAME(1))
      else
        sNewVTableName = trim(pTS%sSeriesName)//"_VTBL"
      endif

      pDATE_FILE => pBlock%getString("DATE_FILE")
      if(.not. str_compare(pDATE_FILE(1), "NA")) then
        deallocate(pDR)
        pDR => read_dates_file(pDATE_FILE(1))
      endif

      pMONTHLY_VOLUMES => pBlock%getString("MONTHLY_VOLUMES")
      if(.not. str_compare(pMONTHLY_VOLUMES(1), "NA")) then
        deallocate(pDR)
        pDR => make_monthly_dates_list_fn(pTS%tStartDate, pTS%tEndDate)
      endif

      pFLOW_TIME_UNITS => pBlock%getString("FLOW_TIME_UNITS")
      if(.not. str_compare(pFLOW_TIME_UNITS(1), "NA")) then
        if(str_compare(pFLOW_TIME_UNITS(1),"year")) then
          rFlowTimeFactor = 1. / 365.25
        elseif(str_compare(pFLOW_TIME_UNITS(1),"month")) then
          rFlowTimeFactor = 1. / 12.
        elseif(str_compare(pFLOW_TIME_UNITS(1),"day")) then
          rFlowTimeFactor = 1.
        elseif(str_compare(pFLOW_TIME_UNITS(1),"hour")) then
          rFlowTimeFactor = 24.
        elseif(str_compare(pFLOW_TIME_UNITS(1),"min")) then
          rFlowTimeFactor = 1440.
        elseif(str_compare(pFLOW_TIME_UNITS(1),"sec")) then
          rFlowTimeFactor = 86400.
        else
          call Assert(lFALSE,"Unknown FLOW_TIME_UNIT specified in block " &
            //"starting at line "//trim(asChar(pBlock%iStartingLineNumber)), &
            trim(__FILE__), __LINE__)
        endif
      endif

      pFACTOR => pBlock%getString("FACTOR")
      if(.not. str_compare(pFACTOR(1), "NA")) then
        read(pFACTOR(1),fmt=*, iostat=iStat) rConversionFactor
        call Assert(iStat == 0, "Error reading in conversion factor value in " &
          //"block starting at line "//trim(asChar(pBlock%iStartingLineNumber)), &
            trim(__FILE__), __LINE__)
      endif

      deallocate(pFACTOR, pFLOW_TIME_UNITS, pDATE_FILE, pNEW_V_TABLE_NAME, &
         pSERIES_NAME)

    else  ! no pBlock argument present; assume default values and make the table

      sNewVTableName = trim(pTS%sSeriesName)//"_VTBL"

    endif

    rFactor = rConversionFactor * rFlowTimeFactor

    allocate(tTableData(size(pDR)), stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for temporary V_TABLE data", &
      trim(__FILE__), __LINE__)

    ! iterate over the date ranges provided by user
    do i=1,size(pDR)

      if(tMINDATE >= pDR(i)%tStartDate) tMINDATE = pDR(i)%tStartDate
      if(tMAXDATE <= pDR(i)%tEndDate) tMAXDATE = pDR(i)%tEndDate

      rVolume = pTS%integrate( GE = pDR(i)%tStartDate, LE = pDR(i)%tEndDate, &
           rConversionFactor =  rFactor)
      sBuf = "From "//trim(pDR(i)%tStartDate%listdatetime() )//" to " &
              //trim(pDR(i)%tEndDate%listdatetime() )//"  volume = "
      tTableData(i)%sDescription = trim(sBuf)
      tTableData(i)%sValue(1) = asChar(rVolume)
      call echolog( trim(sBuf)//asChar(rVolume,16,8)  )
    enddo

    call this%new(tTableData)
    this%iTableType = iVTABLE
    this%tStartDate = tMINDATE
    this%tEndDate = tMAXDATE
    this%sHeader = "Volumes calculated from series "//quote(pTS%sSeriesname) &
        //" are as follows:-"
    this%sSeriesName = trim(sNewVTableName)

    deallocate(pDR)

end subroutine table_calc_v_table

!------------------------------------------------------------------------------

  subroutine table_list_output_sub(this, sDateFormat, iLU)

    class(T_TABLE) :: this
    character(len=*), intent(in), optional :: sDateFormat
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: LU
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

    write(LU,fmt="(/,a,' ---->')") " "//TABLE_TYPE(this%iTableType)//" "//quote(this%sSeriesName)

    if( this%iTableType == iSTABLE ) then
      do i=1,size(this%tTableData)
         write(LU,fmt="(t4,a48,t55,a)") trim(adjustl(this%tTableData(i)%sDescription)), &
            trim(this%tTableData(i)%sValue(1))
      enddo

    elseif( this%iTableType == iCTABLE ) then
      do i=1,size(this%tTableData)
         write(LU,fmt="(t4,a48,t55,a)") trim(adjustl(this%tTableData(i)%sDescription)), &
            trim(this%tTableData(i)%sValue(1))
      enddo

    elseif( this%iTableType == iETABLE ) then
      write(LU,fmt= "(a)") trim(this%sHeader)
      do i=1,size(this%tTableData)
         write(LU,fmt="(1x,a,t18,a,t38,a,t58,a)") trim(this%tTableData(i)%sDescription), &
            trim(this%tTableData(i)%sValue(1)),trim(this%tTableData(i)%sValue(2)), &
            trim(this%tTableData(i)%sValue(3))
      enddo

    elseif( this%iTableType == iVTABLE ) then
      write(LU,fmt= "(a)") trim(this%sHeader)
      do i=1,size(this%tTableData)
         write(LU,fmt="(t4,a58,t63,a)") trim(adjustl(this%tTableData(i)%sDescription)), &
            trim(this%tTableData(i)%sValue(1))
      enddo

    elseif( this%iTableType == iITABLE ) then
      write(LU,fmt= "(a)") trim(this%sHeader)
      do i=1,size(this%tTableData)
         write(LU,fmt="(1x,a70,t74,a)") adjustl(trim(this%tTableData(i)%sDescription)), &
            trim(this%tTableData(i)%sValue(1))
      enddo

    endif

  end subroutine table_list_output_sub

!------------------------------------------------------------------------------

  subroutine table_list_output_instructions_sub(this, iLU)

    class(T_TABLE) :: this
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    integer (kind=T_INT) :: i, j
    integer (kind=T_INT) :: LU
    integer (kind=T_INT) :: iInitialCharacter

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    ! write the instruction to move the file marker up to the beginning
    ! of the next time series

    if( this%iTableType == iSTABLE ) then
      iInitialCharacter = 53
      write(LU,fmt="('$S_TABLE$')" )
      write(LU,fmt="(a)") "l8"
      j = 0
      do i=9,size(this%tTableData)
        j = j + 1
        write(LU,fmt="(a)") "l1 ["//trim(this%sSeriesName)//"_"//trim(asChar(j) )//"]" &
          //trim(asChar(iInitialCharacter) )//":"//trim(asChar(iInitialCharacter + 20) )
      enddo

    elseif( this%iTableType == iCTABLE ) then
      iInitialCharacter = 53
      write(LU,fmt="('$S_TABLE$')" )
      write(LU,fmt="(a)") "l8"
      j = 0
      do i=7,size(this%tTableData)
        j = j + 1
        write(LU,fmt="(a)") "l1 ["//trim(this%sSeriesName)//"_"//trim(asChar(j) )//"]" &
          //trim(asChar(iInitialCharacter) )//":"//trim(asChar(iInitialCharacter + 20) )
      enddo

    elseif( this%iTableType == iETABLE ) then
      iInitialCharacter = 60
      write(LU,fmt="('$E_TABLE$')" )
      write(LU,fmt="(a)") "l1"
      do i=1,size(this%tTableData)
        write(LU,fmt="(a)") "l1 ["//trim(this%sSeriesName)//"_"//trim(asChar(i) )//"]" &
          //trim(asChar(iInitialCharacter) )//":"//trim(asChar(iInitialCharacter + 20) )
      enddo

    elseif( this%iTableType == iVTABLE ) then
      iInitialCharacter = 63
      write(LU,fmt="('$V_TABLE$')" )
      write(LU,fmt="(a)") "l1"
      do i=1,size(this%tTableData)
        write(LU,fmt="(a)") "l1 ["//trim(this%sSeriesName)//"_"//trim(asChar(i) )//"]" &
          //trim(asChar(iInitialCharacter) )//":"//trim(asChar(iInitialCharacter + 20) )
      enddo

    elseif( this%iTableType == iITABLE ) then
      iInitialCharacter = 74
      write(LU,fmt="('$I_TABLE$')" )
      do i=1,size(this%tTableData)
        write(LU,fmt="(a)") "l1 ["//trim(this%sSeriesName)//"_"//trim(asChar(i) )//"]" &
          //trim(asChar(iInitialCharacter) )//":"//trim(asChar(iInitialCharacter + 20) )
      enddo

    endif

  end subroutine table_list_output_instructions_sub

end module tsp_table_manager
