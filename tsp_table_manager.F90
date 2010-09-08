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

  integer (kind=T_INT), parameter :: iOBSERVED = 1
  integer (kind=T_INT), parameter :: iSIMULATED = 2
  integer (kind=T_INT), parameter :: iOTHER = 3

  character (len=7), dimension(4), parameter :: TABLE_TYPE = &
       (/ "S_TABLE", "C_TABLE", "E_TABLE", "V_TABLE" /)

  type, public :: T_TABLE_DATA
    character (len=MAXARGLENGTH) :: sKeyword
    character (len=256) :: sDescription
    character (len=256) :: sValue
    logical (kind=T_LOGICAL) :: lSelect = lFALSE
  end type T_TABLE_DATA

!  type, extends(T_TABLE_DATA) :: T_VTABLE_DATA
!    type (T_DATETIME) :: tStartDate
!    type (T_DATETIME) :: tEndDate
!  end type T_VTABLE_DATA

  type, public :: T_TABLE
    character (len=MAXNAMELENGTH) :: sSeriesName
    character (len=256) :: sDescription
    character (len=256) :: sHeader
    integer (kind=T_INT) :: iTableType
    integer (kind=T_INT) :: iListOutputPosition = 0
    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate
    type (T_TABLE_DATA), dimension(:), allocatable :: tTableData

  contains

    procedure, public :: calc_s_table => table_calc_s_table
    procedure, public :: calc_e_table => table_calc_e_table
    procedure, public :: calc_v_table => table_calc_v_table
    procedure, public :: list => table_list_output_sub
    procedure, public :: new => new_table_sub

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

    real (kind=T_SGL), dimension(:), pointer :: pFLOW, pDELAY
    real (kind=T_SGL), dimension(:), allocatable :: rFLOW, rDELAY
    real (kind=T_SGL), dimension(7), parameter :: DEFAULT_QUANTILES = &
                                         [0.1, 0.2, 0.3, 0.5, 0.75, 0.95, 0.99]

    if(present(pBlock)) then  ! obtain user preferences for the E_TABLE

      ! is a "DATE_1" construct present? If so, process user suplied dates
      if(pBlock%select("DATE_1") > 0) then
        call processUserSuppliedDateTime(pBlock, this%tStartDate, this%tEndDate)
      else  ! use all available data
        this%tStartDate = pTS%tStartDate
        this%tEndDate = pTS%tEndDate
      endif

      ! restrict data to specified date range, if desired
      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating S_TABLE: no time series data within " &
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

      print *, trim(this%sSeriesName)

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

      deallocate(pFLOW, pDELAY, pNEW_E_TABLE_NAME)

    else   ! ignore pBlock arguments; use default values in calculating E_TABLE

      this%tStartDate = pTS%tStartDate
      this%tEndDate = pTS%tEndDate

      this%sSeriesName = trim(pTS%sSeriesName)//"_EX" ! default value if none provided

      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating S_TABLE: no time series data within " &
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

      ! if initial flow value exceeds the current flow threshold, set lACCUMULATE flag to true
      if(rTempValue(1) >= rFLOW(i) ) then
        lACCUMULATE = lTRUE
      else
        lACCUMULATE = lFALSE
      endif

      ! begin iterating over all values
      do j=2, size(rTempValue)
        lACCUMULATING = lACCUMULATE

        if(rTempValue(j) >= rFLOW(i) ) then
          lACCUMULATE = lTRUE
        else
          lACCUMULATE = lFALSE
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

      this%tTableData(i)%sValue = asChar(rDuration(i), 18, 8)//"   " &
         //asChar(rFractionOfTime(i), 18, 8)

      this%tTableData(i)%sDescription = asChar(rFLOW(i), 18, 8)//"   " &
         //asChar(rDELAY(i), 18, 8)

      this%tTableData(i)%sKeyword = ""

    enddo

     write(this%sHeader,fmt= &
        "(3x,'Flow',11x,'Time delay (days)',4x,'Time above (days)',3x,'Fraction of time above threshold')")


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
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iSUM)
      this%tTableData(iCount)%sDescription = "Sum of values:"
      this%tTableData(iCount)%sKeyword = "SUM"
    endif

    if(rValue(iMINIMUM) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iMINIMUM)
      this%tTableData(iCount)%sDescription = "Minimum value:"
      this%tTableData(iCount)%sKeyword = "MINIMUM"
    endif

    if(rValue(iMAXIMUM) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iMAXIMUM)
      this%tTableData(iCount)%sDescription = "Maximum value:"
      this%tTableData(iCount)%sKeyword = "MAXIMUM"
    endif

    if(rValue(iRANGE) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iRANGE)
      this%tTableData(iCount)%sDescription = "Range of values:"
      this%tTableData(iCount)%sKeyword = "RANGE"
    endif

    if(rValue(iMEAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iMEAN)
      this%tTableData(iCount)%sDescription = "Mean of values:"
      this%tTableData(iCount)%sKeyword = "MEAN"
    endif

    if(rValue(iSTD_DEV) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iSTD_DEV)
      this%tTableData(iCount)%sDescription = "Standard deviation of values:"
      this%tTableData(iCount)%sKeyword = "STD_DEV"
    endif

    if(rValue(iMEDIAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iMEDIAN)
      this%tTableData(iCount)%sDescription = "Median of values:"
      this%tTableData(iCount)%sKeyword = "MEDIAN"
    endif

    if(rValue(iMINMEAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iMINMEAN)
      this%tTableData(iCount)%sDescription = &
         "Minimum "//trim(asChar(iPERIOD))//"-day mean value:"
      this%tTableData(iCount)%sKeyword = "MINMEAN"
    endif

    if(rValue(iMAXMEAN) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.8)") rValue(iMAXMEAN)
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
       pNEW_V_TABLE_NAME, pDATE_FILE, pFLOW_TIME_UNITS, pFACTOR
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
    call tMAXDATE%parseDate("01/01/0001")
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
        sNewVTableName = pTS%sSeriesName//"_VTBL"
      endif

      pDATE_FILE => pBlock%getString("DATE_FILE")
      if(.not. str_compare(pDATE_FILE(1), "NA")) then
        deallocate(pDR)
        pDR => read_dates_file(pDATE_FILE(1))
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

      sNewVTableName = pTS%sSeriesName//"_VTBL"

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
      tTableData(i)%sValue = asChar(rVolume)
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
            trim(this%tTableData(i)%sValue)
      enddo
    elseif( this%iTableType == iETABLE ) then

      write(LU,fmt= "(a)") trim(this%sHeader)
      do i=1,size(this%tTableData)
         write(LU,fmt="(t4,a,3x,a)") trim(adjustl(this%tTableData(i)%sDescription)), &
            trim(this%tTableData(i)%sValue)
      enddo

    elseif( this%iTableType == iVTABLE ) then

      write(LU,fmt= "(a)") trim(this%sHeader)
      do i=1,size(this%tTableData)
         write(LU,fmt="(t4,a58,t63,a)") trim(adjustl(this%tTableData(i)%sDescription)), &
            trim(this%tTableData(i)%sValue)
      enddo

    endif

  end subroutine table_list_output_sub

end module tsp_table_manager
