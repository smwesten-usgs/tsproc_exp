module tsp_table_manager

  use tsp_data_structures
  use tsp_utilities
  use tsp_control_file_ops
  use tsp_time_series_manager
  implicit none

  integer (kind=T_INT), parameter :: iSTABLE = 1
  integer (kind=T_INT), parameter :: iCTABLE = 2
  integer (kind=T_INT), parameter :: iETABLE = 3
  integer (kind=T_INT), parameter :: iVTABLE = 4

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
    integer (kind=T_INT) :: iTableType
    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate
    type (T_TABLE_DATA), dimension(:), allocatable :: tTableData

  contains

    procedure, public :: calc_s_table

  end type T_TABLE

!  type, public :: T_VTABLE
!    type (T_DATETIME) :: tStartDate
!    type (T_DATETIME) :: tEndDate
!    type (T_VTABLE_DATA), dimension(:), allocatable :: tVTableData
!  end type T_VTABLE

contains

  subroutine calc_s_table(this, pTS, pBlock)

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
    integer (kind=T_SHORT), parameter :: iMINMEAN = 3
    integer (kind=T_SHORT), parameter :: iMAXMEAN = 4
    integer (kind=T_SHORT), parameter :: iSTD_DEV = 5
    integer (kind=T_SHORT), parameter :: iMINIMUM = 6
    integer (kind=T_SHORT), parameter :: iMAXIMUM = 7
    integer (kind=T_SHORT), parameter :: iRANGE = 8

    real (kind=T_SGL) :: rNEAR_TINY

    real (kind=T_SGL), dimension(8) :: rValue
    character (len=MAXARGLENGTH), dimension(:), pointer :: pMINIMUM, pMAXIMUM, &
       pRANGE, pSUM, pMINMEAN, pMAXMEAN, pSTANDARD_DEVIATION

    ! initialize to the smallest representable number for current machine
    rValue =  -huge(rValue)

    rNEAR_TINY = -huge(rValue) + 1.

    if(present(pBlock)) then  ! obtain user preferences for the S_TABLE

      if(pBlock%select("DATE_1") > 0) then
        call processUserSuppliedDateTime(pBlock, this%tStartDate, this%tEndDate)
      else
        this%tStartDate = pTS%tStartDate
        this%tEndDate = pTS%tEndDate
      endif

      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating S_TABLE: no time series data within " &
        //"specified date range for block starting at line " &
        //trim(int2char(pBlock%iStartingLineNumber)), trim(__FILE__), __LINE__)

      pMINIMUM => pBlock%getVals("MINIMUM")
      if(.not. str_compare(pMINIMUM(1),"no")) then
        iCount = iCount + 1
        rValue(iMINIMUM) = MINVAL(pTS%pData%rValue, pTS%pData%lSelect)
      endif

      pMAXIMUM => pBlock%getVals("MAXIMUM")
      if(.not. str_compare(pMAXIMUM(1),"no")) then
        iCount = iCount + 1
        rValue(iMAXIMUM) = MAXVAL(pTS%pData%rValue, pTS%pData%lSelect)
      endif

      pRANGE => pBlock%getVals("RANGE")
      if(.not. str_compare(pRANGE(1),"no")) then
        iCount = iCount + 1
        rValue(iRANGE) = rValue(iMAXIMUM) - rValue(iMINIMUM)
      endif

      call Assert(iCount > 0, "No statistics selected in " &
        //trim(pBlock%sBlockName)//" block starting at line "// &
        trim(int2char(pBlock%iStartingLineNumber)), trim(__FILE__), __LINE__)

      iSize = iCount + 8

      deallocate(pMINIMUM, pMAXIMUM, pRANGE)

    else ! just calculate all statistics and populate the S_TABLE

      this%tStartDate = pTS%tStartDate
      this%tEndDate = pTS%tEndDate

      iCount =  pTS%selectByDate(this%tStartDate, this%tEndDate)
      call Assert(iCount > 0, "Problem calculating S_TABLE: no time series data within " &
        //"specified date range", trim(__FILE__), __LINE__)

      rValue(iMINIMUM) = MINVAL(pTS%pData%rValue, pTS%pData%lSelect)
      rValue(iMAXIMUM) = MAXVAL(pTS%pData%rValue, pTS%pData%lSelect)
      rValue(iRANGE) = rValue(iMAXIMUM) - rValue(iMINIMUM)

      iSize = 3 + 8

    endif

    this%iTableType = iSTABLE
    this%sSeriesName = trim(pTS%sSeriesName)//"_STBL" ! default value if none provided

    allocate(this%tTableData(iSize))

    this%tTableData(1:8) = (/ &
      T_TABLE_DATA("SERIES_NAME", &
         "Series for which statistics are calculated",trim(pTS%sSeriesName)), &
      T_TABLE_DATA("DATE_1", &
         "Beginning date of data accumulation", this%tStartDate%listdate()), &
      T_TABLE_DATA("TIME_1", &
         "Beginning time of data accumulation", this%tStartDate%listtime()), &
      T_TABLE_DATA("DATE_2", &
          "Ending date of data accumulation", this%tEndDate%listdate()), &
      T_TABLE_DATA("TIME_2", &
         "Beginning time of data accumulation", this%tEndDate%listtime()), &
      T_TABLE_DATA("", &
          "Number of series terms in this interval",int2char(count(pTS%pData%lSelect))) , &
      T_TABLE_DATA("LOG", &
          "Logarithmic transformation of series?", "no"), &
      T_TABLE_DATA("POWER", &
          "Exponent in power transformation", "1") &
    /)

    iCount = 8

    if(rValue(iMINIMUM) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.4)") rValue(iMINIMUM)
      this%tTableData(iCount)%sDescription = "Minimum value"
      this%tTableData(iCount)%sKeyword = "MINIMUM"
    endif

    if(rValue(iMAXIMUM) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.4)") rValue(iMAXIMUM)
      this%tTableData(iCount)%sDescription = "Maximum value"
      this%tTableData(iCount)%sKeyword = "MAXIMUM"
    endif

    if(rValue(iRANGE) > rNEAR_TINY) then
      iCount = iCount + 1
      write(this%tTableData(iCount)%sValue, fmt="(g14.4)") rValue(iRANGE)
      this%tTableData(iCount)%sDescription = "Range of values"
      this%tTableData(iCount)%sKeyword = "RANGE"
    endif

  end subroutine calc_s_table

!------------------------------------------------------------------------------

end module tsp_table_manager
