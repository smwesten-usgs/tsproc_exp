module tsp_main_loop

  use tsp_data_structures
  use tsp_utilities
  use tsp_control_file_ops
  use tsp_file_readers
  use tsp_file_writers
  use tsp_time_series_manager
  use tsp_table_manager
  use tsp_collections
  use tsp_statistics
  use tsp_datetime_class
  use tsp_equations_interpreter
  use tsp_legacy_code
  implicit none

  !f2py intent(hide) :: INFILE
  !f2py intent(hide) :: pBlock
  type (T_FILE) :: INFILE
  type (T_BLOCK), pointer :: pBlock

  !f2py intent(hide) :: pArgs
  !f2py intent(hide) :: pGage
  !f2py intent(hide) :: tTS
  !f2py intent(hide) :: TS
  character (len=MAXARGLENGTH), dimension(:), pointer :: pArgs
  type(T_USGS_NWIS_GAGE),dimension(:), pointer :: pGage
  type (T_TIME_SERIES) :: tTS
  type (TIME_SERIES_COLLECTION) :: TS

  !f2py intent(hide) :: tStartDate
  !f2py intent(hide) :: tEndDate
  !f2py intent(hide) :: pDateRange
  type (T_DATETIME) :: tStartDate, tEndDate
  type (T_DATERANGE), dimension(:), pointer :: pDateRange

  !f2py intent(hide) :: tDT
  !f2py intent(hide) :: tDT2
  !f2py integer (kind=4), dimension(:), allocatable :: iMonth
  !f2py integer (kind=4), dimension(:), allocatable :: iDay
  !f2py integer (kind=4), dimension(:), allocatable :: iYear
  !f2py integer (kind=4), dimension(:), allocatable :: iHour
  !f2py integer (kind=4), dimension(:), allocatable :: iMinute
  !f2py integer (kind=4), dimension(:), allocatable :: iSecond
  !f2py integer (kind=4), dimension(:), allocatable :: iJulianDay
  !f2py real (kind=4), dimension(:), allocatable :: rFractionOfDay
  !f2py real (kind=4), dimension(:), allocatable :: rValue
  !f2py real (kind=4), dimension(:), allocatable :: rObservedValue
  !f2py real (kind=4), dimension(:), allocatable :: rModeledValue
  !f2py real (kind=4), dimension(:), allocatable :: rWeightValue

  type (T_DATETIME) :: tDT, tDT2
  integer (kind=T_INT), dimension(:), allocatable :: iMonth
  integer (kind=T_INT), dimension(:), allocatable :: iDay
  integer (kind=T_INT), dimension(:), allocatable :: iYear
  integer (kind=T_INT), dimension(:), allocatable :: iHour
  integer (kind=T_INT), dimension(:), allocatable :: iMinute
  integer (kind=T_INT), dimension(:), allocatable :: iSecond
  integer (kind=T_INT), dimension(:), allocatable :: iJulianDay
  real (kind=T_DBL), dimension(:), allocatable :: rFractionOfDay
  real (kind=T_SGL), dimension(:), allocatable :: rValue

  real (kind=T_SGL), dimension(:), allocatable :: rObservedValue
  real (kind=T_SGL), dimension(:), allocatable :: rModeledValue
  real (kind=T_SGL), dimension(:), allocatable :: rWeightValue

contains

  subroutine toggleassert()

   lAssertAlwaysFatal = .not. lAssertAlwaysFatal

   if(lAssertAlwaysFatal) then
     write(unit=LU_STD_OUT, fmt="(a)") &
        "TSPROC errors will now result in a FATAL fortran error."
   else
     write(unit=LU_STD_OUT, fmt="(a)") &
        "TSPROC errors will now result in only a warning."
     write(unit=LU_STD_OUT,fmt="(/,a)") &
        "You should probably exit from Python ASAP and restart your work"
     write(unit=LU_STD_OUT,fmt="(a,/)") &
        "if you receive a serious TSPROC error message."

   endif

  end subroutine toggleassert

!------------------------------------------------------------------------------

  subroutine opencontrolfile(sFilename, sRecFile)

    !f2py intent(in) :: sFilename
    !f2py intent(in), optional :: sRecFile
    character(len=*), intent(in) :: sFilename
    character (len=*), intent(in), optional :: sRecFile

    ! [ LOCALS ]
    integer (kind=T_INT) :: iLen
    iLen = 0

    call tStartDate%systime()

    call INFILE%open(trim(sFilename))

    if(present(sRecFile)) iLen = len_trim(sRecFile)

    if(present(sRecFile) .and. iLen > 0) then
      call openlog(sRecFile)
    else
      call openlog()
    endif

  end subroutine opencontrolfile

!------------------------------------------------------------------------------

subroutine listseriesnames(sSeriesnames)

  !f2py character*4096, intent(out) :: sSeriesnames
  character(len=4096), intent(out) :: sSeriesnames

  ! [ LOCALS ]
  character (len=256) :: sFormatString
  integer (kind=T_INT) :: iCount, i

  sSeriesnames = ""

  if(allocated(TS%tTS)) then

    iCount = size(TS%tTS)

    write(sFormatString, fmt="('(',i4,'(a,1x))')") iCount

    write(sSeriesnames, fmt=trim(adjustl(sFormatString))) &
          (trim(TS%tTS(i)%sSeriesname),i=1,iCount)

  else

    sSeriesnames = "NA"

  endif


end subroutine listseriesnames

!------------------------------------------------------------------------------

subroutine listtablenames(sTablenames)

  !f2py character*4096, intent(out) :: sTablenames
  character(len=4096), intent(out) :: sTablenames

  ! [ LOCALS ]
  character (len=256) :: sFormatString
  integer (kind=T_INT) :: iCount, i

  if(allocated(TS%tTable)) then

    iCount = size(TS%tTable)

    write(sFormatString, fmt="('(',i4,'(a,1x))')") iCount

    write(sTablenames, fmt=trim(adjustl(sFormatString))) &
          (trim(TS%tTable(i)%sSeriesname),i=1,iCount)

  else

    sTablenames = "NA"

  endif

end subroutine listtablenames

!------------------------------------------------------------------------------

  subroutine closeControlFile()

    call INFILE%close()
    call closelog()

  end subroutine closecontrolfile

!------------------------------------------------------------------------------

  subroutine newseriescomparison( sObservedSeries, sModeledSeries, sEquationText)

    character(len=*), intent(in) :: sObservedSeries
    character(len=*), intent(in) :: sModeledSeries
    character(len=*), intent(in) :: sEquationText

    call TS%tsCompare(sObservedSeries, sModeledSeries, sEquationText)

  end subroutine newseriescomparison

!------------------------------------------------------------------------------

  subroutine pestwriteseriescomparison( sObservedSeries, sModeledSeries)

    character(len=*), intent(in) :: sObservedSeries
    character(len=*), intent(in) :: sModeledSeries

    call TS%pestWriteTSComparison(sObservedSeries, sModeledSeries)

  end subroutine pestwriteseriescomparison

!------------------------------------------------------------------------------

  subroutine pestwritetablecomparison( sObservedTable, sModeledTable)

    character(len=*), intent(in) :: sObservedTable
    character(len=*), intent(in) :: sModeledTable

    call TS%pestWriteTableComparison(sObservedTable, sModeledTable)

  end subroutine pestwritetablecomparison

!------------------------------------------------------------------------------

  subroutine newtablecomparison( sObservedTable, sModeledTable, sEquationText)

    character(len=*), intent(in) :: sObservedTable
    character(len=*), intent(in) :: sModeledTable
    character(len=*), intent(in) :: sEquationText

    call TS%tableCompare(sObservedTable, sModeledTable, sEquationText)

  end subroutine newtablecomparison

!------------------------------------------------------------------------------

  subroutine newseries(sSeriesName, sDescription, &
    iMonth, iDay, iYear, iHour, iMinute, iSecond, rValue, n)

!f2py character*(*), intent(in) :: sSeriesName
!f2py character*(*), intent(in) :: sDescription
!f2py integer(4), intent(in), dimension(n) :: iMonth
!f2py integer(4), intent(in), dimension(n) :: iDay
!f2py integer(4), intent(in), dimension(n) :: iYear
!f2py integer(4), intent(in), dimension(n) :: iHour
!f2py integer(4), intent(in), dimension(n) :: iMinute
!f2py integer(4), intent(in), dimension(n) :: iSecond
!f2py real(4), intent(in), dimension(n) :: rValue
!f2py integer(kind=4) optional,intent(in),check(len(imonth)>=n),depend(imonth) :: n=len(imonth)

    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in) :: sDescription
    integer (kind=T_INT), dimension(n), intent(in) :: iMonth
    integer (kind=T_INT), dimension(n), intent(in) :: iDay
    integer (kind=T_INT), dimension(n), intent(in) :: iYear
    integer (kind=T_INT), dimension(n), intent(in) :: iHour
    integer (kind=T_INT), dimension(n), intent(in) :: iMinute
    integer (kind=T_INT), dimension(n), intent(in) :: iSecond
    real (kind=T_SGL), dimension(n), intent(in) :: rValue
    integer (kind=T_INT), intent(in) :: n

    call tTS%new(sSeriesName, sDescription, &
    iMonth, iDay, iYear, iHour, iMinute, iSecond, rValue)

    call TS%add(tTS)

  end subroutine newseries

!------------------------------------------------------------------------------
!> @brief This subroutine copies time series data OUT of the fortran
!>        derived type into simple fortran data types in the form of
!>        MODULE variables. These are exposed to python via F2PY.
  subroutine getseries(sSeriesName)

!f2py character*(*), intent(in) :: sSeriesName

    character(len=*), intent(in) :: sSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    integer (kind=T_INT) :: n

    pTS =>TS%getTS(sSeriesName)

    n = size(pTS%tData)
    if(allocated(iJulianDay)) deallocate(iJulianDay)
    if(allocated(rFractionOfDay)) deallocate(rFractionOfDay)
    if(allocated(iMonth)) deallocate(iMonth)
    if(allocated(iDay)) deallocate(iDay)
    if(allocated(iYear)) deallocate(iYear)
    if(allocated(iHour)) deallocate(iHour)
    if(allocated(iMinute)) deallocate(iMinute)
    if(allocated(iSecond)) deallocate(iSecond)
    if(allocated(rValue)) deallocate(rValue)

    allocate(iJulianDay(n)); allocate(iMonth(n)); allocate(iDay(n))
    allocate(iYear(n)); allocate(iHour(n)); allocate(iMinute(n))
    allocate(iSecond(n)); allocate(rValue(n)); allocate(rFractionOfDay(n))

    iJulianDay = pTS%tData%tDT%iJulianDay
    iMonth = pTS%tData%tDT%iMonth
    iDay = pTS%tData%tDT%iDay
    iYear = pTS%tData%tDT%iYear
    iHour = pTS%tData%tDT%iHour
    iMinute = pTS%tData%tDT%iMinute
    iSecond = pTS%tData%tDT%iSecond
    rFractionOfDay = pTS%tData%tDT%rFractionOfDay
    rValue = pTS%tData%rValue

    nullify(pTS)

  end subroutine getseries

!------------------------------------------------------------------------------
!> @brief This subroutine copies time series data OUT of the fortran
!>        derived type into simple fortran data types in the form of
!>        MODULE variables. These are exposed to python via F2PY.
  subroutine getseriescomparison(sObservedSeries, sModeledSeries)

!f2py character*(*), intent(in) :: sObservedSeries
!f2py character*(*), intent(in) :: sModeledSeries
    character(len=*), intent(in) :: sObservedSeries
    character(len=*), intent(in) :: sModeledSeries

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS_Observed
    type (T_TIME_SERIES), pointer :: pTS_Modeled
    type (T_TS_COMPARISON), pointer :: pTS_Comparison
    integer (kind=T_INT) :: n

    if(allocated(TS%tTSComparison) ) then

      pTS_Comparison => TS%getTSComparison(sObservedSeries,sModeledSeries)

      pTS_Observed =>TS%getTS(sObservedSeries)
      pTS_Modeled =>TS%getTS(sModeledSeries)

      n = size(pTS_Observed%tData)
      if(allocated(iJulianDay)) deallocate(iJulianDay)
      if(allocated(rFractionOfDay)) deallocate(rFractionOfDay)
      if(allocated(iMonth)) deallocate(iMonth)
      if(allocated(iDay)) deallocate(iDay)
      if(allocated(iYear)) deallocate(iYear)
      if(allocated(iHour)) deallocate(iHour)
      if(allocated(iMinute)) deallocate(iMinute)
      if(allocated(iSecond)) deallocate(iSecond)
      if(allocated(rObservedValue)) deallocate(rObservedValue)
      if(allocated(rModeledValue)) deallocate(rModeledValue)
      if(allocated(rWeightValue)) deallocate(rWeightValue)

      allocate(iJulianDay(n)); allocate(iMonth(n)); allocate(iDay(n))
      allocate(iYear(n)); allocate(iHour(n)); allocate(iMinute(n))
      allocate(iSecond(n)); allocate(rFractionOfDay(n))
      allocate(rObservedValue(n)); allocate(rModeledValue(n))
      allocate(rWeightValue(n))

      iJulianDay = pTS_Observed%tData%tDT%iJulianDay
      iMonth = pTS_Observed%tData%tDT%iMonth
      iDay = pTS_Observed%tData%tDT%iDay
      iYear = pTS_Observed%tData%tDT%iYear
      iHour = pTS_Observed%tData%tDT%iHour
      iMinute = pTS_Observed%tData%tDT%iMinute
      iSecond = pTS_Observed%tData%tDT%iSecond
      rFractionOfDay = pTS_Observed%tData%tDT%rFractionOfDay
      rObservedValue = pTS_Observed%tData%rValue
      rModeledValue = pTS_Modeled%tData%rValue
      rWeightValue = pTS_Comparison%rWeightValue

      nullify(pTS_Observed)
      nullify(pTS_Modeled)
      nullify(pTS_Comparison)

    else

      call warn(lFALSE,"There are no time series comparison objects to retrieve")

    endif

  end subroutine getseriescomparison

!------------------------------------------------------------------------------

  subroutine removetable(sSeriesName)

    character(len=*), intent(in) :: sSeriesName

    call TS%removeTable(sSeriesName)

  end subroutine removetable

!------------------------------------------------------------------------------

  subroutine removeseries(sSeriesName)

    character(len=*), intent(in) :: sSeriesName

    call TS%removeTS(sSeriesName)

  end subroutine removeseries

!------------------------------------------------------------------------------

  subroutine listseries(sSeriesname, sDateFormat)

    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in), optional :: sDateFormat

    ! [ LOCALS ]
    character (len=20) :: sDateFmt

    if(present(sDateFormat) .and. len_trim(sDateFormat) > 0) then
      sDateFmt = trim(sDateFormat)
    else
      sDateFmt = "MM/DD/YYYY"
    endif

    call TS%listTS(sSeriesName, sDateFmt)


  end subroutine listseries

!------------------------------------------------------------------------------

  subroutine listtable(sSeriesname)

    character(len=*), intent(in) :: sSeriesName

    ! [ LOCALS ]

    call TS%listTable(sSeriesName)

  end subroutine listtable

!------------------------------------------------------------------------------

  subroutine describeseries(sSeriesname)

    character(len=*), intent(in) :: sSeriesName

    call TS%describe(sSeriesName)

  end subroutine describeseries

!------------------------------------------------------------------------------

  subroutine new_time_base(sSeriesname, sTimeBaseSeriesName, sNewSeriesName)

    !f2py character(len=*), intent(in), optional :: sSeriesName
    !f2py character(len=*), intent(in), optional :: sTimeBaseSeriesName
    !f2py character(len=*), intent(in), optional :: sNewSeriesName
    character(len=*), intent(inout), optional :: sSeriesName
    character(len=*), intent(inout), optional :: sTimeBaseSeriesName
    character(len=*), intent(inout), optional :: sNewSeriesName

    ! [ LOCALS ]
    integer (kind=T_INT) :: iLen1, iLen2, iLen3
    integer (kind=T_INT) :: i, j, iCount
    character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME, &
       pNEW_SERIES_NAME, pTB_SERIES_NAME
    character (len=256) :: sNewName

    iLen1 = 0; iLen2 = 0; iLen3 = 0

    if(present(sSeriesName)) iLen1 = len_trim(sSeriesName)
    if(present(sTimeBaseSeriesName)) iLen2 = len_trim(sTimeBaseSeriesName)
    if(present(sNewSeriesName)) iLen3 = len_trim(sNewSeriesName)

    if( iLen1 > 0 .and. iLen2 > 0 ) then    ! ignore current block and process
                                            ! using dummy variable names

      ! note: need to test for the length of an optional argument, since it
      ! appears that Python supplies an empty string as the optional argumen
      if(present(sNewSeriesName) .and. len_trim(sNewSeriesName) > 0 ) then
        sNewName = trim(adjustl(sNewSeriesName))
      else
        sNewName = trim(adjustl(sSeriesName))//"_TB"
      endif

    else  ! assume all arguments come via a block; parse block data and proceed

      pSERIES_NAME = pBlock%getString("SERIES_NAME")
      pTB_SERIES_NAME = pBlock%getString("TB_SERIES_NAME")
      pNEW_SERIES_NAME = pBlock%getString("NEW_SERIES_NAME")

      sSeriesName = trim(pSERIES_NAME(1))
      sNewSeriesName = trim(pNEW_SERIES_NAME(1))
      sTimeBaseSeriesName = trim(pTB_SERIES_NAME(1))

    endif

    call TS%newTimeBase(sSeriesname, sTimeBaseSeriesName, sNewName)

  end subroutine new_time_base

!------------------------------------------------------------------------------

  subroutine testquantiles( rQuantileVals, rDataVals, rReturnVals, n, m)

    !f2py real*4, dimension(n), intent(in) :: rQuantileVals
    !f2py real*4, dimension(m), intent(in) :: rDataVals
    !f2py integer*4, optional, check(len(rQuantileVals)>=n), depend(rQuantileVals) :: n = len(rQuantileVals)
    !f2py integer*4, optional, check(len(rDataVals)>=m), depend(rDataVals) :: m = len(rDataVals)
    !f2py real*4, dimension(n), intent(out) :: rReturnVals
    real (kind=T_SGL), dimension(n), intent(in) :: rQuantileVals
    real (kind=T_SGL), dimension(m), intent(in) :: rDataVals
    real (kind=T_SGL), dimension(n), intent(out) ::rReturnVals
    integer (kind=T_INT) :: n
    integer (kind=T_INT) :: m

    ! [ LOCALS ]
    integer (kind=T_INT) :: iStat

    ! for now (July 2010, gfortran is NOT able to accept allocatable function results
    ! as assignments to an unallocated allocatable array; MUST allocate first,
    ! then call the allocatable function. This implies that one must know how many results
    ! are being returned, which somewhat negates the value of this feature!

    rReturnVals = quantiles( rQuantileVals, rDataVals)

  end subroutine testquantiles

!------------------------------------------------------------------------------

  subroutine hydrologic_indices(sSeriesname)

    !f2py character*(*), intent(in), optional :: sSeriesName
    character(len=*), intent(in), optional :: sSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    type (T_TABLE) :: tTable
    integer (kind=T_INT) :: n
    character (len=MAXARGLENGTH), dimension(:), pointer :: pArgs
    character (len=MAXARGLENGTH) :: sTempSeriesname

    if(present(sSeriesname) .and. len_trim(sSeriesName) > 0 ) then

      pTS =>TS%getTS(sSeriesName)

      ! don't pass along the block object; use defaults
      call tTable%calc_i_table(pTS)

    elseif(str_compare(pBlock%sBlockName, "HYDROLOGIC_INDICES")) then

      pArgs =>pBlock%getString("SERIES_NAME")
      sTempSeriesname = pArgs(1)
      pTS => TS%getTS( sTempSeriesname )
      call tTable%calc_i_table(pTS, pBlock)

    else

      call Assert(lFALSE, "Unhandled case in routine hydrologic_indices", &
        trim(__FILE__), __LINE__)

    endif

    call TS%add(tTable)
    nullify(pTS)

  end subroutine hydrologic_indices

!------------------------------------------------------------------------------

  subroutine exceedence_time(sSeriesname)

    !f2py character*(*), intent(in), optional :: sSeriesName
    character(len=*), intent(in), optional :: sSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    type (T_TABLE) :: tTable
    integer (kind=T_INT) :: n
    character (len=MAXARGLENGTH), dimension(:), pointer :: pArgs
    character (len=MAXARGLENGTH) :: sTempSeriesname

    if(present(sSeriesname) .and. len_trim(sSeriesName) > 0 ) then

      pTS =>TS%getTS(sSeriesName)

      ! don't pass along the block object; use defaults
      call tTable%calc_e_table(pTS)

    elseif(str_compare(pBlock%sBlockName, "EXCEEDENCE_TIME")) then

      pArgs =>pBlock%getString("SERIES_NAME")
      sTempSeriesname = pArgs(1)
      pTS => TS%getTS( sTempSeriesname )
      call tTable%calc_e_table(pTS, pBlock)

    else

      call Assert(lFALSE, "Unhandled case in routine exceedence_time", &
        trim(__FILE__), __LINE__)

    endif

    call TS%add(tTable)
    nullify(pTS)

  end subroutine exceedence_time

!------------------------------------------------------------------------------

  subroutine series_statistics(sSeriesname)

    character(len=*), intent(in), optional :: sSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    type (T_TABLE) :: tTable
    integer (kind=T_INT) :: n
    character (len=MAXARGLENGTH), dimension(:), pointer :: pArgs
    character (len=MAXARGLENGTH) :: sTempSeriesname

    if(present(sSeriesname) .and. len_trim(sSeriesName) > 0 ) then

      pTS =>TS%getTS(sSeriesName)

      ! don't pass along the block object; use defaults
      call tTable%calc_s_table(pTS)

    elseif(str_compare(pBlock%sBlockName, "SERIES_STATISTICS")) then

      pArgs =>pBlock%getString("SERIES_NAME")
      sTempSeriesname = pArgs(1)
      pTS => TS%getTS( sTempSeriesname )
      call tTable%calc_s_table(pTS, pBlock)

    else

      call Assert(lFALSE, "Unhandled case in routine seriesstatistics", &
        trim(__FILE__), __LINE__)

    endif

    call TS%add(tTable)
    nullify(pTS)

  end subroutine series_statistics

!------------------------------------------------------------------------------

subroutine reduce_time_span(sSeriesname, sStartdate, sEnddate)

  !f2py character*(*), intent(in) :: sSeriesname
  !f2py character*(*), intent(in) :: sStartdate
  !f2py character*(*), intent(in) :: sEnddate
  character(len=*), optional :: sSeriesname
  character(len=*), optional :: sStartdate
  character(len=*), optional :: sEnddate

  ! [ LOCALS ]
  integer (kind=T_INT) :: iLen1, iLen2, iLen3
  integer (kind=T_INT) :: i, j, iCount
  character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME, &
     pNEW_SERIES_NAME

  character (len=256) :: sRecord, sItem

  type (T_DATETIME) :: tDATETIME_1, tDATETIME_2
  type (T_TIME_SERIES) :: tNewSeries
  type (T_TIME_SERIES), pointer :: pTS

  iLen1 = 0; iLen2 = 0; iLen3 = 0

  if(present(sSeriesName)) iLen1 = len_trim(sSeriesName)
  if(present(sStartdate)) iLen2 = len_trim(sStartdate)
  if(present(sEnddate)) iLen3 = len_trim(sEnddate)

  do

    if(iLen1 > 0 .and. iLen2 > 0 .and. iLen3 > 0) then

      ! assume there is no block data; parse date info from dummy args
      sRecord = sStartDate
      call Chomp(sRecord, sItem)
      call tDATETIME_1%parseDate(sItem)
      if(len_trim(sRecord) > 0) then
        call tDATETIME_1%parseTime(sRecord)
      else
        call tDATETIME_1%parseTime("12:00:00")
      endif
      call tDATETIME_1%calcJulianDay()

      sRecord = sEndDate
      call Chomp(sRecord, sItem)
      call tDATETIME_2%parseDate(sItem)
      if(len_trim(sRecord) > 0) then
        call tDATETIME_2%parseTime(sRecord)
      else
        call tDATETIME_2%parseTime("12:00:00")
      endif
      call tDATETIME_2%calcJulianDay()

      if(tDATETIME_2 <= tDATETIME_1) then
        call warn(lFALSE, &
          "DATE_2 and TIME_2 ("//trim(tDATETIME_2%listdatetime() )//") must be greater" &
          //" than DATE_1 and TIME_1 ("//trim(tDATETIME_1%listdatetime() )//")", trim(__FILE__),__LINE__)
        exit
      endif

      tNewSeries%sSeriesName = trim(sSeriesName)//"_RDC"

    elseif(str_compare(pBlock%sBlockName, "REDUCE_TIME_SPAN")) then

      ! get DATE_1, TIME_1, DATE_2, TIME_2 if supplied
      call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)
      pNEW_SERIES_NAME = pBlock%getString("NEW_SERIES_NAME")
      if(str_compare(pNEW_SERIES_NAME(1),"NA")) then
        tNewSeries%sSeriesName = trim(sSeriesName)//"_RDC"
      else
        tNewSeries%sSeriesName = trim(pNEW_SERIES_NAME(1))
      endif

    else
      call warn(lFALSE, "Must supply series name, startdate, and enddate (as strings)" &
         //" OR call this routine with no arguments at all")
      exit
    endif

    ! get pointer to series of interest; restrict to specified date
    pTS => TS%getTS( sSeriesName )
    iCount = pTS%selectByDate( tDATETIME_1, tDATETIME_2)
    allocate(tNewSeries%tData(iCount))

    ! transfer selected data over to new series
    j=0
    do i=1,size(pTS%tData)
      if(pTS%tData(i)%lSelect) then
        j = j + 1
        tNewSeries%tData(j) = pTS%tData(i)
      endif
    enddo

    ! add new series to TS collection
    call TS%add(tNewSeries)

    exit
  enddo

end subroutine reduce_time_span

!------------------------------------------------------------------------------

subroutine usgs_hysep(sInputSeriesname, sHysepType, sTimeInterval, sStartdate, sEnddate)

  !f2py character*(*), intent(in) :: sInputSeriesname
  !f2py character*(*), intent(in), optional :: sHysepType
  !f2py character*(*), intent(in), optional :: sTimeInterval
  !f2py character*(*), intent(in), optional :: sStartdate
  !f2py character*(*), intent(in), optional :: sEnddate
  character(len=*), optional :: sInputSeriesname
  character(len=*), optional :: sHysepType
  character(len=*), optional :: sTimeInterval
  character(len=*), optional :: sStartdate
  character(len=*), optional :: sEnddate

  ! [ LOCALS ]
  integer (kind=T_INT) :: iLen1, iLen2, iLen3, iLen4, iLen5
  integer (kind=T_INT) :: i, j, iCount
  integer (kind=T_INT) :: iInterval
  real (kind=T_SGL) :: rInterval
  real (kind=T_SGL) :: rDrainageArea
  character (len=MAXNAMELENGTH) :: sSeriesName

  integer (kind=T_INT) :: iHYSEP_TYPE
  integer (kind=T_INT), parameter :: iFIXED_INTERVAL = 1
  integer (kind=T_INT), parameter :: iSLIDING_INTERVAL = 2
  integer (kind=T_INT), parameter :: iLOCAL_MINIMUM = 3

  character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME, &
     pNEW_SERIES_NAME, pHYSEP_TYPE
  real, dimension(:), pointer :: pDRAINAGE_AREA, pTIME_INTERVAL

  character (len=256) :: sRecord, sItem

  type (T_DATETIME) :: tDATETIME_1, tDATETIME_2
  type (T_TIME_SERIES) :: tNewSeries_BF
  type (T_TIME_SERIES) :: tNewSeries_SF
  type (T_TIME_SERIES), pointer :: pTS

  iLen1 = 0; iLen2 = 0; iLen3 = 0; iLen4 = 0; iLen5 = 0
  iHYSEP_TYPE = iFIXED_INTERVAL
  iInterval = 5

  if(present(sInputSeriesName)) iLen1 = len_trim(sInputSeriesName)
  if(present(sHysepType)) iLen2 = len_trim(sHysepType)
  if(present(sTimeInterval)) iLen3 = len_trim(sTimeInterval)
  if(present(sStartdate)) iLen4 = len_trim(sStartdate)
  if(present(sEnddate)) iLen5 = len_trim(sEnddate)

  if(iLen2 > 0) then
    if(str_compare(sHysepType,"fixed_interval") ) then
      iHYSEP_TYPE = iFIXED_INTERVAL
    elseif(str_compare(sHysepType,"sliding_interval") ) then
      iHYSEP_TYPE = iSLIDING_INTERVAL
    elseif(str_compare(sHysepType,"local_minimum") ) then
      iHYSEP_TYPE = iLOCAL_MINIMUM
    else
      call Assert(lFALSE, "Unknown HYSEP_TYPE value: "//trim(sHysepType), &
        trim(__FILE__), __LINE__)
    endif
  endif

  if(iLen3 > 0 ) iInterval = asInt(sTimeInterval)

  do

    ! need to check that length > 0 as well as presence; Python
    ! seems to be happy passing zero-length optional arguments
    if(iLen1 > 0 ) then

      sSeriesName = trim(sInputSeriesName)

      ! assume there is no block data; parse date info from dummy args
      if(iLen4 > 0) then
        sRecord = sStartDate
        call Chomp(sRecord, sItem)
        call tDATETIME_1%parseDate(sItem)
        if(len_trim(sRecord) > 0) then
          call tDATETIME_1%parseTime(sRecord)
        else
          call tDATETIME_1%parseTime("12:00:00")
        endif
      else
        call tDATETIME_1%parseDate("01/01/0001")
        call tDATETIME_1%parseTime("12:00:00")
      endif
      call tDATETIME_1%calcJulianDay()

      sRecord = sEndDate
      call Chomp(sRecord, sItem)

      if(iLen5 > 0) then
        sRecord = sEndDate
        call Chomp(sRecord, sItem)
        call tDATETIME_2%parseDate(sItem)
        if(len_trim(sRecord) > 0) then
          call tDATETIME_2%parseTime(sRecord)
        else
          call tDATETIME_2%parseTime("12:00:00")
        endif
      else
        call tDATETIME_2%parseDate("12/31/3000")
        call tDATETIME_2%parseTime("12:00:00")
      endif
      call tDATETIME_2%calcJulianDay()

      if(tDATETIME_2 <= tDATETIME_1) then
        call warn(lFALSE, &
          "DATE_2 and TIME_2 ("//trim(tDATETIME_2%listdatetime() )//") must be greater" &
          //" than DATE_1 and TIME_1 ("//trim(tDATETIME_1%listdatetime() )//")", trim(__FILE__),__LINE__)
        exit
      endif

      tNewSeries_BF%sSeriesName = trim(sSeriesName)//"_BF"
      tNewSeries_SF%sSeriesName = trim(sSeriesName)//"_SF"

    elseif(str_compare(pBlock%sBlockName, "USGS_HYSEP")) then

      ! get DATE_1, TIME_1, DATE_2, TIME_2 if supplied
      call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)
      pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
      if(str_compare(pNEW_SERIES_NAME(1),"NA")) then
        tNewSeries_BF%sSeriesName = trim(sSeriesName)//"_BF"
        tNewSeries_SF%sSeriesName = trim(sSeriesName)//"_SF"
      else
        tNewSeries_BF%sSeriesName = trim(pNEW_SERIES_NAME(1))
        tNewSeries_SF%sSeriesName = trim(pNEW_SERIES_NAME(1))//"_SF"
      endif

      pSERIES_NAME => pBlock%getString("SERIES_NAME")
      sSeriesName = trim(pSERIES_NAME(1) )

      ! get the values
      pHYSEP_TYPE => pBlock%getString("HYSEP_TYPE")

      select case(trim(lowercase(pHYSEP_TYPE(1)) ) )
        case("fixed_interval")
          iHYSEP_TYPE = iFIXED_INTERVAL
        case("sliding_interval")
          iHYSEP_TYPE = iSLIDING_INTERVAL
        case("local_minimum")
          iHYSEP_TYPE = iLOCAL_MINIMUM
      end select

      pDRAINAGE_AREA => pBlock%getReal("DRAINAGE_AREA")
      pTIME_INTERVAL => pBlock%getReal("TIME_INTERVAL")

      if(pTIME_INTERVAL(1) > rNEAR_TINY ) iInterval = int(pTIME_INTERVAL(1), kind=T_INT)

      if(pDRAINAGE_AREA(1) > rNEAR_TINY) then
!           rintr = 2.0*(tarea**0.2)
         rInterval = 2.0 * ( pDRAINAGE_AREA(1)**0.2 )

         if (rInterval <= 4.0) then
           iInterval = 3
         else if (rInterval <= 6.0  .AND.  rInterval > 4.0) then
           iInterval = 5
         else if (rInterval <= 8.0  .AND.  rInterval > 6.0) then
           iInterval = 7
         else if (rInterval <= 10.  .AND.  rInterval > 8.0) then
           iInterval = 9
         else
           iInterval = 11
         end if
       endif

    else
      call warn(lFALSE, "Must supply series name, and optionally the startdate, and enddate (as strings)" &
         //" OR call this routine with no arguments at all")
      exit
    endif

    ! get pointer to series of interest; restrict to specified date
    pTS => TS%getTS( sSeriesName )
    iCount = pTS%selectByDate( tDATETIME_1, tDATETIME_2)
    allocate(tNewSeries_BF%tData(iCount))
    allocate(tNewSeries_SF%tData(iCount))

    tNewSeries_BF%tData = pack(pTS%tData, pTS%tData%lSelect)
    tNewSeries_SF%tData = pack(pTS%tData, pTS%tData%lSelect)

    if(iInterval < 3 .or. iInterval > 11 .or. mod(iInterval,2) /=1) then
      call warn(lFALSE,"Interval must be in the set [3,5,7,9,11]. You entered " &
        //asChar(iInterval) )
      exit
    endif

    ! make call to appropriate baseflow calculation routine
    select case ( iHYSEP_TYPE )
      case(iFIXED_INTERVAL)
        call fixed(iCount,pack(pTS%tData%rValue, pTS%tData%lSelect), &
                  iInterval,1,0,tNewSeries_BF%tData%rValue)

      case(iSLIDING_INTERVAL)
        call slide(iCount,pack(pTS%tData%rValue, pTS%tData%lSelect), &
                  iInterval,tNewSeries_BF%tData%rValue)

      case(iLOCAL_MINIMUM)
        call locmin(iCount,pack(pTS%tData%rValue, pTS%tData%lSelect), &
                  iInterval,tNewSeries_BF%tData%rValue)

      case default
        call Assert(lFALSE, "Internal logic error - failed to match appropriate" &
          //" hysep_type argument", trim(__FILE__), __LINE__)

    end select

    ! at this point, tNewSeries_SF%tData%rValue holds the total mean daily streamflow
    tNewSeries_SF%tData%rValue = tNewSeries_SF%tData%rValue - tNewSeries_BF%tData%rValue

    ! add new series to TS collection
    call TS%add(tNewSeries_BF)
    call TS%add(tNewSeries_SF)

    exit
  enddo

  nullify(pTS)
  if(associated(pDRAINAGE_AREA) ) deallocate(pDRAINAGE_AREA)
  if(associated(pTIME_INTERVAL) ) deallocate(pTIME_INTERVAL)
  if(associated(pHYSEP_TYPE) ) deallocate(pHYSEP_TYPE)
  if(associated(pNEW_SERIES_NAME) ) deallocate(pNEW_SERIES_NAME)
  if(associated(pSERIES_NAME) ) deallocate(pSERIES_NAME)

end subroutine usgs_hysep

!------------------------------------------------------------------------------

  subroutine summarize()

    call TS%summarize()

  end subroutine summarize

!------------------------------------------------------------------------------

  subroutine addseries()

    call TS%add(tTS)

  end subroutine addseries

!------------------------------------------------------------------------------

  subroutine findgaps(sSeriesname)

    character(len=*) :: sSeriesname

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    pTS => TS%getTS( sSeriesName )

    pDateRange => pTS%findDataGaps()

    deallocate(pTS)

  end subroutine findgaps

!------------------------------------------------------------------------------

  subroutine getNextBlock(sBlockname)

    !f2py intent(out) :: sBlockname
    character (len=32), intent(out) :: sBlockname

    ! deallocate memory potentially still in use from previous block
    if(associated(pBlock)) call pBlock%deallocate()

    ! "readBlock" returns a pointer to the next block in the control file
    pBlock => INFILE%readBlock()
    sBlockname = pBlock%sBlockname

  end subroutine getNextBlock

!------------------------------------------------------------------------------
!
!  what have we learned, then? At this writing, we cannot have an allocatable
!  array returned from a function without first ALLOCATING the requisite
!  memory BEFORE calling the function. Catch-22.
!
!  This is supposed to be O.K. with the introduction of F2003 allocatable
!  function results, but this is apparently not (yet) supported in gfortran.
!
!  Therefore, we must continue to return pointers to arrays passed from a
!  function.

  subroutine continueProcessing(iReturnCode)

    !f2py integer (kind=4), intent(out) :: iReturnCode
    integer (kind=T_INT), intent(out) :: iReturnCode

    iReturnCode = -1

    if(str_compare(pBlock%sBlockname,"SETTINGS")) then

      call INFILE%processSettingsBlock(pBlock)

    elseif(str_compare(pBlock%sBlockname,"SERIES_STATISTICS")) then

      call series_statistics()

    elseif(str_compare(pBlock%sBlockname,"SERIES_DIFFERENCE")) then

      call series_difference()

    elseif(str_compare(pBlock%sBlockname,"SERIES_EQUATION")) then

      call series_equation()

    elseif(str_compare(pBlock%sBlockname,"HYDROLOGIC_INDICES")) then

      call hydrologic_indices()

    elseif(str_compare(pBlock%sBlockname,"EXCEEDENCE_TIME")) then

      call exceedence_time()

    elseif(str_compare(pBlock%sBlockname, "REDUCE_TIME_SPAN")) then

      call reduce_time_span()

    elseif(str_compare(pBlock%sBlockname, "PERIOD_STATISTICS")) then

      call period_statistics()

    elseif(str_compare(pBlock%sBlockname, "NEW_TIME_BASE")) then

      call new_time_base()

    elseif(str_compare(pBlock%sBlockname, "LIST_OUTPUT")) then

      call list_output()

    elseif(str_compare(pBlock%sBlockname, "VOLUME_CALCULATION")) then

      call volume_calculation()

    elseif(str_compare(pBlock%sBlockname,"GET_MUL_SERIES_NWIS")) then

      call read_USGS_NWIS(pBlock, TS)

    elseif(str_compare(pBlock%sBlockname,"GET_MUL_SERIES_SSF")) then

      call get_mul_series_ssf(pBlock, TS)

    elseif(str_compare(pBlock%sBlockname,"GET_MUL_SERIES_STATVAR")) then

      call get_mul_series_statvar(pBlock, TS)

    elseif(str_compare(pBlock%sBlockname,"GET_SERIES_WDM")) then

      call get_series_WDM(pBlock, TS)

    elseif(str_compare(pBlock%sBlockname,"USGS_HYSEP")) then

      call usgs_hysep()

    elseif(str_compare(pBlock%sBlockname,"INACTIVE")) then

    elseif(str_compare(pBlock%sBlockname,"EOF")) then
      iReturnCode = 0
    endif

end subroutine continueProcessing

!------------------------------------------------------------------------------

subroutine series_difference(sSeriesname, sNewSeriesName)

  !f2py character*(*), intent(in) :: sSeriesname
  character(len=*), optional :: sSeriesname
  character(len=*), optional :: sNewSeriesname

  ! [ LOCALS ]
  integer (kind=T_INT) :: iLen1, iLen2
  integer (kind=T_INT) :: i, j, iCount, iStat
  character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME, &
     pNEW_SERIES_NAME

  character (len=256) :: sNewName

  type (T_TIME_SERIES) :: tNewSeries
  type (T_TIME_SERIES), pointer :: pTS

  iLen1 = 0; iLen2 = 0

  if(present(sSeriesName)) iLen1 = len_trim(sSeriesName)
  if(present(sNewSeriesName)) iLen2 = len_trim(sNewSeriesName)

  if(iLen1 > 0) then   ! ignore block; perform operations on sSeriesname

    pTS => TS%getTS(sSeriesName)

    if(iLen2 > 0) then
      sNewName = trim(sNewSeriesName)
    else
      sNewName = trim(sSeriesname)//"_DIFF"
    endif

  elseif(str_compare(pBlock%sBlockName,"SERIES_DIFFERENCE")) then

    pSERIES_NAME => pBlock%getString("SERIES_NAME")
    pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
    pTS => TS%getTS(pSERIES_NAME(1))

    sNewName = trim(pNEW_SERIES_NAME(1))

    deallocate(pSERIES_NAME, pNEW_SERIES_NAME)

  else

    call Assert(lFALSE, "Unhandled exception -- perhaps this subroutine called" &
      //" without being passed a valid SERIES_DIFFERENCE block?", &
      trim(__FILE__), __LINE__)

  endif

  iCount = size(pTS%tData)

  allocate(tNewSeries%tData(iCount - 1), stat = iStat)
  call Assert(iStat == 0, "Problem allocating memory for time series", &
    trim(__FILE__), __LINE__)

  tNewSeries%sSeriesname = trim(sNewName)
  tNewSeries%tData = pTS%tData(2:iCount)

  do i=2,iCount
    tNewSeries%tData(i-1)%rValue = pTS%tData(i)%rValue - pTS%tData(i-1)%rValue
  enddo

  call TS%add(tNewSeries)

end subroutine series_difference

!------------------------------------------------------------------------------

subroutine series_equation()

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, j, iCount, iStat
  character (len=MAXARGLENGTH), dimension(:), pointer :: pEQUATION, &
     pNEW_SERIES_NAME

  character (len=256) :: sNewName
  character (len=MAXEQUATIONLENGTH) :: sFunctionText

  if(str_compare(pBlock%sBlockName,"SERIES_EQUATION")) then

    pEQUATION => pBlock%getString("EQUATION")
    pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")

    sNewName = trim(pNEW_SERIES_NAME(1))
    sFunctionText = trim(pEQUATION(1) )

    deallocate( pEQUATION, pNEW_SERIES_NAME)
  else

    call Assert(lFALSE, "Unhandled exception -- perhaps this subroutine called" &
      //" without being passed a valid SERIES_DIFFERENCE block?", &
      trim(__FILE__), __LINE__)

  endif

  call newseriesfromequation(sFunctionText, sNewName)

end subroutine series_equation

!------------------------------------------------------------------------------

subroutine datestampsequal(sSeriesname1, sSeriesname2, lbool)

  !f2py character (len=*), intent(in) :: sSeriesname1
  !f2py character (len=*), intent(in) :: sSeriesname2
  !f2py logical, intent(out) :: lBool
  character (len=*), intent(in) :: sSeriesname1
  character (len=*), intent(in) :: sSeriesname2
  logical (kind = T_LOGICAL ) :: lBool

  lBool = TS%datesEqual(sSeriesname1, sSeriesname2)

end subroutine datestampsequal

!------------------------------------------------------------------------------

  subroutine volume_calculation(sSeriesname)

    character(len=*), intent(in), optional :: sSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    type (T_TABLE) :: tTable
    integer (kind=T_INT) :: n
    character (len=MAXARGLENGTH), dimension(:), pointer :: pArgs
    character (len=MAXARGLENGTH) :: sTempSeriesname

    if(present(sSeriesname) .and. len_trim(sSeriesName) > 0 ) then

      pTS =>TS%getTS(sSeriesName)

      ! don't pass along the block object; use defaults
      call tTable%calc_v_table(pTS)

    elseif(str_compare(pBlock%sBlockName, "VOLUME_CALCULATION")) then

      pArgs =>pBlock%getString("SERIES_NAME")
      sTempSeriesname = pArgs(1)

      call Assert(.not. str_compare(sTempSeriesname, "NA"), &
        "A series name must be provided in a VOLUME_CALCULATION block", &
        trim(__FILE__), __LINE__)

      pTS => TS%getTS( sTempSeriesname )
      call tTable%calc_v_table(pTS, pBlock)

    else

      call Assert(lFALSE, "Unhandled case in routine volume_calculation", &
        trim(__FILE__), __LINE__)

    endif

    call TS%add(tTable)
    nullify(pTS)

  end subroutine volume_calculation

!------------------------------------------------------------------------------

subroutine calcvolume(sSeriesname)

    !f2py character*(*), intent(in) :: sSeriesname
    character(len=*), intent(in) :: sSeriesname

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    real (kind=T_DBL) :: rVolume

    pTS =>TS%getTS(sSeriesName)

    rVolume = pTS%integrate( rConversionFactor = 86400. )

    call echolog("Volume from "//trim(pTS%tStartDate%listdatetime() )//" to " &
      //trim(pTS%tEndDate%listdatetime() )//": "//asChar(rVolume) )

end subroutine calcvolume

!------------------------------------------------------------------------------

subroutine testreaddates(sFilename)

  !f2py character*(*), intent(in) :: sFilename
  character (len=*), intent(in) :: sFilename

  ! [ LOCALS ]
  type (T_DATERANGE), dimension(:), pointer :: pDR
  integer (kind=T_INT) :: i

  pDR => read_dates_file(sFilename)

  call echolog("")
  call echolog("Dates file "//quote(sFilename)//" contains the following date ranges:")

  do i=1, size(pDR)

    call echolog("  "//trim(pDR(i)%tStartDate%listdatetime() )//" to " &
         //trim(pDR(i)%tEndDate%listdatetime() ) )

  enddo

  deallocate(pDR)

end subroutine testreaddates

!------------------------------------------------------------------------------

subroutine list_output()

  call write_list_output_block(pBlock, TS)

end subroutine list_output

!------------------------------------------------------------------------------

subroutine write_pest_files()

  call pest_files(pBlock, TS)

end subroutine write_pest_files

!------------------------------------------------------------------------------

subroutine finalize()

  !f2py real*8, intent(hide) :: rElapsedSeconds
  real (kind=T_DBL) :: rElapsedSeconds
  character (len=16) :: sElapsedSeconds

  call tEndDate%systime()

  rElapsedSeconds = ( tEndDate - tStartDate ) * 86400_T_DBL
  write(sElapsedSeconds, fmt="(g16.5)") rElapsedSeconds

  call echolog("")
  call echolog("TSPROC run completed in "//trim(adjustl(sElapsedSeconds))//" seconds.", "(a)")
  call echolog("")

  call INFILE%close()
  call TS%clear()
  call closelog()

end subroutine finalize

!------------------------------------------------------------------------------

subroutine clear()

  call TS%clear()

end subroutine clear

!------------------------------------------------------------------------------

subroutine evaluate(sFunctionText, sVariables, rVariables, rOut, m, n)

  implicit none
  !f2py character*(*), intent(in) :: sFunctionText
  !f2py character*(*), intent(in) :: sVariables
  !f2py real*4, dimension(m, n), intent(in) :: rVariables
  !f2py real*4, dimension(m), intent(out) :: rOut
  !f2py integer*4, optional, check(shape(rVariables,0)==m), depend(rVariables), intent(in) :: m=shape(rVariables,0)
  !f2py integer*4, optional, check(shape(rVariables,1)==n), depend(rVariables), intent(in) :: n=shape(rVariables,1)
  character (len=*), intent(in) :: sFunctionText
  character (len=*), intent(in) :: sVariables
  real (kind=T_SGL), dimension(m,n), intent(in) :: rVariables
  real (kind=T_SGL), dimension(m), intent(out) :: rOut
  integer (kind=T_INT), intent(in) :: m
  integer (kind=T_INT), intent(in) :: n

  ! [ LOCALS ]
  character (len=MAXEQUATIONLENGTH) :: sFuncTxt
  character (len=256) :: sBuf
  character (len=256), dimension(:), allocatable :: sVarTxt
  integer (kind=T_INT), dimension(2) :: iDim
  integer (kind=T_INT) :: iNumFields, i
  character (len=5) :: sStatusFlag

  !
  ! we're sending variable names as one big honking space-delimited string
  ! from Python; now we must parse it in fortran. aaaargh!
  !

  iNumFields = countFields(trim(sVariables))

  allocate(sVarTxt(iNumFields) )

  sFuncTxt = trim(sFunctionText)
  sBuf = trim(sVariables)

  do i=1,iNumFields
    call Chomp(sBuf, sVarTxt(i) )
  enddo

  call init_equation (sFuncTxt, sVarTxt, sStatusflag)

  if(str_compare(sStatusFlag(1:2),"ok") ) then

    write(LU_STD_OUT, "('Initialization of equation parser succeeded; status = ',a)" ) &
      trim(sStatusFlag)

  else

    write(LU_STD_OUT, "('Initialization of equation has failed; status = ',a)" ) &
      trim(sStatusFlag)

  endif

  do i=1,m
    rOut(i) = evaluate_expression (rVariables(i,:) )
  enddo

  call destroyfunc()

  deallocate(sVarTxt)


end subroutine evaluate

!------------------------------------------------------------------------------

subroutine newseriesfromequation(sFunctionText, sSeriesname)

  implicit none
  !f2py character*(*), intent(in) :: sFunctionText
  !f2py character*(*), intent(in) :: sSeriesName

  character (len=*), intent(in) :: sFunctionText
  character (len=*), intent(in) :: sSeriesName

  ! [ LOCALS ]
  character (len=MAXEQUATIONLENGTH) :: sFuncTxt
  character (len=256) :: sNameTxt
  character (len=MAXEQUATIONLENGTH) :: sBuf
  character (len=256), dimension(:), allocatable :: sVarTxt
  logical (kind=T_LOGICAL), dimension(:), allocatable :: lInclude
  logical (kind=T_LOGICAL) :: lConsistentTimebase
  character (len=256), dimension(:), allocatable :: sSeriesNamesTxt
  integer (kind=T_INT) :: iNumFields, i, j
  character (len=5) :: sStatusFlag
  character (len=4096) :: sTimeSeriesList

  type (TIME_SERIES_COLLECTION), target :: TSCOL
  type (T_TIME_SERIES), pointer :: pTS
  real (kind=T_SGL), dimension(:), allocatable :: rTempValue
  integer (kind=T_INT) :: iNumRecords
  integer (kind=T_INT) :: iNumSeries
  character (len=256) :: sPreviousSeriesName
  real (kind=T_SGL), dimension(:), allocatable :: rOut

  sNameTxt = trim(sSeriesName)
  sFuncTxt = ""

  ! first create a list of the current time series objects in memory
  call listseriesnames(sTimeSeriesList)
  iNumFields = countFields(trim(sTimeSeriesList))
  allocate(sSeriesNamesTxt(iNumFields))
  sSeriesNamesTxt = ""
  do i=1,iNumFields
    call Chomp(sTimeSeriesList, sSeriesNamesTxt(i) )
  enddo

  sFuncTxt = trim(sFunctionText)
  sBuf = sFuncTxt
  iNumFields = countFields(trim(sFuncTxt),OPERATORS//" ")
  allocate(sVarTxt(iNumFields), lInclude(iNumFields) )
  lInclude = lFALSE
  lConsistentTimebase = lTRUE

  iNumSeries = 0
  do i=1,iNumFields
    call Chomp(sBuf, sVarTxt(i) , OPERATORS//" ")
    if(isElement(sVarTxt(i), sSeriesNamesTxt)) then
      iNumSeries = iNumSeries + 1
      pTS => TS%getTS( sVarTxt(i) )
      iNumRecords = size(pTS%tData)
      call TSCOL%add( pTS )
      lInclude(i) = lTRUE
      if(iNumSeries>1) then
        call datestampsequal(sPreviousSeriesName, sVarTxt(i), lConsistentTimebase )
        if(.not. lConsistentTimebase) exit
      endif
      sPreviousSeriesName = sVarTxt(i)
    endif
  enddo

  do

    if(lConsistentTimebase .and. iNumSeries > 0) then

      call init_equation (sFuncTxt, pack(sVarTxt, lInclude), sStatusflag)

      if(str_compare(sStatusFlag(1:2),"ok") ) then

        write(LU_STD_OUT, "('Initialization of equation parser succeeded; status = ',a)" ) &
          trim(sStatusFlag)

      else

        write(LU_STD_OUT, "('Initialization of equation has failed; status = ',a)" ) &
        trim(sStatusFlag)
        exit

      endif

      allocate(rOut(iNumRecords) )

      allocate(rTempValue(count(lInclude) ) )

      do i=1,iNumRecords
        do j=1,count(lInclude)
          rTempValue(j) = TSCOL%tTS(j)%tData(i)%rValue
        enddo
!      print *, '  IN:', rTempValue
        rOut(i) = evaluate_expression (rTempValue , TSCOL%tTS(1)%tData(i)%tDT )
!      print *, '  OUT:', rOut(i)
      enddo

      call tTS%new( sNameTxt, &
        "Series calculated from the equation '"//trim(sFunctionText)//"'", &
        TSCOL%tTS(1)%tData%tDT, rOut)

      call TS%add(tTS)

    else    ! inconsistent timebase or no time series provided

      write(LU_STD_OUT,fmt="(/,a)") "Problem with equation: '"//trim(sFunctionText)//"'"

      call warn(lConsistentTimebase,"Time series referenced in equation have differing time bases" &
        //" - cannot evaluate equation", trim(__FILE__),__LINE__)

      call warn(iNumSeries > 0,"At least one time series must be referenced in your equation.",&
         trim(__FILE__),__LINE__)

    endif

    exit

  enddo

  deallocate(sVarTxt)
  call destroyfunc()
  call TSCOL%clear()

end subroutine newseriesfromequation

!------------------------------------------------------------------------------

function tolower(sString)          result(sLowercase)

  implicit none
  !f2py character (len=*), intent(in) :: sString
  !f2py character (len=256 ), intent(out) :: sLowercase
  character (len=*), intent(in) :: sString
  character (len=256 ) :: sLowercase

  sLowercase = lowercase(sString)

end function tolower

!------------------------------------------------------------------------------

subroutine newblock(sBlockname, sKeywords, sArgs)

  character (len=*), intent(in)                                :: sBlockname
  character (len=*)                                            :: sKeywords
  character (len=*)                                            :: sArgs

  ! [ LOCALS ]
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sKeyword
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sArg1
  character (len=256) :: sBuf
  integer (kind=T_INT) :: iNumKeywords, iNumArgs, i
  character (len=5) :: sStatusFlag

  !
  ! we're sending keywords and args as one big honking space-delimited string
  ! from Python; now we must parse it in fortran. aaaargh!
  !
  iNumKeywords = countFields(trim(sKeywords))
  iNumArgs = countFields(trim(sArgs))


  allocate(sKeyword(iNumKeywords) )
  allocate(sArg1(iNumArgs) )

  do i=1,iNumKeywords
    call Chomp( sKeywords, sBuf )
    sKeyword(i) = trim(sBuf)
  enddo

  do i=1,iNumArgs
    call Chomp( sArgs, sBuf )
    sArg1(i) = trim(sBuf)
  enddo

  if(iNumArgs == iNumKeywords) then

    if(associated(pBlock)) then
      call pBlock%deallocate()
      deallocate(pBlock)
      allocate(pBlock)
    else
      allocate(pBlock)
    endif

    call pBlock%new(sBlockname, sKeyword, sArg1)

  else

    call warn(lFALSE,"Unequal number of keywords and arguments supplied.")

  endif

  deallocate(sArg1, sKeyword)

end subroutine newblock

!------------------------------------------------------------------------------

subroutine period_statistics()

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pBaseTS
    type (T_TIME_SERIES), pointer, dimension(:) :: pStatSeries
    integer (kind=T_INT) :: i
    character (len=MAXARGLENGTH), dimension(:), pointer :: pArgs
    character (len=MAXARGLENGTH) :: sTempSeriesname



    if(str_compare(pBlock%sBlockName, "PERIOD_STATISTICS")) then

      pArgs =>pBlock%getString("SERIES_NAME")
      sTempSeriesname = pArgs(1)
      pBaseTS => TS%getTS( sTempSeriesname )
      pStatSeries => pBaseTS%calcPeriodStatistics( pBlock)

    else

      call Assert(lFALSE, "Unhandled case in routine period_statistics", &
        trim(__FILE__), __LINE__)

    endif

    do i=1,size(pStatSeries)

      call TS%add(pStatSeries(i))

    enddo

    nullify(pBaseTS)

end subroutine period_statistics

!------------------------------------------------------------------------------

subroutine addtoblock(sKeywords, sArgs)

  character (len=*)                                            :: sKeywords
  character (len=*)                                            :: sArgs

  ! [ LOCALS ]
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sKeyword
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sArg1
  character (len=256) :: sBuf
  integer (kind=T_INT) :: iNumKeywords, iNumArgs, i
  character (len=5) :: sStatusFlag

  !
  ! we're sending keywords and args as one big honking space-delimited string
  ! from Python; now we must parse it in fortran. aaaargh!
  !
  iNumKeywords = countFields(trim(sKeywords))
  iNumArgs = countFields(trim(sArgs))


  allocate(sKeyword(iNumKeywords) )
  allocate(sArg1(iNumArgs) )

  do i=1,iNumKeywords
    call Chomp( sKeywords, sBuf )
    sKeyword(i) = trim(sBuf)
  enddo

  do i=1,iNumArgs
    call Chomp( sArgs, sBuf )
    sArg1(i) = trim(sBuf)
  enddo

  if(iNumArgs == iNumKeywords) then

    call pBlock%add(sKeyword, sArg1)

  else

    call warn(lFALSE,"Unequal number of keywords and arguments supplied.")

  endif

  deallocate(sArg1, sKeyword)

end subroutine addtoblock

!------------------------------------------------------------------------------

subroutine listblock()

  call pBlock%printDict()

end subroutine listblock

end module tsp_main_loop
