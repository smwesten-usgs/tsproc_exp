module tsp_main_loop

  use tsp_data_structures
  use tsp_utilities
  use tsp_control_file_ops
  use tsp_file_readers
  use tsp_time_series_manager
  use tsp_table_manager
  use tsp_collections
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

  type (T_DATETIME) :: tDT, tDT2
  integer (kind=T_INT), dimension(:), allocatable :: iMonth
  integer (kind=T_INT), dimension(:), allocatable :: iDay
  integer (kind=T_INT), dimension(:), allocatable :: iYear
  integer (kind=T_INT), dimension(:), allocatable :: iHour
  integer (kind=T_INT), dimension(:), allocatable :: iMinute
  integer (kind=T_INT), dimension(:), allocatable :: iSecond
  integer (kind=T_INT), dimension(:), allocatable :: iJulianDay
  real (kind=T_SGL), dimension(:), allocatable :: rFractionOfDay
  real (kind=T_SGL), dimension(:), allocatable :: rValue

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

  subroutine opencontrolfile(sFilename)

    !f2py intent(in) :: sFilename
    character(len=*), intent(in) :: sFilename

    call INFILE%open(trim(sFilename))
    call openlog()

  end subroutine opencontrolfile

!------------------------------------------------------------------------------

  subroutine closeControlFile()

    call INFILE%close()
    call closelog()

  end subroutine closecontrolfile

!------------------------------------------------------------------------------

  subroutine newtimeseries(sSeriesName, sDescription, &
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

  end subroutine newtimeseries

!------------------------------------------------------------------------------
!> @brief This subroutine copies time series data OUT of the fortran
!>        derived type into simple fortran data types in the form of
!>        MODULE variables. These are exposed to python via F2PY.
  subroutine gettimeseries(sSeriesName)

!f2py character*(*), intent(in) :: sSeriesName

    character(len=*), intent(in) :: sSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    integer (kind=T_INT) :: n

    pTS =>TS%getTS(sSeriesName)

    n = size(pTS%pData)
    if(allocated(iJulianDay)) deallocate(iJulianDay)
    if(allocated(iMonth)) deallocate(iMonth)
    if(allocated(iDay)) deallocate(iDay)
    if(allocated(iYear)) deallocate(iYear)
    if(allocated(iHour)) deallocate(iHour)
    if(allocated(iMinute)) deallocate(iMinute)
    if(allocated(iSecond)) deallocate(iSecond)
    if(allocated(rFractionOfDay)) deallocate(rFractionOfDay)
    if(allocated(rValue)) deallocate(rValue)

    allocate(iJulianDay(n)); allocate(iMonth(n)); allocate(iDay(n))
    allocate(iYear(n)); allocate(iHour(n)); allocate(iMinute(n))
    allocate(iSecond(n)); allocate(rFractionOfDay(n)); allocate(rValue(n))

    iJulianDay = pTS%pData%tDT%iJulianDay
    iMonth = pTS%pData%tDT%iMonth
    iDay = pTS%pData%tDT%iDay
    iYear = pTS%pData%tDT%iYear
    iHour = pTS%pData%tDT%iHour
    iMinute = pTS%pData%tDT%iMinute
    iSecond = pTS%pData%tDT%iSecond
    rFractionOfDay = pTS%pData%tDT%rFractionOfDay
    rValue = pTS%pData%rValue

    nullify(pTS)

  end subroutine gettimeseries

!------------------------------------------------------------------------------

  subroutine removetable(sSeriesName)

    character(len=*), intent(in) :: sSeriesName

    call TS%removeTable(sSeriesName)

  end subroutine removetable

!------------------------------------------------------------------------------

  subroutine removetimeseries(sSeriesName)

    character(len=*), intent(in) :: sSeriesName

    call TS%removeTS(sSeriesName)

  end subroutine removetimeseries

!------------------------------------------------------------------------------

  subroutine listtimeseries(sSeriesname, sDateFormat)

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


  end subroutine listtimeseries

!------------------------------------------------------------------------------

  subroutine listtable(sSeriesname)

    character(len=*), intent(in) :: sSeriesName

    ! [ LOCALS ]

    call TS%listTable(sSeriesName)

  end subroutine listtable

!------------------------------------------------------------------------------

  subroutine describetimeseries(sSeriesname)

    character(len=*), intent(in) :: sSeriesName

    call TS%describe(sSeriesName)

  end subroutine describetimeseries

!------------------------------------------------------------------------------

  subroutine calcseriesstatistics(sSeriesname)

    character(len=*), intent(in) :: sSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS
    type (T_TABLE) :: tTable
    integer (kind=T_INT) :: n

    pTS =>TS%getTS(sSeriesName)

    ! if the block type is "SERIES_STATISTICS", call the routine along
    ! with the block object
    if(str_compare(pBlock%sBlockName, "SERIES_STATISTICS")) then

      call tTable%calc_s_table(pTS, pBlock)

    else

      ! don't pass along the block object; use defaults
      call tTable%calc_s_table(pTS)

    endif

    call TS%add(tTable)
    nullify(pTS)

  end subroutine calcseriesstatistics

!------------------------------------------------------------------------------

  subroutine summarizeseries()

    call TS%summarize()

  end subroutine summarizeseries

!------------------------------------------------------------------------------

  subroutine addtimeseries()

    call TS%add(tTS)

  end subroutine addtimeseries

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

!      print *, INFILE%sActiveContext
!      print *, INFILE%sDateFormat

!      call tDT%parseDate("19660722", "YYYYMMDD")
!      call tDT%parseTime("1347", "HHMM")
!      call tDT%calcJulianDay()

!      call tDT%writelog()

!      call tDT2%parseDate("20100722", "YYYYMMDD")
!      call tDT2%parseTime("0347", "HHMM")
!      call tDT2%calcJulianDay()
!      call tDT2%writelog()

!      print *, "Difference: ", tDT2 - tDT


    elseif(str_compare(pBlock%sBlockname,"GET_MUL_SERIES_NWIS")) then

      call read_USGS_NWIS(pBlock, TS)

    elseif(str_compare(pBlock%sBlockname,"GET_MUL_SERIES_SSF")) then

      call get_mul_series_ssf(pBlock, TS)

    elseif(str_compare(pBlock%sBlockname,"INACTIVE")) then

    elseif(str_compare(pBlock%sBlockname,"EOF")) then
      iReturnCode = 0
    endif

end subroutine continueProcessing

!------------------------------------------------------------------------------

subroutine finalizeRun()

  call TS%summarize()

  call TS%removeTS("05357225")

  call TS%summarize()

  call closelog()

end subroutine finalizeRun

end module tsp_main_loop
