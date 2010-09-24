module tsp_collections

  use tsp_data_structures
  use tsp_utilities
  use tsp_time_series_manager
  use tsp_table_manager
  use tsp_datetime_class
  use tsp_equations_interpreter

  implicit none

  type T_TS_COMPARISON
    character (len=MAXNAMELENGTH) :: sObservedSeries = ""
    character (len=MAXNAMELENGTH) :: sModeledSeries = ""
    character (len=MAXNAMELENGTH) :: sObservationGroup = ""
    character (len=1024) :: sWeightsEquation
    real (kind=T_SGL), dimension(:), allocatable :: rWeightValue
  end type T_TS_COMPARISON

  type T_TABLE_COMPARISON
    character (len=MAXNAMELENGTH) :: sObservedTable = ""
    character (len=MAXNAMELENGTH) :: sModeledTable = ""
    character (len=MAXNAMELENGTH) :: sObservationGroup = ""
    character (len=1024) :: sWeightsEquation
    real (kind=T_SGL), dimension(:), allocatable :: rWeightValue
  end type T_TABLE_COMPARISON

  type TIME_SERIES_COLLECTION
    integer (kind=T_INT) :: iNumTimeSeries  = 0
    integer (kind=T_INT) :: iNumTables      = 0

    type (T_TIME_SERIES), dimension(:), allocatable :: tTS
    type (T_TS_COMPARISON), dimension(:), allocatable :: tTSComparison
    type (T_TABLE), dimension(:), allocatable :: tTable
    type (T_TABLE_COMPARISON), dimension(:), allocatable :: tTableComparison

  contains

    procedure :: add_ts_sub, add_table_sub
    procedure :: tsCompare => add_ts_comparison_sub
    procedure :: tableCompare => add_table_comparison_sub
    generic :: add => add_ts_sub, add_table_sub
    procedure :: clear => remove_all_sub
    procedure :: calculate => calc_values_from_equation_fn
    procedure :: newTimeBase => conform_ts_sub
    procedure :: removeTS => remove_ts_sub
    procedure :: removeTable => remove_table_sub
    procedure :: summarize => summarize_sub
    procedure :: describe => describe_ts_sub
    procedure :: getTS => get_ts_pointer_fn
    procedure :: getTSComparison => get_ts_comparison_pointer_fn
    procedure :: getTableComparison => get_table_comparison_pointer_fn
    procedure :: getTable => get_table_pointer_fn
    procedure :: listTS => list_output_ts_sub
    procedure :: pestWriteTSComparison => pest_write_ts_comparison_sub
    procedure :: pestWriteTableComparison => pest_write_table_comparison_sub
    procedure :: listTable => list_output_table_sub
    procedure :: datesEqual => are_datetime_stamps_identical_fn

  end type TIME_SERIES_COLLECTION


contains

  subroutine add_ts_sub(this, tTS)

    ! add tTS to the COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TIME_SERIES) ::  tTS

    ! [ LOCALS ]
    type(T_TIME_SERIES), dimension(:), allocatable :: tTempTS
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    if(allocated(this%tTS)) then

      ! get the number of time series currently in time series container
      iCount = size(this%tTS)

      ! check to see whether this name has been used already
      do i = 1,iCount
        call Assert(.not. str_compare(this%tTS(i)%sSeriesName, tTS%sSeriesName), &
         "Series name "//trim(tTS%sSeriesName)//" has already been used", &
         trim(__FILE__), __LINE__)
      end do

      ! allocate memory for size of current TS
      allocate(tTempTS(iCount),stat=iStat)
      call Assert(iStat==0, "Unable to allocate temporary memory for time series", &
        TRIM(__FILE__), __LINE__)

      ! make a copy of all previous TS objects
      tTempTS = this%tTS

      ! deallocate TS objects collection
      deallocate(this%tTS, stat=iStat)
      call Assert(iStat==0, "Unable to deallocate memory for time series", &
        TRIM(__FILE__), __LINE__)

      ! allocate TS objects collection to include room for new object
      allocate(this%tTS(iCount + 1), stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for time series", &
        TRIM(__FILE__), __LINE__)

      this%tTS(1:iCount) = tTempTS

      ! ensure that the minimum and maximum date range fields are populated
      ! before adding the TS object to the collection
!      call tTS%findDateMinAndMax()

      this%tTS(iCount+1) = tTS
      this%tTS(iCount+1)%tData = tTS%tData

!      deallocate(this%tTS, stat=iStat)
!      call Assert(iStat==0, "Unable to deallocate memory for time series", &
!        TRIM(__FILE__), __LINE__)

!      this%tTS => tTempTS

    else

      allocate(this%tTS(1),stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for time series", &
        TRIM(__FILE__), __LINE__)

      ! ensure that the minimum and maximum date range fields are populated
      ! before adding the TS object to the collection
!      call tTS%findDateMinAndMax()

      this%tTS(1) = tTS

      if(allocated(this%tTS(1)%tData)) deallocate(this%tTS(1)%tData)
      allocate(this%tTS(1)%tData(size(tTS%tData)))

      this%tTS(1)%tData = tTS%tData

    endif

    this%iNumTimeSeries = this%iNumTimeSeries + 1

  end subroutine add_ts_sub

!------------------------------------------------------------------------------

  subroutine add_table_sub(this, tTable)

    ! add tTS to the COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TABLE) ::  tTable

    ! [ LOCALS ]
    type(T_TABLE), dimension(:), allocatable :: tTempTable
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    if(allocated(this%tTable)) then

      ! get the number of tables currently in time series container
      iCount = size(this%tTable)

      ! check to see whether this name has been used already
      do i = 1,iCount
        call Assert(.not. str_compare(this%tTable(i)%sSeriesName, tTable%sSeriesName), &
         "Table name "//trim(tTable%sSeriesName)//" has already been used", &
         trim(__FILE__), __LINE__)
      end do

      ! allocate memory for size of current table
      allocate(tTempTable(iCount),stat=iStat)
      call Assert(iStat==0, "Unable to allocate temporary memory for table", &
        TRIM(__FILE__), __LINE__)

      ! make a copy of all previous table objects
      tTempTable = this%tTable

      ! deallocate table objects collection
      deallocate(this%tTable, stat=iStat)
      call Assert(iStat==0, "Unable to deallocate memory for table", &
        TRIM(__FILE__), __LINE__)

      ! allocate table objects collection to include room for new object
      allocate(this%tTable(iCount + 1), stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for table", &
        TRIM(__FILE__), __LINE__)

      this%tTable(1:iCount) = tTempTable

      this%tTable(iCount+1) = tTable

    else

      allocate(this%tTable(1),stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for table", &
        TRIM(__FILE__), __LINE__)

      this%tTable(1) = tTable

      if(allocated(this%tTable(1)%tTableData)) deallocate(this%tTable(1)%tTableData)
      allocate(this%tTable(1)%tTableData(size(tTable%tTableData)))

      this%tTable(1)%tTableData = tTable%tTableData

    endif

    this%iNumTables = this%iNumTables + 1

  end subroutine add_table_sub

!------------------------------------------------------------------------------

  subroutine remove_ts_sub(this, sSeriesName)

    ! remove tTS from a COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TIME_SERIES), dimension(:), allocatable :: pTempTS
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTS)) then

      ! get the number of time series currently in time series container
      iCount = size(this%tTS)

!      print *, "iCount: ", iCount

      ! check to see whether a time series with this name exists
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTS(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call warn(lMatch, "No time series with the name "//quote(sSeriesName)// &
         " could be found")
      if(.not. lMatch) return

      ! allocate memory for size of TS collection MINUS one time series
      allocate(pTempTS(iCount-1),stat=iStat)
      call Assert(iStat==0, "Unable to allocate temporary memory for time series", &
        TRIM(__FILE__), __LINE__)

      j = 0
      ! make a copy of objects we wish to keep
      do i = 1,iCount
!        print *, "#:",i
        if(str_compare(this%tTS(i)%sSeriesName, sSeriesName)) cycle
        j = j + 1
        pTempTS(j) = this%tTS(i)
!        print *, i, "keeping "//trim(this%tTS(i)%sSeriesname)
      enddo

      ! deallocate TS objects collection
      do i=1,size(this%tTS)
        deallocate( this%tTS(i)%tData, stat=iStat )
      enddo
      call Assert(iStat==0, "Unable to deallocate memory for time series data", &
        TRIM(__FILE__), __LINE__)

      deallocate(this%tTS, stat=iStat)
      call Assert(iStat==0, "Unable to deallocate memory for time series", &
        TRIM(__FILE__), __LINE__)


      ! allocate TS objects collection to include room for new object
      allocate(this%tTS(iCount - 1), stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for time series", &
        TRIM(__FILE__), __LINE__)

      this%tTS = pTempTS
      this%iNumTimeSeries = this%iNumTimeSeries - 1


!      deallocate(this%tTS, stat=iStat)
!      call Assert(iStat==0, "Unable to deallocate memory for time series", &
!        TRIM(__FILE__), __LINE__)

!      this%tTS => pTempTS

      call writelog("  <deleted time series "//quote(sSeriesName)//">")

    else

      call warn(lFALSE, "There are no time series objects available for deletion")

    endif

    deallocate(pTempTS, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory for time series", &
        TRIM(__FILE__), __LINE__)

  end subroutine remove_ts_sub

!------------------------------------------------------------------------------

  subroutine remove_table_sub(this, sSeriesName)

    ! remove tTable from a COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TABLE), dimension(:), allocatable :: pTempTable
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTable)) then

      ! get the number of time series currently in time series container
      iCount = size(this%tTable)

      ! check to see whether this name exists in the collection
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTable(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call warn(lMatch, "No table with the name "//quote(sSeriesName)// &
         " could be found")
      if(.not. lMatch) return

      if(iCount > 1) then
        ! allocate memory for size of TABLE collection MINUS one table
        allocate(pTempTable(iCount-1),stat=iStat)
        call Assert(iStat==0, "Unable to allocate temporary memory for table", &
          TRIM(__FILE__), __LINE__)

        j = 0
        ! make a copy of objects we wish to keep
        do i = 1,iCount
          if(str_compare(this%tTable(i)%sSeriesName, sSeriesName)) cycle
          j = j + 1
          pTempTable(j) = this%tTable(i)
        enddo

        ! deallocate Table objects collection
        deallocate(this%tTable, stat=iStat)
        call Assert(iStat==0, "Unable to deallocate memory for time series", &
          TRIM(__FILE__), __LINE__)

        ! allocate Table objects collection to include room for new object
        allocate(this%tTable(iCount - 1), stat=iStat)
        call Assert(iStat==0, "Unable to allocate memory for table collection", &
          TRIM(__FILE__), __LINE__)

        this%tTable = pTempTable
        this%iNumTables = this%iNumTables - 1

      else ! only one table exists; delete it...nothing else to copy

        ! deallocate Table objects collection
        deallocate(this%tTable, stat=iStat)
        call Assert(iStat==0, "Unable to deallocate memory for time series", &
          TRIM(__FILE__), __LINE__)

      endif

      call writelog("  <deleted table "//quote(sSeriesName)//">")

    else

      call warn(lFALSE, "There are no table objects available for deletion")

    endif

    if(allocated(pTempTable)) deallocate(pTempTable, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory for temporary table objects", &
        TRIM(__FILE__), __LINE__)

  end subroutine remove_table_sub

!------------------------------------------------------------------------------

  subroutine remove_all_sub(this)

    class (TIME_SERIES_COLLECTION) :: this

    ! [ LOCALS ]
    integer (kind=T_INT) :: iSize     ! holds size of subobject arrays
    integer (kind=T_INT) :: iDataSize ! holds size of associated data structures
    integer (kind=T_INT) :: i         ! loop counter

    if(allocated(this%tTS)) then
      iSize = size(this%tTS)
      do i=1,iSize
        if(allocated(this%tTS(i)%tData)) deallocate(this%tTS(i)%tData)
      enddo
      deallocate(this%tTS)
    endif

    if(allocated(this%tTable)) then
      iSize = size(this%tTable)
      do i=1,iSize
        if(allocated(this%tTable(i)%tTableData)) deallocate(this%tTable(i)%tTableData)
      enddo
      deallocate(this%tTable)
    endif

  end subroutine remove_all_sub

!------------------------------------------------------------------------------

  function get_ts_pointer_fn(this, sSeriesName)    result( pTS )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TIME_SERIES), pointer :: pTS

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTS)) then

      ! get the number of time series currently in time series container
      iCount = size(this%tTS)

      ! check to see whether this name has been used already
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTS(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No time series with the name "//trim(sSeriesName)// &
         " could be found",trim(__FILE__),__LINE__)

      pTS => this%tTS(i)

    endif

  end function get_ts_pointer_fn

!------------------------------------------------------------------------------

  function get_ts_comparison_pointer_fn(this, sObservedSeries, sModeledSeries)    result( pTSComparison )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedSeries
    character (len=*) :: sModeledSeries
    type(T_TS_COMPARISON), pointer :: pTSComparison

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTSComparison)) then

      ! get the number of time series currently in time series comparison container
      iCount = size(this%tTSComparison)

      ! check to see whether these series have been compared already
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTSComparison(i)%sObservedSeries, sObservedSeries) &
            .and. str_compare(this%tTSComparison(i)%sModeledSeries, sModeledSeries) ) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No time series comparison between the series "//trim(sObservedSeries)// &
         " and "//trim(sModeledSeries)//" could be found",trim(__FILE__),__LINE__)

      pTSComparison => this%tTSComparison(i)

    endif

  end function get_ts_comparison_pointer_fn

!------------------------------------------------------------------------------

  function get_table_comparison_pointer_fn(this, sObservedTable, sModeledTable) &
                                                            result( pTableComparison )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedTable
    character (len=*) :: sModeledTable
    type(T_TABLE_COMPARISON), pointer :: pTableComparison

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTableComparison)) then

      ! get the number of tables currently in table comparison container
      iCount = size(this%tTableComparison)

      ! check to see whether these tables have been compared already
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTableComparison(i)%sObservedTable, sObservedTable) &
            .and. str_compare(this%tTableComparison(i)%sModeledTable, sModeledTable) ) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No table comparisons between the series "//trim(sObservedTable)// &
         " and "//trim(sModeledTable)//" could be found",trim(__FILE__),__LINE__)

      pTableComparison => this%tTableComparison(i)

    endif

  end function get_table_comparison_pointer_fn

!------------------------------------------------------------------------------

  function get_table_pointer_fn(this, sSeriesName)    result( pTable )

    ! get values associated with tTable from a COLLECTION of table objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TABLE), pointer :: pTable

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(allocated(this%tTable)) then

      ! get the number of table entries currently in the table collection
      iCount = size(this%tTable)

      ! find the matching table entry
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%tTable(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No table with the name "//trim(sSeriesName)// &
         " could be found",trim(__FILE__),__LINE__)

      pTable => this%tTable(i)

    endif

  end function get_table_pointer_fn

!------------------------------------------------------------------------------

  subroutine summarize_sub(this)

    class(TIME_SERIES_COLLECTION) :: this

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: i, j
    integer (kind=T_INT) :: iMemoryInBytes
    integer (kind=T_INT) :: iSum
    real (kind=T_SGL) :: rSSE
    type (T_TIME_SERIES), pointer :: pObservedSeries, pModeledSeries
    type (T_TABLE), pointer :: pObservedTable, pModeledTable

    write(LU_STD_OUT,fmt="(/,/,a,/)") '*** TSPROC TIME SERIES and TABLE OBJECTS CURRENTLY IN MEMORY ***'
    write(LU_STD_OUT, &
      fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
           t80,'MAX',t86,'MEMORY(kb)',/)")

    if(.not. allocated(this%tTS)) then
      write(LU_STD_OUT,fmt="(a,/)") '     ===> No TIME SERIES objects currently in memory <==='

    else

      iCount = size(this%tTS)

      iSum = 0
      do i=1,iCount

        if(allocated(this%tTS(i)%tData)) then
          iMemoryInBytes = sizeof(this%tTS(i)) + sizeof(this%tTS(i)%tData)
          write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
            this%tTS(i)%sSeriesName, &
            this%tTS(i)%tStartDate%prettyDate(), &
            this%tTS(i)%tEndDate%prettyDate(), &
            size(this%tTS(i)%tData), &
            MINVAL(this%tTS(i)%tData%rValue), &
            SUM(this%tTS(i)%tData%rValue)/ size(this%tTS(i)%tData), &
            MAXVAL(this%tTS(i)%tData%rValue), &
            iMemoryInBytes / 1024
            iSum = iSum + size(this%tTS(i)%tData)
        else
          iMemoryInBytes = sizeof(this%tTS(i))

          write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
            this%tTS(i)%sSeriesName, iMemoryInBytes / 1024

        endif
      end do

      write(LU_STD_OUT, fmt="(/,a,/)") trim(asChar(iSum))//" total records in memory."

    endif

    if(.not. allocated(this%tTable)) then
      write(LU_STD_OUT,fmt="(/,a,/)") '     ===> No TABLE objects currently in memory <==='

    else

      iCount = size(this%tTable)

      write(LU_STD_OUT, &
        fmt="(/,'SERIES_NAME',t24,'DATE RANGE',t45,'TABLE TYPE',/)")


      do i=1,iCount

        if(allocated(this%tTable(i)%tTableData)) then
          iMemoryInBytes = sizeof(this%tTable(i)) + sizeof(this%tTable(i)%tTableData)
          write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
            this%tTable(i)%sSeriesName, &
            this%tTable(i)%tStartDate%prettyDate(), &
            this%tTable(i)%tEndDate%prettyDate(), &
            TABLE_TYPE(this%tTable(i)%iTableType)

        else


        endif
      end do
    endif

    write(LU_STD_OUT,fmt="(/,/,a,/)") '*** TSPROC TIME SERIES and TABLE COMPARISON OBJECTS CURRENTLY IN MEMORY ***'
    write(LU_STD_OUT, &
      fmt="('OBSERVED SERIES    MODELED SERIES          DATE RANGE    COUNT    RESIDUAL')")

    if(.not. allocated(this%tTSComparison)) then
      write(LU_STD_OUT,fmt="(a,/)") &
        '     ===> No TIME SERIES COMPARISON objects currently in memory <==='

    else

      iCount = size(this%tTSComparison)

      do i=1,iCount

        pObservedSeries => this%getTS(this%tTSComparison(i)%sObservedSeries)
        pModeledSeries => this%getTS(this%tTSComparison(i)%sModeledSeries)
        rSSE = 0.

        do j=1,size(pObservedSeries%tData%rValue)

          rSSE = rSSE + ((pObservedSeries%tData(j)%rValue &
                 - pModeledSeries%tData(j)%rValue ) **2 &
                 * this%tTSComparison(i)%rWeightValue(j)**2)

        enddo

        write(LU_STD_OUT,fmt="(a18,1x,a18,1x,a10,'-',a10, i10, g16.8)") &
          this%tTSComparison(i)%sObservedSeries, &
          this%tTSComparison(i)%sModeledSeries, &
          pObservedSeries%tStartDate%prettyDate(), &
          pObservedSeries%tEndDate%prettyDate(), &
          size(pObservedSeries%tData), rSSE

      end do
    endif

    write(LU_STD_OUT, &
      fmt="('OBSERVED TABLE    MODELED TABLE          DATE RANGE    COUNT    RESIDUAL')")
    if(.not. allocated(this%tTableComparison)) then
      write(LU_STD_OUT,fmt="(a,/)") &
        '     ===> No TABLE COMPARISON objects currently in memory <==='

    else

      iCount = size(this%tTableComparison)

      do i=1,iCount

        pObservedTable => this%getTable(this%tTableComparison(i)%sObservedTable)
        pModeledTable => this%getTable(this%tTableComparison(i)%sModeledTable)
        rSSE = 0.

        do j=1,size(pObservedTable%tTableData%sValue)


          rSSE = rSSE + (( asReal(pObservedTable%tTableData(j)%sValue) &
                 - asReal(pModeledTable%tTableData(j)%sValue) ) **2 &
                 * this%tTableComparison(i)%rWeightValue(j)**2)

        enddo

        write(LU_STD_OUT,fmt="(a18,1x,a18,1x,a10,'-',a10, i10, g16.8)") &
          this%tTableComparison(i)%sObservedTable, &
          this%tTableComparison(i)%sModeledTable, &
          pObservedTable%tStartDate%prettyDate(), &
          pObservedTable%tEndDate%prettyDate(), &
          size(pObservedTable%tTableData), rSSE

      end do
    endif


  end subroutine summarize_sub

!------------------------------------------------------------------------------

  subroutine describe_ts_sub(this,sSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pTS
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iMemoryInBytes

    pTS => this%getTS(sSeriesName)

    write(LU_STD_OUT,fmt="(a)") trim(pTS%sDescription)

    if(allocated(pTS%tData)) then
    write(LU_STD_OUT, &
      fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
           t80,'MAX',t86,'MEMORY(kb)')")
      iMemoryInBytes = sizeof(pTS) + sizeof(pTS%tData)
      write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
        pTS%sSeriesName, &
        pTS%tStartDate%prettyDate(), &
        pTS%tEndDate%prettyDate(), &
        size(pTS%tData), &
        MINVAL(pTS%tData%rValue), &
        SUM(pTS%tData%rValue)/ size(pTS%tData), &
        MAXVAL(pTS%tData%rValue), &
        iMemoryInBytes / 1024

    else
      iMemoryInBytes = sizeof(pTS)
       write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
        pTS%sSeriesName, iMemoryInBytes / 1024

    endif


  end subroutine describe_ts_sub

!------------------------------------------------------------------------------
  subroutine pest_write_ts_comparison_sub(this, sObservedSeries, sModeledSeries, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedSeries
    character (len=*) :: sModeledSeries
    integer (kind=T_INT), optional :: iLU

    ! [ LOCALS ]
    type(T_TS_COMPARISON), pointer :: pTSComparison
    type(T_TIME_SERIES), pointer :: pObservedSeries
    integer (kind=T_INT) :: LU
    character (len=256) :: sFormatString
    integer (kind=T_INT) :: i

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    pTSComparison => this%getTSComparison(sObservedSeries, sModeledSeries)
    pObservedSeries => this%getTS(sObservedSeries)

    sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,g16.8,3x,g16.8,3x,a)"

    ! add observation numbers to the end of the observation name
    do i=1,size(pObservedSeries%tData)

       write(LU,fmt=trim(sFormatString)) &
          trim(pObservedSeries%sSeriesName)//"_"//trim(asChar(i) ), &
          pObservedSeries%tData(i)%rValue, pTSComparison%rWeightValue(i), &
          pTSComparison%sObservedSeries

    enddo

    nullify(pTSComparison, pObservedSeries)

  end subroutine pest_write_ts_comparison_sub

!------------------------------------------------------------------------------

  subroutine list_output_ts_sub(this, sSeriesName, sDateFormat, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in), optional :: sDateFormat
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pTS
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

    pTS => this%getTS(sSeriesName)

    ! now that we have a pointer to a specific time series, call the
    ! type-bound method for that time series to do the actual output
    call pTS%list(sDateFmt, LU)

  end subroutine list_output_ts_sub

!------------------------------------------------------------------------------

  subroutine list_output_table_sub(this, sSeriesName, sDateFormat, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in), optional :: sDateFormat
    integer (kind=T_INT), intent(in), optional :: iLU

    ! [ LOCALS ]
    type(T_TABLE), pointer :: pTable
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

    pTable => this%getTable(sSeriesName)

    call pTable%list(sDateFormat, iLU)

    nullify(pTable)

  end subroutine list_output_table_sub


!------------------------------------------------------------------------------

  subroutine pest_write_table_comparison_sub(this, sObservedSeries, sModeledSeries, iLU)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedSeries
    character (len=*) :: sModeledSeries
    integer (kind=T_INT), optional :: iLU

    ! [ LOCALS ]
    type(T_TABLE_COMPARISON), pointer :: pTableComparison
    type(T_TABLE), pointer :: pTable
    integer (kind=T_INT) :: LU
    character (len=256) :: sFormatString
    integer (kind=T_INT) :: i

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    pTableComparison => this%getTableComparison(sObservedSeries, sModeledSeries)
    pTable => this%getTable(sObservedSeries)

    sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,a20,3x,g16.8,3x,a)"

    ! add observation numbers to the end of the observation name
    do i=1,size(pTable%tTableData)

       write(LU,fmt=trim(sFormatString)) &
          trim(pTable%sSeriesName)//"_"//trim(asChar(i) ), &
          pTable%tTableData(i)%sValue, pTableComparison%rWeightValue(i), &
          trim(pTable%sSeriesName)

    enddo

    nullify(pTableComparison, pTable)

  end subroutine pest_write_table_comparison_sub

!------------------------------------------------------------------------------

  subroutine conform_ts_sub(this, sSeriesname, sTimeBaseName, sNewSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName
    character(len=*), intent(in) :: sTimeBaseName
    character(len=*), intent(in), optional :: sNewSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS, pTimeBaseTS
    type (T_TIME_SERIES) :: tNewSeries

    real (kind=T_SGL), dimension(:), allocatable :: rX1, rX2, rY1, rY2
    real (kind=T_DBL) :: rOffset

   character (len=256) :: sNewName

   ! create a new series name if one is not provided
   if(present(sNewSeriesName)) then
     sNewName = trim(sNewSeriesName)
   else
     sNewName = trim(sSeriesname)//"_TB"
   endif

    ! get pointers to the two series involved
    pTS => this%getTS(sSeriesname)
    pTimeBaseTS => this%getTS(sTimeBaseName)

    ! perform sanity checks on date boundaries
    call Assert(pTS%tStartDate <= pTimeBaseTS%tStartDate, &
       "Series "//trim(sSeriesname)//" must start before the series (" &
       //trim(sTimeBaseName)//") used to define the new time base", &
       trim(__FILE__), __LINE__)
    call Assert(pTS%tEndDate >= pTimeBaseTS%tEndDate, &
       "Series "//trim(sSeriesname)//" must end after the series (" &
       //trim(sTimeBaseName)//") used to define the new time base", &
       trim(__FILE__), __LINE__)

    allocate(tNewSeries%tData(size(pTimeBaseTS%tData)))

    tNewSeries%sSeriesname = sNewName
    tNewSeries%sDescription = "Data from series "//trim(pTS%sSeriesName)//", " &
      //"interpolated to the datetimes found in series "//trim(pTimeBaseTS%sSeriesName)

    allocate(rX1(size(pTS%tData)))
    allocate(rY1(size(pTS%tData)))

    allocate(rX2(size(pTimeBaseTS%tData)))
    allocate(rY2(size(pTimeBaseTS%tData)))

    ! we're subtracting this (large) value so that we can get away with using
    ! single precision real values to represent the datetime values
    rOffset = real(MINVAL(pTS%tData%tDT%iJulianDay), kind=T_DBL)


    rX1 = real( pTS%tData%tDT%iJulianDay, kind=T_DBL ) - rOffset &
           + real(pTS%tData%tDT%rFractionOfDay, kind=T_DBL)
    rY1 = pTS%tData%rValue

    rX2 = real( pTimeBaseTS%tData%tDT%iJulianDay, kind=T_DBL ) - rOffset &
           + real(pTimeBaseTS%tData%tDT%rFractionOfDay, kind=T_DBL)

    call interp_1d( rX1, rY1, rX2, rY2)

    ! copy datetime values from the timebase series to the new series
    tNewSeries%tData%tDT = pTimeBaseTS%tData%tDT
    ! copy the interpolated values to the new series
    tNewSeries%tData%rValue = rY2

    call tNewSeries%findDateMinAndMax()

    ! add new series to collection of series
    call this%add(tNewSeries)

    deallocate(rX1, rY1, rX2, rY2)

  end subroutine conform_ts_sub

!------------------------------------------------------------------------------

  function are_datetime_stamps_identical_fn(this, sSeriesname1, sSeriesname2)   result(lBool)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sSeriesName1
    character(len=*), intent(in) :: sSeriesName2
    logical (kind=T_LOGICAL) :: lBool

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS1, pTS2
    integer (kind=T_INT) :: iCount, iCount1, iCount2, i

    lBool = lTRUE

    ! get pointers to the two series involved
    pTS1 => this%getTS(sSeriesname1)
    pTS2 => this%getTS(sSeriesName2)

    iCount1 = size(pTS1%tData)
    iCount2 = size(pTS2%tData)

    if( iCount1 /= iCount2 ) then
       call warn(lFALSE, &
         "Series "//quote(sSeriesname1)//" does not contain the same number of data elements " &
         //"as "//quote(sSeriesName2) )
       lBool = lFALSE
    endif

    do i=1,iCount
      if(.not. pTS1%tData(i)%tDT == pTS2%tData(i)%tDT) then
        call warn(lFALSE, &
          "Series "//quote(sSeriesname1)//" is not equal to " &
          //quote(sSeriesName2)//"; datetime1: "//trim(pTS1%tData(i)%tDT%listdatetime() ) &
          //"  datetime2: "//trim(pTS2%tData(i)%tDT%listdatetime() ) )
        lBool = lFALSE
        exit
      endif
    enddo

    if(lBool) call echolog("  => Series "//quote(sSeriesname1) &
         //" has datestamps identical to those in "//quote(sSeriesName2)//".")

    nullify(pTS1, pTS2)

  end function are_datetime_stamps_identical_fn

!------------------------------------------------------------------------------

  subroutine add_ts_comparison_sub(this, sObservedSeries, sModeledSeries, sEquationText)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sObservedSeries
    character(len=*), intent(in) :: sModeledSeries
    character(len=*), intent(in) :: sEquationText

    ! [ LOCALS ]
    type(T_TS_COMPARISON), dimension(:), allocatable :: tTempTSComparison
    type (T_TIME_SERIES), pointer :: pObservedSeries, pModeledSeries
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iCountObserved, iCountModeled
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iNumDigits
    integer (kind=T_INT) :: iRecordToReplace
    logical (kind=T_LOGICAL) :: lIsNewComparison

    iCount = 0
    lIsNewComparison = lTRUE

    do

      if(.not. this%datesEqual(sObservedSeries, sModeledSeries) ) then

        call warn(lFALSE, "Cannot create comparison object: "//trim(sObservedSeries) &
          //" and "//trim(sModeledSeries)//" do not have identical timebases.")
        exit

      else  ! timebases for the two series are identical; proceed

        ! get pointers to the two series involved
        pObservedSeries => this%getTS(sObservedSeries)
        pModeledSeries => this%getTS(sModeledSeries)

        iCountObserved = size(pObservedSeries%tData)
        iCountModeled = size(pModeledSeries%tData)

        iNumDigits = len_trim(asChar(iCountObserved) )

        if(len_trim(sObservedSeries) + 1 + iNumDigits > MAXNAMELENGTH) then
          call warn(lFALSE, "Cannot create comparison object: the series name " &
            //quote(sObservedSeries)//" is too long to allow for unique observation names to be created.")
          exit
        endif

        if(.not. allocated(this%tTSComparison)) then

          allocate(this%tTSComparison(1),stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for time series", &
            TRIM(__FILE__), __LINE__)

          this%tTSComparison(iCount + 1)%sObservedSeries = sObservedSeries
          this%tTSComparison(iCount + 1)%sModeledSeries = sModeledSeries
!          this%tTSComparison(1)%pObservedSeries => pObservedSeries
!          this%tTSComparison(1)%pModeledSeries => pModeledSeries
          this%tTSComparison(1)%sWeightsEquation = sEquationText
          allocate(this%tTSComparison(1)%rWeightValue(iCountObserved) , stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for time series", &
              TRIM(__FILE__), __LINE__)
          this%tTSComparison(1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

        else   ! there are already TS comparison objects; add or replace

          ! get the number of time series currently in time series container
          iCount = size(this%tTSComparison)

          ! test to see if this comparison object already exists
          do i=1,iCount
            if( str_compare(this%tTSComparison(i)%sObservedSeries, sObservedSeries) &
              .and. str_compare(this%tTSComparison(i)%sModeledSeries, sModeledSeries) ) then
                lIsNewComparison = lFALSE
                iRecordToReplace = i
                exit
            endif
          enddo

          if( lIsNewComparison ) then
            ! allocate memory for size of current TSComparison object
            allocate(tTempTSComparison(iCount),stat=iStat)
            call Assert(iStat==0, "Unable to allocate temporary memory for time " &
              //"series comparison object", &
              TRIM(__FILE__), __LINE__)

            ! make a copy of all previous TS objects
            tTempTSComparison = this%tTSComparison

            ! deallocate TS comparison objects collection
            deallocate(this%tTSComparison, stat=iStat)
            call Assert(iStat==0, "Unable to deallocate memory for time " &
            //"series comparison object", &
            TRIM(__FILE__), __LINE__)

            ! allocate TS comparison objects collection to include room for new object
            allocate(this%tTSComparison(iCount + 1), stat=iStat)
            call Assert(iStat==0, "Unable to allocate memory for time series comparison object", &
              TRIM(__FILE__), __LINE__)

            this%tTSComparison(1:iCount) = tTempTSComparison

!            this%tTSComparison(iCount + 1)%pObservedSeries => pObservedSeries
!            this%tTSComparison(iCount + 1)%pModeledSeries => pModeledSeries
            this%tTSComparison(iCount + 1)%sObservedSeries = sObservedSeries
            this%tTSComparison(iCount + 1)%sModeledSeries = sModeledSeries
            this%tTSComparison(iCount + 1)%sWeightsEquation = sEquationText
            allocate(this%tTSComparison(iCount + 1)%rWeightValue(iCountObserved) , stat=iStat)
            call Assert(iStat==0, "Unable to allocate memory for time series", &
              TRIM(__FILE__), __LINE__)

            this%tTSComparison(iCount + 1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

          else

            ! if object already exists, just replace the equation text
            this%tTSComparison(iRecordToReplace)%sWeightsEquation = sEquationText
            this%tTSComparison(iRecordToReplace)%rWeightValue = this%calculate(sEquationText, iCountObserved)

          endif  ! lIsNewComparison

        endif  ! tTScomparison .not. allocated

        exit

      endif  ! .not. dates are equal

    enddo


  end subroutine add_ts_comparison_sub

!------------------------------------------------------------------------------

  subroutine add_table_comparison_sub(this, sObservedTable, sModeledTable, sEquationText)

    class(TIME_SERIES_COLLECTION) :: this
    character(len=*), intent(in) :: sObservedTable
    character(len=*), intent(in) :: sModeledTable
    character(len=*), intent(in) :: sEquationText

    ! [ LOCALS ]
    type(T_TABLE_COMPARISON), dimension(:), allocatable :: tTempTableComparison
    type (T_TABLE), pointer :: pObservedTable, pModeledTable
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iCountObserved, iCountModeled
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iNumDigits
    integer (kind=T_INT) :: iRecordToReplace
    logical (kind=T_LOGICAL) :: lIsNewComparison

    ! get pointers to the two series involved
    pObservedTable => this%getTable(sObservedTable)
    pModeledTable => this%getTable(sModeledTable)

    iCountObserved = size(pObservedTable%tTableData)
    iCountModeled = size(pModeledTable%tTableData)
    iCount = 0

    lIsNewComparison = lTRUE

    do

      if( iCountObserved /= iCountModeled) then

        call warn(lFALSE, "Cannot create comparison object: "//trim(sObservedTable) &
          //" and "//trim(sModeledTable)//" do not have the same number of entries.")
        exit

      else  ! the two tables at least have the same number of entries; proceed

        iNumDigits = len_trim(asChar(iCountObserved) )

        if(len_trim(sObservedTable) + 1 + iNumDigits > MAXNAMELENGTH) then
          call warn(lFALSE, "Cannot create comparison object: the table name " &
            //quote(sObservedTable)//" is too long to allow for unique observation names to be created.")
          exit
        endif

        if(.not. allocated(this%tTableComparison)) then

          allocate(this%tTableComparison(1),stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for table comparison", &
            TRIM(__FILE__), __LINE__)

          this%tTableComparison(iCount + 1)%sObservedTable = sObservedTable
          this%tTableComparison(iCount + 1)%sModeledTable = sModeledTable
          this%tTableComparison(1)%sWeightsEquation = sEquationText
          allocate(this%tTableComparison(1)%rWeightValue(iCountObserved) , stat=iStat)
          call Assert(iStat==0, "Unable to allocate memory for table comparison", &
              TRIM(__FILE__), __LINE__)
          this%tTableComparison(1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

        else   ! there are already table comparison objects; add or replace

          ! get the number of table comparisons currently in table comparisons container
          iCount = size(this%tTableComparison)

          ! test to see if this comparison object already exists
          do i=1,iCount
            if( str_compare(this%tTableComparison(i)%sObservedTable, sObservedTable) &
              .and. str_compare(this%tTableComparison(i)%sModeledTable, sModeledTable) ) then
                lIsNewComparison = lFALSE
                iRecordToReplace = i
                exit
            endif
          enddo

          if( lIsNewComparison ) then
            ! allocate memory for size of current TableComparison object
            allocate(tTempTableComparison(iCount),stat=iStat)
            call Assert(iStat==0, "Unable to allocate temporary memory for table " &
              //"comparison object", &
              TRIM(__FILE__), __LINE__)

            ! make a copy of all previous table objects
            tTempTableComparison = this%tTableComparison

            ! deallocate table comparison objects collection
            deallocate(this%tTableComparison, stat=iStat)
            call Assert(iStat==0, "Unable to deallocate memory for table " &
            //"comparison object", &
            TRIM(__FILE__), __LINE__)

            ! allocate table comparison objects collection to include room for new object
            allocate(this%tTableComparison(iCount + 1), stat=iStat)
            call Assert(iStat==0, "Unable to allocate memory for table comparison object", &
              TRIM(__FILE__), __LINE__)

            this%tTableComparison(1:iCount) = tTempTableComparison

            this%tTableComparison(iCount + 1)%sObservedTable = sObservedTable
            this%tTableComparison(iCount + 1)%sModeledTable = sModeledTable
            this%tTableComparison(iCount + 1)%sWeightsEquation = sEquationText
            allocate(this%tTableComparison(iCount + 1)%rWeightValue(iCountObserved) , stat=iStat)
            call Assert(iStat==0, "Unable to allocate memory for table comparison", &
              TRIM(__FILE__), __LINE__)

            this%tTableComparison(iCount + 1)%rWeightValue = this%calculate(sEquationText, iCountObserved)

          else

            ! if object already exists, just replace the equation text
            this%tTableComparison(iRecordToReplace)%sWeightsEquation = sEquationText
            this%tTableComparison(iRecordToReplace)%rWeightValue = this%calculate(sEquationText, iCountObserved)

          endif  ! lIsNewComparison

        endif  ! tTablecomparison .not. allocated

        exit

      endif  ! .not. dates are equal

    enddo

  end subroutine add_table_comparison_sub

!------------------------------------------------------------------------------

function calc_values_from_equation_fn(this,sFunctionText, iNumRecords) result(rOut)

  implicit none

  class(TIME_SERIES_COLLECTION) :: this
  character (len=*), intent(in) :: sFunctionText
  integer (kind=T_INT), intent(in) :: iNumRecords
  real (kind=T_SGL), dimension(:), allocatable :: rOut

  ! [ LOCALS ]
  character (len=MAXEQUATIONLENGTH) :: sFuncTxt
  character (len=4096) :: sBuf
  character (len=256), dimension(:), allocatable :: sVarTxt
  character (len=256), dimension(:), allocatable :: sSeriesTxt
  logical (kind=T_LOGICAL), dimension(:), allocatable :: lInclude
  logical (kind=T_LOGICAL) :: lConsistentTimebase
  integer (kind=T_INT) :: iNumFields, i, j
  character (len=5) :: sStatusFlag

  type (TIME_SERIES_COLLECTION), target :: TSCOL
  type (T_TIME_SERIES), pointer :: pTS
  real (kind=T_SGL), dimension(:), allocatable :: rTempValue
  integer (kind=T_INT) :: iNumSeries
  character (len=256) :: sPreviousSeriesName

  iNumFields = countFields(trim(sFuncTxt),OPERATORS//" ")

  allocate(sVarTxt(iNumFields), lInclude(iNumFields) )

  sFuncTxt = trim(sFunctionText)
  sBuf = sFuncTxt
  lInclude = lFALSE
  lConsistentTimebase = lTRUE

  ! scan equation for time series names; if found, add the time series name to
  ! a collections object and check for timebase consistency
  iNumSeries = 0
  do i=1,iNumFields
    call Chomp(sBuf, sVarTxt(i) , OPERATORS//" ")
!    print *, "|"//sVarTxt(i)//"|"
    if(isElement(sVarTxt(i), this%tTS%sSeriesName)) then
      iNumSeries = iNumSeries + 1
      pTS => this%getTS( sVarTxt(i) )
      call TSCOL%add( pTS )
      lInclude(i) = lTRUE
      if(iNumSeries>1) then
        lConsistentTimebase = this%datesEqual(sPreviousSeriesName, sVarTxt(i) )
        if(.not. lConsistentTimebase) exit
      endif
      sPreviousSeriesName = sVarTxt(i)
    endif
  enddo

  if(lConsistentTimebase) then

    if(iNumSeries > 0) then

      allocate(sSeriesTxt(iNumSeries))
      sSeriesTxt = pack(sVarTxt, lInclude)

!      print *, iNumSeries
!      print *, sSeriesTxt

      call init_equation (sFuncTxt, sSeriesTxt, sStatusflag)

      call warn(str_compare(sStatusflag, "ok"), "Equation was not properly initialized", &
        trim(__FILE__),__LINE__)

      allocate(rOut(iNumRecords) )

      allocate(rTempValue( count(lInclude) ) )

      do i=1,iNumRecords
        do j=1,size(TSCOL%tTS)
          rTempValue(j) = TSCOL%tTS(j)%tData(i)%rValue
        enddo
        rOut(i) = evaluate_expression (rTempValue , TSCOL%tTS(1)%tData(i)%tDT )
      enddo

    else  ! no time series are referenced in equation text; modify calls accordingly

      deallocate(sVarTxt); allocate(sVarTxt(1) )
      sVarTxt = "none"
      call init_equation (sFuncTxt, sVarTxt, sStatusflag)

      call warn(str_compare(sStatusflag, "ok"), "Equation was not properly initialized", &
        trim(__FILE__),__LINE__)

      allocate(rOut(iNumRecords) )

      allocate(rTempValue(1) )
      rTempValue = 0.

      do i=1,iNumRecords
        rOut(i) = evaluate_expression (rTempValue)
      enddo

    endif

    call destroyfunc()

  else

    allocate(rOut(1))
    rOut = -huge(rOut)

    call warn(lFALSE,"Time series referenced in equation have differing time bases" &
      //" - cannot evaluate equation", trim(__FILE__),__LINE__)

  endif

  deallocate(sVarTxt)

  call TSCOL%clear()

end function calc_values_from_equation_fn

!------------------------------------------------------------------------------

end module tsp_collections
