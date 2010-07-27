module tsp_collections

  use tsp_data_structures
  use tsp_utilities
  use tsp_time_series_manager
  use tsp_table_manager

  type TIME_SERIES_COLLECTION
    integer (kind=T_INT) :: iNumTimeSeries  = 0
    integer (kind=T_INT) :: iNumTables      = 0
    integer (kind=T_INT) :: iNum_VTables    = 0

    type (T_TIME_SERIES), dimension(:), allocatable :: tTS
    type (T_TABLE), dimension(:), allocatable :: tTable
!    type (T_VTABLE), dimension(:), allocatable :: tVTable

  contains

    procedure :: add_ts_sub, add_table_sub
    generic :: add => add_ts_sub, add_table_sub
    procedure :: removeTS => remove_ts_sub
    procedure :: removeTable => remove_table_sub
    procedure :: summarize => summarize_sub
    procedure :: describe => describe_ts_sub
    procedure :: getTS => get_ts_pointer_fn
    procedure :: getTable => get_table_pointer_fn
    procedure :: listTS => list_output_ts_sub
    procedure :: listTable => list_output_table_sub

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

      ! check to see whether thie name has been used already
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

      this%tTS(iCount+1) = tTS
      this%tTS(iCount+1)%pData = tTS%pData

!      deallocate(this%tTS, stat=iStat)
!      call Assert(iStat==0, "Unable to deallocate memory for time series", &
!        TRIM(__FILE__), __LINE__)

!      this%tTS => tTempTS

    else

      allocate(this%tTS(1),stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for time series", &
        TRIM(__FILE__), __LINE__)

      this%tTS(1) = tTS

      if(allocated(this%tTS(1)%pData)) deallocate(this%tTS(1)%pData)
      allocate(this%tTS(1)%pData(size(tTS%pData)))

      this%tTS(1)%pData = tTS%pData

    endif

!    deallocate(tTS, stat=iStat)
!    call Assert(iStat==0, "Unable to deallocate memory for time series", &
!        TRIM(__FILE__), __LINE__)

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

      ! check to see whether thie name has been used already
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
      if(lMatch) return

      ! allocate memory for size of TS collection MINUS one time series
      allocate(pTempTS(iCount-1),stat=iStat)
      call Assert(iStat==0, "Unable to allocate temporary memory for time series", &
        TRIM(__FILE__), __LINE__)

!      allocate(pTempTS(iCount + 1)%pData(size(tTS%pData)))

      j = 0
      ! make a copy of objects we wish to keep
      do i = 1,iCount
        if(str_compare(this%tTS(i)%sSeriesName, sSeriesName)) cycle
        j = j + 1
        pTempTS(j) = this%tTS(i)
      enddo

      ! deallocate TS objects collection
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

    else

      call Assert(lFALSE, "There are no time series objects available for deletion", &
         trim(__FILE__), __LINE__)

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

      call Assert(lMatch, "No table with the name "//trim(sSeriesName)// &
         " could be found",trim(__FILE__),__LINE__)
      if(lMatch) return

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

    else

      call Assert(lFALSE, "There are no table objects available for deletion", &
         trim(__FILE__), __LINE__)

    endif

    deallocate(pTempTable, stat=iStat)
    call Assert(iStat==0, "Unable to deallocate memory for temporary table objects", &
        TRIM(__FILE__), __LINE__)

  end subroutine remove_table_sub

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
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iMemoryInBytes

    write(LU_STD_OUT,fmt="(/,/,a,/)") '*** TSPROC TIME SERIES and TABLE OBJECTS CURRENTLY IN MEMORY ***'
    write(LU_STD_OUT, &
      fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
           t80,'MAX',t86,'MEMORY(kb)',/)")

    if(.not. allocated(this%tTS)) then
      write(LU_STD_OUT,fmt="(a,/)") '     ===> No TIME SERIES objects currently in memory <==='

    else

      iCount = size(this%tTS)

      do i=1,iCount

        if(allocated(this%tTS(i)%pData)) then
          iMemoryInBytes = sizeof(this%tTS(i)) + sizeof(this%tTS(i)%pData)
          write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
            this%tTS(i)%sSeriesName, &
            this%tTS(i)%tStartDate%prettyDate(), &
            this%tTS(i)%tEndDate%prettyDate(), &
            size(this%tTS(i)%pData), &
            MINVAL(this%tTS(i)%pData%rValue), &
            SUM(this%tTS(i)%pData%rValue)/ size(this%tTS(i)%pData), &
            MAXVAL(this%tTS(i)%pData%rValue), &
            iMemoryInBytes / 1024
        else
          iMemoryInBytes = sizeof(this%tTS(i))

          write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
            this%tTS(i)%sSeriesName, iMemoryInBytes / 1024

        endif
      end do
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

    if(allocated(pTS%pData)) then
    write(LU_STD_OUT, &
      fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
           t80,'MAX',t86,'MEMORY(kb)')")
      iMemoryInBytes = sizeof(pTS) + sizeof(pTS%pData)
      write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
        pTS%sSeriesName, &
        pTS%tStartDate%prettyDate(), &
        pTS%tEndDate%prettyDate(), &
        size(pTS%pData), &
        MINVAL(pTS%pData%rValue), &
        SUM(pTS%pData%rValue)/ size(pTS%pData), &
        MAXVAL(pTS%pData%rValue), &
        iMemoryInBytes / 1024

    else
      iMemoryInBytes = sizeof(pTS)
       write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
        pTS%sSeriesName, iMemoryInBytes / 1024

    endif


  end subroutine describe_ts_sub

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

    write(LU,fmt="(/,' TIME_SERIES ',a,' ---->')") trim(pTS%sSeriesName)

    do i=1,size(pTS%pData)

       write(LU,fmt="(a,t20,a,3x,1pg14.7)") trim(pTS%sSeriesName), &
          pTS%pData(i)%tDT%listdatetime(sDateFmt), pTS%pData(i)%rValue

    enddo


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

    write(LU,fmt="(/,a,' ---->')") " "//TABLE_TYPE(pTable%iTableType)//" "//trim(pTable%sSeriesName)

    if( pTable%iTableType == iSTABLE ) then
      do i=1,size(pTable%tTableData)
         write(LU,fmt="(t5,a48,t55,a)") trim(pTable%tTableData(i)%sDescription), &
            trim(pTable%tTableData(i)%sValue)
      enddo
    else

    endif


  end subroutine list_output_table_sub

!------------------------------------------------------------------------------

end module tsp_collections
