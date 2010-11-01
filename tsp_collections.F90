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
    integer (kind=T_INT) :: iNumTSComparisons = 0
    integer (kind=T_INT) :: iNumTableComparisons = 0

    type (T_TIME_SERIES), dimension(:), pointer :: pTS => null()
    type (T_TIME_SERIES), pointer :: pTS_Head => null()
    type (T_TS_COMPARISON), dimension(:), allocatable :: tTSComparison
    type (T_TABLE), dimension(:), allocatable :: tTable
    type (T_TABLE), pointer :: pTable_Head => null()
    type (T_TABLE_COMPARISON), dimension(:), allocatable :: tTableComparison

  contains

!    procedure :: add_ts_sub, add_table_sub
    procedure :: add_ts_link_sub, add_table_link_sub
    procedure :: tsCompare => add_ts_comparison_sub
    procedure :: tableCompare => add_table_comparison_sub
    generic :: add => add_ts_link_sub, add_table_link_sub
    procedure :: clear => remove_all_sub
    procedure :: calculate => calc_values_from_equation_fn
    procedure :: newTimeBase => conform_ts_sub
    procedure :: removeTS => remove_ts_link_sub
    procedure :: removeTable => remove_table_link_sub
    procedure :: summarize => summarize_sub
    procedure :: describe => describe_ts_sub
    procedure :: getTS => get_ts_pointer_link_fn
    procedure :: getTSComparison => get_ts_comparison_pointer_fn
    procedure :: getTableComparison => get_table_comparison_pointer_fn
    procedure :: getTable => get_table_pointer_link_fn
    procedure :: listTS => list_output_ts_sub
    procedure :: pestWriteTSComparison => pest_write_ts_comparison_sub
    procedure :: pestWriteTableComparison => pest_write_table_comparison_sub
    procedure :: listTable => list_output_table_sub
    procedure :: datesEqual => are_datetime_stamps_identical_fn

  end type TIME_SERIES_COLLECTION

contains

  subroutine add_ts_link_sub(this, pNewSeries)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TIME_SERIES), pointer ::  pNewSeries

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pCurrentTS
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iIteration

    pNewSeries%pNext => null()
    pNewSeries%pPrevious => null()
    pCurrentTS => null()

    print *, "associated(this%pTS_Head): ",associated(this%pTS_Head)
    if(associated(this%pTS_Head)) print *, quote(this%pTS_Head%sSeriesName)

    if(.not. associated(this%pTS_Head)) then  ! no series are stored yet; set head node to
                                              ! pNewSeries
      this%pTS_Head => pNewSeries
      print *, "associated(this%pTS_Head): ",associated(this%pTS_Head)
      this%pTS_Head%pNext => null()
      this%pTS_Head%pPrevious => null()
      this%iNumTimeSeries = this%iNumTimeSeries + 1

      call echolog("  Added series "//quote(pNewSeries%sSeriesName)//". " &
        //"There is now "//trim(asChar(this%iNumTimeSeries))//" time series in memory.")
      call echolog("")

    else

      iIteration = 0

      ! reset pCurrentTS to the head node
      pCurrentTS => this%pTS_Head

      do

        iIteration = iIteration + 1
        print *, trim(asChar(iIteration))//") "//quote(pCurrentTS%sSeriesName)
        if(associated(pCurrentTS%pPrevious)) print *,quote(pCurrentTS%pPrevious%sSeriesName)//"<=="
        if(associated(pCurrentTS%pNext)) print *,"                  ==>"//quote(pCurrentTS%pNext%sSeriesName)

        ! recurse through list until we come to the end of the line
        if(associated(pCurrentTS%pNext) ) then
          pCurrentTS => pCurrentTS%pNext
          cycle
        else
          ! update forward-pointing and backward-pointing pointers
          if(associated(pNewSeries%pPrevious)) then
            call warn(lFALSE,"pNewSeries is being reallocated")
            print *, " (was: "//quote(pNewSeries%pPrevious%sSeriesName)//")"
          endif
          pCurrentTS%pNext => pNewSeries
          pNewSeries%pPrevious => pCurrentTS
          pNewSeries%pNext => null()
          this%iNumTimeSeries = this%iNumTimeSeries + 1
          call echolog("  Added series "//quote(pNewSeries%sSeriesName)//". " &
            //"There are now "//trim(asChar(this%iNumTimeSeries))//" time series in memory.")
          call echolog("")
                  print *, trim(asChar(iIteration))//") "//quote(pCurrentTS%sSeriesName)
          print *, "NEW SERIES ADDED"
          print *, "  current series:"//quote(pCurrentTS%sSeriesName)
          if(associated(pCurrentTS%pPrevious)) print *,quote(pCurrentTS%pPrevious%sSeriesName)//"<=="
          if(associated(pCurrentTS%pNext)) print *,"                  ==>"//quote(pCurrentTS%pNext%sSeriesName)

          print *, "  new series:"//quote(pNewSeries%sSeriesName)
          if(associated(pNewSeries%pPrevious)) print *,"  "//quote(pNewSeries%pPrevious%sSeriesName)//"<=="
          if(associated(pNewSeries%pNext)) print *,"                    ==>"//quote(pNewSeries%pNext%sSeriesName)

          exit
        endif
      enddo

    endif

    nullify(pNewSeries)
    nullify(pCurrentTS)

  end subroutine add_ts_link_sub

!------------------------------------------------------------------------------

  subroutine add_table_link_sub(this, pNewTable)

    class(TIME_SERIES_COLLECTION) :: this
    type(T_TABLE), pointer ::  pNewTable

    ! [ LOCALS ]
    type(T_TABLE), pointer :: pCurrentTable => null()
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    if(associated(this%pTable_Head)) then
      pCurrentTable => this%pTable_Head
      do
        ! recurse through list until we come to the end of the line
        if(associated(pCurrentTable%pNext) ) then
          pCurrentTable => pCurrentTable%pNext
          cycle
        else
          ! update forward-pointing and backward-pointing pointers
          pCurrentTable%pNext => pNewTable
          pNewTable%pPrevious => pCurrentTable
          pNewTable%pNext => null()
          this%iNumTables = this%iNumTables + 1
          call echolog("  Added table "//quote(pNewTable%sSeriesName)//". " &
            //"There are now "//trim(asChar(this%iNumTables))//" tables in memory.")
          call echolog("")

          exit
        endif
      enddo
    else
      this%pTable_Head => pNewTable
      this%pTable_Head%pNext => null()
      this%pTable_Head%pPrevious => null()
      this%iNumTables = this%iNumTables + 1
      pCurrentTable => this%pTable_Head

      call echolog("  Added table "//quote(pNewTable%sSeriesName)//". " &
        //"There is now "//trim(asChar(this%iNumTables))//" table in memory.")
      call echolog("")

    endif

    nullify(pNewTable)
    nullify(pCurrentTable)

  end subroutine add_table_link_sub

!------------------------------------------------------------------------------

!   subroutine add_ts_sub(this, pTS)
!
!     ! add tTS to the COLLECTION of time series objects (this)
!
!     class(TIME_SERIES_COLLECTION) :: this
!     type(T_TIME_SERIES), pointer ::  pTS
!
!     ! [ LOCALS ]
!     type(T_TIME_SERIES), dimension(:), pointer :: pTempTS
!     integer (kind=T_INT) :: iCount
!     integer (kind=T_INT) :: iStat
!     integer (kind=T_INT) :: i
!
!     print *, "ENTERED the ADD_TS_SUB"
!
!     if(associated(this%pTS)) then
!
!       ! get the number of time series currently in time series container
!       iCount = size(this%pTS)
!
!       print *, "iCount = ",iCount
!       ! check to see whether this name has been used already
!       do i = 1,iCount
!         print *, quote(this%pTS(i)%sSeriesName)
!         call Assert(.not. str_compare(this%pTS(i)%sSeriesName, pTS%sSeriesName), &
!          "Series name "//trim(pTS%sSeriesName)//" has already been used", &
!          trim(__FILE__), __LINE__)
!       end do
!
! !      ! allocate memory for size of current TS + 1
!       allocate(pTempTS(iCount + 1),stat=iStat)
!       call Assert(iStat==0, "Unable to allocate temporary memory for time series", &
!         TRIM(__FILE__), __LINE__)
!
!       ! copy current TS objects
!       pTempTS = this%pTS
!       pTempTS%tData = this%pTS%tData
!
!       ! deallocate TS objects collection
!       deallocate(this%pTS, stat=iStat)
!       call Assert(iStat==0, "Unable to deallocate memory for time series", &
!         TRIM(__FILE__), __LINE__)
!
!       move_alloc(pTempTS, this%pTS)
!
!       ! allocate TS objects collection to include room for new object
! !      allocate(this%pTS(iCount + 1), stat=iStat)
! !      call Assert(iStat==0, "Unable to allocate memory for time series", &
! !        TRIM(__FILE__), __LINE__)
!
! !      this%pTS(1:iCount) = pTempTS
! !      this%pTS(1:iCount)%pData = tTempTS(1:iCount)%pData
!
!       ! ensure that the minimum and maximum date range fields are populated
!       ! before adding the TS object to the collection
! !      call tTS%findDateMinAndMax()
!
!       this%pTS(iCount+1) = pTS
!       this%pTS(iCount+1)%tData = pTS%tData
!
! !      deallocate(this%pTS, stat=iStat)
! !      call Assert(iStat==0, "Unable to deallocate memory for time series", &
! !        TRIM(__FILE__), __LINE__)
!
! !      this%pTS => tTempTS
!
!     else
!
!       allocate(this%pTS(1),stat=iStat)
!       call Assert(iStat==0, "Unable to allocate memory for time series", &
!         TRIM(__FILE__), __LINE__)
!
!       ! ensure that the minimum and maximum date range fields are populated
!       ! before adding the TS object to the collection
! !      call tTS%findDateMinAndMax()
!
!       this%pTS = pTS
!
! !      if(associated(this%pTS(1)%pData)) deallocate(this%pTS(1)%pData)
! !      allocate(this%pTS(1)%pData(size(tTS%pData)))
!
!       this%pTS(1)%tData = pTS%tData
!
!     endif
!
!     this%iNumTimeSeries = this%iNumTimeSeries + 1
!
!     nullify(pTS)
!
!   end subroutine add_ts_sub

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

  subroutine remove_ts_link_sub(this, sSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TIME_SERIES), pointer :: pCurrent
    type(T_TIME_SERIES), pointer :: pPrevious
    type(T_TIME_SERIES), pointer :: pNext
    logical (kind=T_LOGICAL) :: lMatch
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    lMatch = lFALSE

    if(associated(this%pTS_Head)) then

      ! start at head of linked list
      pCurrent => this%pTS_Head
      do

        if(.not. associated(pCurrent) ) then
          exit

        elseif(str_compare(pCurrent%sSeriesName, sSeriesName) ) then

          ! reset pointers away/around object to be deleted
          pPrevious => pCurrent%pPrevious
          if(associated(pPrevious)) then
            pPrevious%pNext => pCurrent%pNext
          else
            ! if the pPrevious pointer is null, it means we're at the
            ! head of the list; need to redefine the head of the list
            ! within the TS data structure
            this%pTS_Head => pCurrent%pNext
          endif

          pNext => pCurrent%pNext
          if(associated(pNext)) pNext%pPrevious => pCurrent%pPrevious
          deallocate(pCurrent%tData, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a time series", &
            trim(__FILE__), __LINE__)
          deallocate(pCurrent, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a time series", &
            trim(__FILE__), __LINE__)

          lMatch = lTRUE
          this%iNumTimeSeries = this%iNumTimeSeries - 1

          call echolog("Removed series "//quote(sSeriesName)//". " &
            //trim(asChar(this%iNumTimeSeries))//" series remaining.")

          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      call warn(lMatch, "Unable to find series "//quote(sSeriesName)//" for removal", &
        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no series objects in memory. Unable to remove series " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)
    endif


!    pCurrent => this%pTS_Head
!
!      if(associated(pCurrent)) then
!        do
!          if(.not. associated(pCurrent%pNext) ) then
!            exit
!          else
!            pCurrent => pCurrent%pNext
!          endif
!        enddo
!      endif

  end subroutine remove_ts_link_sub

!------------------------------------------------------------------------------

  subroutine remove_table_link_sub(this, sSeriesName)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TABLE), pointer :: pCurrent
    type(T_TABLE), pointer :: pPrevious
    type(T_TABLE), pointer :: pNext
    logical (kind=T_LOGICAL) :: lMatch
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i

    lMatch = lFALSE

    if(associated(this%pTable_Head)) then

      ! start at head of linked list
      pCurrent => this%pTable_Head
      do

        if(.not. associated(pCurrent) ) then
          exit

        elseif(str_compare(pCurrent%sSeriesName, sSeriesName) ) then

          ! reset pointers away/around object to be deleted
          pPrevious => pCurrent%pPrevious
          if(associated(pPrevious)) then
            pPrevious%pNext => pCurrent%pNext
          else
            ! if the pPrevious pointer is null, it means we're at the
            ! head of the list; need to redefine the head of the list
            ! within the TS data structure
            this%pTable_Head => pCurrent%pNext
          endif

          pNext => pCurrent%pNext
          if(associated(pNext)) pNext%pPrevious => pCurrent%pPrevious
          deallocate(pCurrent%tTableData, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a table", &
            trim(__FILE__), __LINE__)
          deallocate(pCurrent, stat=iStat)
          call Assert(iStat==0, "Problem deallocating memory while removing a table", &
            trim(__FILE__), __LINE__)

          lMatch = lTRUE
          this%iNumTables = this%iNumTables - 1

          call echolog("Removed table "//quote(sSeriesName)//". " &
            //trim(asChar(this%iNumTables))//" table remaining.")

          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      call warn(lMatch, "Unable to find series "//quote(sSeriesName)//" for removal", &
        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no series objects in memory. Unable to remove series " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)
    endif

  end subroutine remove_table_link_sub

!------------------------------------------------------------------------------

  subroutine remove_ts_sub(this, sSeriesName)

    ! remove tTS from a COLLECTION of time series objects (this)

    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName

    ! [ LOCALS ]
    type(T_TIME_SERIES), dimension(:), pointer :: pTempTS
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch

    if(associated(this%pTS)) then

      ! get the number of time series currently in time series container
      iCount = size(this%pTS)

!      print *, "iCount: ", iCount

      ! check to see whether a time series with this name exists
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%pTS(i)%sSeriesName, sSeriesName)) then
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
        if(str_compare(this%pTS(i)%sSeriesName, sSeriesName)) cycle
        j = j + 1
        pTempTS(j) = this%pTS(i)
!        print *, i, "keeping "//trim(this%pTS(i)%sSeriesname)
      enddo

      ! deallocate TS objects collection
      do i=1,size(this%pTS)
        deallocate( this%pTS(i)%tData, stat=iStat )
      enddo
      call Assert(iStat==0, "Unable to deallocate memory for time series data", &
        TRIM(__FILE__), __LINE__)

      deallocate(this%pTS, stat=iStat)
      call Assert(iStat==0, "Unable to deallocate memory for time series", &
        TRIM(__FILE__), __LINE__)


      ! allocate TS objects collection to include room for new object
      allocate(this%pTS(iCount - 1), stat=iStat)
      call Assert(iStat==0, "Unable to allocate memory for time series", &
        TRIM(__FILE__), __LINE__)

      this%pTS = pTempTS
      this%iNumTimeSeries = this%iNumTimeSeries - 1


!      deallocate(this%pTS, stat=iStat)
!      call Assert(iStat==0, "Unable to deallocate memory for time series", &
!        TRIM(__FILE__), __LINE__)

!      this%pTS => pTempTS

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

    if(associated(this%pTS)) then
      iSize = size(this%pTS)
      do i=1,iSize
        if(allocated(this%pTS(i)%tData)) deallocate(this%pTS(i)%tData)
      enddo
      deallocate(this%pTS)
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

    if(associated(this%pTS)) then

      ! get the number of time series currently in time series container
      iCount = size(this%pTS)

      ! scan to find a series with this name
      lMatch = lFALSE
      do i = 1,iCount
        if(str_compare(this%pTS(i)%sSeriesName, sSeriesName)) then
          lMatch = lTRUE
          exit
        endif
      end do

      call Assert(lMatch, "No time series with the name "//quote(sSeriesName)// &
         " could be found",trim(__FILE__),__LINE__)

      pTS => this%pTS(i)

    endif

  end function get_ts_pointer_fn

!------------------------------------------------------------------------------

  function get_ts_pointer_link_fn(this, sSeriesName)    result( pTS )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TIME_SERIES), pointer :: pTS

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch
    type(T_TIME_SERIES), pointer :: pCurrent => null()

    lMatch = lFALSE

    if(associated(this%pTS_Head)) then

      ! start at head of linked list
      pCurrent => this%pTS_Head
      do
        if(.not. associated(pCurrent) ) exit

        if(str_compare(pCurrent%sSeriesName, sSeriesName) ) then
          lMatch = lTRUE
          pTS => pCurrent
          exit
        elseif(.not. associated(pCurrent%pNext) ) then
          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      if(.not. lMatch) then
        call this%summarize()
        call assert(lFALSE,"Unable to find series "//quote(sSeriesName)//".", &
        trim(__FILE__), __LINE__)
      endif
!      call assert(lMatch, "Unable to find series "//quote(sSeriesName)//".", &
!        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no series objects in memory. Unable to obtain pointer to series " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)

      pTS => null()

    endif

    nullify(pCurrent)

  end function get_ts_pointer_link_fn

!------------------------------------------------------------------------------

  function get_table_pointer_link_fn(this, sSeriesName)    result( pTable )

    ! get a pointer to a table from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sSeriesName
    type(T_TABLE), pointer :: pTable

    ! [ LOCALS ]
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: i, j
    logical (kind=T_LOGICAL) :: lMatch
    type(T_TABLE), pointer :: pCurrent => null()

    lMatch = lFALSE

    if(associated(this%pTable_Head)) then

      ! start at head of linked list
      pCurrent => this%pTable_Head
      do
        if(.not. associated(pCurrent) ) exit

        if(str_compare(pCurrent%sSeriesName, sSeriesName) ) then
          lMatch = lTRUE
          pTable => pCurrent
          exit
        elseif(.not. associated(pCurrent%pNext) ) then
          exit
        else
          pCurrent => pCurrent%pNext
        endif
      enddo

      call warn(lMatch, "Unable to find table "//quote(sSeriesName)//".", &
        trim(__FILE__), __LINE__)
    else
      call warn(lFALSE,"There are no table objects in memory. Unable to obtain pointer to table " &
        //quote(sSeriesName), trim(__FILE__), __LINE__)

      pTable => null()

    endif

    nullify(pCurrent)

  end function get_table_pointer_link_fn

!------------------------------------------------------------------------------

  function get_ts_comparison_pointer_fn(this, sObservedSeries, sModeledSeries)    result( tTSComparison )

    ! get values associated with tTS from a COLLECTION of time series objects (this)
    class(TIME_SERIES_COLLECTION) :: this
    character (len=*) :: sObservedSeries
    character (len=*) :: sModeledSeries
    type(T_TS_COMPARISON), pointer :: tTSComparison

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

      call Assert(lMatch, "No time series comparison between the series "//quote(sObservedSeries)// &
         " and "//quote(sModeledSeries)//" could be found",trim(__FILE__),__LINE__)

!      tTSComparison => this%tTSComparison(i)
      tTSComparison => null()

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

      call Assert(lMatch, "No table comparisons between the series "//quote(sObservedTable)// &
         " and "//quote(sModeledTable)//" could be found",trim(__FILE__),__LINE__)

!      pTableComparison => this%tTableComparison(i)
      pTableComparison => null()

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

      call Assert(lMatch, "No table with the name "//quote(sSeriesName)// &
         " could be found",trim(__FILE__),__LINE__)

!      pTable => this%tTable(i)
      pTable => null()

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
    type (T_TIME_SERIES), pointer :: pObservedSeries => null()
    type (T_TIME_SERIES), pointer :: pModeledSeries => null()
    type(T_TIME_SERIES), pointer :: pCurrentTS => null()
    type (T_TABLE), pointer :: pObservedTable => null()
    type (T_TABLE), pointer :: pModeledTable => null()
    type (T_TABLE), pointer :: pCurrentTable => null()

    write(LU_STD_OUT,fmt="(/,/,a,/)") '*** TSPROC TIME SERIES and TABLE OBJECTS CURRENTLY IN MEMORY ***'
    write(LU_STD_OUT, &
      fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
           t80,'MAX',t86,'MEMORY(kb)',/)")

    if(associated(this%pTS_Head)) then

      pCurrentTS => this%pTS_Head

      do

        if(allocated(pCurrentTS%tData)) then
          iMemoryInBytes = sizeof(pCurrentTS) + sizeof(pCurrentTS%tData)
          write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
            pCurrentTS%sSeriesName, &
            pCurrentTS%tStartDate%prettyDate(), &
            pCurrentTS%tEndDate%prettyDate(), &
            size(pCurrentTS%tData), &
            MINVAL(pCurrentTS%tData%rValue), &
            SUM(pCurrentTS%tData%rValue)/ size(pCurrentTS%tData), &
            MAXVAL(pCurrentTS%tData%rValue), &
            iMemoryInBytes / 1024
            iSum = iSum + size(pCurrentTS%tData)
        else
          iMemoryInBytes = sizeof(pCurrentTS)

          write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
            pCurrentTS%sSeriesName, iMemoryInBytes / 1024

        endif

        ! recurse through list until we come to the end of the line
        if(associated(pCurrentTS%pNext) ) then
          pCurrentTS => pCurrentTS%pNext
          cycle
        else
          exit
        endif
      enddo

    endif

!     if(.not. associated(this%pTS)) then
!       write(LU_STD_OUT,fmt="(a,/)") '     ===> No TIME SERIES objects currently in memory <==='
!
!     else
!
!       iCount = size(this%pTS)
!
!       iSum = 0
!       do i=1,iCount
!
!         if(allocated(this%pTS(i)%tData)) then
!           iMemoryInBytes = sizeof(this%pTS(i)) + sizeof(this%pTS(i)%tData)
!           write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k')") &
!             this%pTS(i)%sSeriesName, &
!             this%pTS(i)%tStartDate%prettyDate(), &
!             this%pTS(i)%tEndDate%prettyDate(), &
!             size(this%pTS(i)%tData), &
!             MINVAL(this%pTS(i)%tData%rValue), &
!             SUM(this%pTS(i)%tData%rValue)/ size(this%pTS(i)%tData), &
!             MAXVAL(this%pTS(i)%tData%rValue), &
!             iMemoryInBytes / 1024
!             iSum = iSum + size(this%pTS(i)%tData)
!         else
!           iMemoryInBytes = sizeof(this%pTS(i))
!
!           write(LU_STD_OUT,fmt="(a,t20,51x,i6,'k')") &
!             this%pTS(i)%sSeriesName, iMemoryInBytes / 1024
!
!         endif
!       end do
!
!       write(LU_STD_OUT, fmt="(/,a,/)") trim(asChar(iSum))//" total records in memory."
!
!     endif

!     if(.not. allocated(this%tTable)) then
!       write(LU_STD_OUT,fmt="(/,a,/)") '     ===> No TABLE objects currently in memory <==='
!
!     else
!
!       iCount = size(this%tTable)
!
!       write(LU_STD_OUT, &
!         fmt="(/,'SERIES_NAME',t24,'DATE RANGE',t45,'TABLE TYPE',/)")
!
!
!       do i=1,iCount
!
!         if(allocated(this%tTable(i)%tTableData)) then
!           iMemoryInBytes = sizeof(this%tTable(i)) + sizeof(this%tTable(i)%tTableData)
!           write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
!             this%tTable(i)%sSeriesName, &
!             this%tTable(i)%tStartDate%prettyDate(), &
!             this%tTable(i)%tEndDate%prettyDate(), &
!             TABLE_TYPE(this%tTable(i)%iTableType)
!
!         else
!
!
!         endif
!       end do
!     endif

    if(.not. associated(this%pTable_Head)) then
      write(LU_STD_OUT,fmt="(/,a,/)") '     ===> No TABLE objects currently in memory <==='

    else


      write(LU_STD_OUT, &
        fmt="(/,'SERIES_NAME',t24,'DATE RANGE',t45,'TABLE TYPE',/)")


      pCurrentTable => this%pTable_Head

      do

        if(allocated(pCurrentTable%tTableData)) then
          iMemoryInBytes = sizeof(pCurrentTable) + sizeof(pCurrentTable%tTableData)
          write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
            pCurrentTable%sSeriesName, &
            pCurrentTable%tStartDate%prettyDate(), &
            pCurrentTable%tEndDate%prettyDate(), &
            TABLE_TYPE(pCurrentTable%iTableType)

        else
          write(LU_STD_OUT,fmt="(a18,a10,'-',a10, t47, a)") &
            pCurrentTable%sSeriesName, '***', '***', '***'

        endif

        ! recurse through list until we come to the end of the line
        if(associated(pCurrentTable%pNext) ) then
          pCurrentTable => pCurrentTable%pNext
          cycle
        else
          exit
        endif

      end do
    endif


    write(LU_STD_OUT,fmt="(/,/,a,/)") '*** TSPROC TIME SERIES and TABLE COMPARISON OBJECTS CURRENTLY IN MEMORY ***'
    write(LU_STD_OUT, &
      fmt="(/,'OBSERVED SERIES',t19,'MODELED SERIES',t38,'DATE RANGE',t60,'COUNT'," &
         //"t76,'RESIDUAL',/)")

    if(.not. allocated(this%tTSComparison)) then
      write(LU_STD_OUT,fmt="(a)") &
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
      fmt="(/,'OBSERVED TABLE',t19,'MODELED TABLE',t38,'DATE RANGE',t60,'COUNT'," &
         //"t76,'RESIDUAL',/)")
    if(.not. allocated(this%tTableComparison)) then
      write(LU_STD_OUT,fmt="(a,/)") &
        '     ===> No TABLE COMPARISON objects currently in memory <==='

    else

      iCount = size(this%tTableComparison)

      do i=1,iCount

        pObservedTable => this%getTable(this%tTableComparison(i)%sObservedTable)
        pModeledTable => this%getTable(this%tTableComparison(i)%sModeledTable)
        rSSE = 0.

        if(pObservedTable%iTableType == iETABLE) then

          ! calculate SSE using the third element of "sValue" (i.e. fraction of days exceeded)
          do j=1,size(pObservedTable%tTableData)
            rSSE = rSSE + (( asReal(pObservedTable%tTableData(j)%sValue(3)) &
                   - asReal(pModeledTable%tTableData(j)%sValue(3)) ) **2 &
                   * this%tTableComparison(i)%rWeightValue(j)**2)
          enddo

        elseif(pObservedTable%iTableType == iSTABLE) then

          ! ignore the first 8 entries of the S table in calculating SSE
          do j=9,size(pObservedTable%tTableData)
            rSSE = rSSE + (( asReal(pObservedTable%tTableData(j)%sValue(1)) &
                   - asReal(pModeledTable%tTableData(j)%sValue(1)) ) **2 &
                   * this%tTableComparison(i)%rWeightValue(j)**2)
          enddo

        else

          do j=1,size(pObservedTable%tTableData)
            rSSE = rSSE + (( asReal(pObservedTable%tTableData(j)%sValue(1)) &
                   - asReal(pModeledTable%tTableData(j)%sValue(1)) ) **2 &
                   * this%tTableComparison(i)%rWeightValue(j)**2)
          enddo

        endif

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

    write(LU_STD_OUT,fmt="(/,a,/)") trim(pTS%sDescription)

    if(allocated(pTS%tData)) then
    write(LU_STD_OUT, &
      fmt="('SERIES_NAME',t24,'DATE RANGE',t45,'COUNT',t58,'MIN',t68,'MEAN', &
           t80,'MAX',t86,'MEMORY(kb)')")
      iMemoryInBytes = sizeof(pTS) + sizeof(pTS%tData)
      write(LU_STD_OUT,fmt="(a18,a10,'-',a10,i10, f11.2,f11.2,f11.2,3x,i6,'k',/)") &
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
    type(T_TS_COMPARISON), pointer :: tTSComparison
    type(T_TIME_SERIES), pointer :: pObservedSeries
    integer (kind=T_INT) :: LU
    character (len=256) :: sFormatString
    integer (kind=T_INT) :: i

    if(present(iLU)) then
      LU = iLU
    else
      LU = LU_STD_OUT
    endif

    tTSComparison => this%getTSComparison(sObservedSeries, sModeledSeries)
    pObservedSeries => this%getTS(sObservedSeries)

    sFormatString = "(1x,a"//trim(asChar(MAXNAMELENGTH) )//",3x,g16.8,3x,g16.8,3x,a)"

    ! add observation numbers to the end of the observation name
    do i=1,size(pObservedSeries%tData)

       write(LU,fmt=trim(sFormatString)) &
          trim(pObservedSeries%sSeriesName)//"_"//trim(asChar(i) ), &
          pObservedSeries%tData(i)%rValue, tTSComparison%rWeightValue(i), &
          tTSComparison%sObservedSeries

    enddo

    nullify(tTSComparison, pObservedSeries)

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
    character(len=256), intent(in) :: sSeriesName
    character(len=256), intent(in) :: sTimeBaseName
    character(len=256), intent(in), optional :: sNewSeriesName

    ! [ LOCALS ]
    type (T_TIME_SERIES), pointer :: pTS, pTimeBaseTS
    type (T_TIME_SERIES), pointer :: pNewSeries
    type (T_TIME_SERIES), allocatable, target :: pTempSeries
    integer (kind=T_INT) :: iStat

    real (kind=T_SGL), dimension(:), allocatable :: rX1, rX2, rY1, rY2
    real (kind=T_DBL) :: rOffset

   character (len=256) :: sNewName

   call Assert(len_trim(sSeriesname) > 0, &
     "internal error: must provide 'sSeriesname'", trim(__FILE__),__LINE__)
   call Assert(len_trim(sTimeBasename) > 0, &
     "internal error: must provide 'sSeriesname'", trim(__FILE__),__LINE__)

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

    allocate(pTempSeries, stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(pTempSeries%tData(size(pTimeBaseTS%tData)), stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    pTempSeries%sSeriesname = sNewName
    pTempSeries%sDescription = "Data from series "//trim(pTS%sSeriesName)//", " &
      //"interpolated to the datetimes found in series "//trim(pTimeBaseTS%sSeriesName)

    allocate(rX1(size(pTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(rY1(size(pTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(rX2(size(pTimeBaseTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

    allocate(rY2(size(pTimeBaseTS%tData)),stat=iStat)
    call Assert(iStat == 0, "Problem allocating memory for NEW_TIME_BASE calculation", &
      trim(__FILE__), __LINE__)

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
    pTempSeries%tData%tDT = pTimeBaseTS%tData%tDT
    ! copy the interpolated values to the new series
    pTempSeries%tData%rValue = rY2
    call pTempSeries%findDateMinAndMax()
    pNewSeries => pTempSeries
    ! add new series to collection of series

    call this%add(pNewSeries)
    deallocate(rX1, rY1, rX2, rY2)
    nullify(pTS)
    nullify(pTimeBaseTS)
    nullify(pNewSeries)

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
    type(T_TS_COMPARISON), dimension(:), allocatable :: tTemtTSComparison
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
            allocate(tTemtTSComparison(iCount),stat=iStat)
            call Assert(iStat==0, "Unable to allocate temporary memory for time " &
              //"series comparison object", &
              TRIM(__FILE__), __LINE__)

            ! make a copy of all previous TS objects
            tTemtTSComparison = this%tTSComparison

            ! deallocate TS comparison objects collection
            deallocate(this%tTSComparison, stat=iStat)
            call Assert(iStat==0, "Unable to deallocate memory for time " &
            //"series comparison object", &
            TRIM(__FILE__), __LINE__)

            ! allocate TS comparison objects collection to include room for new object
            allocate(this%tTSComparison(iCount + 1), stat=iStat)
            call Assert(iStat==0, "Unable to allocate memory for time series comparison object", &
              TRIM(__FILE__), __LINE__)

            this%tTSComparison(1:iCount) = tTemtTSComparison

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

    ! ensure that we're about to compare tables of the same type
    call Assert(pObservedTable%iTableType == pModeledTable%iTableType, &
      "Cannot compare modeled table ("//quote(TABLE_TYPE(pModeledTable%iTableType)) &
      //") to observed table ("//quote(TABLE_TYPE(pObservedTable%iTableType)), &
      trim(__FILE__), __LINE__)

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
    if(isElement(sVarTxt(i), this%pTS%sSeriesName)) then
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
        do j=1,size(TSCOL%pTS)
          rTempValue(j) = TSCOL%pTS(j)%tData(i)%rValue
        enddo
        rOut(i) = evaluate_expression (rTempValue , TSCOL%pTS(1)%tData(i)%tDT )
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
