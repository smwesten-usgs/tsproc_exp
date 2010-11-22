module tsp_file_readers
  use tsp_data_structures
  use tsp_control_file_ops
  use tsp_time_series_manager
  use tsp_utilities
  use tsp_collections
  use tsp_datetime_class
  implicit none

  type T_DATA_FILE
    integer (kind=T_INT) :: iLU
    character (len=256)  :: sFilename
    integer (kind=T_INT) :: iLineNumber = 0
    type (T_DATETIME) :: tDateTime_1
    type (T_DATETIME) :: tDateTime_2
  end type T_DATA_FILE

contains

!------------------------------------------------------------------------------

subroutine get_mul_series_usgs_nwis(pBlock, TS)

  implicit none

  type (T_BLOCK), pointer :: pBlock
  type (TIME_SERIES_COLLECTION), intent(inout) :: TS

!  type(T_USGS_NWIS_GAGE),dimension(:), pointer :: pGage
!  type(T_USGS_NWIS_DAILY),dimension(:), allocatable :: pTempDaily

  ! [ LOCALS ]
  integer (kind=T_INT) :: iMM, iDD, iYYYY
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iLineNum
  character (len=256)   :: sAgencyCode
  character (len=256)   :: sSiteID, sOldSiteID
  character (len=256) :: sDateTxt
  type (T_DATETIME) :: tDATETIME_1, tDATETIME_2, tCurrDate

  integer (kind=T_INT) :: iMonth, iDay, iYear, iJulianDay, iWaterYear

  integer (kind=T_INT) :: iCount
  integer (kind=T_INT) :: i, j
  integer (kind=T_INT), dimension(200) :: iTotalNumLines
  character (len=MAXNAMELENGTH), dimension(200) :: sSiteIDArray
  character (len=80), dimension(200) :: sSiteDescriptionArray
  type (T_TIME_SERIES), dimension(:), pointer :: pTS
  type (T_TIME_SERIES), pointer :: pTempSeries
!  type (T_TIME_SERIES), dimension(:), allocatable :: tTS
  integer (kind=T_INT) :: iSiteNum
  integer (kind=T_INT) :: iTotalNumSites
  integer (kind=T_INT) :: iNewIndex

  character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pDATE_FORMAT
  character (len=MAXARGLENGTH), dimension(:), pointer :: pNEW_SERIES_NAME
  character (len=MAXARGLENGTH), dimension(:), pointer :: pSITE
  character (len=256), dimension(:), pointer :: pDESCRIPTION


  character (len=256) :: sRecord, sItem
  character (len=256) :: sNewSeriesName
  integer (kind=T_INT) :: iIceFlag

  ! initialize variables
  iCount = 0; iTotalNumLines = 0; iSiteNum = 0; iTotalNumSites = 0
  sOldSiteID = ""; sSiteID = ""

  ! get DATE_1, TIME_1, DATE_2, TIME_2 if supplied
  call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2, "YYYY-MM-DD")

  ! get user supplied values for recognized keywords
  pFILE => pBlock%getString("FILE")
  pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
  pSITE => pBlock%getString("SITE")

  open(unit=newunit(LU_DATA),file=TRIM(ADJUSTL(pFILE(1))),status='OLD',iostat=iStat)
  call Assert(iStat==0,'Error opening NWIS file '//TRIM(pFILE(1)))

  ! make first pass through the NWIS file to determine what it contains
  do
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
    if(iStat /= 0) exit
    if(sRecord(1:24) .eq. '# Data for the following') then  ! next lines describe the gages
      do
        read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
        if(sRecord(1:5) .eq. '# ---') exit
        iTotalNumSites = iTotalNumSites + 1
        call Chomp(sRecord, sItem)
        sSiteDescriptionArray = trim(sRecord)
      end do
!      allocate(iTotalNumLines(iTotalNumGages))
      iTotalNumLines = 0
      cycle
    elseif(sRecord(1:9) .eq. 'agency_cd') then
      read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord

!      iSiteNum = iSiteNum + 1
      cycle
    else if(sRecord(1:1) .eq. "#") then
      cycle
    end if

    call Chomp_tab(sRecord,sItem)  ! agency cd

    call Chomp_tab(sRecord,sItem)  ! site id (USGS Gage ID)
    sSiteID = trim(sItem)
    if(.not. str_compare(sSiteID, sOldSiteID)) then
      sOldSiteID = sSiteID
      iSiteNum = iSiteNum + 1
      sSiteIDArray(iSiteNum) = sSiteID
    endif

    call Chomp_tab(sRecord,sDateTxt)  ! date

    call tCurrDate%parseDate(sDateTxt)
    call tCurrDate%parseTime("12:00:00")
    call tCurrDate%calcJulianDay()

    ! if the current date is not within the date range of interest, IGNORE!
    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    call Chomp_tab(sRecord,sItem)  ! discharge and data flag

    iIceFlag = max(index(sItem,"_Ice"),index(sItem,"_ice"))

    if(len_trim(sItem)>0 .and. iIceFlag == 0 .and. scan(sItem,"_") == 0) &
      iTotalNumLines(iSiteNum) = iTotalNumLines(iSiteNum) + 1
  end do

  iTotalNumSites = iSiteNum

  ! check to see if the SITEs given by user are actually found in the NWIS file
  if(.not. str_compare(pSITE(1),"NA")) then
    do j=1,size(pSITE)
      call Assert(isElement(pSITE(j),sSiteIDArray), &
        "Site "//trim(pSITE(j))//" was not found in NWIS file "//TRIM(pFILE(1)))
    enddo
  endif

  ! if the user has not supplied series names, we assume that the site IDs in the
  ! file are sufficient and that all data elements should be read in
  if(str_compare(pSITE(1),"NA")) then
    deallocate(pSITE)
    deallocate(pNEW_SERIES_NAME)
    allocate(pSITE(iTotalNumSites))
    allocate(pNEW_SERIES_NAME(iTotalNumSites))
    pSITE = sSiteIDArray(1:iTotalNumSites)
    pNEW_SERIES_NAME = sSiteIDArray(1:iTotalNumSites)
  endif

  allocate(pDESCRIPTION(iTotalNumSites))
  pDESCRIPTION = sSiteDescriptionArray(1:iTotalNumSites)

  ! rewind the file and make a second pass through the file
  rewind(unit=LU_DATA, iostat=iStat)
  call Assert(iStat==0,"Problem rewinding NWIS data file")

  ! ALLOCATE memory for the GAGE object (collection of gages)
!  allocate(pGage(iTotalNumSites),stat=iStat)
!  allocate(pGage(size(pSITE)),stat=iStat)
  allocate(pTS(size(pSITE)),stat=iStat)
  call Assert(iStat==0,"Problem allocating memory for time series data structure", &
     TRIM(__FILE__),__LINE__)
  call writelog("Allocated space for "//trim(asChar(size(pSITE)))//" sites")

  ! ALLOCATE memory for the time-series data associated with each site
  iNewIndex = 0
  do iSiteNum = 1,iTotalNumSites
    if(isElement(sSiteIDArray(iSiteNum),pSITE)) then
      iNewIndex = iNewIndex + 1
!      allocate(pGage(iNewIndex)%pGageData(iTotalNumLines(iSiteNum)),stat=iStat)
      allocate(pTS(iNewIndex)%tData(iTotalNumLines(iSiteNum)),stat=iStat)
      call Assert(iStat==0,"Problem allocating memory for time series data structure", &
        TRIM(__FILE__),__LINE__)
      call echolog(" => Site "//trim(sSiteIDArray(iSiteNum))//" has "// &
        trim(asChar(iTotalNumLines(iSiteNum)))//" data elements")
    endif
  end do

  iLineNum = 0
  iSiteNum = 0
  sOldSiteID = ""

  ! now make second pass through the NWIS file, commiting values to memory
  do
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
    if(iStat /= 0) then
      exit
    end if

    if(iStat /= 0) exit
!    if(sRecord(1:24) .eq. '# Data for the following') then  ! next lines describe the gages
!      do
!        read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
!        if(sRecord(1:5) .eq. '# ---') exit
!        iTotalNumGages = iTotalNumGages + 1
!      end do
!      allocate(iTotalNumLines(iTotalNumGages))
!      iTotalNumLines = 0
!      cycle
    if(sRecord(1:9) .eq. 'agency_cd') then
      read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
!      iSiteNum = iSiteNum + 1
      cycle
    else if(sRecord(1:1) .eq. "#") then
      cycle
    endif

    ! read in the agency code and site ID
    call Chomp_tab(sRecord, sAgencyCode)
    call Chomp_tab(sRecord, sSiteID)

    ! if current site ID is not on the list of desired sites, read next line
    if(.not. isElement(sSiteID, pSITE)) cycle

    if(.not. str_compare(sSiteID, sOldSiteID)) then
      sOldSiteID = sSiteID
      iSiteNum = iSiteNum + 1
      iLineNum = 0
!      pGage(iSiteNum)%sAgencyCode = TRIM(ADJUSTL(sAgencyCode))
!      pGage(iSiteNum)%sSiteNumber = TRIM(ADJUSTL(sSiteID))
      pTS(iSiteNum)%sSeriesName = TRIM(ADJUSTL(sSiteID))
    endif

    ! obtain a value for the DATE field
    call Chomp_tab(sRecord,sDateTxt)

    call tCurrDate%parseDate(sDateTxt)
    call tCurrDate%parseTime("12:00:00")
    call tCurrDate%calcJulianDay()

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    ! obtain a value for the DISCHARGE and DATA FLAG fields
    call Chomp_tab(sRecord, sItem)

    iIceFlag = max(index(sItem,"_Ice"),index(sItem,"_ice"))

    if(len_trim(sItem)>0 .and. iIceFlag == 0 .and. scan(sItem,"_") == 0) then
      iLineNum = iLineNum + 1
!      read(sItem,fmt=*) pGage(iSiteNum)%pGageData(iLineNum)%rMeanDischarge
      read(sItem,fmt=*) pTS(iSiteNum)%tData(iLineNum)%rValue
      call Chomp_tab(sRecord, sItem)
!      pGage(iSiteNum)%pGageData(iLineNum)%sDataFlag = TRIM(sItem)
      pTS(iSiteNum)%tData(iLineNum)%sDataFlag = TRIM(sItem)
    else
      cycle
    end if

      ! we have valid data; record the date
!    pGage(iSiteNum)%pGageData(iLineNum)%tDT = tCurrDate
    pTS(iSiteNum)%tData(iLineNum)%tDT = tCurrDate

    ! calculate a value for the WATER YEAR field
!    call pGage(iSiteNum)%pGageData(iLineNum)%tDT%calcWaterYear()
    call pTS(iSiteNum)%tData(iLineNum)%tDT%calcWaterYear()

  end do

!  allocate(tTS(size(pGage)),stat=iStat)
!  call Assert(iStat==0,"Memory allocation error",trim(__FILE__),__LINE__)

  do i=1,size(pTS)

!    sNewSeriesName = pNEW_SERIES_NAME(i)
     pTS(i)%sSeriesname = pNEW_SERIES_NAME(i)
!    if(size(pGage(i)%pGageData) > 0) then            ! if there are no records, no point in adding
    if(size(pTS(i)%tData) > 0) then            ! if there are no records, no point in adding
      do j=1,size(pDESCRIPTION)
        if(index(trim(pDESCRIPTION(j)), trim(pTS(i)%sSeriesName)) > 0) &
          pTS(i)%sDescription = trim(pDESCRIPTION(j))
      enddo
      call pTS(i)%findDateMinAndMax()
      pTempSeries => pTS(i)
!      call TS%addSeries(pTempSeries)
      call TS%insert(pNewSeries=pTempSeries)
    endif

  end do

  close(unit=LU_DATA)

  deallocate(pFILE); deallocate(pSITE); deallocate(pNEW_SERIES_NAME)
  deallocate(pDESCRIPTION)

end subroutine get_mul_series_usgs_nwis

!------------------------------------------------------------------------------

subroutine get_mul_series_ssf(pBlock, TS)

  implicit none

  type (T_BLOCK), pointer :: pBlock
  type (TIME_SERIES_COLLECTION), intent(inout) :: TS

  ! [ LOCALS ]
  type (T_TIME_SERIES), dimension(:), pointer :: pTS

  integer (kind=T_INT) :: iMM, iDD, iYYYY
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iLineNum = 0
  character (len=MAXNAMELENGTH) :: sOldSiteID
  character (len=MAXNAMELENGTH) :: sSiteID
  character (len=MAXNAMELENGTH) :: sTempSiteID
  character (len=MAXARGLENGTH) :: sFilename

  integer (kind=T_INT) :: iMonth, iDay, iYear, iJulianDay, iWaterYear

  integer (kind=T_INT) :: iCount
  integer (kind=T_INT) :: i, j
  integer (kind=T_INT),dimension(1000)  :: iTotalNumLines

  character (len=MAXNAMELENGTH), dimension(1000) :: sSiteIDArray

  type (T_TIME_SERIES), pointer :: pTempSeries
  integer (kind=T_INT) :: iSiteNum
  integer (kind=T_INT) :: iTotalNumSites
  integer (kind=T_INT) :: iNewIndex

  character (len=2), parameter :: DELIMITERS = ACHAR(9)//" "

  type (T_DATETIME) :: tDATETIME_1, tDATETIME_2, tCurrDate

  character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pSITE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pNEW_SERIES_NAME

  character (len=256) sRecord, sItem
  character (len=256) sDateTxt, sTimeTxt

  iCount = 0; i=0; iTotalNumLines = 0; iSiteNum = 0; iTotalNumSites = 0
  sOldSiteID = ""; sSiteID = ""

  call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)

  ! obtain user-entered values from TSPROC block
  pFILE => pBlock%getString("FILE")
  pSITE => pBlock%getString("SITE")
  pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")

  close(LU_DATA)
  open(unit=newunit(LU_DATA),file=TRIM(ADJUSTL(pFILE(1))),status='OLD',iostat=iStat)
  call Assert(iStat==0,'Error opening file or missing file: '//TRIM(pFILE(1)), &
    TRIM(__FILE__), __LINE__)

  call echolog('Opened SSF file: '//TRIM(pFILE(1)))

  ! make first pass through the SSF file to determine what it contains
  do
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
    if(iStat /= 0) then
      exit
    elseif(sRecord(1:1) .eq. "#") then
      cycle
    end if

    call Chomp(sRecord,sItem, DELIMITERS)  ! site id
    sSiteID = trim(sItem)
    if(.not. str_compare(sSiteID, sOldSiteID)) then
      sOldSiteID = sSiteID
      iSiteNum = iSiteNum + 1
      sSiteIDArray(iSiteNum) = sSiteID
    endif

    call Chomp(sRecord,sDateTxt, DELIMITERS)  ! date
    call Chomp(sRecord,sTimeTxt, DELIMITERS)  ! time

    call tCurrDate%parseDate(sDateTxt)
    call tCurrDate%parseTime(sTimeTxt)
    call tCurrDate%calcJulianDay()

    call writelog(quote(sSiteID)//" | "// tCurrDate%listdate())

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    if(str_compare(sSiteID,"04060500") ) then
      call writelog(" ** ")
    endif

    call Chomp(sRecord,sItem, DELIMITERS)  ! discharge and data flag

    if(len_trim( sItem ) > 0 ) &
      iTotalNumLines(iSiteNum) = iTotalNumLines(iSiteNum) + 1
  end do

  iTotalNumSites = iSiteNum

  call echolog("Number of data lines in SSF file: "//trim(asChar(SUM(iTotalNumLines))) )

  ! check to see if the SITEs given by user are actually found in the SSF file
  if(.not. str_compare(pSITE(1),"NA")) then
    do j=1,size(pSITE)
      call Assert(isElement(pSITE(j),sSiteIDArray), &
        "Site "//trim(pSITE(j))//" was not found in SSF file "//TRIM(pFILE(1)))
    enddo
  endif

  ! if the user has not supplied series names, we assume that the site IDs in the
  ! file are sufficient and that all data elements should be read in
  if(str_compare(pSITE(1),"NA")) then
    deallocate(pSITE)
    deallocate(pNEW_SERIES_NAME)
    allocate(pSITE(iTotalNumSites))
    allocate(pNEW_SERIES_NAME(iTotalNumSites))
    pSITE = sSiteIDArray(1:iTotalNumSites)
    pNEW_SERIES_NAME = sSiteIDArray(1:iTotalNumSites)
  endif

  ! rewind the file and make a second pass through the file
  rewind(unit=LU_DATA, iostat=iStat)
  call Assert(iStat==0,"Problem rewinding SSF data file")

  allocate(pTS(size(pSITE)),stat=iStat)
  call Assert(iStat==0,"Problem allocating memory for SSF data structure", &
     TRIM(__FILE__),__LINE__)
  call writelog("Allocated space for "//trim(asChar(size(pSITE)))//" sites")

  ! ALLOCATE memory for the time-series data associated with each site
  iNewIndex = 0
  do iSiteNum = 1,iTotalNumSites
    if(isElement(sSiteIDArray(iSiteNum),pSITE)) then
      iNewIndex = iNewIndex + 1
      allocate(pTS(iNewIndex)%tData(iTotalNumLines(iSiteNum)),stat=iStat)
      call Assert(iStat==0,"Problem allocating memory for SSF data structure", &
        TRIM(__FILE__),__LINE__)
      call echolog("Site number "//trim(sSiteIDArray(iSiteNum))//" has "// &
        trim(asChar(iTotalNumLines(iSiteNum)))//" data elements")
    endif
  end do

  !
  !
  ! TODO: change code so that the sitename absolutely corresponds to the proper
  !       user-specified site number and new series name
  !
  !

  iLineNum = 0
  iSiteNum = 0
  sOldSiteID = ""

  ! now make second pass through the SSF file, commiting values to memory
  do
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
    if(iStat /= 0) then
      exit
    elseif(sRecord(1:1) .eq. "#") then
      cycle
    end if

    ! get the SITE ID from the SSF file
    call Chomp(sRecord,sItem, DELIMITERS)
    sSiteID = trim(sItem)

    ! if the site ID read from the SSF file is not en element of the list
    ! of sites specified by the user, IGNORE and read the next line!
    if(.not. isElement(sSiteID,pSITE)) cycle

    ! if the current site ID is not the same as the one read during last iteration,
    ! we must have begun reading data corresponding to a different site
    if(.not. str_compare(sSiteID, sOldSiteID)) then
      sOldSiteID = sSiteID
      iSiteNum = iSiteNum + 1
      iLineNum = 0
!      pGage(iSiteNum)%sSiteNumber = sSiteID
!      pTS(iSiteNum)%sSeriesName = sSiteID
       pTS(iSiteNum)%sSeriesName = sSiteID
    endif

    ! obtain a value for the DATE field
    call Chomp(sRecord, sItem, DELIMITERS)
    sDateTxt = trim(sItem)

    ! obtain a value for the TIME field
    call Chomp(sRecord, sItem, DELIMITERS)
    sTimeTxt = trim(sItem)

    call tCurrDate%parseDate(sDateTxt)
    call tCurrDate%parseTime(sTimeTxt)
    call tCurrDate%calcJulianDay()

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    ! obtain a value for the DISCHARGE and DATA FLAG fields
    call Chomp(sRecord, sItem, DELIMITERS)

    if(len_trim(sItem)>0) then
      iLineNum = iLineNum + 1
!      read(sItem,fmt=*) pGage(iSiteNum)%pGageData(iLineNum)%rMeanDischarge
      read(sItem,fmt=*) pTS(iSiteNum)%tData(iLineNum)%rValue
      call Chomp(sRecord, sItem, DELIMITERS)
!      pGage(iSiteNum)%pGageData(iLineNum)%sDataFlag = TRIM(sItem)
      pTS(iSiteNum)%tData(iLineNum)%sDataFlag = TRIM(sItem)
    else
      cycle
    end if

    ! we have valid data; record the date
    pTS(iSiteNum)%tData(iLineNum)%tDT = tCurrDate
    call pTS(iSiteNum)%tData(iLineNum)%tDT%calcWaterYear()

  end do

  do i=1,size(pTS)
    if(size(pTS(i)%tData) > 0) then    ! if there are no records, no point in adding

      pTS(i)%sDescription = "Series "//quote(pNEW_SERIES_NAME(i))//" from SSF file " &
         //quote(pFILE(1))//", site "//trim(pSITE(i))
      pTS(i)%sSeriesName = pNEW_SERIES_NAME(i)
!      call tTS(i)%new(pGage(i),sNewSeriesName)    ! a time series object
      pTempSeries => pTS(i)
      call pTempSeries%findDateMinAndMax()
!      call TS%addSeries(pTempSeries)
      call TS%insert(pNewSeries=pTempSeries)
!      call echolog(" ==> Added series "//quote(pTS(i)%sSeriesname)//" from site " &
!         //trim(pSITE(i))//"; "//trim(asChar(size(pTS(i)%tData)))//" data elements")

    endif

  end do

  close(unit=LU_DATA)

  nullify(pTempSeries)
  if(associated(pFILE) ) deallocate(pFILE)
  if(associated(pSITE) ) deallocate(pSITE)
  if(associated(pNEW_SERIES_NAME) ) deallocate(pNEW_SERIES_NAME)

end subroutine get_mul_series_ssf

!------------------------------------------------------------------------------

subroutine get_mul_series_ssf_fast(pBlock, TS)

  implicit none

  type (T_BLOCK), pointer :: pBlock
  type (TIME_SERIES_COLLECTION), intent(inout) :: TS

  ! [ LOCALS ]
  type (T_TIME_SERIES), pointer :: pTS

  integer (kind=T_INT) :: iMM, iDD, iYYYY
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iLineNum = 0
  character (len=MAXNAMELENGTH) :: sOldSiteID
  character (len=MAXNAMELENGTH) :: sSiteID
  character (len=MAXARGLENGTH) :: sFilename

  real (kind=T_SGL) :: rValue

  integer (kind=T_INT) :: iMonth, iDay, iYear, iJulianDay, iWaterYear

  integer (kind=T_INT) :: iCount
  integer (kind=T_INT) :: i, j
  logical (kind=T_LOGICAL) :: lMatch
  logical (kind=T_LOGICAL) :: lSkipThisSite
  logical (kind=T_LOGICAL) :: lFirstTimeThrough

  type (T_TIME_SERIES), pointer :: pTempSeries
  integer (kind=T_INT) :: iSiteNum
  integer (kind=T_INT) :: iTotalNumSites

  character (len=2), parameter :: DELIMITERS = ACHAR(9)//" "

  type (T_DATETIME) :: tDATETIME_1, tDATETIME_2, tCurrDate

  character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pSITE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pNEW_SERIES_NAME

  character (len=256) sRecord, sItem
  character (len=256) sDateTxt, sTimeTxt

  iCount = 0; i=0 ; iSiteNum = 0
  sOldSiteID = ""; sSiteID = ""
  lFirstTimeThrough = lTRUE

  call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)

  ! obtain user-entered values from TSPROC block
  pFILE => pBlock%getString("FILE")
  pSITE => pBlock%getString("SITE")
  pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")

  close(LU_DATA)
  open(unit=newunit(LU_DATA),file=TRIM(ADJUSTL(pFILE(1))),status='OLD',iostat=iStat)
  call Assert(iStat==0,'Error opening file or missing file: '//TRIM(pFILE(1)), &
    TRIM(__FILE__), __LINE__)

  call echolog('Opened SSF file: '//TRIM(pFILE(1)))

  lSkipThisSite = lFALSE

  ! begin reading SSF file
  do
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
    if(iStat /= 0) then
      ! process the temporary file one last time to capture the
      ! last time series read from the SSF file
      if( .not. lSkipThisSite ) then
        call pTS%resizeTemp()
        call pTS%findDateMinAndMax()
!        call TS%addSeries(pTS)
        call TS%insert(pNewSeries=pTS)
        nullify(pTS)
        allocate(pTS, stat=iStat)
        call assert(iStat==0, "Memory allocation error", &
          trim(__FILE__),__LINE__)
        call pTS%newTemp()
      endif
      exit
    elseif(sRecord(1:1) .eq. "#") then
      cycle
    end if

    call Chomp(sRecord,sItem, DELIMITERS)  ! site id
    sSiteID = trim(sItem)

    ! are we reading a record from a new site?
    if(.not. str_compare(sSiteID, sOldSiteID)) then
      sOldSiteID = sSiteID
      iSiteNum = iSiteNum + 1
      ! if this is not the first time we detect a change between the old site id
      ! and the current site id, then assume we have a time series to add
      if( (.not. lFirstTimeThrough) .and. (.not. lSkipThisSite)) then
        call pTS%resizeTemp()
        call pTS%findDateMinAndMax()
!        call TS%addSeries(pTS)
        call TS%insert(pNewSeries=pTS)
        nullify(pTS)
        allocate(pTS, stat=iStat)
        call assert(iStat==0, "Memory allocation error", &
          trim(__FILE__),__LINE__)
        call pTS%newTemp()
      else
        lFirstTimeThrough = lFALSE
        allocate(pTS, stat=iStat)
        call assert(iStat==0, "Memory allocation error", &
          trim(__FILE__),__LINE__)
        call pTS%newTemp()
      endif

      ! has the user specified a list of site ids? If so, is this site on the list?
      if(.not. str_compare(pSITE(1),"NA")) then
        lMatch = lFALSE
        do j=1,size(pSITE)
          if(str_compare(pSITE(j),sSiteID)) then
            lMatch = lTRUE
            pTS%sDescription = "Series "//quote(pNEW_SERIES_NAME(j))//" from SSF file " &
             //quote(pFILE(1))//", site "//trim(pSITE(j))
            pTS%sSeriesName = pNEW_SERIES_NAME(j)
            exit
          endif
        enddo

        ! did we find a matching site id in the users list of requested sites?
        if(lMatch) then
          lSkipThisSite = lFALSE
        else
          lSkipThisSite = lTRUE
        endif
      else  ! user has not specified a list of sites; assume that all sites are requested
        lSkipThisSite = lFALSE
        pTS%sDescription = "Series "//quote(pSITE(j))//" from SSF file " &
           //quote(pFILE(1))//", site "//trim(pSITE(j))
        pTS%sSeriesName = pSITE(j)
      endif

    endif

    if(lSkipThisSite) cycle

    call Chomp(sRecord,sDateTxt, DELIMITERS)  ! date
    call Chomp(sRecord,sTimeTxt, DELIMITERS)  ! time

    call tCurrDate%parseDate(sDateTxt)
    call tCurrDate%parseTime(sTimeTxt)
    call tCurrDate%calcJulianDay()

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    call Chomp(sRecord,sItem, DELIMITERS)  ! discharge and data flag

    if(len_trim(sItem)>0) then
      read(sItem,fmt=*) rValue
      call Chomp(sRecord, sItem, DELIMITERS)
      call pTS%addTemp(rValue, tCurrDate, trim(sItem))
    else
      cycle
    end if

  end do

  close(unit=LU_DATA)

  nullify(pTS)
  if(associated(pFILE) ) deallocate(pFILE)
  if(associated(pSITE) ) deallocate(pSITE)
  if(associated(pNEW_SERIES_NAME) ) deallocate(pNEW_SERIES_NAME)

end subroutine get_mul_series_ssf_fast

!------------------------------------------------------------------------------

subroutine get_mul_series_statvar(pBlock, TS)

  implicit none

  type (T_BLOCK), pointer :: pBlock
  type (TIME_SERIES_COLLECTION), intent(inout) :: TS

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat, iStat2
  integer (kind=T_INT) :: iTotalNumLines
  integer (kind=T_INT) :: iLineNum
  character (len=MAXARGLENGTH) :: sFilename

  real (kind=T_SGL), dimension(:), allocatable  :: rStatvarValues

  integer (kind=T_INT) :: iMonth, iDay, iYear, iJulianDay, iWaterYear
  integer (kind=T_INT) :: iHour, iMinute, iSecond, iRecNum

  integer (kind=T_INT) :: iCount
  integer (kind=T_INT) :: i, j
  integer (kind=T_INT) :: iNumVariables
  integer (kind=T_INT) :: iNumOutputVariables
  logical (kind=T_LOGICAL) :: lMatch
  type (T_TIME_SERIES), pointer :: pTempSeries
  type (T_TIME_SERIES), dimension(:), pointer :: pTS
  type (T_DATETIME) :: tDATETIME_1, tDATETIME_2, tCurrDate

  character (len=256), dimension(:), allocatable :: sPRMSVariable
  integer (kind=T_INT), dimension(:), allocatable :: iVariableNameIndex

  character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pNEW_SERIES_NAME
  character (len=MAXARGLENGTH), dimension(:), pointer :: pDATE_FORMAT
  character (len=MAXARGLENGTH), dimension(:), pointer :: pVARIABLE_NAME
  character (len=MAXARGLENGTH), dimension(:), pointer :: pLOCATION_ID

  character (len=256) sRecord, sItem
  character (len=256) sDateTxt, sTimeTxt
  character (len=4096) :: sBuf
  character (len=32) :: sDateFormat

  iCount = 0; i=0

  ! process any user-supplied date keywords
  call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)

  ! obtain user-entered values from TSPROC block
  pFILE => pBlock%getString("FILE")
  pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
  pDATE_FORMAT => pBlock%getString("DATE_FORMAT")
  pVARIABLE_NAME => pBlock%getString("VARIABLE_NAME")
  pLOCATION_ID => pBlock%getString("LOCATION_ID")

  allocate (iVariableNameIndex(size(pVARIABLE_NAME)), stat = iStat )
  call Assert(iStat ==0, "Problem allocating memory to store output variable indices", &
    trim(__FILE__),__LINE__)

  if(str_compare(pDATE_FORMAT(1),"NA")) then
    sDateFormat = sDEFAULT_DATE_FORMAT
  else
    sDateFormat = trim(pDATE_FORMAT(1))
  endif

  open(unit=newunit(LU_DATA),file=TRIM(ADJUSTL(pFILE(1))),status='OLD',iostat=iStat)
  call Assert(iStat==0,'Error opening file or missing file: '//TRIM(pFILE(1)), &
    TRIM(__FILE__), __LINE__)

  call echolog('Opened PRMS statvar file: '//TRIM(pFILE(1)))

  ! read in the number of PRMS variables that are contained in the statvar file
  read(unit=LU_DATA, fmt=*,iostat=iStat) iNumVariables
  call Assert( iStat == 0, "Problem reading the number of variables contained in " &
    //"statvar file "//quote(pFILE(1) ), trim(__FILE__), __LINE__)


  allocate(sPRMSVariable(iNumVariables), stat=iStat)
  call Assert(iStat ==0, "Problem allocating memory to store PRMS variable names", &
    trim(__FILE__),__LINE__)

  allocate(rStatvarValues(iNumVariables), stat=iStat)
  call Assert(iStat ==0, "Problem allocating memory to store PRMS variable values", &
    trim(__FILE__),__LINE__)

  ! now read in the statvar variable names and location ids
  do i=1,iNumVariables
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
    call chomp(sRecord, sItem)
    sPRMSVariable(i) = trim(sItem)//"_"//trim(sRecord)
  enddo

  ! count number of entries within the window defined by DATETIME_1 and DATETIME_2
  iTotalNumLines = 0
  do

    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sBuf
    read(unit=sBuf, fmt=*,iostat=iStat2) iRecNum, iYear, iMonth, iDay, &
         iHour, iMinute, iSecond

    if(iStat /= 0 .or. iStat2 /= 0) then
      exit
    end if

    call tCurrDate%calcJulianDay(iMonth, iDay, iYear, iHour, iMinute, iSecond)

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle
    iTotalNumLines = iTotalNumLines + 1

  end do

  ! check to see if the VARIABLE_NAMEs given by user are actually found in the statvar file
  if(.not. str_compare(pVARIABLE_NAME(1),"NA")) then
    do i=1,size(pVARIABLE_NAME)
      lMatch = lFALSE
      do j=1,iNumVariables
        if(str_compare(trim(pVARIABLE_NAME(i))//"_"//trim(pLOCATION_ID(i)),sPRMSVariable(j) ) ) then
            lMatch = lTRUE
            iVariableNameIndex(i) = j
        endif
      enddo
      call Assert(lMatch, "Variable "//quote(pVARIABLE_NAME(i))//", with location ID " &
        //trim(pLOCATION_ID(i))//" was not found in statvar file "//TRIM(pFILE(1)))
    enddo
  endif

  ! if the user has not supplied series names, we assume that the variable names and IDs in the
  ! file are sufficient and that all data elements should be read in
  if(str_compare(pVARIABLE_NAME(1),"NA")) then
    deallocate(pVARIABLE_NAME)
    deallocate(pNEW_SERIES_NAME)
    deallocate(iVariableNameIndex)
    allocate(pVARIABLE_NAME(iNumVariables))
    allocate(pNEW_SERIES_NAME(iNumVariables))
    allocate(iVariableNameIndex(iNumVariables))
    pVARIABLE_NAME = sPRMSVariable
    pNEW_SERIES_NAME = sPRMSVariable
    do i=1,iNumVariables
      iVariableNameIndex(i) = i
    enddo
  endif

  iNumOutputVariables = size(pVARIABLE_NAME)

  ! rewind the file and make a second pass through the file
  rewind(unit=LU_DATA, iostat=iStat)
  call Assert(iStat==0,"Problem rewinding statvar data file")

  ! allocate memory for an array of time series objects
  allocate(pTS(iNumOutputVariables),stat=iStat)
  call Assert(iStat==0,"Problem allocating memory for statvar data structure", &
     TRIM(__FILE__),__LINE__)
  call writelog("Allocated space for "//trim(asChar(iNumOutputVariables))//" variables")

  ! ALLOCATE memory for the time-series data associated with each output variable
  do i=1,iNumOutputVariables
    allocate(pTS(i)%tData(iTotalNumLines),stat=iStat)
    call Assert(iStat==0,"Problem allocating memory for statvar data structure", &
      TRIM(__FILE__),__LINE__)
  enddo

  ! now make second pass through the statvar file, comitting values to memory
  ! first, plough through the header and variable definitions
  do i=1,iNumVariables + 1
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sRecord
  enddo

  iLineNum = 0
  do
    read(unit=LU_DATA, fmt="(a)",iostat=iStat) sBuf
    read(unit=sBuf, fmt=*,iostat=iStat2) iRecNum, iYear, iMonth, iDay, &
         iHour, iMinute, iSecond, (rStatvarValues(j), j=1,iNumVariables)

    if(iStat /= 0) exit
    if(iStat2 /= 0) call Assert(lFALSE, &
      "Problem reading MM/DD/YYYY HH:MM:SS from SSF file; line = "//quote(sBuf) )

    call tCurrDate%calcJulianDay(iMonth, iDay, iYear, iHour, iMinute, iSecond)

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    iLineNum = iLineNum + 1

    do j=1,iNumOutputVariables
!      call assert(j<=ubound(pTS,1), "upper bound of pTS exceeded: "//trim(asChar(j)), &
!        trim(__FILE__),__LINE__)
!      call assert(iLineNum<=ubound(pTS(j)%tData,1), "upper bound of pTS%tData exceeded: " &
!        //trim(asChar(j)), trim(__FILE__),__LINE__)
!      call assert(iLineNum>=lbound(pTS(j)%tData,1), "lower bound of pTS%tData exceeded: " &
!        //trim(asChar(j)), trim(__FILE__),__LINE__)
!      call assert(iVariableNameIndex(j)>=lbound(rStatvarValues,1), "lower bound of rStatvarValues exceeded: " &
!        //trim(asChar(iVariableNameIndex(j))), trim(__FILE__),__LINE__)
!      call assert(iVariableNameIndex(j)<=ubound(rStatvarValues,1), "upper bound of rStatvarValues exceeded: " &
!        //trim(asChar(iVariableNameIndex(j))), trim(__FILE__),__LINE__)

      pTS(j)%tData(iLineNum)%rValue = rStatvarValues(iVariableNameIndex(j) )
      pTS(j)%tData(iLineNum)%tDT = tCurrDate
      call pTS(j)%tData(iLineNum)%tDT%calcWaterYear()
    enddo

  end do

  nullify(pTempSeries)
  do i=1,size(pTS)
    if(size(pTS(i)%tData) > 0) then    ! if there are no records, no point in adding
      pTS(i)%sDescription = "Series "//quote(pNEW_SERIES_NAME(i))//" from statvar file " &
         //quote(pFILE(1))
      pTS(i)%sSeriesName = pNEW_SERIES_NAME(i)

      pTempSeries => pTS(i)
      call pTempSeries%findDateMinAndMax()
!      call TS%addSeries(pTempSeries)
      call TS%insert(pNewSeries=pTempSeries)
      nullify(pTempSeries)
!     call echolog(" ==> Added series "//quote(pTS(i)%sSeriesname))
    endif

  end do

  close(unit=LU_DATA)

  if(associated(pFILE) )  deallocate(pFILE)
  if(associated(pVARIABLE_NAME) )  deallocate(pVARIABLE_NAME)
  if(associated(pNEW_SERIES_NAME) )  deallocate(pNEW_SERIES_NAME)
  if(associated(pLOCATION_ID) )  deallocate(pLOCATION_ID)
  if(associated(pDATE_FORMAT) )  deallocate(pDATE_FORMAT)

end subroutine get_mul_series_statvar

!------------------------------------------------------------------------------

subroutine get_series_wdm(pBlock, TS)

  implicit none

  type (T_BLOCK), pointer :: pBlock
  type (TIME_SERIES_COLLECTION), intent(inout) :: TS

  ! [ LOCALS ]
  character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pSITE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pDSN
  character (len=MAXARGLENGTH), dimension(:), pointer :: pNEW_SERIES_NAME

  integer (kind=T_INT) :: LU_WDM
  integer (kind=T_INT) :: iReturnCode
  integer (kind=T_INT) :: iCount
  integer (kind=T_INT) :: TSTEP, TCODE, RETCODE
  integer (kind=T_INT) :: iDSN, i
  integer (kind=T_INT) :: STRT(6), STOP(6)
  integer (kind=T_INT) :: LSDAT(6), LEDAT(6)
  integer (kind=T_INT) :: iRecLength
  character (len=64) :: sFilename

  type (T_DATETIME) :: tStartDate
  type (T_DATETIME) :: tEndDate
  type (T_DATETIME) :: tDATETIME_1
  type (T_DATETIME) :: tDATETIME_2

  type (T_TIME_SERIES), pointer :: pTS

  integer (kind=T_INT), dimension(:), allocatable :: iMM, iDD, iYY, iHour, iMin, iSec
  type (T_DATETIME), dimension(:), allocatable :: tDate
  real (kind=T_SGL), dimension(:), allocatable :: rValue
  logical (kind=T_LOGICAL), dimension(:), allocatable :: lSelect

  character (len=256) sNewSeriesName

  integer, parameter :: SECOND = 1
  integer, parameter :: MINUTE = 2
  integer, parameter :: HOUR = 3
  integer, parameter :: DAY = 4
  integer, parameter :: MONTH = 5
  integer, parameter :: YEAR = 6

  LU_WDM = 87

  call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)

  pFILE => pBlock%getString("FILE")
  pDSN => pBlock%getString("DSN")
  if(.not. str_compare(pDSN(1), "NA") ) then
    iDSN = asInt(pDSN(1) )
  else
    iDSN = 1
  endif

  pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
  if(.not. str_compare(pNEW_SERIES_NAME(1), "NA") ) then
    sNewSeriesName = pNEW_SERIES_NAME(1)
  else
    sNewSeriesName = "HSPF_Series"
  endif

  sFilename = trim(pFILE(1) )

  call WDBOPN(LU_WDM, sFilename, 1, iReturnCode)

  call WDATIM(LU_WDM, iDSN, STRT, STOP, TSTEP, TCODE, RETCODE)

  call tStartDate%calcJulianDay(STRT(2), STRT(3), STRT(1), STRT(4), STRT(5), STRT(6) )
  call tEndDate%calcJulianDay(STOP(2), STOP(3), STOP(1), STOP(4), STOP(5), STOP(6) )

  if(tcode == DAY) then
    iCount = int(tEndDate - tStartDate , kind=T_INT)

  else if(tcode == HOUR) then
    iCount = int(tEndDate - tStartDate , kind=T_INT) * 24

  else if(tcode == MINUTE) then
    iCount = int(tEndDate - tStartDate , kind=T_INT) * 24 * 60

  else if(tcode == SECOND) then
    iCount = int(tEndDate - tStartDate , kind=T_INT) * 86400

  else if(tcode == MONTH) then
    iCount = int(tEndDate - tStartDate , kind=T_INT) / 12 + 2

  else
    iCount = int(tEndDate - tStartDate , kind=T_INT) / 365.25 +2

  end if
  if(tstep /=0 ) then
    iCount = iCount / tstep
  end if

!  print *, iCount
!  print *, tStartDate%prettyDate()
!  print *, tEndDate%prettyDate()

  allocate(iMM(iCount), iDD(iCount), iYY(iCount), iHour(iCount), iMin(iCount), iSec(iCount) )
  allocate(tDate(iCount) )
  allocate(rValue(iCount) )
  allocate(lSelect(iCount) )

  call PRWMTE(LU_WDM, iDSN, STRT, STOP, iCount, &
        iMM, iDD, iYY, iHour, iMin, iSec, rValue)

  lSelect = lTRUE

  do i=1,iCount

    call tDate(i)%calcJulianDay(iMM(i), iDD(i), iYY(i), &
        iHour(i), iMin(i), iSec(i))
!    print *, trim(tDate(i)%listdatetime() )//": "//trim(asChar(rValue(i)))

    if(tDate(i) < tDATETIME_1) lSelect(i) = lFALSE
    if(tDate(i) > tDATETIME_2) lSelect(i) = lFALSE

  enddo

  ! for now, only allow one TS per block to be added

  call pTS%newFmDT(sNewSeriesName, "Series from WDM file", &
     pack(tDate, lSelect), pack(rValue, lSelect) )

!  call TS%addSeries(pTS)
  call TS%insert(pNewSeries=pTS)

  end subroutine get_series_wdm

end module tsp_file_readers
