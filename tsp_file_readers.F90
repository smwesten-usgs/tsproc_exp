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

subroutine read_USGS_NWIS(pBlock, TS)

  implicit none

  type (T_BLOCK), pointer :: pBlock
  type (TIME_SERIES_COLLECTION), intent(inout) :: TS
  type(T_USGS_NWIS_GAGE),dimension(:), pointer :: pGage
  type(T_USGS_NWIS_DAILY),dimension(:), allocatable :: pTempDaily

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
  type (T_TIME_SERIES), dimension(:), allocatable :: tTS
  integer (kind=T_INT) :: iSiteNum
  integer (kind=T_INT) :: iTotalNumSites
  integer (kind=T_INT) :: iNewIndex

  character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
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
  call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)

  ! get user supplied values for recognized keywords
  pFILE => pBlock%getString("FILE")
  pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
  pSITE => pBlock%getString("SITE")

  open(newunit=LU_DATA,file=TRIM(ADJUSTL(pFILE(1))),status='OLD',iostat=iStat)
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

    call tCurrDate%parseDate(sDateTxt, "YYYY-MM-DD")
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
  allocate(pGage(size(pSITE)),stat=iStat)
  call Assert(iStat==0,"Problem allocating memory for NWIS data structure", &
     TRIM(__FILE__),__LINE__)
  call writelog("Allocated space for "//trim(asChar(size(pSITE)))//" sites")

  ! ALLOCATE memory for the time-series data associated with each site
  iNewIndex = 0
  do iSiteNum = 1,iTotalNumSites
    if(isElement(sSiteIDArray(iSiteNum),pSITE)) then
      iNewIndex = iNewIndex + 1
      allocate(pGage(iNewIndex)%pGageData(iTotalNumLines(iSiteNum)),stat=iStat)
      call Assert(iStat==0,"Problem allocating memory for NWIS data structure", &
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
      pGage(iSiteNum)%sAgencyCode = TRIM(ADJUSTL(sAgencyCode))
      pGage(iSiteNum)%sSiteNumber = TRIM(ADJUSTL(sSiteID))
    endif

    ! obtain a value for the DATE field
    call Chomp_tab(sRecord,sDateTxt)

    call tCurrDate%parseDate(sDateTxt, "YYYY-MM-DD")
    call tCurrDate%parseTime("12:00:00")
    call tCurrDate%calcJulianDay()

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    ! obtain a value for the DISCHARGE and DATA FLAG fields
    call Chomp_tab(sRecord, sItem)

    iIceFlag = max(index(sItem,"_Ice"),index(sItem,"_ice"))

    if(len_trim(sItem)>0 .and. iIceFlag == 0 .and. scan(sItem,"_") == 0) then
      iLineNum = iLineNum + 1
      read(sItem,fmt=*) pGage(iSiteNum)%pGageData(iLineNum)%rMeanDischarge
      call Chomp_tab(sRecord, sItem)
      pGage(iSiteNum)%pGageData(iLineNum)%sDataFlag = TRIM(sItem)
    else
      cycle
    end if

      ! we have valid data; record the date
    pGage(iSiteNum)%pGageData(iLineNum)%tDT = tCurrDate

    ! calculate a value for the WATER YEAR field
    call pGage(iSiteNum)%pGageData(iLineNum)%tDT%calcWaterYear()

  end do

  allocate(tTS(size(pGage)),stat=iStat)
  call Assert(iStat==0,"Memory allocation error",trim(__FILE__),__LINE__)

  do i=1,size(pGage)

    sNewSeriesName = pNEW_SERIES_NAME(i)
    if(size(pGage(i)%pGageData) > 0) then            ! if there are no records, no point in adding
      do j=1,size(pDESCRIPTION)
        if(index(trim(pDESCRIPTION(j)), trim(pGage(i)%sSiteNumber)) > 0) &
          pGage(i)%sDescription = trim(pDESCRIPTION(j))
      enddo
      call tTS(i)%new(pGage(i),sNewSeriesName)  ! a time series object
      call echolog(" ==> Added series "//quote(tTS(i)%sSeriesname)//" from site " &
         //trim(pSITE(i))//"; "//trim(asChar(size(tTS(i)%tData)))//" data elements")
      call TS%add(tTS(i))
    endif

  end do

  close(unit=LU_DATA)

  deallocate(pFILE); deallocate(pSITE); deallocate(pNEW_SERIES_NAME)
  deallocate(pDESCRIPTION)

end subroutine read_USGS_NWIS

!------------------------------------------------------------------------------

subroutine get_mul_series_ssf(pBlock, TS)

  implicit none

  type (T_BLOCK), pointer :: pBlock
  type (TIME_SERIES_COLLECTION), intent(inout) :: TS

  ! [ LOCALS ]
  type(T_USGS_NWIS_GAGE),dimension(:), pointer :: pGage
  type(T_USGS_NWIS_DAILY),dimension(:), allocatable :: pTempDaily

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
  integer (kind=T_INT),dimension(200)  :: iTotalNumLines
  character (len=MAXNAMELENGTH), dimension(200) :: sSiteIDArray
  type (T_TIME_SERIES), dimension(:), allocatable :: tTS
  integer (kind=T_INT) :: iSiteNum
  integer (kind=T_INT) :: iTotalNumSites
  integer (kind=T_INT) :: iNewIndex

  type (T_DATETIME) :: tDATETIME_1, tDATETIME_2, tCurrDate

  character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pSITE
  character (len=MAXARGLENGTH), dimension(:), pointer :: pNEW_SERIES_NAME
  character (len=MAXARGLENGTH), dimension(:), pointer :: pDATE_FORMAT

  character (len=256) sRecord, sItem
  character (len=256) sDateTxt, sTimeTxt
  character (len=256) sNewSeriesName
  character (len=32) :: sDateFormat


  iCount = 0; i=0; iTotalNumLines = 0; iSiteNum = 0; iTotalNumSites = 0
  sOldSiteID = ""; sSiteID = ""

  call processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)

  ! obtain user-entered values from TSPROC block
  pFILE => pBlock%getString("FILE")
  pSITE => pBlock%getString("SITE")
  pNEW_SERIES_NAME => pBlock%getString("NEW_SERIES_NAME")
  pDATE_FORMAT => pBlock%getString("DATE_FORMAT")

  if(str_compare(pDATE_FORMAT(1),"NA")) then
    sDateFormat = "MM/DD/YYYY"
  else
    sDateFormat = trim(pDATE_FORMAT(1))
  endif

  open(newunit=LU_DATA,file=TRIM(ADJUSTL(pFILE(1))),status='OLD',iostat=iStat)
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

    call Chomp_tab(sRecord,sItem)  ! site id
    sSiteID = trim(sItem)
    if(.not. str_compare(sSiteID, sOldSiteID)) then
      sOldSiteID = sSiteID
      iSiteNum = iSiteNum + 1
      sSiteIDArray(iSiteNum) = sSiteID
    endif

    call Chomp_tab(sRecord,sDateTxt)  ! date
    call Chomp_tab(sRecord,sTimeTxt)  ! time

    call tCurrDate%parseDate(sDateTxt, sDateFormat)
    call tCurrDate%parseTime(sTimeTxt)
    call tCurrDate%calcJulianDay()

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    call Chomp_tab(sRecord,sItem)  ! discharge and data flag

    if(len_trim( sItem ) > 0 ) &
      iTotalNumLines(iSiteNum) = iTotalNumLines(iSiteNum) + 1
  end do

  iTotalNumSites = iSiteNum

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

  ! ALLOCATE memory for the GAGE object (collection of gages)
!  allocate(pGage(iTotalNumSites),stat=iStat)
  allocate(pGage(size(pSITE)),stat=iStat)
  call Assert(iStat==0,"Problem allocating memory for SSF data structure", &
     TRIM(__FILE__),__LINE__)
  call writelog("Allocated space for "//trim(asChar(size(pSITE)))//" sites")

  ! ALLOCATE memory for the time-series data associated with each site
  iNewIndex = 0
  do iSiteNum = 1,iTotalNumSites
    if(isElement(sSiteIDArray(iSiteNum),pSITE)) then
      iNewIndex = iNewIndex + 1
      allocate(pGage(iNewIndex)%pGageData(iTotalNumLines(iSiteNum)),stat=iStat)
      call Assert(iStat==0,"Problem allocating memory for SSF data structure", &
        TRIM(__FILE__),__LINE__)
      call echolog("Site number "//trim(sSiteIDArray(iSiteNum))//" has "// &
        trim(asChar(iTotalNumLines(iSiteNum)))//" data elements")
    endif
  end do

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
    call Chomp_tab(sRecord,sItem)
    sSiteID = trim(sItem)

    if(.not. isElement(sSiteID,pSITE)) cycle

    if(.not. str_compare(sSiteID, sOldSiteID)) then
      sOldSiteID = sSiteID
      iSiteNum = iSiteNum + 1
      iLineNum = 0
      pGage(iSiteNum)%sSiteNumber = sSiteID
    endif

    ! obtain a value for the DATE field
    call Chomp_tab(sRecord, sItem)
    sDateTxt = trim(sItem)

    ! obtain a value for the TIME field
    call Chomp_tab(sRecord, sItem)
    sTimeTxt = trim(sItem)

    call tCurrDate%parseDate(sDateTxt, sDateFormat)
    call tCurrDate%parseTime(sTimeTxt)
    call tCurrDate%calcJulianDay()

    if(tCurrDate < tDATETIME_1) cycle
    if(tCurrDate > tDATETIME_2) cycle

    ! obtain a value for the DISCHARGE and DATA FLAG fields
    call Chomp_tab(sRecord, sItem)

    if(len_trim(sItem)>0) then
      iLineNum = iLineNum + 1
      read(sItem,fmt=*) pGage(iSiteNum)%pGageData(iLineNum)%rMeanDischarge
      call Chomp_tab(sRecord, sItem)
      pGage(iSiteNum)%pGageData(iLineNum)%sDataFlag = TRIM(sItem)
    else
      cycle
    end if

    ! we have valid data; record the date
    pGage(iSiteNum)%pGageData(iLineNum)%tDT = tCurrDate
    call pGage(iSiteNum)%pGageData(iLineNum)%tDT%calcWaterYear()

  end do

  allocate(tTS(size(pGage)),stat=iStat)
  call Assert(iStat==0,"Memory allocation error",trim(__FILE__),__LINE__)

  do i=1,size(pGage)

    sNewSeriesName = pNEW_SERIES_NAME(i)
    if(size(pGage(i)%pGageData) > 0) then    ! if there are no records, no point in adding
      call tTS(i)%new(pGage(i),sNewSeriesName)    ! a time series object
      call echolog(" ==> Added series "//quote(tTS(i)%sSeriesname)//" from site " &
         //trim(pSITE(i))//"; "//trim(asChar(size(tTS(i)%tData)))//" data elements")

      call TS%add(tTS(i))
    endif

  end do

  close(unit=LU_DATA)

  deallocate(pFILE); deallocate(pSITE); deallocate(pNEW_SERIES_NAME)
  deallocate(pDATE_FORMAT)


end subroutine get_mul_series_ssf
subroutine read_param_groups_file()

end subroutine read_param_groups_file

subroutine read_param_data_file()
end subroutine read_param_data_file

end module tsp_file_readers
