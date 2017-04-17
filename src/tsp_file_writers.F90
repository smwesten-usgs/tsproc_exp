module tsp_file_writers

  use tsp_data_structures
  use tsp_collections
  use tsp_time_series_manager
  use tsp_statistics
  use tsp_table_manager
  implicit none

contains

  subroutine write_list_output_block(pBlock, TS)

    type (TIME_SERIES_COLLECTION), intent(inout) :: TS
    type (T_BLOCK), pointer, intent(in) :: pBlock

    ! [ LOCALS ]
    character (len=MAXARGLENGTH), dimension(:), pointer :: pFILE
    character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_NAME
    character (len=MAXARGLENGTH), dimension(:), pointer :: pSERIES_FORMAT
    character (len=MAXARGLENGTH), dimension(:), pointer :: pTABLE_NAME

    character (len=MAXARGLENGTH), dimension(:), allocatable :: sSeriesName
    integer (kind=T_INT), dimension(:), allocatable :: iOrder

    integer (kind=T_INT) :: LU_LIST_OUTPUT
    integer (kind=T_INT) :: LU_INSTRUCTIONS_FILE

    character (len=256) :: sInstructionsFilename
    type (T_TIME_SERIES), pointer :: pTS
    type (T_TABLE), pointer :: pTable

    integer (kind=T_INT) :: iSize    ! holds the size of various array arguments
    integer (kind=T_INT) :: i        ! generic loop index
    integer (kind=T_INT) :: iStat    ! holds status return code on file i/o
    integer (kind=T_INT) :: iCount
    integer (kind=T_INT) :: iOutputFormat

    real (kind=T_DBL) :: rStart, rFinish

!    close(unit=LU_LIST_OUTPUT)
!    close(unit=LU_INSTRUCTIONS_FILE)

    ! set default output format for file
    iOutputFormat = iOUTPUT_LONG_FORMAT

    pFILE => pBlock%getString("FILE")
    if(str_compare(pFILE(1),"NA") ) &
      call assert(lFALSE, "No filename given in "//trim(pBlock%sBlockName) &
        //" block starting at line "//trim(asChar(pBlock%iStartingLineNumber) ), &
        trim(__FILE__), __LINE__)

    open(unit=newunit(LU_LIST_OUTPUT),file=TRIM(ADJUSTL(pFILE(1))), &
      status='REPLACE',iostat=iStat)
    call Assert(iStat==0,'Error opening TSPROC output file '//TRIM(pFILE(1)), &
      trim(__FILE__), __LINE__)
    sMostRecentListOutputFile = TRIM(ADJUSTL(pFILE(1)))

    sInstructionsFilename = pFILE(1)(1:len_trim(pFILE(1))-3)//"ins"

    open(unit=newunit(LU_INSTRUCTIONS_FILE),file=TRIM(sInstructionsFilename), &
      status='REPLACE',iostat=iStat)
    call Assert(iStat==0,'Error opening list output instruction file '//quote(sInstructionsFilename), &
      trim(__FILE__), __LINE__)
    sMostRecentInstructionFile = TRIM(sInstructionsFilename)
    write(unit=LU_INSTRUCTIONS_FILE, fmt="(a)") "pif $"

    ! get user-specified series format for this file
    pSERIES_FORMAT => pBlock%getString("SERIES_FORMAT")
    if(str_compare(pSERIES_FORMAT(1),"long") ) then
      iOutputFormat = iOUTPUT_LONG_FORMAT
    elseif(str_compare(pSERIES_FORMAT(1),"short") ) then
      iOutputFormat = iOUTPUT_SHORT_FORMAT
    elseif(str_compare(pSERIES_FORMAT(1),"ssf") ) then
      iOutputFormat = iOUTPUT_SSF_FORMAT
    elseif(str_compare(pSERIES_FORMAT(1),"extended") ) then
      iOutputFormat = iOUTPUT_EXTENDED_FORMAT
    else
      call assert(lFALSE, "Unknown SERIES_FORMAT type "//quote(pSERIES_FORMAT(1)) &
        //" in block starting at line "//trim(asChar(pBlock%iStartingLineNumber) ), &
        trim(__FILE__), __LINE__)
    endif

    ! write LIST OUTPUT for TIME SERIES objects
    pSERIES_NAME => pBlock%getString("SERIES_NAME")
    if(.not. str_compare(pSERIES_NAME(1),"NA") ) then
      do i=1,size(pSERIES_NAME)
        pTS => TS%getSeries(pSERIES_NAME(i))
        call pTS%list( iLU_ = LU_LIST_OUTPUT, iOutputFormat_ = iOutputFormat)
!        call pTS%writeInstructions( iLU = LU_INSTRUCTIONS_FILE)
      enddo
    endif

    ! write LIST OUTPUT for TABLE objects
    pTABLE_NAME => pBlock%findString("TABLE_NAME")
    if(.not. str_compare(pTABLE_NAME(1),"NA") ) then
      do i=1,size(pTABLE_NAME)
        pTable => TS%getTable(pTABLE_NAME(i))
        call pTable%list( iLU = LU_LIST_OUTPUT)
        call pTable%writeInstructions( iLU = LU_INSTRUCTIONS_FILE)
      enddo
    endif

    ! END of LIST OUTPUT processing; now CLEAN UP and DEALLOCATE MEMORY
    deallocate(pFILE)
    if(associated(pSERIES_NAME)) deallocate(pSERIES_NAME)
    if(associated(pSERIES_FORMAT)) deallocate(pSERIES_FORMAT)
    if(associated(pTABLE_NAME)) deallocate(pTABLE_NAME)

    flush(unit=LU_LIST_OUTPUT)
    flush(unit=LU_INSTRUCTIONS_FILE)
    close(unit=LU_LIST_OUTPUT)
    close(unit=LU_INSTRUCTIONS_FILE)

  end subroutine write_list_output_block

end module tsp_file_writers
