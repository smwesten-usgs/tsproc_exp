module tsp_file_writers

  use tsp_data_structures
  use tsp_collections
  use tsp_time_series_manager
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
    character (len=MAXARGLENGTH), dimension(:), pointer :: pC_TABLE_NAME
    character (len=MAXARGLENGTH), dimension(:), pointer :: pS_TABLE_NAME
    character (len=MAXARGLENGTH), dimension(:), pointer :: pV_TABLE_NAME
    character (len=MAXARGLENGTH), dimension(:), pointer :: pE_TABLE_NAME
    character (len=MAXARGLENGTH), dimension(:), pointer :: pI_TABLE_NAME
    character (len=MAXARGLENGTH), dimension(:), pointer :: pTABLE_NAME

    integer (kind=T_INT) :: LU_LIST_OUTPUT
    integer (kind=T_INT) :: LU_INSTRUCTIONS_FILE

    character (len=256) :: sInstructionsFilename

    integer (kind=T_INT) :: iSize    ! holds the size of various array arguments
    integer (kind=T_INT) :: i        ! generic loop index
    integer (kind=T_INT) :: iStat    ! holds status return code on file i/o

    close(unit=LU_LIST_OUTPUT)
    close(unit=LU_INSTRUCTIONS_FILE)

    TS%tTable%iListOutputPosition = -999
    TS%pTS%iListOutputPosition = -999

    pFILE => pBlock%getString("FILE")
    open(newunit=LU_LIST_OUTPUT,file=TRIM(ADJUSTL(pFILE(1))),status='REPLACE',iostat=iStat)
    call Assert(iStat==0,'Error opening TSPROC output file '//TRIM(pFILE(1)), &
       trim(__FILE__), __LINE__)
    sMostRecentListOutputFile = TRIM(ADJUSTL(pFILE(1)))

    sInstructionsFilename = pFILE(1)(1:len_trim(pFILE(1))-3)//"ins"

    open(newunit=LU_INSTRUCTIONS_FILE,file=TRIM(sInstructionsFilename),status='REPLACE',iostat=iStat)
    call Assert(iStat==0,'Error opening list output instruction file '//quote(sInstructionsFilename), &
       trim(__FILE__), __LINE__)
    sMostRecentInstructionFile = TRIM(sInstructionsFilename)
    write(unit=LU_INSTRUCTIONS_FILE, fmt="(a)") "pif $"

    ! reset position indicators (used to determine proper sequence of instructions)
    TS%pTS%iListOutputPosition = 0
    TS%tTable%iListOutputPosition = 0

    ! write LIST OUTPUT for TIME SERIES objects
    pSERIES_NAME => pBlock%getString("SERIES_NAME")
    if(associated(TS%pTS)) then
      iSize = size(TS%pTS)     ! get the number of T_TIME_SERIES objects

      if(iSize > 0) then
        do i=1,iSize
          ! is the current series name targeted in the "LIST_OUTPUT" block?
          if( isElement(TS%pTS(i)%sSeriesname, pSERIES_NAME)) then
            call TS%pTS(i)%list( iLU = LU_LIST_OUTPUT)
            call TS%pTS(i)%writeInstructions( iLU = LU_INSTRUCTIONS_FILE)
            TS%pTS(i)%iListOutputPosition = MAXVAL(TS%pTS%iListOutputPosition) + 1
          endif
        enddo
      endif
    endif

    ! write LIST OUTPUT for TABLE objects
    if(allocated(TS%tTable)) then
      iSize = size(TS%tTable)  ! get the number of TABLE objects

      pTABLE_NAME => pBlock%findString("TABLE_NAME")

      if(iSize > 0) then
        do i=1,size(TS%tTable)
          if( isElement(TS%tTable(i)%sSeriesname, pTABLE_NAME)) then
            call TS%tTable(i)%list( iLU = LU_LIST_OUTPUT)
            call TS%tTable(i)%writeInstructions( iLU = LU_INSTRUCTIONS_FILE)
            TS%tTable(i)%iListOutputPosition = MAXVAL(TS%tTable%iListOutputPosition) + 1
          endif
        enddo
      endif
    endif

    ! END of LIST OUTPUT processing; now CLEAN UP and DEALLOCATE MEMORY
    deallocate(pFILE)
    if(associated(pSERIES_NAME)) deallocate(pSERIES_NAME)
    if(associated(pTABLE_NAME)) deallocate(pTABLE_NAME)

    flush(unit=LU_LIST_OUTPUT)
    flush(unit=LU_INSTRUCTIONS_FILE)
    close(unit=LU_LIST_OUTPUT)
    close(unit=LU_INSTRUCTIONS_FILE)

  end subroutine write_list_output_block


end module tsp_file_writers
