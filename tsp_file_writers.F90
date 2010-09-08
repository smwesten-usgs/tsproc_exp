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

    integer (kind=T_INT) :: LU_LIST_OUTPUT

    integer (kind=T_INT) :: iSize    ! holds the size of various array arguments
    integer (kind=T_INT) :: i        ! generic loop index
    integer (kind=T_INT) :: iStat    ! holds status return code on file i/o

    pFILE => pBlock%getString("FILE")
    open(newunit=LU_LIST_OUTPUT,file=TRIM(ADJUSTL(pFILE(1))),status='REPLACE',iostat=iStat)
    call Assert(iStat==0,'Error opening TSPROC output file '//TRIM(pFILE(1)), &
       trim(__FILE__), __LINE__)

    ! reset position indicators (used to determine proper sequence of instructions)
    TS%tTS%iListOutputPosition = 0
    TS%tTable%iListOutputPosition = 0

    ! write LIST OUTPUT for TIME SERIES objects
    pSERIES_NAME => pBlock%getString("SERIES_NAME")
    if(allocated(TS%tTS)) then
      iSize = size(TS%tTS)     ! get the number of T_TIME_SERIES objects

      if(iSize > 0) then
        do i=1,iSize
          ! is the current series name targeted in the "LIST_OUTPUT" block?
          if( isElement(TS%tTS(i)%sSeriesname, pSERIES_NAME)) &
             call TS%tTS(i)%list( iLU = LU_LIST_OUTPUT)
             TS%tTS(i)%iListOutputPosition = MAXVAL(TS%tTS%iListOutputPosition) + 1
        enddo
      endif
    endif

    ! write LIST OUTPUT for S_TABLE objects
    if(allocated(TS%tTable)) then
      iSize = count(TS%tTable%iTableType == iSTABLE)  ! get the number of S_TABLE objects

      pS_TABLE_NAME => pBlock%getString("S_TABLE_NAME")

      if(iSize > 0) then
        do i=1,iSize
          if( isElement(TS%tTable(i)%sSeriesname, pS_TABLE_NAME)) &
             call TS%tTable(i)%list( iLU = LU_LIST_OUTPUT)
             TS%tTable(i)%iListOutputPosition = MAXVAL(TS%tTable%iListOutputPosition) + 1
        enddo
      endif
    endif

    ! write LIST OUTPUT for V_TABLE objects
    if(allocated(TS%tTable)) then
      iSize = count(TS%tTable%iTableType == iVTABLE)  ! get the number of V_TABLE objects

      pV_TABLE_NAME => pBlock%getString("V_TABLE_NAME")

      if(iSize > 0) then
        do i=1,iSize
          if( isElement(TS%tTable(i)%sSeriesname, pV_TABLE_NAME)) &
             call TS%tTable(i)%list( iLU = LU_LIST_OUTPUT)
             TS%tTable(i)%iListOutputPosition = MAXVAL(TS%tTable%iListOutputPosition) + 1
        enddo
      endif
    endif

    ! write LIST OUTPUT for E_TABLE objects
    if(allocated(TS%tTable)) then
      iSize = count(TS%tTable%iTableType == iETABLE)  ! get the number of E_TABLE objects

      pE_TABLE_NAME => pBlock%getString("E_TABLE_NAME")

      if(iSize > 0) then
        do i=1,iSize
          if( isElement(TS%tTable(i)%sSeriesname, pE_TABLE_NAME)) &
             call TS%tTable(i)%list( iLU = LU_LIST_OUTPUT)
             TS%tTable(i)%iListOutputPosition = MAXVAL(TS%tTable%iListOutputPosition) + 1
        enddo
      endif
    endif

    ! END of LIST OUTPUT processing; now CLEAN UP and DEALLOCATE MEMORY
    deallocate(pFILE); deallocate(pSERIES_NAME)
    if(associated(pS_TABLE_NAME)) deallocate(pS_TABLE_NAME)
    if(associated(pV_TABLE_NAME)) deallocate(pV_TABLE_NAME)
    if(associated(pE_TABLE_NAME)) deallocate(pE_TABLE_NAME)

    flush(unit=LU_LIST_OUTPUT)
    close(unit=LU_LIST_OUTPUT)

  end subroutine write_list_output_block


end module tsp_file_writers
