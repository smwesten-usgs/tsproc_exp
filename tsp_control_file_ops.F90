module tsp_control_file_ops

  use tsp_data_structures
  use tsp_utilities
  use tsp_datetime_class
  implicit none

  integer (kind=T_INT), parameter :: MAXARGLENGTH = 256
  integer, parameter    :: MAXBLOCKLENGTH = 1000


  !> @brief This defined type holds keyword and argument pairs for
  !> a TSPROC block. Bound procedures allow access to class members.
  type, public :: T_BLOCK
    character (len=MAXARGLENGTH) :: sBlockname
    integer (kind=T_INT) :: iStartingLineNumber
    character (len=DATETEXTLENGTH) :: sDateFormat = "MM/DD/YYYY"
    character (len=MAXARGLENGTH), dimension(:), allocatable :: sKeyword
    character (len=MAXARGLENGTH), dimension(:), allocatable :: sArg1
    character (len=MAXARGLENGTH), dimension(:), allocatable :: sArg2
    integer (kind=T_INT), dimension(:), allocatable :: iLineNum
    logical (kind=T_LOGICAL), dimension(:), allocatable :: lSelect

  contains

    procedure, public :: printDict => print_dictionary_sub
    procedure, public :: deallocate => deallocate_block_sub
    procedure, public :: select => select_by_keyword_fn
    procedure, public :: index => find_by_keyword_fn
    procedure, public :: getString => get_character_values_by_keyword_fn
    procedure, public :: getReal => get_real_values_by_keyword_fn
    procedure, public :: getInt => get_integer_values_by_keyword_fn
    procedure, public :: new => new_block_from_list_sub
    procedure, public :: add => add_to_block_from_list_sub

  end type T_BLOCK

  type, public :: T_FILE
    integer (kind=T_INT) :: iLU
    character (len=256)  :: sFilename
    integer (kind=T_INT) :: iLineNumber = 0
    character (len=MAXARGLENGTH) :: sActiveContext
    character (len=DATETEXTLENGTH) :: sDateFormat = "MM/DD/YYYY"

  contains

!    procedure :: getLineNum => get_line_number_fn
    procedure :: open => open_file_sub
    procedure :: close => close_file_sub
    procedure :: readBlock => read_block_fn
    procedure :: processSettingsBlock => process_settings_block_sub

  end type T_FILE

contains

subroutine processUserSuppliedDateTime(pBlock, tDATETIME_1, tDATETIME_2)

  type (T_BLOCK), pointer, intent(in) :: pBlock
  type (T_DATETIME), intent(out) :: tDATETIME_1, tDATETIME_2

  ! [ LOCALS ]
  character (len=MAXARGLENGTH), dimension(:), pointer :: pDATE_1
  character (len=MAXARGLENGTH), dimension(:), pointer :: pTIME_1
  character (len=MAXARGLENGTH), dimension(:), pointer :: pDATE_2
  character (len=MAXARGLENGTH), dimension(:), pointer :: pTIME_2

  character (len=MAXARGLENGTH) :: sDATE_1, sDATE_2, sTIME_1, sTIME_2

  ! set default values
  sDATE_1 = "01/01/0001"; sTIME_1 = "00:00:00"
  sDATE_2 = "12/31/3000"; sTIME_2 = "00:00:00"

  ! obtain user-supplied values, if any
  pDATE_1 => pBlock%getString("DATE_1")
  pTIME_1 => pBlock%getString("TIME_1")
  pDATE_2 => pBlock%getString("DATE_2")
  pTIME_2 => pBlock%getString("TIME_2")

  call warn(.not. (str_compare(pDATE_1(1),"NA") .and. str_compare(pDATE_2(1),"NA")), &
    "No date argument was supplied in the "//trim(pBlock%sBlockname)//" block " &
    //"(block starts at line "//trim(asChar(pBlock%iStartingLineNumber))//")")

  ! set default date values in the event that the user hasn't supplied TIME values
  if(.not. str_compare(pDATE_1(1),"NA"))  sDATE_1 = trim(pDATE_1(1))
  if(.not. str_compare(pDATE_2(1),"NA"))  sDATE_2 = trim(pDATE_2(1))
  if(.not. str_compare(pTIME_1(1),"NA"))  sTIME_1 = trim(pTIME_1(1))
  if(.not. str_compare(pTIME_2(1),"NA"))  sTIME_2 = trim(pTIME_2(1))

  call tDATETIME_1%parseDate(sDATE_1)
  call tDATETIME_1%parseTime(sTIME_1)
  call tDATETIME_1%calcJulianDay()

  call tDATETIME_2%parseDate(sDATE_2)
  call tDATETIME_2%parseTime(sTIME_2)
  call tDATETIME_2%calcJulianDay()

  call Assert(tDATETIME_2 > tDATETIME_1, &
    "DATE_2 and TIME_2 must be greater than DATE_1 and TIME_1", trim(__FILE__),__LINE__)

  deallocate(pDATE_1, pTIME_1, pDATE_2, pTIME_2)

end subroutine processUserSuppliedDateTime

!------------------------------------------------------------------------------

subroutine open_file_sub(this, sFilename)

  class ( T_FILE ) :: this
  character (len=*) :: sFilename
  integer (kind=T_INT) :: iStat

  this%sFilename = TRIM(sFilename)
!  this%iLU = getNextLogicalUnit()
  open(newunit=this%iLU, file=TRIM(sFilename), status='OLD', iostat=iStat)
  call Assert(iStat == 0, &
      "Could not open file "//TRIM(sFilename), &
      TRIM(__FILE__),__LINE__)

end subroutine open_file_sub

!------------------------------------------------------------------------------

subroutine close_file_sub(this)

  class ( T_FILE ) :: this
  integer (kind=T_INT) :: iStat

  close(unit=this%iLU, iostat=iStat)
  call Assert(iStat == 0, &
      "Problem closing file "//TRIM(this%sFilename), &
      TRIM(__FILE__),__LINE__)

  this%iLU = 0
  this%sFilename = "none"
  this%iLineNumber = 0
  this%sActiveContext = "NA"
  this%sDateFormat = "MM/DD/YYYY"

end subroutine close_file_sub

!------------------------------------------------------------------------------

!> @brief This function READS thenext TSPROC block and returns a pointer
!> to the arguments contained in that block. Line numbers of the individual
!> keywords are preserved within the \c iLineNum attribute of the block.
!>
!> @return  pBlock   Pointer to a block of TSPROC keywords and arguments
!> @memberof T_BLOCK
!>
function read_block_fn(this)      result(pBlock)

  class ( T_FILE ) :: this
  type ( T_BLOCK ), pointer :: pBlock

  ! [ LOCALS ]
  character (len=256) :: sLine, sKey, sArg
  character (len=32), dimension(MAXBLOCKLENGTH)  :: sKeyword
  character (len=MAXARGLENGTH), dimension(MAXBLOCKLENGTH)  :: sArg1
  character (len=MAXARGLENGTH), dimension(MAXBLOCKLENGTH)  :: sArg2
  integer (kind=T_INT), dimension(MAXBLOCKLENGTH) :: iLineNum
  integer (kind=T_INT) :: iNumContext
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: i, j
  logical (kind=T_LOGICAL) :: lInBlock
  logical (kind=T_LOGICAL) :: lOKtoAllocate

  allocate(pBlock, stat=iStat)
  call Assert(iStat == 0, "Could not allocate memory for TSPROC block", &
      TRIM(__FILE__),__LINE__)

  ! initialize temp variables
  sKeyword = ""; sArg1 = ""; sArg2 = ""; iLineNum = 0; i = 0

!  pBlock%iStartingLineNumber = this%iLineNumber

  lInBlock = lFALSE

  do
    read(this%iLU, fmt="(a)", iostat=iStat) sLine
    if(iStat /= 0) then
      pBlock%sBlockname = "EOF"
      return
    endif

    this%iLineNumber = this%iLineNumber + 1

    if( len_trim(sLine) == 0 ) cycle         ! ignore completely blank lines

    call Chomp(sLine, sKey)
    call Chomp(sLine, sArg)

    if( str_compare(sKey(1:1),"#")) cycle    ! ignore comments

    if( str_compare(sKey,"START")) then
      lInBlock = lTRUE
      pBlock%sBlockname = TRIM(sArg)
      pBlock%iStartingLineNumber = this%iLineNumber
    elseif ( str_compare(sKey,"END" )) then
      lInBlock = lFALSE
      call Assert(str_compare(TRIM(sArg),pBlock%sBlockname), &
        "Block names associated with START and END do not agree: " &
        //"block beginning at line number "// &
          asChar(this%iLineNumber), trim(__FILE__),__LINE__)
      exit
    elseif ( lInBlock ) then
      i = i + 1
      sKeyword(i) = uppercase(TRIM(sKey))
      sArg1(i) = TRIM(sArg)
      sArg2(i) = TRIM(sLine)    ! return remainder of line as second arg
      iLineNum(i) = this%iLineNumber

    else
      call Assert( lFALSE, &
        "Problem with control file at line: "//asChar(this%iLineNumber), &
        TRIM(__FILE__), __LINE__)
    endif

  end do

  lOKtoAllocate = lFALSE
  ! is this block active within the current context
  if( .not. str_compare(pBlock%sBlockname,"SETTINGS")) then

      iNumContext = 0

      do j=1, i

        if(str_compare(sKeyword(j),"CONTEXT")) then
          iNumContext = iNumContext + 1
        endif

        if(str_compare(sKeyword(j),"CONTEXT") .and. &
                 (str_compare(sArg1(j),this%sActiveContext) &
                 .or. str_compare(sArg1(j),"ALL")) ) then
          lOKtoAllocate = lTRUE
!          call writelog('  block is enabled for context "'//trim(sArg1(j))//'"')
!          call writelog('  (current context is "'//trim(this%sActiveContext)//'")')
        endif

      enddo

      call Assert(iNumContext > 0 , &
         "Must have at least one context statement within "// &
          trim(pBlock%sBlockname)//" block (line "//trim(asChar(this%iLineNumber))//")",&
          trim(__FILE__), __LINE__)

  elseif( str_compare(pBlock%sBlockname,"SETTINGS")) then
    lOKtoAllocate = lTRUE
  endif

  call echolog("")

  if(lOKtoAllocate) then
    call echolog('Processing '//TRIM(pBlock%sBlockName)//' block beginning at line number '// &
        asChar(pBlock%iStartingLineNumber),"(a)")
    do j=1,i
      call writelog('  <'//trim(asChar(iLineNum(j)))//'> '//trim(sKeyword(j)) &
          //' '//trim(sArg1(j)) )
    enddo
  else
    call echolog('IGNORING '//TRIM(pBlock%sBlockName)//' block beginning at line number '// &
        trim(asChar(pBlock%iStartingLineNumber))//"; block inactive in context "//trim(this%sActiveContext))
  endif

  ! allocate memory and return block data entries
  if(lOKtoAllocate) then
    allocate(pBlock%sKeyword(i))
    allocate(pBlock%sArg1(i))
    allocate(pBlock%sArg2(i))
    allocate(pBlock%iLineNum(i))
    allocate(pBlock%lSelect(i))
    pBlock%sKeyword = sKeyword(1:i)
    pBlock%sArg1 = sArg1(1:i)
    pBlock%sArg2 = sArg2(1:i)
    pBlock%iLineNum = iLineNum(1:i)
  else
    pBlock%sBlockname = "INACTIVE"
  endif

end function read_block_fn

!------------------------------------------------------------------------------

subroutine print_dictionary_sub(this)

  class ( T_BLOCK ) :: this

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, n

  write(LU_STD_OUT, fmt="(/,a,/)") "  ** CONTENTS of block "//quote(this%sBlockName)

  n = size(this%sKeyword)

  do i=1,n
    write(LU_STD_OUT,fmt="(i3,t6,a,1x,a,1x,a)") this%iLineNum(i), &
             trim(this%sKeyword(i)),trim(this%sArg1(i)),trim(this%sArg2(i))
  enddo

end subroutine print_dictionary_sub

!------------------------------------------------------------------------------

subroutine new_block_from_list_sub(this, sBlockname, sKeyword, sArg1)

  class ( T_BLOCK ) :: this
  character (len=*) :: sBlockname
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sKeyword
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sArg1

  ! [ LOCALS ]
  integer (kind=T_INT) :: iSize
  integer (kind=T_INT) :: i

  iSize = size(sKeyword)

  call this%deallocate()

  allocate(this%sKeyword(iSize))
  allocate(this%sArg1(iSize))
  allocate(this%sArg2(iSize))
  allocate(this%iLineNum(iSize))
  allocate(this%lSelect(iSize))
  this%sKeyword = sKeyword
  this%sArg1 = sArg1
  this%sArg2 = ""

  do i=1,iSize
    this%iLineNum(i) = i
  enddo

  this%iStartingLineNumber = 1
  this%sBlockname = trim(sBlockname)

end subroutine new_block_from_list_sub

!------------------------------------------------------------------------------

subroutine add_to_block_from_list_sub(this, sKeyword, sArg1)

  class ( T_BLOCK ) :: this
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sKeyword
  character (len=MAXARGLENGTH), dimension(:), allocatable :: sArg1
  type (T_BLOCK) :: tTempBlock

  ! [ LOCALS ]
  integer (kind=T_INT) :: iSize, iCurrsize, iNewsize
  integer (kind=T_INT) :: i

  iCurrsize = size(this%sKeyword)
  iSize = size(sKeyword)
  iNewsize = iSize + iCurrSize

  ! allocate space for temporary data structure
  allocate(tTempBlock%sKeyword(iCurrSize))
  allocate(tTempBlock%sArg1(iCurrSize))
  allocate(tTempBlock%sArg2(iCurrSize))
  allocate(tTempBlock%iLineNum(iCurrSize))
  allocate(tTempBlock%lSelect(iCurrSize))

  ! copy contents into temporary data structure
  tTempBlock%sKeyword = this%sKeyword
  tTempBlock%sArg1 = this%sArg1
  tTempBlock%sArg2 = this%sArg2
  tTempBlock%lSelect = this%lSelect

  ! deallocate
  call this%deallocate()

  ! now allocate space for new (bigger) data structure
  allocate(this%sKeyword(iSize + iCurrsize))
  allocate(this%sArg1(iSize + iCurrsize))
  allocate(this%sArg2(iSize + iCurrsize))
  allocate(this%iLineNum(iSize + iCurrsize))
  allocate(this%lSelect(iSize + iCurrsize))

  this%sKeyword(1:iCurrsize) = tTempBlock%sKeyword
  this%sArg1(1:iCurrsize) = tTempBlock%sArg1
  this%sArg2(1:iCurrsize) = tTempBlock%sArg2
  this%lSelect(1:iCurrsize) = tTempBlock%lSelect

  this%sKeyword(iCurrsize+1:iNewsize) = sKeyword
  this%sArg1(iCurrsize+1:iNewsize) = sArg1
  this%sArg2(iCurrsize+1:iNewsize) = ""
  this%lSelect(iCurrsize+1:iNewsize) = lFALSE

  do i=1,(iNewSize)
    this%iLineNum(i) = i
  enddo

end subroutine add_to_block_from_list_sub

!------------------------------------------------------------------------------

subroutine deallocate_block_sub(this)

  class ( T_BLOCK ) :: this


  if(allocated(this%sKeyword)) deallocate(this%sKeyword)
  if(allocated(this%sArg1)) deallocate(this%sArg1)
  if(allocated(this%sArg2)) deallocate(this%sArg2)
  if(allocated(this%iLineNum)) deallocate(this%iLineNum)
  if(allocated(this%lSelect)) deallocate(this%lSelect)

end subroutine deallocate_block_sub

!------------------------------------------------------------------------------

function select_by_keyword_fn(this, sKeyword)    result(iCount)

  class ( T_BLOCK ) :: this
  character (len=*) :: sKeyword
  integer (kind=T_INT) :: iCount

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, n

  n = size(this%sKeyword)

  do i=1,n
    if(str_compare(this%sKeyword(i)(1:len_trim(sKeyword)),sKeyword)) then
      this%lSelect(i) = lTRUE
    else
      this%lSelect(i)=lFALSE
    endif

  enddo

  iCount = count(this%lSelect)

end function select_by_keyword_fn

!------------------------------------------------------------------------------

function find_by_keyword_fn(this, sKeyword)    result(iIndex)

  class ( T_BLOCK ) :: this
  character (len=*) :: sKeyword
  integer (kind=T_INT) :: iIndex

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, n, iCount

  iCount = 0
  iIndex = -99999

  n = size(this%sKeyword)

  do i=1,n
    if(str_compare(this%sKeyword(i)(1:len_trim(sKeyword)),sKeyword))   then
      iIndex = i
      iCount = iCount + 1
    endif
  enddo

  call Assert(iCount /= 0, "Keyword "//trim(sKeyword)//" was not found in block " &
     //trim(this%sBlockname)//" starting at line "//trim(asChar(this%iStartingLineNumber)), &
     trim(__FILE__), __LINE__)

  call Assert(iCount <= 1, "Too many keywords ("//trim(sKeyword)//") found in block " &
     //trim(this%sBlockname)//" starting at line "//trim(asChar(this%iStartingLineNumber)), &
     trim(__FILE__), __LINE__)

end function find_by_keyword_fn

!------------------------------------------------------------------------------

function get_character_values_by_keyword_fn(this, sKeyword)    result(pArgs)

  class ( T_BLOCK ) :: this
  character (len=*) :: sKeyword
  character(len=MAXARGLENGTH), dimension(:), pointer :: pArgs
!  character(len=MAXARGLENGTH), dimension(:), allocatable :: sArgs

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, n, iCount, iStat

  n = size(this%sKeyword)

  do i=1,n
    if(str_compare(this%sKeyword(i)(1:len_trim(sKeyword)),sKeyword)) then
      this%lSelect(i) = lTRUE
    else
      this%lSelect(i)=lFALSE
    endif

  enddo

  iCount = count(this%lSelect)

!  call Warn(iCount > 0,trim(sKeyword)// &
!     " keyword was NOT found in the block starting at line number " &
!     //trim(asChar(this%iStartingLineNumber))//": "//trim(this%sBlockname))

  if(iCount == 0) then
    allocate(pArgs(1), stat=iStat)
    call Assert(iStat==0, "Problem with allocation",trim(__FILE__),__LINE__)
    pArgs = ""
    pArgs = "NA"
  else
    allocate(pArgs(iCount), stat=iStat)
    call Assert(iStat==0, "Problem with allocation",trim(__FILE__),__LINE__)
    pArgs = ""
    pArgs = transfer(pack(this%sArg1,this%lSelect),pArgs)
  endif

end function get_character_values_by_keyword_fn

function get_integer_values_by_keyword_fn(this, sKeyword)    result(pArgs)

  class ( T_BLOCK ) :: this
  character (len=*) :: sKeyword
  integer (kind=T_INT), dimension(:), pointer :: pArgs

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, n, iCount
  character(len=MAXARGLENGTH), dimension(:), allocatable :: sArgs

  n = size(this%sKeyword)

  do i=1,n
    if(str_compare(this%sKeyword(i),sKeyword)) then
      this%lSelect(i) = lTRUE
    else
      this%lSelect(i)=lFALSE
    endif

  enddo

  iCount = count(this%lSelect)

  call Warn(iCount > 0,trim(sKeyword)// &
     " keyword was NOT found in block starting at line number " &
     //trim(asChar(this%iStartingLineNumber))//": "//trim(this%sBlockname))

  if(iCount == 0) then
    allocate(pArgs(1))
    pArgs(1) = iNODATA
  else
    allocate(pArgs(iCount))
    allocate(sArgs(iCount))
    sArgs = pack(this%sArg1,this%lSelect)
    do i=1,iCount
      read(sArgs(i),fmt=*) pArgs(i)
    enddo
  endif

end function get_integer_values_by_keyword_fn

!------------------------------------------------------------------------------

function get_real_values_by_keyword_fn(this, sKeyword)    result(pArgs)

  class ( T_BLOCK ) :: this
  character (len=*) :: sKeyword
  real (kind=T_SGL), dimension(:), pointer :: pArgs

  ! [ LOCALS ]
  integer (kind=T_INT) :: i, n, iCount
  character(len=MAXARGLENGTH), dimension(:), allocatable :: sArgs

  n = size(this%sKeyword)

  do i=1,n
    if(str_compare(this%sKeyword(i),sKeyword)) then
      this%lSelect(i) = lTRUE
    else
      this%lSelect(i)=lFALSE
    endif

  enddo

  iCount = count(this%lSelect)

  call Warn(iCount > 0,trim(sKeyword)// &
     " keyword was NOT found in block starting at line number " &
     //trim(asChar(this%iStartingLineNumber))//": "//trim(this%sBlockname))

  if(iCount == 0) then
    allocate(pArgs(1))
    pArgs(1) = -huge(pArgs)
  else
    allocate(pArgs(iCount))
    allocate(sArgs(iCount))
    sArgs = pack(this%sArg1,this%lSelect)
    do i=1,iCount
      read(sArgs(i),fmt=*) pArgs(i)
    enddo

  endif

end function get_real_values_by_keyword_fn

!------------------------------------------------------------------------------

subroutine process_settings_block_sub(this, pBlock)

  class ( T_FILE ) :: this
  type (T_BLOCK), pointer :: pBlock

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat
  character (len=MAXARGLENGTH), dimension(:), pointer :: pActiveContext
  character (len=MAXARGLENGTH), dimension(:), pointer :: pDateFormat

  pActiveContext => pBlock%getString("CONTEXT")
  pDateFormat => pBlock%getString("DATE_FORMAT")

  call Assert(size(pActiveContext)==1, "Only one 'CONTEXT' keyword allowed in a SETTINGS block", &
    trim(__FILE__),__LINE__)
  call Assert(size(pDateFormat)==1, "Only one 'DATE_FORMAT' keyword allowed in a SETTINGS block", &
    trim(__FILE__),__LINE__)

  this%sDateFormat = pDateFormat(1)
  this%sActiveContext = uppercase(trim(pActiveContext(1)))

  deallocate(pDateFormat)
  deallocate(pActiveContext)

end subroutine process_settings_block_sub

!------------------------------------------------------------------------------

! function current_block_active_fn(sArgs, sActiveContext)    result(lResult)
!
!   character (len=*), dimension(:) :: sArgs
!   character (len=*) :: sActiveContext
!
!   ! [ LOCALS ]
!   integer (kind=T_INT) :: i
!   logical (kind=T_LOGICAL) :: lResult
!
!   lResult = lFALSE
!
! !  print *, size(sArgs)
! !  print *, sArgs
!
!   do i=1,size(sArgs)
!     if(str_compare(sArgs(i),sActiveContext)) then
!       lResult = lTRUE
!       exit
!     endif
!   end do
!
! end function current_block_active_fn

end module tsp_control_file_ops
