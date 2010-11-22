module tsp_utilities

  use tsp_data_structures
  implicit none

  ! declare generic interfaces

  interface isElement
    module procedure isElement_vec_vec
    module procedure isElement_scalar_vec
    module procedure isElement_vec_scalar
  end interface isElement

  interface asChar
    module procedure int2char
    module procedure real2char
    module procedure real2char_wsf
    module procedure dbl2char
    module procedure dbl2char_wsf
  end interface asChar

  interface isKeyInList
    module procedure is_key_in_sorted_array_fn
  end interface isKeyInList

  interface asReal
    module procedure char2dbl
  end interface asReal

  interface asInt
    module procedure char2int
  end interface asInt

  interface uppercase
!    module procedure uppercase_sub
    module procedure uppercase_fn
  end interface

  interface lowercase
!    module procedure lowercase_sub
    module procedure lowercase_fn
  end interface

  interface equals
    module procedure equals_int
    module procedure equals_real
    module procedure equals_dbl
  end interface

  interface chomp
    module procedure chomp_delim_sub
    module procedure chomp_default_sub
  end interface

contains



!------------------------------------------------------------------------------

! This is a simple function to search for an available unit.
! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
! The UNIT value is returned by the function, and also by the optional
! argument. This allows the function to be used directly in an OPEN
! statement, and optionally save the result in a local variable.
! If no units are available, -1 is returned.
integer function newunit(unit)
  integer, intent(out), optional :: unit
! local
  integer, parameter :: LUN_MIN=10, LUN_MAX=1000
  logical :: opened
  integer :: lun
! begin
  newunit=-1
  do lun=LUN_MIN,LUN_MAX
    inquire(unit=lun,opened=opened)
    if (.not. opened) then
      newunit=lun
      exit
    end if
  end do
  if (present(unit)) unit=newunit
end function newunit

!------------------------------------------------------------------------------

function is_key_in_sorted_array_fn(sList, sKey)  result(lFound)

  character (len=*), dimension(:) :: sList
  character (len=*) :: sKey
  logical (kind=T_LOGICAL) :: lFound

  ! [ LOCALS ]
  integer (kind=T_INT) :: iLeft, iRight, iMiddle, iCount, iIter

  iCount = size(sList)
  lFound = lFALSE
  iLeft = lbound(sList,1)
  iRight = iCount
  if(iCount < iLeft) return  ! array is empty

  iIter = 0
  do
    iMiddle = (iLeft + iRight) / 2
    iIter = iIter + 1
    if(LLT(sKey, sList(iMiddle)) ) then
      if(iLeft >= iRight) exit        ! element not found in array
      iRight = iMiddle
    elseif(LGT(sKey,sList(iMiddle)) ) then
      if(iLeft >= iRight) exit        ! element not found in array
      iLeft = iMiddle + 1
    else
      lFound = lTRUE
      exit
    endif
  enddo

end function is_key_in_sorted_array_fn

!------------------------------------------------------------------------------

function str_compare(sString1, sString2)                   result(lBool)

  character(len=*), intent(in) :: sString1
  character(len=*), intent(in) :: sString2
  logical (kind=T_LOGICAL) :: lBool

  if(trim(adjustl(uppercase(sString1))) .eq. trim(adjustl(uppercase(sString2)))) then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function str_compare

!------------------------------------------------------------------------------

function str_contains(sString, sSubString)                   result(lBool)

  character(len=*), intent(in) :: sString
  character(len=*), intent(in) :: sSubString
  logical (kind=T_LOGICAL) :: lBool

  if(index(trim(uppercase(sString)), trim(uppercase(sSubString))) > 0) then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function str_contains

!------------------------------------------------------------------------------

function quote(sString)                                   result(sQuotedString)

  character (len=*), intent(in) :: sString
  character (len=len_trim(adjustl(sString))+2) :: sQuotedString

  sQuotedString = '"'//trim(adjustl(sString))//'"'

end function quote

!------------------------------------------------------------------------------

function str_compare_elem(sString1, sString2)              result(lBool)

  character(len=*), intent(in) :: sString1
  character(len=*), intent(in) :: sString2
  logical (kind=T_LOGICAL) :: lBool

  if(trim(adjustl(uppercase(sString1))) .eq. trim(adjustl(uppercase(sString2)))) then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function str_compare_elem

!------------------------------------------------------------------------------

function isElement_vec_vec(sStringvec1, sStringvec2)     result(lResult)

  character(len=*), dimension(:), intent(in) :: sStringVec1
  character(len=*), dimension(:), intent(in) :: sStringVec2

  ! [ LOCALS ]
  integer (kind=T_INT) :: iCount1
  integer (kind=T_INT) :: iCount2
  integer (kind=T_INT) :: i, j
  logical (kind=T_LOGICAL) :: lResult

  iCount1 = size(sStringVec1)
  iCount2 = size(sStringVec2)

  lResult = lFALSE

  do i=1,iCount1
    do j=1,iCount2
      if(str_compare(sStringVec1(i),sStringVec2(j))) then
        lResult = lTRUE
        exit
      endif
    enddo
  enddo

end function isElement_vec_vec

!------------------------------------------------------------------------------

function isElement_scalar_vec(sString1, sStringvec2)     result(lResult)

  character(len=*), intent(in) :: sString1
  character(len=*), dimension(:), intent(in) :: sStringVec2

  ! [ LOCALS ]
  integer (kind=T_INT) :: iCount2
  integer (kind=T_INT) :: i, j
  logical (kind=T_LOGICAL) :: lResult

  iCount2 = size(sStringVec2)

  lResult = lFALSE

  do j=1,iCount2
    if(str_compare(sString1,sStringVec2(j))) then
      lResult = lTRUE
      exit
    endif
  enddo

end function isElement_scalar_vec

function isElement_vec_scalar(sStringvec1, sString2)     result(lResult)

  character(len=*), dimension(:), intent(in) :: sStringVec1
  character(len=*), intent(in) :: sString2

  ! [ LOCALS ]
  integer (kind=T_INT) :: iCount1
  integer (kind=T_INT) :: i, j
  logical (kind=T_LOGICAL) :: lResult

  iCount1 = size(sStringVec1)

  lResult = lFALSE

  do i=1,iCount1
    if(str_compare(sStringVec1(i),sString2)) then
      lResult = lTRUE
      exit
    endif
  enddo

end function isElement_vec_scalar

!--------------------------------------------------------------------------
!!****s* types/Chomp_tab
! NAME
!   Chomp_tab - Chomps all text up to the first tab character from a text string.
!
! SYNOPSIS
!   Chomps all text up to the first tab character from the text string in
!   sRecord and returns it in sItem; leaves the remaining text in sRecord.
!
! INPUTS
!   sRecord - Character string to operate on.
!
! OUTPUTS
!   sRecord - Character string to operate on.
!   sItem - Character string to operate on.
!
! EXAMPLE
!   input:   sRecord = "THIS IS THE TIME"    sItem = ""
!   output:  sRecord = " THE TIME"                sItem = "THIS IS"
!
! SOURCE

subroutine Chomp_tab(sRecord, sItem)

  ! ARGUMENTS
  character (len=*), intent(inout) :: sRecord
  character (len=256), intent(out) :: sItem
  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord

  iR = SCAN(sRecord,cTAB)

  if(iR==0) then
    sItem = trim(sRecord)      ! no tab found; return entirety of sRecord
   sRecord = ""            ! as sItem
  else
    sItem = trim(sRecord(1:iR-1))
   sRecord = trim(sRecord(iR+1:))
    do iR=1,len_trim(sRecord)
     if (sRecord(iR:iR) == " " ) then
        cycle
     else
       exit
     end if
   end do
    sRecord = sRecord(iR:)
  end if

  return
end subroutine Chomp_tab


!------------------------------------------------------------------------------

!!***

!--------------------------------------------------------------------------
!!****s* types/Chomp
! NAME
!   Chomp - Chomps the first space-delimited word from the
!           beginning of a text string.
!
! SYNOPSIS
!   Chomps the first space-delimited word from the the text string in
!   sRecord and returns it in sItem; leaves the remaining text in sRecord.
!
! INPUTS
!   sRecord - Character string to operate on.
!
! OUTPUTS
!   sRecord - Character string to operate on.
!   sItem - Character string to operate on.
!
! EXAMPLE
!   input:   sRecord = "THIS IS THE TIME"    sItem = ""
!   output:  sRecord = "IS THE TIME"         sItem = "THIS"
!
! SOURCE

!subroutine Chomp(sRecord, sItem)

  ! ARGUMENTS
!  character (len=*), intent(inout) :: sRecord
!  character (len=256), intent(out) :: sItem
  ! LOCALS
!  integer (kind=T_INT) :: iR                      ! Index in sRecord
!  integer (kind=T_INT) :: iS                      ! Index in sItem
!  logical (kind=T_LOGICAL) :: lSkip               ! TRUE while skipping spaces

  ! Set my pointers and remove leading and trailing spaces
!  iR = 1
!  iS = 1
!  sItem = ""
!  lSkip = lTRUE
!  do iR=1,len_trim(sRecord)
!      if ( lSkip .and. sRecord(iR:iR) == " " ) then
!          cycle
!      else if ( .not. lSkip .and. sRecord(iR:iR) == " " ) then
!          exit
!      else
!          lSkip = lFALSE
!          sItem(iS:iS) = sRecord(iR:iR)
!          iS = iS+1
!      end if
!  end do
!  sRecord = sRecord(iR:)
!
!!end subroutine Chomp

!!***

!------------------------------------------------------------------------------
!> Convert an integer value into a formatted character string
function int2char(iValue)  result(sBuf)

  integer (kind=T_INT) :: iValue
  character(len=14) :: sBuf

  write(UNIT=sBuf,FMT="(i14)") iValue
  sBuf = trim(ADJUSTL(sBuf))

end function int2char


!------------------------------------------------------------------------------
!> Convert an character string into an integer
integer function char2int(sBuf)  result(iValue)

  character(len=*) :: sBuf
!  integer (kind=T_INT) :: iValue
  integer (kind=T_INT) :: iStat

  read(UNIT=sBuf,FMT=*, iostat=iStat) iValue
  call Assert(iStat == 0, "Internal error reading integer value" &
    //" from character string: "//trim(sBuf), &
    trim(__FILE__), __LINE__)

end function char2int

!------------------------------------------------------------------------------
!> Convert an character string into a double
double precision function char2dbl(sBuf)  result(rValue)

  character(len=*) :: sBuf
!  real (kind=T_DBL) :: rValue
  integer (kind=T_INT) :: iStat

  read(UNIT=sBuf,FMT=*, iostat=iStat) rValue
  call Assert(iStat == 0, "Internal error reading real (kind=T_DBL) value" &
    //" from character string: "//trim(sBuf), &
    trim(__FILE__), __LINE__)

end function char2dbl

!------------------------------------------------------------------------------
!> Convert a real value into a formatted character string
function real2char(rValue)  result(sBuf)

  real (kind=T_SGL) :: rValue
  character(len=16) :: sBuf

  write(UNIT=sBuf,FMT="(G16.8)") rValue
  sBuf = trim(ADJUSTL(sBuf))

end function real2char

!------------------------------------------------------------------------------
!> Convert a real (single precision) value into a formatted character string
!> INCLUDES provision for accepting WIDTH and SIG FIG arguments
function real2char_wsf(rValue, iWidth, iSignificantFigures)  result(sBuf)

  real (kind=T_SGL) :: rValue
  integer (kind=T_INT) :: iWidth
  integer (kind=T_INT) :: iSignificantFigures
  character(len=iWidth) :: sBuf
  character (len=8) :: sFormat
  character(len=3) :: sWidth, sSignificantFigures

  write(sWidth, fmt="(i3)") iWidth
  write(sSignificantFigures, fmt="(i3)") iSignificantFigures

  sFormat = "(G"//trim(adjustl(sWidth))//"."//trim(adjustl(sSignificantFigures))//")"

  write(UNIT=sBuf,FMT=sFormat) rValue

  sBuf = ADJUSTL(sBuf)

end function real2char_wsf

!------------------------------------------------------------------------------
!> Convert a real (double precision) value into a formatted character string
function dbl2char(rValue)  result(sBuf)

  real (kind=T_DBL) :: rValue
  character(len=16) :: sBuf

  write(UNIT=sBuf,FMT="(G16.8)") rValue
  sBuf = trim(ADJUSTL(sBuf))

end function dbl2char

!------------------------------------------------------------------------------
!> Convert a real (double precision) value into a formatted character string
!> INCLUDES provision for accepting WIDTH and SIG FIG arguments
function dbl2char_wsf(rValue, iWidth, iSignificantFigures)  result(sBuf)

  real (kind=T_DBL) :: rValue
  integer (kind=T_INT) :: iWidth
  integer (kind=T_INT) :: iSignificantFigures
  character(len=iWidth) :: sBuf
  character (len=8) :: sFormat
  character(len=3) :: sWidth, sSignificantFigures

  write(sWidth, fmt="(i3)") iWidth
  write(sSignificantFigures, fmt="(i3)") iSignificantFigures

  sFormat = "(G"//trim(adjustl(sWidth))//"."//trim(adjustl(sSignificantFigures))//")"

  write(UNIT=sBuf,FMT=sFormat) rValue

  sBuf = ADJUSTR(sBuf)

end function dbl2char_wsf

!------------------------------------------------------------------------------

  subroutine openlog(sFilename)

    character(len=*), intent(in), optional :: sFilename

    ! [ LOCALS ]
    integer (kind=T_INT) :: iStat
    character(len=24) :: sDateStr, sDateStrPretty
    character(len=256) :: sFilenameText

    if(present(sFilename)) then
      sFilenameText = trim(sFilename)
      open(unit=newunit(LU_REC), file=trim(sFilename),status='REPLACE',iostat=iStat)
    else
      call GetSysTimeDate(sDateStr,sDateStrPretty)
      sFilenameText = trim("tsproc_logfile_"//trim(adjustl(sDateStr))//".txt")
      open(unit=newunit(LU_REC), file=sFilenameText, &
            status='REPLACE',iostat=iStat)
    end if

    call Assert(iStat==0, "Problem opening TSPROC logfile " &
      //quote(sFilenameText), trim(__FILE__), __LINE__)

  end subroutine openlog

!------------------------------------------------------------------------------

  subroutine writelog(sMessage, sFormat)

    character(len=*), intent(in)             :: sMessage
    character(len=*), intent(in), optional   :: sFormat

    ! [ LOCALS ]
    character (len=256) :: sFormatString = ""
    integer (kind=T_INT) :: iStat
    logical (kind=T_LOGICAL) :: lOpened

    if(present(sFormat)) then
      sFormatString = '('//trim(sFormat)//')'
    else
      sFormatString = '(3x,a)'
    endif

    inquire(unit=LU_REC, opened = lOpened)
    if(.not. lOpened) call openlog()

    write(unit=LU_REC, fmt=trim(sFormatString), iostat = iStat) sMessage

  end subroutine writelog

!------------------------------------------------------------------------------

!> @brief echo to screen AND write to logfile
  subroutine echolog(sMessage, sFormat)

    character(len=*), intent(in)             :: sMessage
    character(len=*), intent(in), optional   :: sFormat

    ! [ LOCALS ]
    character (len=256) :: sFormatString = ""
    logical (kind=T_LOGICAL) :: lOpened

    if(present(sFormat)) then
      sFormatString = '('//trim(sFormat)//')'
    else
      sFormatString = '(3x,a)'
    endif

    write(unit=LU_STD_OUT, fmt=trim(sFormatString)) sMessage

    inquire(unit=LU_REC, opened = lOpened)
    if(.not. lOpened) call openlog()

    write(unit=LU_REC, fmt=trim(sFormatString)) sMessage

  end subroutine echolog

!------------------------------------------------------------------------------

  subroutine closelog()

    flush(unit=LU_REC)
    close(unit=LU_REC)

  end subroutine closelog

!------------------------------------------------------------------------------

!--------------------------------------------------------------------------

subroutine uppercase_sub ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s
  ! LOCALS
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT) :: LOWER_TO_UPPER = ichar( "A" ) - ichar( "a" )

  do i=1,len_trim(s)
      if ( ichar(s(i:i) ) >= ichar("a") .and. ichar(s(i:i)) <= ichar("z") ) then
          s(i:i) = char( ichar( s(i:i) ) + LOWER_TO_UPPER )
      end if
  end do

  return
end subroutine uppercase_sub

!--------------------------------------------------------------------------

function uppercase_fn ( s )                               result(sOut)

  ! ARGUMENTS
  character (len=*), intent(in) :: s
  character(len=len(s)) :: sOut
  ! LOCALS
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT), parameter :: LOWER_TO_UPPER = -32

!  LOWER_TO_UPPER = ichar( "A" ) - ichar( "a" )

  sOut = s

  do i=1,len_trim(sOut)
      if ( ichar(sOut(i:i) ) >= ichar("a") .and. ichar(sOut(i:i)) <= ichar("z") ) then
          sOut(i:i) = char( ichar( sOut(i:i) ) + LOWER_TO_UPPER )
      end if
  end do

  return
end function uppercase_fn

!--------------------------------------------------------------------------

function lowercase_fn ( s )                               result(sOut)

  ! ARGUMENTS
  character (len=*), intent(in) :: s
  character(len=len(s)) :: sOut
  ! LOCALS
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT), parameter :: UPPER_TO_LOWER = 32

!  UPPER_TO_LOWER = ichar( "a" ) - ichar( "A" )

  sOut = s

  do i=1,len_trim(sOut)
      if ( ichar(sOut(i:i) ) >= ichar("A") .and. ichar(sOut(i:i)) <= ichar("Z") ) then
          sOut(i:i) = char( ichar( sOut(i:i) ) + UPPER_TO_LOWER )
      end if
  end do

end function lowercase_fn

!--------------------------------------------------------------------------

subroutine lowercase_sub ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s

  ! LOCALS
  integer (kind=T_INT) :: i    ! do loop index
  ! CONSTANTS
  integer (kind=T_INT) :: UPPER_TO_LOWER

  UPPER_TO_LOWER = ichar( "a" ) - ichar( "A" )

  do i=1,len_trim(s)
      if ( ichar(s(i:i) ) >= ichar("A") .and. ichar(s(i:i)) <= ichar("Z") ) then
          s(i:i) = char( ichar( s(i:i) ) + UPPER_TO_LOWER )
      end if
  end do

end subroutine lowercase_sub

!--------------------------------------------------------------------------
!!****s* types/Assert
! NAME
!   Assert - General-purpose error-checking routine.
!
! SYNOPSIS
!   General-purpose error-checking routine. If lCondition is .false.,
!   prints the error message and stops!
!
! INPUTS
!   lCondition - statement that evaluates to a logical .true. or .false value.
!   sErrorMessage - accompanying error message to print if lCondition is .false.
!   sFilename - name of the offending file; populate with the C compiler
!                 preprocessor macro __FILE__
!   sLineNo - line number of error; populate with preprocessor macro __LINE__
!
! OUTPUTS
!   NONE
!
! SOURCE

subroutine Assert(lCondition,sErrorMessage,sFilename,iLineNo)

  ! ARGUMENTS
  logical (kind=T_LOGICAL), intent(in) :: lCondition
  character (len=*), intent(in) :: sErrorMessage
  character (len=*), optional :: sFilename
  integer (kind=T_INT), optional :: iLineNo

  logical :: lFileOpen

  if ( .not. lCondition ) then

    if(lAssertAlwaysFatal) then
      print *
      print *,'FATAL ERROR - HALTING TSPROC'
      print *,trim(sErrorMessage)
      print *
      if(present(sFilename)) print *,"module: ", trim(sFilename)
      if(present(iLineNo)) print *,"line no.: ",iLineNo
    else
      print *, "TSPROC error:"
      print *,trim(sErrorMessage)
    endif

      ! echo error condition to the log file ONLY if it is open!
      !
      ! ACHTUNG!! inquire is poorly supported under GFORTRAN - need to look at alternatives
      !
      inquire (unit=LU_REC, opened=lFileOpen)

      if(lFileOpen) then

        if(lAssertAlwaysFatal) then
          write(UNIT=LU_REC,FMT="(/,a)") 'FATAL ERROR - HALTING TSPROC'
          write(UNIT=LU_REC,FMT="(a,/)") trim(sErrorMessage)
          if(present(sFilename)) write(UNIT=LU_REC,FMT=*) "module: ", &
             trim(sFilename)
          if(present(iLineNo)) write(UNIT=LU_REC,FMT=*) "line no.: ",iLineNo
        else
          write(UNIT=LU_REC,FMT="(a)") 'TSPROC error'
          write(UNIT=LU_REC,FMT="(a,/)") trim(sErrorMessage)
        endif

      end if

    if(lAssertAlwaysFatal) stop

  end if

end subroutine Assert

!--------------------------------------------------------------------------

subroutine Warn(lCondition,sWarningMessage,sFilename,iLineNo)

  ! ARGUMENTS
  logical (kind=T_LOGICAL), intent(in) :: lCondition
  character (len=*), intent(in) :: sWarningMessage
  character (len=*), optional :: sFilename
  integer (kind=T_INT), optional :: iLineNo
  logical :: lFileOpen

  if ( .not. lCondition ) then
      print *,' *** WARNING ***'
      print *,trim(sWarningMessage)
      if(present(sFilename)) print *,"   filename: ", trim(sFilename)
      if(present(iLineNo)) print *,"   line no.: ",iLineNo
      print *

      ! echo error condition to the log file ONLY if it is open!
      inquire (unit=LU_REC, opened=lFileOpen)
      if(lFileOpen) then

        write(UNIT=LU_REC,FMT=*) ' *** WARNING ***'
        write(UNIT=LU_REC,FMT=*) trim(sWarningMessage)
        if(present(sFilename)) write(UNIT=LU_REC,FMT=*) "   filename: ", &
           trim(sFilename)
        if(present(iLineNo)) write(UNIT=LU_REC,FMT=*) "   line no.: ",iLineNo
        write(UNIT=LU_REC,FMT=*)

      end if
  end if

  return
end subroutine Warn

!------------------------------------------------------------------------------

function clean(sRecord,sDelimiters)                       result(sItem)

  ! ARGUMENTS
  character (len=*), intent(inout)           :: sRecord
  character (len=*), intent(in), optional    :: sDelimiters

  ! LOCALS
  character (len=256) :: sItem
  integer (kind=T_INT) :: iR                 ! Index in sRecord
  integer (kind=T_INT) :: i, j

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)
  sItem = ""
  j = 0

  do i = 1,len_trim(sRecord)

    if(present(sDelimiters)) then
      iR = SCAN(sRecord(i:i),sDelimiters)
    else
      iR = SCAN(sRecord(i:i),":/;,")
    endif

    if(iR==0) then
      j = j + 1
      sItem(j:j) = sRecord(i:i)
    end if

  enddo

end function clean



subroutine GetSysTimeDate(sDateStr,sDateStrPretty)

  character(len=24), intent(out) :: sDateStr, sDateStrPretty

  character (len=8) :: sDate
  character (len=10) :: sTime
  character (len=2) :: sDay
  character (len=2) :: sMon
  character (len=4) :: sYear
  character (len=2) :: sHH
  character (len=2) :: sMM
  character (len=2) :: sSS
  character (len=3) :: sMS

!  sRecord = FDATE()
  call DATE_AND_TIME(sDate, sTime)

!  print *, quote(sDate)," ",quote(sTime)

!  call chomp(sRecord,sDay)
!  call chomp(sRecord,sMon)
!  call chomp(sRecord,sDD)
!  call chomp(sRecord,sTime)
!  call chomp(sRecord,sYear)
  sDay = sDate(7:8)
  sMon = sDate(5:6)
  sYear = sDate(1:4)
  sHH = sTime(1:2)
  sMM = sTime(3:4)
  sSS = sTime(5:6)
  sMS = sTime(8:10)

  sDateStr = TRIM(sYear)//"_"//TRIM(sMon)//"_"//TRIM(sDay)//"__"//&
    TRIM(sHH)//"_"//TRIM(sMM)//"_"//TRIM(sSS)//"_"//trim(sMS)
  sDateStrPretty = &
    TRIM(sDay)//" "//TRIM(sMon)//" "//TRIM(sYear)//" " &
     //TRIM(sHH)//":"//TRIM(sMM)

  return

end subroutine GetSysTimeDate

!------------------------------------------------------------------------------

function countFields(sRecord, sDelimiters)             result(iNumFields)

  character (len=*), intent(in)              :: sRecord
  character (len=*), intent(in), optional    :: sDelimiters
  integer (kind=T_INT) :: iNumFields

  ! [ LOCALS ]
  integer (kind=T_INT) :: i

  iNumFields = 1

  if (present(sDelimiters)) then
    do i=1,len_trim(sRecord)-1
      if(scan(sRecord(i:i),sDelimiters) /= 0 ) iNumFields = iNumFields + 1
    enddo
  else
    do i=1,len_trim(sRecord)-1
      if(scan(sRecord(i:i)," ") /= 0 ) iNumFields = iNumFields + 1
    enddo
  endif

end function countFields

!------------------------------------------------------------------------------

function pluck(sRecord, sDelimiter)    result(pItem)

  ! ARGUMENTS
  character (len=*), intent(inout)                                  :: sRecord
  character (len=1), intent(in)                                     :: sDelimiter
  character (len=256), dimension(:), pointer                        :: pItem

  ! LOCALS
  integer (kind=T_INT) :: iLen
  integer (kind=T_INT) :: iNumDelimiters
  integer (kind=T_INT) :: iNumItems
  integer (kind=T_INT), dimension(:), allocatable :: iPos
  integer (kind=T_INT) :: i
  integer (kind=T_INT) :: iStartPosIdx, iEndPosIdx

  iNumDelimiters = 0

  ! find the end position of 'sRecord'
  iLen = len_trim(sRecord)

  do i=1,iLen
    if(sRecord(i:i) .eq. trim(sDelimiter) ) iNumDelimiters = iNumDelimiters + 1
  enddo

  call assert(mod(iNumDelimiters,2) == 0, "Unequal number of delimiters in file", &
      trim(__FILE__), __LINE__)

  allocate(iPos(iNumDelimiters))

  iNumDelimiters = 0
  do i=1,iLen
    if(sRecord(i:i) .eq. trim(sDelimiter) ) then
      iNumDelimiters = iNumDelimiters + 1
      iPos(iNumDelimiters) = i
    endif
  enddo

  if(iNumDelimiters > 0) then

    iNumItems = iNumDelimiters / 2

    allocate(pItem(iNumItems) )

    do i=1,iNumItems

      iStartPosIdx = (i-1) * 2 + 1
      iEndPosIdx = iStartPosIdx + 1
      pItem(i) = sRecord( iPos(iStartPosIdx)+1:iPos(iEndPosIdx)-1 )

    enddo

  else

    allocate(pItem(1))
    pItem(1) = "NA"

  endif

end function pluck

!------------------------------------------------------------------------------

subroutine Chomp_delim_sub(sRecord, sItem, sDelimiters)

  ! ARGUMENTS
  character (len=*), intent(inout)           :: sRecord
  character (len=256), intent(out)           :: sItem
  character (len=*), intent(in)              :: sDelimiters
  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord
  integer (kind=T_INT) :: iB                      !
  integer (kind=T_INT) :: iLen

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)
  ! find the end position of 'sRecord'
  iLen = len_trim(sRecord)

  ! find first occurrance of delimiter
  iR = SCAN(trim(sRecord),sDelimiters)
  ! find *next* occurance of *NON*-delimiter (needed to skip over multiple delimiters)
  iB = verify(string = sRecord(max(iR,1):iLen), set = sDelimiters)

  if(iR==0) then
    sItem = trim(sRecord)   ! no delimiters found; return entirety of sRecord
    sRecord = ""            ! as sItem
  else
    sItem = trim(sRecord(1:iR-1))
    sRecord = trim(sRecord(iR+iB-1:))
  end if

end subroutine Chomp_delim_sub

!------------------------------------------------------------------------------

subroutine Chomp_default_sub(sRecord, sItem)

  ! ARGUMENTS
  character (len=*), intent(inout)           :: sRecord
  character (len=256), intent(out)           :: sItem

  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord
  integer (kind=T_INT) :: iB                      !
  integer (kind=T_INT) :: iLen

  ! eliminate any leading spaces
  sRecord = adjustl(sRecord)
  ! find the end position of 'sRecord'
  iLen = len_trim(sRecord)

  iR = SCAN(trim(sRecord)," ")
  iB = verify(string = sRecord(max(iR,1):iLen), set = " ")

  if(iR==0) then
    sItem = trim(sRecord)   ! no delimiters found; return entirety of sRecord
    sRecord = ""            ! as sItem
  else
    sItem = trim(sRecord(1:iR-1))
    sRecord = trim(sRecord(iR+iB-1:))
  end if

end subroutine Chomp_default_sub

!------------------------------------------------------------------------------

function equals_int(i1,i2)              result(lBool)

  integer (kind=T_INT), intent(in)   :: i1
  integer (kind=T_INT), intent(in)   :: i2
  logical (kind=T_LOGICAL) :: lBool

  lBool = (i1 == i2)

end function equals_int

!------------------------------------------------------------------------------

function equals_real(r1,r2)             result(lBool)

  real (kind=T_SGL) , intent(in)      :: r1
  real (kind=T_SGL) , intent(in)      :: r2
  logical (kind=T_LOGICAL) :: lBool

  ! [ LOCALS ]
  real (kind=T_SGL) :: rtemp

  ! intrinsic function 'spacing' returns the absolute spacing
  ! of the numbers near X in the model used to represent real numbers.
  ! therefore, if the two numbers are within a factor of three of the
  ! smallest representable numbers near X in real number space, conclude the
  ! numbers are equal
  rtemp=abs( 3.0 * spacing(r1) )
  if( abs( r1 - r2 ) < rtemp )then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function equals_real

!------------------------------------------------------------------------------

function equals_dbl(r1,r2)             result(lBool)

  real (kind=T_DBL) , intent(in)      :: r1
  real (kind=T_DBL) , intent(in)      :: r2
  logical (kind=T_LOGICAL) :: lBool

  ! [ LOCALS ]
  real (kind=T_DBL) :: rtemp

  ! intrinsic function 'spacing' returns the absolute spacing
  ! of the numbers near X in the model used to represent real numbers.
  ! therefore, if the two numbers are within a factor of three of the
  ! smallest representable numbers near X in real number space, conclude the
  ! numbers are equal
  rtemp=abs( 3.0 * spacing(r1) )
  if( abs( r1 - r2 ) < rtemp )then
    lBool = lTRUE
  else
    lBool = lFALSE
  end if

end function equals_dbl

!------------------------------------------------------------------------------


end module tsp_utilities
