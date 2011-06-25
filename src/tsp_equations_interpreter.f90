module tsp_equations_interpreter
!
!  This module interprets the function, builds it to be evaluated
!  next
!
!  modified from the "fparser" code obtained at http://fparser.sourceforge.net/
!
  use tsp_data_structures
  use tsp_utilities
!  use tsp_time_series_manager
!  use tsp_collections
  use tsp_datetime_class
  implicit none

  character (len=256),   dimension(:), allocatable, private :: sVarnames
  character (len=256),   dimension(:), allocatable, private :: sTokens
  integer (kind=T_INT),  dimension(:), allocatable, private :: iOpCode
  integer (kind=T_INT),                             private :: iNumVariables
  integer (kind=T_INT),                             private :: iNumNumbers
  integer (kind=T_INT),                             private :: iNumTokens = 0
  character,             dimension(:), allocatable, private :: sOpAddSub      !Operador
  integer (kind=T_INT),                             private :: isaddsub = 1
  character,             dimension(:), allocatable, private :: sOpMulDiv      !Operador
  integer (kind=T_INT),                             private :: ismuldiv = 1
  character (len=256),                              private :: sToken = ""
  integer (kind=T_INT),                             private :: iToken = 1
  integer (kind=T_INT),                             private :: iOpIndex = 1
  integer (kind=T_INT),                             private :: iNumIndex = 1
  real (kind=T_DBL),     dimension(:), allocatable, private :: rStackData
  real (kind=T_DBL),     dimension(:), allocatable, private :: rNumber
  character (len=5),                                public  :: sStatusFlag = "ok"
  logical (kind=T_LOGICAL),                         private :: lIsValidEquation = lFALSE
!  type (T_TIME_SERIES), dimension(:), pointer,      private :: pTS

  integer (kind=T_INT), parameter                           :: NUMOPCODES = 30

  ! used in "basic_error_check"
  character (len=36), parameter           :: ALPHANUMERIC = &
                                               '0123456789abcdefghijklmnopqrstuvwxyz'
  ! used in "tokens_analyzer"
  character (len=11), parameter           :: NUMBERS = '.0123456789'
  character (len=28), parameter           :: CHARS = 'abcdefghijklmnopqrstuvwxyz@_'
  character (len=7), parameter            :: OPERATORS = '+-*/^()'
  character (len=1), parameter            :: QUOTECHAR = char(22)

   ! Enumerated constants for OPERATIONS CODES
   enum, bind(c)
     enumerator :: eNUMERAL = 1
     enumerator :: eNEGATE, ePLUS, eMINUS, eMULTIPLY, eDIVIDE, ePOWER
     enumerator :: eSIN, eCOS, eTAN, eASIN, eACOS, eATAN, eSINH, eCOSH
     enumerator :: eTANH, eDSIN, eDCOS, eDTAN, eLOG, eLOG10, eNINT, eANINT
     enumerator :: eAINT, eEXP, eSQRT, eABS, eFLOOR, eDOY, eDAYS
   end enum

contains

  subroutine init_equation (sFuncText, sVarNames, sStatusFlag)

   implicit none
  !
  !   This subroutine shifts  all characters of the function
  !   expression to lowercase and converts exponents signals ** to ^
  !
  character (len=*),                   intent(in)    :: sFuncText
  character (len=256), dimension(:), intent(in)      :: sVarNames
  integer (kind=T_INT)                        :: i, k, funclen
  character (len=5)         , intent(out)    :: sStatusFlag
  integer (kind=T_INT) :: iStat

  character (len=MAXEQUATIONLENGTH) :: sFunctionText
  character (len=256), dimension(size(sVarNames)) :: sVariableNames

  lIsValidEquation = lFALSE
  sStatusFlag = "ok"

  sFunctionText = sFuncText
  sVariableNames = sVarNames

  iNumVariables = size(sVariableNames)

  !detects errors

  sFunctionText = lowercase(sFunctionText)
!  call lowercase(sFunctionText)
  call convert_brackets(sFunctionText)
  call remove_blanks(sFunctionText)
  call basic_error_check(sFunctionText)
  call set_variable_names (sFunctionText, sVariableNames)
  sStatusFlag = trim(sStatusFlag)
  if(str_compare(sStatusFlag,"ok") ) lIsValidEquation = lTRUE

!  if(allocated(iOpCode) ) then
!    do i=1,size(iOpCode)
!      write(*,fmt="('iOpCode(',a,') = ',a,t30," &
!        //"'rNumber(',a,') = ',a,t60,'sTokens(',a,') = ',a)") trim(asChar(i)), trim(asChar(iOpCode(i) )), &
!        trim(asChar(i)), trim(asChar(rNumber(i) )), trim(asChar(i)), trim(sTokens(i) )
!    enddo
!  endif

  end subroutine init_equation

!------------------------------------------------------------------------------

  subroutine set_variable_names (sFunctionText, sVariableNames)
  !  This subroutine recognizes the variables and set their values
  !
   implicit none
  character (len=256), dimension(:), intent(in)      :: sVariableNames
  character (len=MAXEQUATIONLENGTH), intent(inout)          :: sFunctionText

  iNumVariables = size(sVariableNames)
   if(allocated(sVarnames)) deallocate(sVarnames)
  allocate(sVarnames(iNumVariables))
  sVarnames = sVariableNames
  call tokens_analyzer (sFunctionText)

  end subroutine set_variable_names

!------------------------------------------------------------------------------

  subroutine tokens_analyzer (sFunctionText)
  !
  !  This subroutine scans the sFunctionText string, counting out its basic elements.
  !  At the end of this routine we know how much memory is needed to
  !  store the tokens, which will be evaluated later
  !
   implicit none
  character (len=MAXEQUATIONLENGTH), intent(in)          :: sFunctionText

  ! [ LOCALS ]
  integer (kind=T_INT)                :: k = 1, i = 1
  integer (kind=T_INT)                :: irightbrackets = 1, ileftbrackets = 1
  logical                             :: lStatus = lTRUE

  k = 1
  i = 1
  irightbrackets = 1
  ileftbrackets = 1
  iNumTokens = 0
  lStatus = lTRUE

  do while (k <= len_trim(sFunctionText))

    !It's quoted text
    if (index("<", sFunctionText(k:k)) /= 0) then
      lStatus = lTRUE
      do while (lStatus)
        if (index(">", sFunctionText(k:k)) == 0 ) then
          k = k + 1
        else
          lStatus = lFALSE
          k = k + 1
        end if
      end do
      iNumTokens = iNumTokens + 3

    !It's a variable, or function name
    elseif (index(CHARS,sFunctionText(k:k)) /= 0) then
      lStatus = lTRUE
      do while (lStatus)
        if (index(OPERATORS//"<>", sFunctionText(k+1:k+1)) == 0 &
           .and. sFunctionText(k+1:k+1) /= ' ' ) then
          k = k + 1
        else
          lStatus = lFALSE
          k = k + 1
        end if
      end do
      iNumTokens = iNumTokens + 1

    !It's a number
    else if (index(NUMBERS,sFunctionText(k:k)) /= 0) then
      lStatus = lTRUE
      do while (lStatus)
        if ((index(OPERATORS//"<>", sFunctionText(k+1:k+1)) == 0 .and. sFunctionText(k+1:k+1) /= ' ') .or. sFunctionText(k+1:k+1) == 'e' .or. sFunctionText(k+1:k+1) == 'd') then
          k = k + 1
        else
          if(sFunctionText(k:k) == 'e' .or. sFunctionText(k:k) == 'd') then
            k = k + 1
          else
            lStatus = lFALSE
            k = k + 1
          end if
        end if
      end do
      iNumTokens = iNumTokens + 1

    !It's an operator or delimiter
    elseif(index(OPERATORS, sFunctionText(k:k)) /= 0 ) then
      k = k + 1
      iNumTokens = iNumTokens + 1
    end if
  end do

  if(allocated(sTokens)) deallocate(sTokens)
  allocate(sTokens(iNumTokens))

  ! now essentially rewind and start the process again, this time actually recording
  ! the token values

  k = 1
  i = 1

  do while (k <= len_trim(sFunctionText))

    !It's quoted text
    if (index('<',sFunctionText(k:k)) /= 0) then
      lStatus = lTRUE
      sTokens(i) = "("
      ileftbrackets = ileftbrackets + 1
      i = i + 1
      k = k + 1
       sTokens(i) = '"'
     do while (lStatus)
       if (index('>', sFunctionText(k:k)) == 0 ) then
         sTokens(i) = trim(sTokens(i)) // sFunctionText(k:k)
         k = k + 1
       else
         lStatus = lFALSE
         sTokens(i) = trim(sTokens(i)) // '"'
         k = k + 1
         i = i + 1
       end if
     end do
      sTokens(i) = ")"
      irightbrackets = irightbrackets + 1
      i = i + 1
!      k = k + 1

    !It's a variable, or function  name
    elseif (index(CHARS,sFunctionText(k:k)) /= 0) then
      sTokens(i) = sFunctionText(k:k)
      lStatus = lTRUE
      do while (lStatus)
        if (index(OPERATORS//"<>", sFunctionText(k+1:k+1)) == 0 &
           .and. sFunctionText(k+1:k+1) /= ' ' &
           .and. sFunctionText(k+1:k+1) /= QUOTECHAR) then
          sTokens(i) = trim(sTokens(i)) // sFunctionText(k+1:k+1)
          k = k + 1
        else
          lStatus = lFALSE
          k = k + 1
          i = i + 1
        end if
      end do

    !It's a number
    else if (index(NUMBERS,sFunctionText(k:k)) /= 0) then
      sTokens(i) = sFunctionText(k:k)
      lStatus = lTRUE
      do while (lStatus)
        if ((index(OPERATORS//"<>", sFunctionText(k+1:k+1)) == 0 .and. sFunctionText(k+1:k+1) /= ' ') .or. sFunctionText(k+1:k+1) == 'e' .or. sFunctionText(k+1:k+1) == 'd') then
          sTokens(i) = trim(sTokens(i)) // sFunctionText(k+1:k+1)
          k = k + 1
        else
          if(sFunctionText(k:k) == 'e' .or. sFunctionText(k:k) == 'd') then
            sTokens(i) = trim(sTokens(i)) // sFunctionText(k+1:k+1)
            k = k + 1
          else
            lStatus = lFALSE
            i = i + 1
            k = k + 1
          end if
        end if
      end do

    !It's an operator or delimiter
    elseif(index(OPERATORS, sFunctionText(k:k)) /= 0 ) then
      sTokens(i) = sFunctionText(k:k)
      if(sTokens(i) == '(')then
        ileftbrackets = ileftbrackets + 1
      else if(sTokens(i) == ')') then
        irightbrackets = irightbrackets + 1
      end if
      i = i + 1
      k = k + 1
    end if
  end do

  if (irightbrackets /= ileftbrackets) then
    sStatusFlag = 'error'
    return
  end if

  iToken = 1
  isaddsub = 1
  ismuldiv = 1
  iOpIndex = 1
  iNumIndex = 1
  sToken = sTokens(iToken)

  if (allocated(sOpAddSub)) deallocate(sOpAddSub)
  if (allocated(sOpMulDiv)) deallocate(sOpMulDiv)
  if (allocated(rNumber)) deallocate(rNumber)
  if (allocated(rStackData)) deallocate(rStackData)
  if (allocated(iOpCode)) deallocate(iOpCode)

  allocate(sOpAddSub(2))
  allocate(sOpMulDiv(2))
  allocate(rNumber(iNumTokens))
  allocate(rStackData(iNumTokens))
  allocate(iOpCode(iNumTokens))

  iOpCode = -99999
  rStackData = -huge(rStackData)
  rNumber = -huge(rStackData)

  call add_sub()
  iOpIndex = iOpIndex - 1

  end subroutine tokens_analyzer

!------------------------------------------------------------------------------

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !The following subroutines call themselves recursively
  !to build the expression to be parsed based on an algorithm
  !called Recursive Descendent Parsing
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine add_sub ()
  !
  ! Enter description here
  !
   implicit none

  call mul_div ()

  do while (trim(sToken) == '+' .or. trim(sToken) == '-')
    sOpAddSub(isaddsub) = trim(sToken)
    isaddsub = isaddsub + 1
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call mul_div()

    selectcase(sOpAddSub(isaddsub-1))
    case('+')
      isaddsub = isaddsub - 1
      iOpCode(iOpIndex) = ePLUS
      iOpIndex = iOpIndex + 1

    case('-')
      isaddsub = isaddsub - 1
      iOpCode(iOpIndex) = eMINUS
      iOpIndex = iOpIndex + 1
    end select
  end do

  end subroutine add_sub

!------------------------------------------------------------------------------

  recursive subroutine mul_div ()
  !
  ! Enter description here
  !
   implicit none

  call unary()

  do while (trim(sToken) == '*' .or. trim(sToken) == '/')
    sOpMulDiv(ismuldiv) = trim(sToken)
    ismuldiv = ismuldiv + 1
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call unary()

    selectcase(sOpMulDiv(ismuldiv-1))
    case('*')
      ismuldiv = ismuldiv - 1
      iOpCode(iOpIndex) = eMULTIPLY
      iOpIndex = iOpIndex + 1
    case('/')
      ismuldiv = ismuldiv - 1
      iOpCode(iOpIndex) = eDIVIDE
      iOpIndex = iOpIndex + 1
    end select
  end do

  end subroutine mul_div

!------------------------------------------------------------------------------

  recursive subroutine unary()
  !
  ! Enter description here
  !
   implicit none

  if (trim(sToken) == '-') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call pow()
    iOpCode(iOpIndex) = eNEGATE
    iOpIndex = iOpIndex + 1
  else if (trim(sToken) == '+') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call pow()
  else
    call pow()
  end if

  end subroutine unary

!------------------------------------------------------------------------------

  recursive subroutine pow ()
  !
  ! Enter description here
  !
   implicit none

  call functions()

  if (trim(sToken) == '^') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call functions()
    iOpCode(iOpIndex) = ePOWER
    iOpIndex = iOpIndex + 1
  end if

  end subroutine pow

!------------------------------------------------------------------------------

  recursive subroutine functions ()
  !
  ! Enter description here
  !
   implicit none

  if (trim(sToken) == 'sin') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eSIN
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'cos') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eCOS
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'tan') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eTAN
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'asin') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eASIN
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'acos') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eACOS
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'atan') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eATAN
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'sinh') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eSINH
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'cosh') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eCOSH
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'tanh') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eTANH
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'dsin') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eDSIN
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'dcos') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eDCOS
    iOpIndex = iOpIndex + 1

  else if(trim(sToken) == 'dtan') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eDTAN
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'log') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eLOG
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'log10') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eLOG10
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'nint') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eNINT
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'anint') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eANINT
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'aint') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eAINT
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'exp') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eEXP
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'sqrt') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eSQRT
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'abs') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eABS
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == '@_days_') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eDAYS
    iOpIndex = iOpIndex + 1

  else if (trim(sToken) == 'floor') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call brackets()
    iOpCode(iOpIndex) = eFLOOR
    iOpIndex = iOpIndex + 1

  else
    call brackets()

  end if

  end subroutine functions

!------------------------------------------------------------------------------

  recursive subroutine brackets()
  !
  ! Enter description here
  !
   implicit none

  if (trim(sToken) == '(') then
    iToken = iToken + 1
    sToken = sTokens(iToken)
    call add_sub()
    if (trim(sToken) /= ')') then
         call Warn(lFALSE,"Missing parenthesis in equation",trim(__FILE__),__LINE__)
      sStatusFlag =  'error'
      return
    end if
    if (iToken < iNumTokens) then
      iToken = iToken + 1
      sToken = sTokens(iToken)
    end if
    if (trim(sToken) == '(') then
         call Warn(lFALSE,"Left parenthesis was found where a right parenthesis was expected", &
            trim(__FILE__),__LINE__)
      sStatusFlag =  'error'
      return
    end if
  else
    call recog_vars ()
  end if

  end subroutine brackets

!------------------------------------------------------------------------------

  recursive subroutine recog_vars ()
  !
  ! Enter description here
  !
   implicit none

  integer (kind=T_INT)           :: i
  integer (kind=T_INT)           :: iLen
  integer (kind=T_INT)           :: ierror
  character (len=7)              :: operators = '+-*/^()'
  character(len=256)             :: sRecord, sItem
  type(T_DATETIME)               :: tDT

  main: do

    ! Check for extraneous operators in token
    if (index(operators, trim(sToken)) /= 0) then
        call Warn(lFALSE,"Found an unexpected operator in token "//trim(sToken), &
           trim(__FILE__),__LINE__)
        sStatusFlag = 'error'
     exit
    end if

    ! scan for known variables
    do i = 1, iNumVariables

      !It's a variable
      if (str_compare(sToken, sVarnames(i) ) ) then
        iOpCode(iOpIndex) = NUMOPCODES+i
        iOpIndex = iOpIndex + 1
        if (iToken < iNumTokens) then
          iToken = iToken + 1
          sToken = sTokens(iToken)
        end if
        exit main
      end if
    end do

    ! take special action if "@doy"
    ! this ended up here because it's really a function that accepts *no*
    ! arguments (and thus doesn't work well within the "functions" subroutine
    if (trim(sToken) == "@doy" .or. trim(sToken) == "@_days_start_year") then
      iOpCode(iOpIndex) = eDOY
      iOpIndex = iOpIndex + 1
      if (iToken < iNumTokens) then
        iToken = iToken + 1
        sToken = sTokens(iToken)
      end if
      exit main
    endif

    ! check to see if this could be a datetime string
    ! if it is, substitute the Julian Date as a real value
    ! in place of the text representation of the date/time
    iLen = len_trim(sToken)
    if(sToken(1:1) == '"' .and. sToken(iLen:iLen) == '"') then
!      print *, "|"//sToken//"|"
      sRecord = sToken(2:iLen-1)
      call Chomp(sRecord, sItem, "_")
      call tDT%parseDate(sItem)
      call tDT%parseTime(sRecord)
      call tDT%calcJulianDay()
!      print *, trim(sItem)," ",trim(sRecord)," ", tDT%prettydate()
      rNumber(iNumIndex) = real(tDT%iJulianDay,kind=T_DBL) &
                            + real(tDT%rFractionOfDay,kind=T_DBL)
      iOpCode(iOpIndex) = 1
      iOpIndex = iOpIndex + 1
      if (iToken < iNumTokens) then
        iToken = iToken + 1
        sToken = sTokens(iToken)
      end if
      iNumIndex = iNumIndex + 1
      exit main
    endif

    !Don't know what else it could be; must be a number
    sToken = trim(sToken)
    read(sToken, *, iostat = ierror) rNumber(iNumIndex)
    if (ierror /= 0) then
         call Warn(lFALSE,"Problem reading numerical value from token: "//trim(sToken), &
           trim(__FILE__),__LINE__)
      sStatusFlag = 'error'
      exit main
!      return
    else
      iOpCode(iOpIndex) = 1
      iOpIndex = iOpIndex + 1
      if (iToken < iNumTokens) then
        iToken = iToken + 1
        sToken = sTokens(iToken)
      end if
      iNumIndex = iNumIndex + 1
      exit
    end if

  enddo main

  end subroutine recog_vars

!------------------------------------------------------------------------------

  function evaluate_expression (rVariables, tTodayDT) result (rResult)
  !
  !  This function will evaluate_expression the expression supplied
  !
    implicit none

    real (kind=T_SGL), dimension(:), intent(in)            :: rVariables
    type(T_DATETIME), intent(in), optional                 :: tTodayDT
    real (kind=T_SGL)                                      :: rResult

    ! [ LOCALS ]
    integer (kind=T_INT)                       :: iStackIndex = 0
    integer (kind=T_INT)                       :: dt = 1
    integer (kind=T_INT)                       :: i, k, j
    type(T_DATETIME)                           :: tBaseDT
    type(T_DATETIME)                           :: tDeltaDT

    rStackData = 0.

    if(size(rVariables) /= iNumVariables) then

      call warn(lFALSE, "Not enough values given for number of variables (" &
        //trim(asChar(iNumVariables))//")", trim(__FILE__), __LINE__)
      rResult = - huge(rResult)

    elseif(.not. lIsValidEquation) then

      call warn(lFALSE, "Equation was not properly initialized; cannot evaluate ", &
          trim(__FILE__), __LINE__)
      rResult = - huge(rResult)

    else

      iStackIndex = 0
      dt = 1

      do i = 1, iOpIndex

      select case(iOpCode(i))
        case (eNUMERAL)
          iStackIndex = iStackIndex + 1
          rStackData(iStackIndex) = rNumber(dt)
          dt = dt + 1
        case (eNEGATE)
          rStackData(iStackIndex) =  - rStackData(iStackIndex)
        case (ePLUS)
          rStackData(iStackIndex-1) = rStackData(iStackIndex-1) + rStackData(iStackIndex)
          rStackData(iStackIndex) = 0.
          iStackIndex = iStackIndex - 1
        case (eMINUS)
          rStackData(iStackIndex-1) = rStackData(iStackIndex-1) - rStackData(iStackIndex)
          rStackData(iStackIndex) = 0.
          iStackIndex = iStackIndex - 1
        case (eMULTIPLY)
          rStackData(iStackIndex-1) = rStackData(iStackIndex-1) * rStackData(iStackIndex)
          rStackData(iStackIndex) = 0.
          iStackIndex = iStackIndex - 1
        case (eDIVIDE)
          rStackData(iStackIndex-1) = rStackData(iStackIndex-1) / rStackData(iStackIndex)
          rStackData(iStackIndex) = 0.
          iStackIndex = iStackIndex - 1
        case (ePOWER)
          rStackData(iStackIndex-1) = rStackData(iStackIndex-1) ** rStackData(iStackIndex)
          rStackData(iStackIndex) = 0.
          iStackIndex = iStackIndex - 1
        case (eSIN)
          rStackData(iStackIndex) = sin(rStackData(iStackIndex))

        case (eCOS)
          rStackData(iStackIndex) = cos(rStackData(iStackIndex))
        case (eTAN)
          rStackData(iStackIndex) = tan(rStackData(iStackIndex))
        case (eASIN)
          rStackData(iStackIndex) = asin(rStackData(iStackIndex))
        case (eACOS)
          rStackData(iStackIndex) = acos(rStackData(iStackIndex))
        case (eATAN)
          rStackData(iStackIndex) = atan(rStackData(iStackIndex))
        case (eSINH)
          rStackData(iStackIndex) = sinh(rStackData(iStackIndex))
        case (eCOSH)
          rStackData(iStackIndex) = cosh(rStackData(iStackIndex))
        case (eTANH)
          rStackData(iStackIndex) = tanh(rStackData(iStackIndex))
        case (eDSIN)
          rStackData(iStackIndex) = dsin(rStackData(iStackIndex))
        case (eDCOS)
          rStackData(iStackIndex) = dcos(rStackData(iStackIndex))
        case (eDTAN)
          rStackData(iStackIndex) = dtan(rStackData(iStackIndex))
        case (eLOG)
          rStackData(iStackIndex) = log(rStackData(iStackIndex))
        case (eLOG10)
          rStackData(iStackIndex) = log10(rStackData(iStackIndex))
        case (eNINT)
          rStackData(iStackIndex) = nint(rStackData(iStackIndex))
        case (eANINT)
          rStackData(iStackIndex) = anint(rStackData(iStackIndex))
        case (eAINT)
          rStackData(iStackIndex) = aint(rStackData(iStackIndex))
        case (eEXP)
          rStackData(iStackIndex) = exp(rStackData(iStackIndex))
        case (eSQRT)
          rStackData(iStackIndex) = sqrt(rStackData(iStackIndex))
        case (eABS)
          rStackData(iStackIndex) = abs(rStackData(iStackIndex))
        case (eFLOOR)
          rStackData(iStackIndex) = floor(rStackData(iStackIndex))
        case (eDOY)
          iStackIndex = iStackIndex + 1
          if(present(tTodayDT) ) then
            rStackData(iStackIndex) = day_of_year(tTodayDT%iJulianDay)
          else
            rStackData(iStackIndex) = 365.25 / 2.
          endif

        case (eDAYS)
!          iStackIndex = iStackIndex + 1
          if(present(tTodayDT) ) then
            tBaseDT%iJulianDay = int(rStackData(iStackIndex))
            tBaseDT%rFractionOfDay = rStackData(iStackIndex) - &
                 real(tBaseDT%iJulianDay, kind=T_SGL)
            rStackData(iStackIndex) = &
                  real(tTodayDT%iJulianDay, kind=T_DBL) &
                + real(tTodayDT%rFractionOfDay, kind=T_DBL) &
                - ( real(tBaseDT%iJulianDay, kind=T_DBL) &
                + real(tBaseDT%rFractionOfDay, kind=T_DBL) )
!            print *, tBaseDT%prettydate(),"  ",tTodayDT%prettydate(),"  _days_:", rStackData(iStackIndex)
          else
            rStackData(iStackIndex) = 365.25 / 2.
          endif

        case default   ! if it's not a number and not a function, must be a variable
          iStackIndex = iStackIndex + 1
          rStackData(iStackIndex) = rVariables(iOpCode(i)-NUMOPCODES)
        end select

      end do

      rResult = rStackData(1)

    endif

  end function evaluate_expression

!------------------------------------------------------------------------------

  subroutine destroyfunc()

  if (allocated(sTokens)) then
    deallocate(sTokens)
  end if
  if (allocated(sOpAddSub)) then
    deallocate(sOpAddSub)
  end if
  if (allocated(sOpMulDiv)) then
    deallocate(sOpMulDiv)
  end if
  if (allocated(rNumber)) then
    deallocate(rNumber)
  end if
  if (allocated(rStackData)) then
    deallocate(rStackData)
  end if
  if (allocated(iOpCode)) then
    deallocate(iOpCode)
  end if
  if (allocated(sVarnames)) then
    deallocate(sVarnames)
  end if
  sStatusFlag = 'ok'

  end subroutine destroyfunc

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------

  recursive subroutine remove_blanks(sFunctionText)
  !
  !  This subroutine removes unnecessary blank spaces
  !
   implicit none

  character (len=MAXEQUATIONLENGTH), intent(inout)    :: sFunctionText
  integer (kind=T_INT)                :: k

  sFunctionText = adjustl(sFunctionText)
  k = index(trim(sFunctionText), ' ')
  if (k /= 0) then
    sFunctionText = sFunctionText(:k-1) // sFunctionText(k+1:)
    call remove_blanks(sFunctionText)
  end if

  end subroutine remove_blanks

!------------------------------------------------------------------------------

   subroutine basic_error_check(sFunction)

   implicit none

   character (len=MAXEQUATIONLENGTH), intent(inout) :: sFunction

   ! [ LOCALS ]
   integer (kind=T_INT) :: i, j
   integer (kind=T_INT) :: iNumChar
   integer (kind=T_INT) :: irightbrackets, ileftbrackets

   irightbrackets = 0; ileftbrackets = 0

   iNumChar = len_trim(sFunction)

   if(iNumChar == 0) then
        call Assert(lFALSE,"Equation was not provided",trim(__FILE__),__LINE__)
     sFunction = 'error'
     return
   end if

   sFunction = lowercase(sFunction)
!   call lowercase(sFunction)

   if(sFunction(iNumChar:iNumChar) == '-' .or. sFunction(iNumChar:iNumChar) == '+' &
       .or. sFunction(iNumChar:iNumChar) == '/' .or. sFunction(iNumChar:iNumChar) == '*') then
     sFunction = 'error'
     call Warn(lFALSE,"Illegal operator at end of equation",trim(__FILE__),__LINE__)
     return
   end if

   if(sFunction(1:1) == '*' .or. sFunction(1:1) == '/') then
     sFunction = 'error'
     call Warn(lFALSE,"Illegal operator at start of equation",trim(__FILE__),__LINE__)
     return
   end if

   do i = 1, iNumChar-1
     if(sFunction(i:i+1) == '--' .or. sFunction(i:i+1) == '-+' .or. sFunction(i:i+1) == '-/' .or. sFunction(i:i+1) == '-*') sFunction = 'error'
     if(sFunction(i:i+1) == '+-' .or. sFunction(i:i+1) == '++' .or. sFunction(i:i+1) == '+/' .or. sFunction(i:i+1) == '+*') sFunction = 'error'
     if(sFunction(i:i+1) == '*-' .or. sFunction(i:i+1) == '*+' .or. sFunction(i:i+1) == '*/') sFunction = 'error'
     if(sFunction(i:i+1) == '/-' .or. sFunction(i:i+1) == '/+' .or. sFunction(i:i+1) == '//' .or. sFunction(i:i+1) == '/*') sFunction = 'error'
   end do
   if(trim(sFunction) == 'error') then
     call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
     return
   endif

   ! scan for right parenthesis without intervening operator
   do i = 1, iNumChar-1
     do j = 1, len_trim(ALPHANUMERIC)
       if(sFunction(i:i+1) == ')'//ALPHANUMERIC(j:j)) then
         call Warn(lFALSE,"Right parenthesis without intervening operator",trim(__FILE__),__LINE__)
         sFunction = 'error'
       endif
     end do
   end do
   if(trim(sFunction) == 'error') return

   ! scan for unbalanced parenthesis
   do i = 1, iNumChar
     if(sFunction(i:i) == "(" ) ileftbrackets = ileftbrackets + 1
     if(sFunction(i:i) == ")" ) irightbrackets = irightbrackets + 1
   enddo
   if(ileftbrackets /= irightbrackets) then
     call Warn(lFALSE,"Unbalanced parentheses in equation",trim(__FILE__),__LINE__)
     sFunction = 'error'
   endif
   if(trim(sFunction) == 'error') return

   do i = 1, iNumChar-1
     do j = 1, len_trim(ALPHANUMERIC)
       if(sFunction(i:i) == '0' .or. sFunction(i:i) == 'n' .or. sFunction(i:i) == 's' &
         .or. sFunction(i:i) == 'h' .or. sFunction(i:i) == 'd' .or. sFunction(i:i) == 'g' &
         .or. sFunction(i:i) == 't' .or. sFunction(i:i) == 'p' .or. sFunction(i:i) == 'r') then
         !não testa, pode ser uma das funções definidas
     else
         if(sFunction(i:i+1) == ALPHANUMERIC(j:j)//'(') then
           call Warn(lFALSE,"Left parenthesis without intervening operator",trim(__FILE__),__LINE__)
           sFunction = 'error'
         endif
       end if
     end do
   end do
   if(trim(sFunction) == 'error') return

   if(iNumChar >= 5) then
   do i = 1,iNumChar-4

   if(sFunction(i:i+4) == 'log10') then
     j = i+5
     19 continue
     if(j >= iNumChar) then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 19
     end if
     if(sFunction(j:j) /= '(') then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+4) == 'anint') then
     j = i+5
     20 continue
     if(j >= iNumChar) then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 20
     end if
     if(sFunction(j:j) /= '(') then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+4) == 'floor') then
     j = i+5
     21 continue
     if(j >= iNumChar) then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 21
     end if
     if(sFunction(j:j) /= '(') then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
   end if

   end do
   end if


   if(iNumChar >= 4) then
   do i = 1,iNumChar-3

   if(sFunction(i:i+3) == 'asin') then
     j = i+4
     7 continue
     if(j >= iNumChar) then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 7
     end if
     if(sFunction(j:j) /= '(') then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'acos') then
     j = i+4
     8 continue
     if(j >= iNumChar) then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 8
     end if
     if(sFunction(j:j) /= '(') then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'atan') then
     j = i+4
     9 continue
     if(j >= iNumChar) then
       call Warn(lFALSE,"Problem in equation parser",trim(__FILE__),__LINE__)
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 9
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'sinh') then
     j = i+4
     10 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 10
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'cosh') then
     j = i+4
     11 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 11
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'tanh') then
     j = i+4
     12 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 12
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'sind') then
     j = i+4
     13 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 13
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'cosd') then
     j = i+4
     14 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 14
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'tand') then
     j = i+4
     15 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 15
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'nint') then
     j = i+4
     16 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 16
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'aint') then
     j = i+4
     17 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 17
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+3) == 'sqrt') then
     j = i+4
     18 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 18
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   end do
   end if




   if(iNumChar >= 3) then
   do i = 1,iNumChar-2

   if(sFunction(i:i+2) == 'sin') then
     j = i+3
     1 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 1
     end if
     if(sFunction(j:j) == 'd' .or. sFunction(j:j) == 'h') goto 51
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
     51 continue
   end if

   if(sFunction(i:i+2) == 'cos') then
     j = i+3
     2 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 2
     end if
     if(sFunction(j:j) == 'd' .or. sFunction(j:j) == 'h') goto 52
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
     52 continue
   end if

   if(sFunction(i:i+2) == 'tan') then
     j = i+3
     3 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 3
     end if
     if(sFunction(j:j) == 'd' .or. sFunction(j:j) == 'h') goto 53
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
     53 continue
   end if

   if(sFunction(i:i+2) == 'log') then
     j = i+3
     4 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 4
     end if
     if(j < (iNumChar-1) .and. sFunction(j:j+1) == '10') goto 54
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
     54 continue
   end if

   if(sFunction(i:i+2) == 'exp') then
     j = i+3
     5 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 5
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   if(sFunction(i:i+2) == 'abs') then
     j = i+3
     6 continue
     if(j >= iNumChar) then
       sFunction = 'error'
     return
     end if
     if(sFunction(j:j) == ' ') then
       j = j + 1
     goto 6
     end if
     if(sFunction(j:j) /= '(') then
       sFunction = 'error'
     return
     end if
   end if

   end do
   endif

   return
   end subroutine

!------------------------------------------------------------------------------

   subroutine convert_brackets(sText)

   implicit none

   character (len=MAXEQUATIONLENGTH), intent(inout) :: sText

   ! [ LOCALS ]
   integer (kind=T_INT) :: iLength
   integer (kind=T_INT) :: iItem
   integer (kind=T_INT) :: k
   integer (kind=T_INT) :: j

   iLength = len_trim(sText)

   ! replace square brackets or curly braces with parenthesis
   do k = 1,iLength
      if(sText(k:k) == '[' .or. sText(k:k) == '{' ) sText(k:k)='('
      if(sText(k:k) == ']' .or. sText(k:k) == '}' ) sText(k:k)=')'
   end do

!   ! replace quotes with "<", ">"symbols
   j = 2
   do k = 1,iLength
      if(sText(k:k) == '"') then
        j = j + 1
        if(mod(j,2) == 1 ) then
          sText(k:k)='<'
        else
          sText(k:k)='>'
        endif
      endif
   enddo

!   call assert(j == 0 .and. mod(j,2) == 0, "Unmatched quotes in equation " &
!     //trim(sText)//".",trim(__FILE__),__LINE__)
!
   ! convert "ln" to "log"
   k=1
   do while (k /= 0)
     k=index(trim(sText), "ln")
     iLength = len_trim(sText)
     if(k /= 0) sText = sText(1:k-1)//'log'//sText(k+2:iLength)
   end do

   ! replace "**" (for exponentiation) with "^"
   k=1
   do while (k /= 0)
     k=index(trim(sText), "**")
     iLength = len_trim(sText)
     if(k /= 0) sText = sText(1:k-1)//'^'//sText(k+2:iLength)
   end do

   ! replace text reference for pi with numerical value
   k=1
   do while (k /= 0)
     k=index(trim(sText), "pi")
     iLength = len_trim(sText)
     if(k /= 0) sText = sText(1:k-1)//'3.1415926535'//sText(k+2:iLength)
   end do

end subroutine

!------------------------------------------------------------------------------

end module tsp_equations_interpreter
