program test

  use tsp_data_structures
  use tsp_main_loop
  implicit none

  character(len=256) :: sFilename
  character (len=256) :: sBlockname
  integer (kind=T_INT) :: iReturnCode

  character (len=32) :: sContext
  character (len=32) :: sDateFormat

  iReturnCode = -1

  sFilename = "tsproc_test.inp"

  call openControlFile(sFilename)

!  call getNextBlock(sBlockname)
!  if(trim(sBlockname) == "SETTINGS") call processSettingsBlock(sContext, sDateFormat)

  do while(iReturnCode /= 0)
    call getNextBlock(sBlockname)
    call continueProcessing(iReturnCode)
  end do

  call finalizeRun()

end program test
