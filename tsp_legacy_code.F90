module tsp_legacy_code

  use tsp_data_structures
  use tsp_control_file_ops
  use tsp_collections
  use tsp_utilities
  implicit none

  private
  public :: pest_files, slide, locmin, fixed

  integer (kind=T_INT) :: MAXSERIES
  integer (kind=T_INT)  :: MAXTABLES
  integer (kind=T_INT), parameter  :: MAXCONTEXT = 5
  integer (kind=T_INT), parameter  :: MAXTEMPDURFLOW = 100
  integer (kind=T_INT), parameter  :: MAXTEMPFILE = 100
  integer (kind=T_INT), parameter  :: MAXPAR = 1000
  character, parameter  :: OBSCHAR='_'

  !variables for writing a message ------->
 	integer                 :: imessage=0
 	character (len=500)     :: amessage= ' '
   character*120 sInfile,sRecfile,sOutfile,sString

contains

subroutine set_global_variables(TS)

  type (TIME_SERIES_COLLECTION), intent(in) :: TS

  MAXSERIES = size(TS%pTS)
  MAXTABLES = size(TS%tTable)

end subroutine set_global_variables

!------------------------------------------------------------------------------

!     Last change:  J     9 Sep 2004   10:39 pm
subroutine pest_files(pBlock, TS)

  implicit none

  type (TIME_SERIES_COLLECTION), intent(inout) :: TS
  type (T_BLOCK), pointer, intent(in) :: pBlock

  integer LU_TSPROC_CONTROL,LU_OUT
  integer NumProcBloc_g,ILine_g,IProcSetting_g
  character*25 Context_g
  character*40 CurrentBlock_g
  character*120 sInfile,sRecfile,sOutfile,sString
  type (T_TIME_SERIES), pointer :: pTS_Observed
  type (T_TIME_SERIES), pointer :: pTS_Modeled
  type (T_TABLE), pointer :: pTable

! -- The following variables are global because they are used to exchange information
!    between the LIST_OUTPUT block and the WRITE_PEST_FILES block.

  integer iMseries_g
  integer iMstable_g
  integer iMctable_g
  integer iMvtable_g
  integer iMdtable_g

!  integer iOutseries_g(MAXSERIES),iOutStable_g(MAXSTABLE),iOutVtable_g(MAXVTABLE), &
!          iOutDtable_g(MAXDTABLE),iOutCtable_g(MAXCTABLE)
  character*10 sSeriesFormat_g

! -- General parameters
       logical lexist
       integer (kind=T_INT) :: ierr,icontext,itempfile,ioseries,iostable, &
         iovtable,iodtable,i,iunit,j, &
         jline,numtempfile,ii1,ll,jj1,jj,kk,io,im,noterm,nmterm,iomseries,iomstable,  &
         iomvtable,iomdtable,iout,nsterm,iterm,il,siout,nobs,nobsgp,ieqnerr,nterm,nnterm, &
         isnum,dd,nn,yy,mm,k,ixcon,auiyesno,itempunit,isvd,iaui,itemp,LU_PEST_CONTROL_FILE
       real rotemp,rmtemp,rprecis,weightmin,weightmax,totim,rtime,eigthresh
       integer, dimension (:), allocatable :: obsseries,obsstable,obsvtable, &
         obsdtable,modseries,modstable, &
         modvtable,moddtable
       real, dimension(:), allocatable ::  sweightmin,sweightmax,stweightmin, &
         stweightmax,vtweightmin,vtweightmax, &
         dtweightmin,dtweightmax
       double precision dval,dtempx
       character*1 aa
       character*3 auiaa
       character*10 aoname,amname,anum,atrans
       character*15 aline,avariable
       character*30 aoption,correct_keyword,last_keyword,atemp,otherblock,aname
       character*120 pardatfile,pestctlfile,instructfile,modcomline,bstring,cstring, &
         micactlfile,pest2micacom
       character*25 acontext(MAXCONTEXT)
       character*12, dimension(:), allocatable :: basename,sbasename, obgnme
       character*120, dimension(:), allocatable :: tempfile,modfile
       character*150, dimension(:), allocatable :: sequation,stequation,vtequation, &
                     dtequation
       character*150 eqntext

! -- Variable used for dealing with parameter groups.

       integer                   :: igp,f_numpargp,npargp
       real,         allocatable :: f_derinc(:),f_derinclb(:),f_derincmul(:), derinc(:), &
                                    derinclb(:),derincmul(:)
       character*14              :: apargp
       character*120             :: pargroupfile
       character*14, allocatable :: f_pargpnme(:),f_inctyp(:),f_forcen(:),f_dermthd(:), &
                                    forcen(:),dermthd(:),pargpnme(:),inctyp(:)

! -- Variable used for dealing with parameter data.

       integer                   :: ipar,f_numpar,npar,tempunit,nnpar
       real, allocatable         :: f_parval1(:),f_parlbnd(:),f_parubnd(:),f_scale(:), &
                                    f_offset(:),parval1(:),parlbnd(:),parubnd(:),      &
                                    scale(:),offset(:)
       character*1               :: pardelim
       character*12              :: aapar
       character*12              :: apar(MAXPAR)
       character*14, allocatable :: f_parnme(:),f_parchglim(:),f_pargp(:), &
                                    parchglim(:),pargp(:)
       character*19, allocatable :: f_partrans(:),partrans(:)

       character (len=256) :: sRecord, sItem
       integer (kind=T_INT) :: iStat
       integer (kind=T_INT) :: iNumSeries, iNumTables

    character (len=MAXARGLENGTH), dimension(:), pointer :: pTEMPLATE_FILE, &
      pMODEL_INPUT_FILE, pPARAMETER_DATA_FILE, pPARAMETER_GROUP_FILE, &
      pOBSERVATION_SERIES_NAME, pMODEL_SERIES_NAME, pSERIES_WEIGHTS_EQUATION, &
      pSERIES_WEIGHTS_MIN_MAX, pOBSERVATION_TABLE_NAME, pMODEL_TABLE_NAME, &
      pTABLE_WEIGHTS_EQUATION, pNEW_PEST_CONTROL_FILE, pAUTOMATIC_USER_INTERVENTION, &
      pNEW_INSTRUCTION_FILE, pMODEL_COMMAND_LINE, pNEW_MICA_CONTROL_FILE, &
      pPEST2MICA_COMMAND

    character (len=256), dimension(:), pointer :: pItems
    real (kind=T_SGL), dimension(:), pointer :: pTRUNCATED_SVD
    character (len=2) :: sSet

! -- Initialisation

       ieqnerr=0
       sSet = cTAB//" "   ! inform 'Chomp' that tabs *or* spaces are the delimiters

       call set_global_variables(TS)

       isvd=0
       iaui=0
       auiyesno=0
       icontext=0
       itempfile=0
!        tempfile=' '             ! tempfile is an array
!        modfile=' '              ! modfile is an array
!
!        sequation=' '            ! sequation is an array
!        stequation=' '           ! stequation is an array
!        vtequation=' '           ! vtequation is an array
!        dtequation=' '           ! dtequation is an array

       ioseries=0; iostable=0; iovtable=0; iodtable=0; iomseries=0
       iomstable=0;iomvtable=0;iomdtable=0

       pardatfile=' '; pargroupfile=' '; pestctlfile=' '; instructfile=' '
       modcomline=' '; micactlfile=' '; pest2micacom=' '

!        sweightmin=-1.0e36              !sweightmin is an array
!        sweightmax= 1.0e36              !sweightman is an array
!        stweightmin=-1.0e36             !stweightmin is an array
!        stweightmax= 1.0e36             !stweightmax is an array
!        vtweightmin=-1.0e36              !vtweightmin is an array
!        vtweightmax= 1.0e36             !vtweightmax is an array
!        dtweightmin=-1.0e36             !dtweightmin is an array
!        dtweightmax= 1.0e36             !dtweightmax is an array

       f_numpargp=0; f_numpar=0
       ixcon=0; iunit=0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -- The PEST_FILES block is first parsed.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         pTEMPLATE_FILE => pBlock%getString("TEMPLATE_FILE")
         pMODEL_INPUT_FILE => pBlock%getString("MODEL_INPUT_FILE")
         pPARAMETER_DATA_FILE => pBlock%getString("PARAMETER_DATA_FILE")
         pPARAMETER_GROUP_FILE => pBlock%getString("PARAMETER_GROUP_FILE")
         pAUTOMATIC_USER_INTERVENTION => pBlock%getString("AUTOMATIC_USER_INTERVENTION")
         if(str_compare(pAUTOMATIC_USER_INTERVENTION(1),"yes") ) then
           auiaa = "yes"
         else
           auiaa = "no"
         endif

         pTRUNCATED_SVD => pBlock%getReal("TRUNCATED_SVD")
         if(pTRUNCATED_SVD(1) > rNEAR_TINY) then
           eigthresh = pTRUNCATED_SVD(1)
           isvd=1
         endif
         pNEW_PEST_CONTROL_FILE => pBlock%getString("NEW_PEST_CONTROL_FILE")
         pNEW_MICA_CONTROL_FILE => pBlock%getString("NEW_MICA_CONTROL_FILE")
         pPEST2MICA_COMMAND => pBlock%getString("PEST2MICA_COMMAND")
         pNEW_INSTRUCTION_FILE => pBlock%getString("NEW_INSTRUCTION_FILE")
         pMODEL_COMMAND_LINE => pBlock%getString("MODEL_COMMAND_LINE")
         pOBSERVATION_SERIES_NAME => pBlock%findString("OBSERVATION_SERIES_NAME")
         pOBSERVATION_TABLE_NAME => pBlock%findString("OBSERVATION_TABLE_NAME")
         pMODEL_SERIES_NAME => pBlock%findString("MODEL_SERIES_NAME")
         pMODEL_TABLE_NAME => pBlock%findString("MODEL_TABLE_NAME")
         pSERIES_WEIGHTS_EQUATION => pBlock%findString("SERIES_WEIGHTS_EQUATION")
         pTABLE_WEIGHTS_EQUATION => pBlock%findString("TABLE_WEIGHTS_EQUATION")

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -- Any absenses in the block are now looked for.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       call assert(.not. (str_compare(pOBSERVATION_SERIES_NAME(1),"NA") .and. &
            str_compare(pOBSERVATION_TABLE_NAME(1),"NA")), &
            "No observation series or table names have been cited in WRITE_PEST_FILES block", &
            trim(__FILE__), __LINE__)

       call assert(.not. str_compare(pTEMPLATE_FILE(1),"NA"), &
            "At least one TEMPLATE_FILE keyword must be provided in WRITE_PEST_FILES block", &
            trim(__FILE__), __LINE__)
       ! we must have at least one template file; OK to process
       numtempfile = size(pTEMPLATE_FILE)
       allocate(tempfile(numtempfile))
       tempfile = pTEMPLATE_FILE

       call assert(.not. str_compare(pMODEL_INPUT_FILE(1),"NA"), &
            "At least one MODEL_INPUT_FILE keyword must be provided in WRITE_PEST_FILES block", &
            trim(__FILE__), __LINE__)

       call assert(numtempfile == size(pMODEL_INPUT_FILE), &
            "A MODEL_INPUT_FILE argument must be supplied for each TEMPLATE_FILE entry", &
            trim(__FILE__),__LINE__)
       allocate(modfile(numtempfile))
       modfile = pMODEL_INPUT_FILE

       call assert (size(pMODEL_SERIES_NAME) == size(pOBSERVATION_SERIES_NAME), &
           "You must provide a model series name for each observation series name", &
           trim(__FILE__),__LINE__)

       call assert (size(pMODEL_SERIES_NAME) == size(pSERIES_WEIGHTS_EQUATION), &
           "You must provide a weights equation for each model and observation series pair", &
           trim(__FILE__),__LINE__)

       nobs = 0
       if( .not.  (str_compare(pOBSERVATION_SERIES_NAME(1),"NA")) ) then
         ! create comparison objects for time series
         do i=1,size(pMODEL_SERIES_NAME)
           call TS%tsCompare(pOBSERVATION_SERIES_NAME(i), pMODEL_SERIES_NAME(i), &
              pSERIES_WEIGHTS_EQUATION(i) )
           nobs = nobs + size(TS%tTSComparison(i)%rWeightValue)
         enddo
       endif

       if( .not.  (str_compare(pOBSERVATION_TABLE_NAME(1),"NA")) ) then
         ! create comparison objects for tables
         do i=1,size(pMODEL_TABLE_NAME)
           call TS%tableCompare(pOBSERVATION_TABLE_NAME(i), pMODEL_TABLE_NAME(i), &
              pTABLE_WEIGHTS_EQUATION(i) )
           nobs = nobs + size(TS%tTableComparison(i)%rWeightValue)
         enddo
       endif

       if(str_compare(pOBSERVATION_TABLE_NAME(1),"NA")) then
         iNumTables = 0
       else
         iNumTables = size(pOBSERVATION_TABLE_NAME)
       endif

       if(str_compare(pOBSERVATION_SERIES_NAME(1),"NA")) then
         iNumSeries = 0
       else
         iNumSeries = size(pOBSERVATION_SERIES_NAME)
       endif

         ! can add more functionality later by allowing user-specified obs groups
         ! to be entered and recorded
!       if(str_compare(pOBSERVATION_GROUP_NAME(1),"NA"))then

         nobsgp = iNumTables + iNumSeries
         allocate(obgnme(nobsgp) )

         j=0
         do i=1,iNumSeries
           j = j + 1
           obgnme(j) = pOBSERVATION_SERIES_NAME(i)
         enddo

         do i=1,iNumTables
           j = j + 1
           obgnme(j) = pOBSERVATION_TABLE_NAME(i)
         enddo

!       else


!       endif



!        if(pestctlfile == ' ')then
!          write(amessage,230) trim(CurrentBlock_g)
! 230      format('a NEW_PEST_CONTROL_FILE keyword must be provided in a ',a,' block.')
!          go to 9800
!        end if
!        if(instructfile == ' ')then
!          write(amessage,240) trim(CurrentBlock_g)
! 240      format('NEW_INSTRUCTION_FILE keyword is missing from the ',a,' block.')
!          go to 9800
!        end if
!        if(ioseries.ne.0)then
!          if(iomseries.ne.ioseries)then
!            write(amessage,241) trim(CurrentBlock_g)
! 241        format('a MODEL_SERIES_NAME keyword has not been provided for each ', &
!              'OBSERVATION_SERIES_NAME cited in the ',a,' block.')
!            go to 9800
!          end if
!          do i=1,ioseries
!            if(sequation(i) == ' ')then
!              write(amessage,250) trim(CurrentBlock_g)
! 250          format('a SERIES_WEIGHTS_EQUATION keyword has not been provided for each ', &
!              'series cited in the ',a,' block.')
!              go to 9800
!            end if
!          end do
!        end if
!        if(iostable.ne.0)then
!          if(iomstable.ne.iostable)then
!            write(amessage,251) trim(CurrentBlock_g)
! 251        format('a MODEL_S_TABLE_NAME keyword has not been provided for each ', &
!              'OBSERVATION_S_TABLE_NAME cited in the ',a,' block.')
!            go to 9800
!          end if
!          do i=1,iostable
!            if(stequation(i) == ' ')then
!              write(amessage,260) trim(CurrentBlock_g)
! 260          format('an S_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
!              's_table cited in the ',a,' block.')
!              go to 9800
!            end if
!          end do
!        end if
!        if(iovtable.ne.0)then
!          if(iomvtable.ne.iovtable)then
!            write(amessage,261) trim(CurrentBlock_g)
! 261        format('a MODEL_V_TABLE_NAME keyword has not been provided for each ', &
!              'OBSERVATION_V_TABLE_NAME cited in the ',a,' block.')
!            go to 9800
!          end if
!          do i=1,iovtable
!            if(vtequation(i) == ' ')then
!              write(amessage,270) trim(CurrentBlock_g)
! 270          format('a V_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
!              'v_table cited in the ',a,' block.')
!              go to 9800
!            end if
!          end do
!        end if
!        if(iodtable.ne.0)then
!          if(iomdtable.ne.iodtable)then
!            write(amessage,271) trim(CurrentBlock_g)
! 271        format('a MODEL_E_TABLE_NAME keyword has not been provided for each ', &
!              'OBSERVATION_E_TABLE_NAME cited in the ',a,' block.')
!            go to 9800
!          end if
!          do i=1,iodtable
!            if(dtequation(i) == ' ')then
!              write(amessage,280) trim(CurrentBlock_g)
! 280          format('a E_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
!              'e_table cited in the ',a,'block.')
!              go to 9800
!            end if
!          end do
!        end if
!        if(icontext == 0)then
!          write(amessage,290) trim(CurrentBlock_g)
! 290      format('no Context_g keyword(s) provided in ',a,' block.')
!          go to 9800
!        end if
!        if((micactlfile.ne.' ').and.(pest2micacom == ' '))then
!          write(amessage,291)
! 291      format('if a NEW_MICA_CONTROL_FILE keyword is supplied, a PEST2MICA_COMMAND ', &
!          'keyword must also be supplied.')
!          go to 9800
!        end if
!        if((isvd == 1).and.(iaui == 1))then
!          write(amessage,292) trim(CurrentBlock_g)
! 292      format('only one of the TRUNCATED_SVD or AUTOMATIC_USER_INTERVENTION keywords ', &
!          'must be supplied in ',a,' block.')
!          go to 9800
!        end if
!
! ! -- Before any processing is done, a check is made that the observation series and
! !    tables correspond to the series and tables requested for output in the last
! !    LIST_OUTPUT block.
!
!        otherblock='LIST_OUTPUT'
!        if((ioseries.ne.iMseries_g).or.(iostable.ne.iMstable_g).or.(iovtable.ne.iMvtable_g).or. &
!           (iodtable.ne.iMdtable_g))then
!           write(amessage,1010) trim(CurrentBlock_g),trim(otherblock)
! 1010      format('the number of series, s_tables, e_tables and v_tables cited in the ', &
!           a,' block does not correspond exactly to the number of these entities cited in ', &
!           'the immediately-preceding ',a,' block.')
!           go to 9800
!        end if
!        if ( iMctable_g .ne. 0 ) then
!          write(amessage,1011)
! 1011     format('a c_table features in the LIST_OUTPUT block preceding ', &
!          'the WRITE_PEST_FILES block. The present version of TSPROC does not ', &
!          'support the use of c_tables in the calibration process.')
!          go to 9800
!        end if
!*********************************************************************************************
!        if(ioseries.ne.0)then
!          do i=1,ioseries
!            io=obsseries(i)
!            im=modseries(i)
!            aoname=series_g(io)%name
!            amname=series_g(im)%name
!            if(io == im) go to 1029
!            noterm=series_g(io)%nterm
!            nmterm=series_g(im)%nterm
!            if(noterm.ne.nmterm)then
!              write(amessage,1020) trim(aoname),trim(amname)
! 1020         format('OBSERVATION_SERIES "',a,'" has been matched to ', &
!              'MODEL_SERIES "',a,'". However these series have different ', &
!              'numbers of terms.')
!              go to 9800
!            end if
!            do j=1,noterm
!              if((series_g(io)%days(j).ne.series_g(im)%days(j)).or.   &
!                 (series_g(io)%secs(j).ne.series_g(im)%secs(j)))then
!                write(amessage,1030) trim(aoname),trim(amname)
! 1030           format('OBSERVATION_SERIES "',a,'" has been matched to ', &
!                'MODEL_SERIES "',a,'". However the dates and times in ', &
!                'these SERIES do not correspond.')
!                go to 9800
!              end if
!            end do
! 1029       continue
!            do j=1,ioseries
!              if(im == iOutseries_g(j)) go to 1035
!            end do
!            write(amessage,1032) trim(amname),trim(CurrentBlock_g)
! 1032       format('MODEL__SERIES "',a,'" is not listed in the ', &
!            'LIST_OUTPUT block immediately preceding the ',a,' block.')
!            go to 9800
! 1035       continue
!          end do
!        end if
!
!        if(iostable.ne.0)then
!          do i=1,iostable
!            io=obsstable(i)
!            im=modstable(i)
!            aoname=stable_g(io)%name
!            amname=stable_g(im)%name
!            if(io == im) go to 1039
!            if(((stable_g(io)%maximum < -1.0e36).and.(stable_g(im)%maximum > -1.0e36)).or.  &
!               ((stable_g(io)%maximum > -1.0e36).and.(stable_g(im)%maximum < -1.0e36)))then
!              avariable='MAXIMUM'
!              go to 9600
!            end if
!            if(((stable_g(io)%minimum < -1.0e36).and.(stable_g(im)%minimum > -1.0e36)).or.  &
!               ((stable_g(io)%minimum > -1.0e36).and.(stable_g(im)%minimum < -1.0e36)))then
!              avariable='MINIMUM'
!              go to 9600
!            end if
!            if(((stable_g(io)%range < -1.0e36).and.(stable_g(im)%range > -1.0e36)).or.  &
!               ((stable_g(io)%range > -1.0e36).and.(stable_g(im)%range < -1.0e36)))then
!              avariable='RANGE'
!              go to 9600
!            end if
!            if(((stable_g(io)%mean < -1.0e36).and.(stable_g(im)%mean > -1.0e36)).or.   &
!               ((stable_g(io)%mean > -1.0e36).and.(stable_g(im)%mean < -1.0e36)))then
!              avariable='MEAN'
!              go to 9600
!            end if
!            if(((stable_g(io)%stddev < -1.0e36).and.(stable_g(im)%stddev > -1.0e36)).or.  &
!               ((stable_g(io)%stddev > -1.0e36).and.(stable_g(im)%stddev < -1.0e36)))then
!              avariable='STD_DEV'
!              go to 9600
!            end if
!            if(((stable_g(io)%total < -1.0e36).and.(stable_g(im)%total > -1.0e36)).or.   &
!               ((stable_g(io)%total > -1.0e36).and.(stable_g(im)%total < -1.0e36)))then
!              avariable='SUM'
!              go to 9600
!            end if
!            if(((stable_g(io)%minmean < -1.0e36).and.(stable_g(im)%minmean > -1.0e36)).or.   &
!               ((stable_g(io)%minmean > -1.0e36).and.(stable_g(im)%minmean < -1.0e36)))then
!              avariable='MINMEAN_*'
!              go to 9600
!            end if
!            if(((stable_g(io)%maxmean < -1.0e36).and.(stable_g(im)%maxmean > -1.0e36)).or.   &
!               ((stable_g(io)%maxmean > -1.0e36).and.(stable_g(im)%maxmean < -1.0e36)))then
!              avariable='MAXMEAN_*'
!              go to 9600
!            end if
!            if((stable_g(io)%maxmean > -1.0e36).or.(stable_g(io)%minmean > -1.0e36))then
!              write(amessage,1023)
! 1023         format('The present version of TSPROC does not support the use of ', &
!              'S_TABLE minimum or maximum sample count averages in the calibration process.')
!              go to 9800
!            end if
! 1039       continue
!            do j=1,iostable
!              if(im == iOutStable_g(j)) go to 1038
!            end do
!            write(amessage,1037) 'S',trim(amname),trim(CurrentBlock_g)
! 1037       format('MODEL_',a,'_TABLE "',a,'" is not listed in the ', &
!            'LIST_OUTPUT block immediately preceding the ',a,' block.')
!            go to 9800
! 1038       continue
!          end do
!        end if
!
!        if(iovtable.ne.0)then
!          do i=1,iovtable
!            io=obsvtable(i)
!            im=modvtable(i)
!            aoname=vtable_g(io)%name
!            amname=vtable_g(im)%name
!            if(io == im) go to 1047
!            noterm=vtable_g(io)%nterm
!            nmterm=vtable_g(im)%nterm
!            if(noterm.ne.nmterm)then
!              write(amessage,1040) trim(aoname),trim(amname)
! 1040         format('OBSERVATION_V_TABLE "',a,'" has been matched to ', &
!              'MODEL_V_TABLE "',a,'". However these V_TABLES ', &
!              'have different numbers of integration times.')
!              go to 9800
!            end if
!            do j=1,noterm
!              if((vtable_g(io)%days1(j).ne.vtable_g(im)%days1(j)).or.   &
!                 (vtable_g(io)%days2(j).ne.vtable_g(im)%days2(j)).or.   &
!                 (vtable_g(io)%secs1(j).ne.vtable_g(im)%secs1(j)).or.   &
!                 (vtable_g(io)%secs2(j).ne.vtable_g(im)%secs2(j)))then
!                write(amessage,1050) trim(aoname),trim(amname)
! 1050           format('OBSERVATION_V_TABLE "',a,'" has been matched to ', &
!                'MODEL_V_TABLE "',a,'". However the integration dates and ', &
!                'times in these V_TABLES do not correspond.')
!                go to 9800
!              end if
!            end do
! 1047       continue
!            do j=1,iovtable
!              if(im == iOutVtable_g(j)) go to 1048
!            end do
!            write(amessage,1037) 'V',trim(amname),trim(CurrentBlock_g)
!            go to 9800
! 1048       continue
!          end do
!        end if
!
!        if(iodtable.ne.0)then
!          do i=1,iodtable
!            io=obsdtable(i)
!            im=moddtable(i)
!            aoname=dtable_g(io)%name
!            amname=dtable_g(im)%name
!            if(io == im) go to 1079
!            noterm=dtable_g(io)%nterm
!            nmterm=dtable_g(im)%nterm
!            if(noterm.ne.nmterm)then
!              write(amessage,1060) trim(aoname),trim(amname)
! 1060         format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
!              'MODEL E_TABLE "',a,'". However these E_TABLES ', &
!              'have different numbers of flows.')
!              go to 9800
!            end if
!            if(dtable_g(io)%under_over.ne.dtable_g(im)%under_over)then
!              write(amessage,1061) trim(aoname),trim(amname)
! 1061         format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
!              'MODEL E_TABLE "',a,'". However these E_TABLES ', &
!              'have different UNDER_OVER specifications.')
!              go to 9800
!            end if
!            do j=1,noterm
!              rotemp=dtable_g(io)%flow(j)
!              rmtemp=dtable_g(im)%flow(j)
!              rprecis=5*spacing(rmtemp)
!              if((rotemp < rmtemp-rprecis).or.(rotemp > rmtemp+rprecis))then
!                write(amessage,1070) trim(aoname),trim(amname)
! 1070           format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
!                'MODEL E_TABLE "',a,'". However the flows in ', &
!                'these E_TABLES do not correspond.')
!                go to 9800
!              end if
!            end do
!            do j=1,noterm
!              rotemp=dtable_g(io)%tdelay(j)
!              rmtemp=dtable_g(im)%tdelay(j)
!              rprecis=5*spacing(rmtemp)
!              if((rotemp < rmtemp-rprecis).or.(rotemp > rmtemp+rprecis))then
!                write(amessage,1071) trim(aoname),trim(amname)
! 1071           format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
!                'MODEL E_TABLE "',a,'". However the time delays in ', &
!                'these E_TABLES do not correspond.')
!                go to 9800
!              end if
!            end do
! 1079       continue
!            do j=1,iodtable
!              if(im == iOutDtable_g(j)) go to 1078
!            end do
!            write(amessage,1037) 'D',trim(amname),trim(CurrentBlock_g)
!            go to 9800
! 1078       continue
!          end do
!        end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -- If present, the parameter group file is read.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (.not. str_compare(pPARAMETER_GROUP_FILE(1),"NA") ) then

    call echolog("")
    call echolog('     Reading parameter group file '//trim(pPARAMETER_GROUP_FILE(1)) )

       open(newunit=iunit,file=trim(pPARAMETER_GROUP_FILE(1)),status='old',iostat=ierr)

       call assert(ierr==0,"cannot open parameter group file "//trim(pPARAMETER_GROUP_FILE(1)), &
         trim(__FILE__), __LINE__)

! -- The file is read a first time to find out the number of groups

       jline=0
       f_numpargp=0
       do
         jline=jline+1
         read(iunit,'(a)',iostat = ierr) sRecord
         if(ierr /= 0) exit
         if(sRecord == ' ') cycle
         if(sRecord(1:1) == '#') cycle
         f_numpargp=f_numpargp+1
       end do

       call assert(f_numpargp > 0, "File "//quote(pPARAMETER_GROUP_FILE(1))&
         //" appears to contain no data.", trim(__FILE__),__LINE__)

       allocate(f_pargpnme(f_numpargp),f_inctyp(f_numpargp),f_derinc(f_numpargp), &
                f_derinclb(f_numpargp),f_forcen(f_numpargp),f_derincmul(f_numpargp), &
                f_dermthd(f_numpargp),stat=ierr)

       rewind(unit=iunit,iostat=ierr)
       call assert(ierr==0,"cannot rewind parameter group file "//quote(pPARAMETER_GROUP_FILE(1)), &
         trim(__FILE__), __LINE__)

! -- Now it is read a second time to obtain the data.

       jline=0
       igp=0
       do
         jline=jline+1
         READ(iunit,'(A)',iostat = iStat) sRecord
         if(iStat /= 0) exit
         if(sRecord == ' ') cycle
         if(sRecord(1:1) == '#') cycle
!       IF(len_trim(atemp) > 12)THEN
!         write(amessage,330) trim(atemp),trim(aline),trim(sString)
!330      format('parameter group name "',a,'" greater than 12 characters ', &
!         'at line ',a,' of file ',a)
!         go to 9800
!       end if

         igp=igp+1

         ! read in parameter group name
         call Chomp(sRecord, sItem, sSet)
         f_pargpnme(igp)=trim(sItem)
         call assert(.not. str_compare(f_pargpnme(igp), "none"), &
          'Parameter group name "none" in file '//quote(pPARAMETER_GROUP_FILE(1))//' is a reserved ' &
          //'name, used for some fixed and tied parameters.', &
           trim(__FILE__),__LINE__)

         ! read in INCTYP  (increment type)
         call Chomp(sRecord, sItem, sSet)
         f_inctyp(igp)=trim(sItem)
         call assert(str_compare(f_inctyp(igp),'relative') .or. &
                  str_compare(f_inctyp(igp),'absolute') .or.  &
                  str_compare(f_inctyp(igp),'rel_to_max'), &
           'INCTYP on line '//trim(asChar(jline))//' of file '//quote(pPARAMETER_GROUP_FILE(1)) &
           //' must be "relative", "absolute" or "rel_to_max".', &
           trim(__FILE__),__LINE__)

         call Chomp(sRecord, sItem, sSet)
         f_derinc(igp) = asReal(sItem)


!        call char2num(ierr,cline(left_word(3):right_word(3)),f_derinc(igp))
!        if(ierr.ne.0)then
!          write(amessage,590) 'DERINC',trim(aline),trim(sString)
!          go to 9800
!        end if
!        if(f_derinc(igp).le.0.0)then
!          write(amessage,370) 'DERINC',trim(aline),trim(sString)
! 370      format('value for ',a,' on line ',a,' of file ',a,' must be positive.')
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_derinclb(igp) = asReal(sItem)

!        call char2num(ierr,cline(left_word(4):right_word(4)),f_derinclb(igp))
!        if(ierr.ne.0)then
!          write(amessage,590) 'DERINCLB',trim(aline),trim(sString)
!          go to 9800
!        end if
!        if(f_derinclb(igp) < 0.0)then
!          write(amessage,390) 'DERINCLB',trim(aline),trim(sString)
! 390      format('value for ',a,' on line ',a,' of file ',a,' must not be negative.')
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_forcen(igp) = trim(sItem)

!        f_forcen(igp)=cline(left_word(5):right_word(5))
!        call remchar(f_forcen(igp),'"')
!        if((f_forcen(igp).ne.'switch').and.(f_forcen(igp).ne.'always_2').and.  &
!           (f_forcen(igp).ne.'always_3'))then
!           write(amessage,400) trim(aline),trim(sString)
! 400       format('FORCEN must be "switch", "always_2" or "always_3" at line ',a,  &
!           ' of file ',a)
!           go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_derincmul(igp) = asReal(sItem)

!        call char2num(ierr,cline(left_word(6):right_word(6)),f_derincmul(igp))
!        if(ierr.ne.0)then
!          write(amessage,590) 'DERINCMUL',trim(aline),trim(sString)
!          go to 9800
!        end if
!        if(f_derincmul(igp).le.0.0)then
!          write(amessage,370) 'DERINCMUL',trim(aline),trim(sString)
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_dermthd(igp) = trim(sItem)

!        f_dermthd(igp)=cline(left_word(7):right_word(7))
!        call remchar(f_dermthd(igp),'"')
!        if((f_dermthd(igp).ne.'parabolic').and.(f_dermthd(igp).ne.'best_fit')  &
!           .and.(f_dermthd(igp).ne.'outside_pts'))then
!           write(amessage,420) trim(aline),trim(sString)
! 420       format('DERMTHD must be "parabolic", "best_fit" or "outside_pts"',  &
!           ' on line ',a,' of file ',a)
!           go to 9800
!        end if
!        go to 325

       enddo
!
! 480    continue
!
        if(f_numpargp > 1)then
          do i=1,f_numpargp-1
            do j=i+1,f_numpargp
              call Assert(.not. str_compare(f_pargpnme(i),f_pargpnme(j)), &
                "Two parameter groups have the same name in file "//quote(pPARAMETER_GROUP_FILE(1)), &
                trim(__FILE__), __LINE__)
            end do
          end do
        end if
!
!        call num2char(f_numpargp,aline)
        call echolog('     - data for '//trim(asChar(f_numpargp))//' parameter groups ' &
          //'read from file '//quote(pPARAMETER_GROUP_FILE(1)) )
        close(unit=iunit)

    endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -- If present, the parameter data file is read.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (.not. str_compare(pPARAMETER_DATA_FILE(1),"NA") ) then

!        call addquote(pardatfile,sString)
    call echolog("")
    call echolog('     Reading parameter data file '//quote(pPARAMETER_DATA_FILE(1)) )
!        iunit=nextunit()
!        open(unit=iunit,file=pardatfile,status='old',iostat=ierr)
!        if(ierr.ne.0)then
!          write(amessage,520) trim(sString)
! 520      format('cannot open parameter data file ',a)
!          go to 9800
!        end if

       open(newunit=iunit,file=trim(pPARAMETER_DATA_FILE(1)),status='old',iostat=ierr)
       call assert(ierr==0,"cannot open parameter data file "//quote(pPARAMETER_DATA_FILE(1)), &
         trim(__FILE__), __LINE__)
!
! ! -- The file is read a first time to obtain the number of parameters.
!
        jline=0
        f_numpar=0

        do
          jline=jline+1
          read(iunit,'(a)',iostat = ierr) sRecord
          if(ierr /= 0) exit
          if(sRecord == ' ') cycle
          if(sRecord(1:1) == '#') cycle
          f_numpar=f_numpar+1
        end do

!        do
!          jline=jline+1
!          read(iunit,'(a)',err=9400,end=550) cline
!          if(cline == ' ') cycle
!          if(cline(1:1) == '#') cycle
!          f_numpar=f_numpar+1
!        end do
! 550    continue

       call assert(f_numpar > 0, "File "//quote(pPARAMETER_DATA_FILE(1)) &
         //" appears to contain no data.", trim(__FILE__),__LINE__)

!        if(f_numpar == 0)then
!          write(amessage,322) trim(sString)
!          go to 9800
!        end if
        allocate(f_parnme(f_numpar),f_partrans(f_numpar),f_parchglim(f_numpar), &
                 f_parval1(f_numpar),f_parlbnd(f_numpar),f_parubnd(f_numpar), &
                 f_pargp(f_numpar),f_scale(f_numpar),f_offset(f_numpar),stat=ierr)
!        if(ierr.ne.0) go to 9200
        rewind(unit=iunit,iostat=ierr)
!        if(ierr.ne.0) go to 9350
!
! ! -- Now it is read a second time to obtain the data.
!
        jline=0
        ipar=0

        do
          jline=jline+1
          READ(iunit,'(A)',iostat = iStat) sRecord
          if(iStat /= 0) exit
          if(sRecord == ' ') cycle
          if(sRecord(1:1) == '#') cycle

! 560    jline=jline+1
!        call num2char(jline,aline)
!        read(iunit,'(A)',ERR=9400,END=620) cline
!        if(cline == ' ') go to 560
!        if(cline(1:1) == '#') go to 560
!        call casetrans(cline,'lo')
!        call linesplit(ierr,9)
!        if(ierr.ne.0) go to 9450
!        atemp=cline(left_word(1):right_word(1))
!        call remchar(atemp,'"')
!        IF(len_trim(atemp) > 12)THEN
!          write(amessage,565) trim(atemp),trim(aline),trim(sString)
! 565      format('parameter name "',a,'" greater than 12 characters in length ', &
!          'at line ',a,' of file ',a)
!          go to 9800
!        end if

          ipar=ipar+1

         ! read in parameter group name
         call Chomp(sRecord, sItem, sSet)
         f_parnme(ipar)=trim(sItem)


!        f_parnme(ipar)=atemp

         call Chomp(sRecord, sItem, sSet)
         f_partrans(ipar) = trim(sItem)
!        f_partrans(ipar)=cline(left_word(2):right_word(2))
!        call remchar(f_partrans(ipar),'"')
!        if((f_partrans(ipar).ne.'log').and.(f_partrans(ipar).ne.'none').and.  &
!           (f_partrans(ipar)(1:4).ne.'tied').and.(f_partrans(ipar).ne.'fixed'))then
!           write(amessage,570) trim(aline),trim(sString)
! 570       format('PARTRANS on line ',a,' of file ',a,' must be ',  &
!           '"none", "log", "fixed" or "tied_(parameter name)".')
!           go to 9800
!        end if
!        if((f_partrans(ipar) == 'tied').or.(f_partrans(ipar) == 'tied_'))then
!          write(amessage,572) trim(aline),trim(sString)
! 572      format('the parent parameter name must follow the "tied_" string at line ',a,  &
!          ' of file ',a)
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_parchglim(ipar) = trim(sItem)
!        f_parchglim(ipar)=cline(left_word(3):right_word(3))
!        call remchar(f_parchglim(ipar),'"')
!        if((f_parchglim(ipar).ne.'relative').and.(f_parchglim(ipar).ne.'factor'))then
!           write(amessage,580) trim(aline),trim(sString)
! 580       format('PARCHGLIM on line ',a,' of file ',a,' must be ',  &
!           '"relative" or "factor".')
!           go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_parval1(ipar) = asReal(sItem)
!        call char2num(ierr,cline(left_word(4):right_word(4)),f_parval1(ipar))
!        if(ierr.ne.0)then
!          write(amessage,590) 'PARVAL1',trim(aline),trim(sString)
! 590      format('cannot read value for ',a,' on line ',a,' of file ',a)
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_parlbnd(ipar) = asReal(sItem)
!        call char2num(ierr,cline(left_word(5):right_word(5)),f_parlbnd(ipar))
!        if(ierr.ne.0)then
!          write(amessage,590) 'PARLBND',trim(aline),trim(sString)
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_parubnd(ipar) = asReal(sItem)
!        call char2num(ierr,cline(left_word(6):right_word(6)),f_parubnd(ipar))
!        if(ierr.ne.0)then
!          write(amessage,590) 'PARUBND',trim(aline),trim(sString)
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_pargp(ipar) = trim(sItem)
!        atemp=cline(left_word(7):right_word(7))
!        call remchar(atemp,'"')
!        if(len_trim(atemp) > 12)then
!          write(amessage,330) trim(atemp),trim(aline),trim(sString)
!          go to 9800
!        end if
!        f_pargp(ipar)=atemp

         call Chomp(sRecord, sItem, sSet)
         f_scale(ipar) = asReal(sItem)
!        call char2num(ierr,cline(left_word(8):right_word(8)),f_scale(ipar))
!        if(ierr.ne.0)then
!          write(amessage,590) 'SCALE',trim(aline),trim(sString)
!          go to 9800
!        end if

         call Chomp(sRecord, sItem, sSet)
         f_offset(ipar) = asReal(sItem)
!        call char2num(ierr,cline(left_word(9):right_word(9)),f_offset(ipar))
!        if(ierr.ne.0)then
!          write(amessage,590) 'OFFSET',trim(aline),trim(sString)
!          go to 9800
!        end if
!        go to 560
!
       enddo
! 620    continue
!
! ! -- Some checks are made of the parameter data.
!
        if(f_numpar > 1)then
          do i=1,f_numpar-1
            do j=i+1,f_numpar
              call Assert(.not. str_compare(f_parnme(i),f_parnme(j)), &
                "Two parameters have the same name in file "//quote(pPARAMETER_DATA_FILE(1)), &
                trim(__FILE__), __LINE__)
            end do
          end do
        end if
!
! ! -- If any parameters are tied, parameter linkages are now read.
        do ipar=1,f_numpar
          if(f_partrans(ipar)(1:4) == 'tied')then

              call Assert(.not. str_compare(f_partrans(ipar)(6:),f_parnme(ipar)), &
                "Parameter"//trim(f_parnme(ipar))//" is tied to itself in file " &
                //quote(pPARAMETER_DATA_FILE(1)), &
                trim(__FILE__), __LINE__)

          end if
        end do
!
!        call num2char(f_numpar,aline)
!        write(*,640) trim(aline),trim(sString)
!        write(LU_REC,640) trim(aline),trim(sString)
! 640    format(t5,'- data for ',a,' parameters read from file ',a)
!        close(unit=iunit)

        call echolog('     - data for '//trim(asChar(f_numpar))//' parameters ' &
          //'read from file '//quote(pPARAMETER_DATA_FILE(1)))
        close(unit=iunit)

!700    continue
    endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -- Next the names of all parameters are ascertained by reading template files.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   if (.not. str_compare(pTEMPLATE_FILE(1),"NA") ) then

       numtempfile = size(pTEMPLATE_FILE)
       npar=0
       do itempfile=1,numtempfile
         nnpar=0
   !           tempunit=nextunit()
   !           call addquote(tempfile(itempfile),sString)
         call echolog("")
         call echolog("     Reading template file "//quote(pTEMPLATE_FILE(itempfile)) )
         open(newunit=tempunit,file=trim(pTEMPLATE_FILE(itempfile)),status='old',iostat=ierr)
         call assert(ierr==0,"cannot open template file "//quote(pTEMPLATE_FILE(itempfile)), &
                 trim(__FILE__), __LINE__)

   !           if(ierr.ne.0)then
   !             write(amessage,720) trim(sString)
   ! 720         format('cannot open template file ',a)
   !             go to 9800
   !           end if

             jline=1
             READ(tempunit,'(A)',iostat = iStat) sRecord

   !      read(tempunit,'(a)',err=9400,end=800) cline
   !           call casetrans(cline,'lo')
         call assert(str_compare(sRecord(1:3),"ptf"), '"ptf" header missing from first line ' &
           //'of file '//quote(pTEMPLATE_FILE(itempfile)), trim(__FILE__),__LINE__)

   !      if(sRecord(1:3).ne.'ptf')then
   !        write(amessage,730) trim(sString)
   ! 730       format('"ptf" header missing from first line of file ',a)
   !             go to 9800
   !      end if
         pardelim=sRecord(5:5)
         if((pardelim == ' ').or.   &
            (index('1234567890,;:',pardelim).ne.0).or.    &
            (index('abcdefghijklmnopqrstuvwxyz',pardelim).ne.0))then

           call Assert(lFALSE, "Invalid parameter delimiter on line 1 of file " &
             //quote(pTEMPLATE_FILE(itempfile)), trim(__FILE__),__LINE__)

         endif

   !         write(amessage,740) trim(sString)
   ! 740        format('invalid parameter delimeter on line 1 of file ',a)
   !              go to 9800
   !      end if
           npar = 0; nnpar = 0
           do
             jline=jline+1
             READ(tempunit,'(A)',iostat = iStat) sRecord
             if(iStat /= 0) exit
             if(sRecord == ' ') cycle
             if(sRecord(1:1) == '#') cycle

             pItems => pluck(sRecord, pardelim)

             if(str_compare(pItems(1), "NA") ) then
               deallocate(pItems)
               cycle
             endif

             do i=1,size(pItems)

               if( isElement(pItems(i),apar) ) cycle   ! if this parameter has already been
                                                    ! added to the list, ignore and move on
               npar=npar+1
               nnpar=nnpar+1
               apar(npar) = pItems(i)
             enddo

             deallocate(pItems)


   !      read_a_line: do
   !        ii1=1
   !        jline=jline+1
   !        read(tempunit,'(a)',err=9400,end=800) cline
   !        ll=len(cline)
   ! 745       j=index(cline(ii1:),pardelim)
   !        if(j == 0) cycle read_a_line
   !        if(j > ll) cycle read_a_line
   !        ii1=ii1+j-1
   !        j=0
   !        if(ii1.le.ll)j=index(cline(ii1+1:),pardelim)
   !        if(j == 0)then
   !          call num2char(jline,aline)
   !          write(amessage,750) trim(aline),trim(sString)
   ! 750         format('unbalanced parameter delimiters on line ',a,  &
   !          ' of template file ',a)
   !          go to 9800
   !        end if
   !        jj1=ii1+j
   !        ii1=ii1+1
   !        jj1=jj1-1
   !        if(jj1-ii1+1.le.0)then
   !          call num2char(jline,aline)
   !          write(amessage,760) trim(aline),trim(sString)
   ! 760         format('parameter space has zero width at line ',a,   &
   !          ' of template file ',a)
   !          go to 9800
   !        end if
   !        do jj=ii1,jj1
   !          if(cline(jj:jj).ne.' ') then
   !            do kk=jj,jj1
   !              if(cline(kk:kk) == ' ') go to 765
   !            end do
   !            kk=jj1+1
   ! 765           kk=kk-1
   !            go to 767
   !          end if
   !        end do
   !        call num2char(jline,aline)
   !        write(amessage,766) trim(aline), trim(sString)
   ! 766       format('blank parameter space at line ',a,' of template ', &
   !        'file ',a)
   !        go to 9800
   ! 767       continue
   !        if(kk-jj+1 > 12)then
   !          call num2char(jline,aline)
   !          write(amessage,768) trim(aline),trim(sString)
   ! 768         format('parameter name greater than 12 characters in ',  &
   !          'line ',a,' of template file ',a)
   !          go to 9800
   !        end if
   !             if(cline(kk+1:jj1).ne.' ')then
   !               call num2char(jline,aline)
   !               write(amessage,769) trim(aline),trim(sString)
   ! 769           format('parameter name includes a space character at line ',a,  &
   !               ' of file ',a)
   !               go to 9800
   !             end if
   !        aapar=cline(jj:kk)
   !        aapar=adjustl(aapar)
   !        call casetrans(aapar,'lo')
   !        if(npar.ne.0)then
   !          do ipar=1,npar
   !            if(aapar == apar(ipar)) go to 785
   !          end do

   !          npar=npar+1
   !          nnpar=nnpar+1
   !          if(npar > MAXPAR)then
   !                 call num2char(MAXPAR,aline)
   !            write(amessage,780) trim(aline)
   ! 780           format('number of parameters cited in template files is limited ', &
   !                 'to ',a,'. Increase MAXPAR and re-compile program.')
   !            go to 9800
   !          end if
   !          apar(npar)=aapar
   !        else
   !          npar=1
   !          nnpar=1
   !          apar(npar)=aapar
   !        end if
   ! 785       ii1=jj1+2
   !        go to 745
           enddo
   ! 800     continue

   !      call num2char(nnpar,aline)
   !           if(itempfile == 1)then
   !        write(*,795) trim(aline),trim(sString)
   !             write(LU_REC,795) trim(aline),trim(sString)
   ! 795       format(t5,'- ',a,' parameter names read from file ',a)
   !           else
   !        write(*,796) trim(aline),trim(sString)
   !             write(LU_REC,796) trim(aline),trim(sString)
   ! 796       format(t5,'- ',a,' more parameter names read from file ',a)
   !           end if

           call echolog('     - '//trim(asChar(nnpar))//' parameter ' &
           //'names read from file '//quote(pTEMPLATE_FILE(itempfile)) )

         close(unit=tempunit,err=9500)
   !    end do read_template_file

       enddo

   endif



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! -- Observations are named and the instruction file is now written.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        nobs=0
!        nobsgp=0
!
!        LU_INSTRUCT=nextunit()
!        call addquote(instructfile,sString)
!        write(*,810) trim(sString)
!        write(LU_REC,810) trim(sString)
! 810    format(t5,'Writing instruction file ',a,' ....')
!        inquire(file=instructfile,exist=lexist)
!        if(lexist)then
! 812      write(6,*)
! 813      write(*,815,advance='no') trim(sString)
! 815      format(' File ',a,' already exists. Overwrite it? [y/n]: ')
!          read(5,'(a)') aa
!          call casetrans(aa,'lo')
!          if((aa.ne.'y').and.(aa.ne.'n')) go to 813
!          if(aa == 'n')then
!            write(*,820)
!            write(LU_REC,820)
! 820        format(/,' Execution terminated so file not overwritten.')
!            ifail=1
!            return
!          end if
!        end if
!        open(unit=LU_INSTRUCT,file=instructfile,iostat=ierr)
!        if(ierr.ne.0)then
!          write(amessage,830) trim(sString)
! 830      format('cannot open file ',a,' for output.')
!          go to 9800
!        end if
!        write(LU_INSTRUCT,840)
! 840    format('pif $')
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! -- First the time series instructions are written.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!        iout=0
!        if(ioseries == 0) go to 1100
!        do i=1,ioseries
!          iout=iout+1
!          im=iOutseries_g(i)
!          do j=1,ioseries
!            if(im == modseries(j)) go to 860
!          end do
!          write(amessage,850) trim(series_g(im)%name)
! 850      format('time series "',a,'" cited in the LIST_OUTPUT block immediately ', &
!          'preceding the WRITE_PEST_FILES block is not cited as a ', &
!          'MODEL_SERIES_NAME in the latter block.')
!          go to 9800
! 860      io=obsseries(j)
!          nsterm=series_g(io)%nterm
!          aname=series_g(im)%name
!          nobsgp=nobsgp+1
!          obgnme(nobsgp)=aname
!          call make_basename(ierr,iout,nsterm,aname,basename)
!          if(ierr.ne.0) go to 9800
!          atemp=basename(iout)
!          do iterm=1,nsterm
!            call num2char(iterm,anum)
!            if(sSeriesFormat_g == 'long')then
!              aname='['//trim(atemp)//trim(anum)//']37:55'
!            else
!              aname='['//trim(atemp)//trim(anum)//']2:20'
!            end if
!            if(iterm == 1)then
!              write(LU_INSTRUCT,880) trim(aname)
! 880          format('l3',t6,a)
!            else
!              write(LU_INSTRUCT,890) trim(aname)
! 890          format('l1',t6,a)
!            end if
!            nobs=nobs+1
!          end do
!        end do
!
! ! -- Next the S_TABLE instructions are written.
!
! 1100   continue
!        if(iostable == 0) go to 1200
!        siout=0
!        do i=1,iostable
!          il=0
!          siout=siout+1
!          im=iOutStable_g(i)
!          do j=1,iostable
!            if(im == modstable(j)) go to 1120
!          end do
!          write(amessage,1110) 's',trim(stable_g(im)%name),'S'
! 1110     format(a,'_table "',a,'" cited in the LIST_OUTPUT block immediately ', &
!          'preceding the WRITE_PEST_FILES block is not cited as a ', &
!          'MODEL_',a,'_TABLE_NAME in the latter block.')
!          go to 9800
! 1120     io=obsstable(j)
!          aname=stable_g(im)%name
!          nobsgp=nobsgp+1
!          obgnme(nobsgp)=aname
!          sbasename(siout)=aname(1:12)
!          if(siout > 1)then
!            do j=1,siout-1
!              if(sbasename(j) == sbasename(siout))then
!                write(amessage,1130)
! 1130           format('TSPROC cannot generate unique observation names from the ',  &
!                'names of the MODEL_S_TABLES involved in the ', &
!                'calibration process. Alter the first twelve letters of at least one ', &
!                'of the model S_TABLE names.')
!                go to 9800
!              end if
!            end do
!          end if
!          if(stable_g(io)%maximum > -1.0e36)then
!            il=il+1
!            aname='['//trim(sbasename(siout))//OBSCHAR//'max]51:69'
!            if(il == 1)then
!              write(LU_INSTRUCT,1140) trim(aname)
! 1140         format('l11',t6,a)
!            else
!              write(LU_INSTRUCT,1150) trim(aname)
! 1150         format('l1',t6,a)
!            end if
!            nobs=nobs+1
!          end if
!          if(stable_g(io)%minimum > -1.0e36)then
!            il=il+1
!            aname='['//trim(sbasename(siout))//OBSCHAR//'min]51:69'
!            if(il == 1)then
!              write(LU_INSTRUCT,1140) trim(aname)
!            else
!              write(LU_INSTRUCT,1150) trim(aname)
!            end if
!            nobs=nobs+1
!          end if
!          if(stable_g(io)%range > -1.0e36)then
!            il=il+1
!            aname='['//trim(sbasename(siout))//OBSCHAR//'range]51:69'
!            if(il == 1)then
!              write(LU_INSTRUCT,1140) trim(aname)
!            else
!              write(LU_INSTRUCT,1150) trim(aname)
!            end if
!            nobs=nobs+1
!          end if
!
!          if(stable_g(io)%total > -1.0e36)then
!            il=il+1
!            aname='['//trim(sbasename(siout))//OBSCHAR//'sum]51:69'
!            if(il == 1)then
!              write(LU_INSTRUCT,1140) trim(aname)
!            else
!              write(LU_INSTRUCT,1150) trim(aname)
!            end if
!            nobs=nobs+1
!          end if
!          if(stable_g(io)%mean > -1.0e36)then
!            il=il+1
!            aname='['//trim(sbasename(siout))//OBSCHAR//'mean]51:69'
!            if(il == 1)then
!              write(LU_INSTRUCT,1140) trim(aname)
!            else
!              write(LU_INSTRUCT,1150) trim(aname)
!            end if
!            nobs=nobs+1
!          end if
!          if(stable_g(io)%stddev > -1.0e36)then
!            il=il+1
!            aname='['//trim(sbasename(siout))//OBSCHAR//'sd]51:69'
!            if(il == 1)then
!              write(LU_INSTRUCT,1140) trim(aname)
!            else
!              write(LU_INSTRUCT,1150) trim(aname)
!            end if
!            nobs=nobs+1
!          end if
!        end do
!
! ! -- Next the V_TABLE instructions are written.
!
! 1200   continue
!
!        if(iovtable == 0) go to 1300
!        do i=1,iovtable
!          iout=iout+1
!          im=iOutVtable_g(i)
!          do j=1,iovtable
!            if(im == modvtable(j)) go to 1220
!          end do
!          write(amessage,1110) 'v',trim(vtable_g(im)%name),'V'
!          go to 9800
! 1220     io=obsvtable(j)
!          nsterm=vtable_g(io)%nterm
!          aname=vtable_g(im)%name
!          nobsgp=nobsgp+1
!          obgnme(nobsgp)=aname
!          call make_basename(ierr,iout,nsterm,aname,basename)
!          if(ierr.ne.0) go to 9800
!          atemp=basename(iout)
!          do iterm=1,nsterm
!            call num2char(iterm,anum)
!            aname='['//trim(atemp)//trim(anum)//']62:81'
!            if(iterm == 1)then
!              write(LU_INSTRUCT,1230) trim(aname)
! 1230         format('l4',t6,a)
!            else
!              write(LU_INSTRUCT,1240) trim(aname)
! 1240         format('l1',t6,a)
!            end if
!            nobs=nobs+1
!          end do
!        end do
!
! ! -- Next the E_TABLE instructions are written.
!
! 1300   continue
!
!        if(iodtable == 0) go to 1400
!        do i=1,iodtable
!          iout=iout+1
!          im=iOutDtable_g(i)
!          do j=1,iodtable
!            if(im == moddtable(j)) go to 1320
!          end do
!          write(amessage,1110) 'e',trim(vtable_g(im)%name),'E'
!          go to 9800
! 1320     io=obsdtable(j)
!          nsterm=dtable_g(io)%nterm
!          aname=dtable_g(im)%name
!          nobsgp=nobsgp+1
!          obgnme(nobsgp)=aname
!          call make_basename(ierr,iout,nsterm,aname,basename)
!          if(ierr.ne.0) go to 9800
!          atemp=basename(iout)
!          do iterm=1,nsterm
!            call num2char(iterm,anum)
!            aname='['//trim(atemp)//trim(anum)//']59:78'
!            if(iterm == 1)then
!              write(LU_INSTRUCT,1230) trim(aname)
!            else
!              write(LU_INSTRUCT,1240) trim(aname)
!            end if
!            nobs=nobs+1
!          end do
!        end do
!
! 1400   continue
!        close(unit=LU_INSTRUCT)
!        write(*,1410) trim(sString)
!        write(LU_REC,1410) trim(sString)
! 1410   format(t5,'- file ',a,' written ok.')
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! -- Parameter and parameter group data are now assimilated on the basis of information
! !    read from the parameter data file, the parameter group file and the template files.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
        allocate(partrans(npar),parchglim(npar),parval1(npar),parlbnd(npar),  &
                 parubnd(npar),pargp(npar),scale(npar),offset(npar), stat=ierr)
!        if(ierr.ne.0) go to 9200
!
        allocate(pargpnme(npar),inctyp(npar),derinc(npar),derinclb(npar),forcen(npar), &
        derincmul(npar),dermthd(npar), stat=ierr)
!        if(ierr.ne.0) go to 9200
!
        do ipar=1,npar
          aapar=apar(ipar)
          if(f_numpar.ne.0)then
            do j=1,f_numpar
              if(aapar == f_parnme(j))then
                partrans(ipar)=f_partrans(j)
                parchglim(ipar)=f_parchglim(j)
                parval1(ipar)=f_parval1(j)
                parlbnd(ipar)=f_parlbnd(j)
                parubnd(ipar)=f_parubnd(j)
                pargp(ipar)=f_pargp(j)
                scale(ipar)=f_scale(j)
                offset(ipar)=f_offset(j)
                go to 1450
              end if
            end do
          end if
          partrans(ipar)='log'
          parchglim(ipar)='factor'
          parval1(ipar)=1.0
          parlbnd(ipar)=1.0e-10
          parubnd(ipar)=1e10
          pargp(ipar)=aapar
          scale(ipar)=1.0
          offset(ipar)=0.0
 1450     continue
        end do
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! -- If any parameters are tied to a parameter which does not exist, this is now
! !    rectified.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
        do ipar=1,npar
          if(partrans(ipar)(1:4) == 'tied') then
            aapar=partrans(ipar)(6:)
            do i=1,npar
              if(aapar == apar(i)) go to 1470
            end do
            partrans(ipar)='none'
 1470       continue
          end if
        end do
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! -- Parameter groups are now organised.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
        npargp=0
        do ipar=1,npar
          apargp=pargp(ipar)
          if(apargp == 'none') then
            if((partrans(ipar).ne.'tied').and.(partrans(ipar).ne.'fixed'))then

              call Assert(lFALSE,'parameter '//quote(apar(ipar))//' has been assigned to ' &
              //'parameter group "none" ' &
              //'in file '//quote(pPARAMETER_DATA_FILE(1))//' but is not tied or fixed.', &
              trim(__FILE__), __LINE__)

!              call addquote(pardatfile,sString)
!               write(amessage,1471)trim(apar(ipar)),trim(sString)
! 1471         format('parameter "',a,'" has been assigned to parameter group "none" ', &
!              'in file ',a,' but is not tied or fixed.')
!              go to 9800
            else
              go to 1500
            end if
          end if
          if(ipar.ne.1)then
            do i=1,ipar-1
              if(pargp(i) == apargp) go to 1500
            end do
          end if
          if(f_numpargp.ne.0)then
            do i=1,f_numpargp
              if(apargp == f_pargpnme(i))then
                npargp=npargp+1
                pargpnme(npargp)=f_pargpnme(i)
                inctyp(npargp)=f_inctyp(i)
                derinc(npargp)=f_derinc(i)
                derinclb(npargp)=f_derinclb(i)
                forcen(npargp)=f_forcen(i)
                derincmul(npargp)=f_derincmul(i)
                dermthd(npargp)=f_dermthd(i)
                go to 1500
              end if
            end do
          end if
          npargp=npargp+1
          pargpnme(npargp)=apargp
          inctyp(npargp)='relative'
          derinc(npargp)=0.01
          derinclb(npargp)=0.00
          forcen(npargp)='switch'
          derincmul(npargp)=2.0
          dermthd(npargp)='parabolic'
 1500     continue
        end do
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! -- The "* control data" section of the PEST control file is now written.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!        iunit=nextunit()
!        call addquote(pestctlfile,sString)
!        write(*,1510) trim(sString)
!        write(LU_REC,1510) trim(sString)
! 1510   format(t5,'Writing PEST control file ',a,' ....')
!        inquire(file=pestctlfile,exist=lexist)
!        if(lexist)then
!          write(6,*)
! 1520     write(*,815,advance='no') trim(sString)
!          read(5,'(a)') aa
!          call casetrans(aa,'lo')
!          if((aa.ne.'y').and.(aa.ne.'n')) go to 1520
!          if(aa == 'n')then
!            write(*,820)
!            write(LU_REC,820)
!            ifail=1
!            return
!          end if
!        end if

         open(newunit=LU_PEST_CONTROL_FILE,file=trim(pNEW_PEST_CONTROL_FILE(1)),status='replace',iostat=ierr)
         call assert(ierr==0,"cannot open PEST control file "//quote(pNEW_PEST_CONTROL_FILE(1)), &
                 trim(__FILE__), __LINE__)

!        open(unit=LU_PEST_CONTROL_FILE,file=pestctlfile,iostat=ierr)
!        if(ierr.ne.0)then
!          write(amessage,830) trim(sString)
!          go to 9800
!        end if
        write(LU_PEST_CONTROL_FILE,fmt="(a)") 'pcf'
        write(LU_PEST_CONTROL_FILE,fmt="(a)") '* control data'
        write(LU_PEST_CONTROL_FILE,fmt="(a)") 'restart estimation'
        write(LU_PEST_CONTROL_FILE,fmt="(5i7)") npar,nobs,npargp,0,nobsgp
        write(LU_PEST_CONTROL_FILE,"(2i6,'   single   point   1   0   0')") numtempfile,1

        if(isvd == 0) then
          write(LU_PEST_CONTROL_FILE,fmt="(a)") '5.0   2.0    0.3    0.03    10  999'
        else
          write(LU_PEST_CONTROL_FILE,fmt="(a)") ' 1e-1   -4.0   0.3  0.03    10       999'
        end if

        write(LU_PEST_CONTROL_FILE,fmt="(a)") '5.0   5.0   1.0e-3'

        if(auiyesno == 0)then
          write(LU_PEST_CONTROL_FILE,fmt="(a)") '0.1  noaui'
        else
          write(LU_PEST_CONTROL_FILE,fmt="(a)") '0.1   aui'
        end if
        write(LU_PEST_CONTROL_FILE,fmt="(a)") '30   .005  4   4  .005   4'
        write(LU_PEST_CONTROL_FILE,fmt="(a)") '1    1    1'

        if(isvd == 1)then
          write(LU_PEST_CONTROL_FILE,fmt="(a)") '* singular value decomposition'
          write(LU_PEST_CONTROL_FILE,fmt="(a)") '1'
          write(LU_PEST_CONTROL_FILE,fmt="(i6,2x,1pg13.7)") npar,eigthresh
          write(LU_PEST_CONTROL_FILE,fmt="(a)") '0'
        end if
!
! -- The "* parameter groups" section of the PEST control file is now written.
!
        write(LU_PEST_CONTROL_FILE,fmt="(a)") '* parameter groups'

        do igp=1,npargp
          write(LU_PEST_CONTROL_FILE,fmt="(a,t14,a,t27,1pg10.4,t39,1pg10.4,t51,a,t62,1pg10.4,2x,a)") &
            trim(pargpnme(igp)),trim(inctyp(igp)),derinc(igp), &
            derinclb(igp),trim(forcen(igp)),derincmul(igp), trim(dermthd(igp))
        end do
!
 ! -- The "* parameter data" section of the PEST control file is now written.
!
        write(LU_PEST_CONTROL_FILE,fmt="(a)") '* parameter data'

        ! first write out parameter data
        do ipar=1,npar

          if(partrans(ipar)(1:4) == 'tied')then
            atrans='tied'
          else
            atrans=partrans(ipar)
          end if

          write(LU_PEST_CONTROL_FILE,fmt="(a,t14,a,t21,a,t33,1pg12.5,t47,1pg12.5,t61,1pg12.5,t75, &
            a,t89,1pg10.4,t101,1pg10.4,t113,'  1')") &
              trim(apar(ipar)),trim(atrans), trim(parchglim(ipar)),parval1(ipar), &
              parlbnd(ipar),parubnd(ipar),trim(pargp(ipar)),scale(ipar),offset(ipar)

        end do

        ! next write out a list of the parameters that are tied to other parameters
        do ipar=1,npar
          if(partrans(ipar)(1:4) == 'tied')then
            write(LU_PEST_CONTROL_FILE,fmt="(a,t14,a)") trim(apar(ipar)),trim(partrans(ipar)(6:))
          end if
        end do
!







! -- The "* observation groups" section of the PEST control file is now written.
!
        write(LU_PEST_CONTROL_FILE,fmt="(a)") '* observation groups'

        do i=1,nobsgp
          write(LU_PEST_CONTROL_FILE,fmt="(a)") trim(obgnme(i))
        end do
!
! -- The "* observation data" section of the PEST control file is now written.
! -- First the time series observations are dealt with.
!
        write(LU_PEST_CONTROL_FILE,fmt="('* observation data')")

! 1705   format('* observation data')
!
!        iout=0
!        ieqnerr=0
!        if(ioseries == 0) go to 2100
!        do i=1,ioseries
!          iout=iout+1
!          im=iOutseries_g(i)
!          do j=1,ioseries
!            if(im == modseries(j)) go to 1860
!          end do
!          write(amessage,850)
!          go to 9800
! 1860     io=obsseries(j)
!          nsterm=series_g(io)%nterm
!          aname=series_g(im)%name
!          call make_basename(ierr,iout,nsterm,aname,basename)
!          atemp=basename(iout)
!          weightmin=max(sweightmin(j),0.0)
!          weightmax=min(sweightmax(j),1.0e36)
!
! ! -- The pertinent equation is parsed and prepared.
!
!          eqntext=sequation(j)
!          call prepare_eqn(ierr,nterm,sequation(j),io)
!          if(ierr.ne.0) then
!            ieqnerr=1
!            go to 9800
!          end if
!          nnterm=nterm
!          do iterm=1,nterm
!            cterm(iterm)=aterm(iterm)
!          end do
!          do iterm=1,nterm
!            qterm(iterm)=rterm(iterm)
!          end do
!          do j=1,nsterm
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            call num2char(j,anum)
!            aname=trim(atemp)//trim(anum)
!
! ! -- First the series numbers in the equation terms are replaced by series values.
!
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '$~$') then
!                call char2num(ierr,aterm(iterm)(4:),isnum)
!                rterm(iterm)=series_g(isnum)%val(j)
!                aterm(iterm)='~!~'
!              end if
!            end do
!
! ! -- The weights equation instinsic function evaluations is carried out if necessary.
!
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(series_g(io)%val(j))
!                aterm(iterm)='~!~'
!              else if(aterm(iterm)(1:3) == '@_1') then
!                call newdate(series_g(io)%days(j),1,1,1970,dd,mm,yy)
!                nn=numdays(1,1,yy,dd,mm,yy)
!                rtime=float(nn)+float(series_g(io)%secs(j))/86400.0
!                rterm(iterm)=rtime
!                aterm(iterm)='~!~'
!              else if(aterm(iterm)(1:3) == '@_3') then
!                call char2num(ierr,aterm(iterm)(5:),dtempx)
!                rterm(iterm)=dble(series_g(io)%days(j))+     &
!                             dble(series_g(io)%secs(j))/86400.0d0-dtempx
!                aterm(iterm)='~!~'
!              end if
!            end do
!
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(ierr.ne.0) go to 9800
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),series_g(io)%val(j),dval,trim(series_g(im)%name)
! 1900       format(a,t22,1pg14.7,t40,1pg12.6,2x,a)
!          end do
!        end do

    do i=1,iNumSeries

      pTS_Observed => TS%getTS(TS%tTSComparison(i)%sObservedSeries)
      pTS_Modeled => TS%getTS(TS%tTSComparison(i)%sModeledSeries)

      call Assert(pTS_Modeled%iListOutputPosition > 0, &
        "The modeled time series "//quote(TS%tTSComparison(i)%sModeledSeries) &
        //" does not appear in the most recent list output block", &
        trim(__FILE__),__LINE__)

      do j=1,size(pTS_Observed%tData)

        write(LU_PEST_CONTROL_FILE,fmt="(a,t22,g14.7,t40,g12.6,2x,a)") &
          trim(pTS_Observed%sSeriesName)//"_"//trim(asChar(j)),pTS_Observed%tData(j)%rValue, &
          TS%tTSComparison(i)%rWeightValue(j),trim(pTS_Observed%sSeriesName)

      enddo

!              trim(aname),series_g(io)%val(j),dval,trim(series_g(im)%name)
! 1900       format(a,t22,1pg14.7,t40,1pg12.6,2x,a)

    enddo

    do i=1,iNumTables

      pTable => TS%getTable(TS%tTableComparison(i)%sObservedTable)

      if( pTable%iTableType == iSTABLE ) then
        do j=9,size(pTable%tTableData)
        write(LU_PEST_CONTROL_FILE,fmt="(a,t22,a,t40,g12.6,2x,a)") &
          trim(pTable%sSeriesName)//"_"//trim(asChar(j)),trim(pTable%tTableData(j)%sValue(1)), &
          TS%tTableComparison(i)%rWeightValue(j),trim(pTable%sSeriesName)
        enddo

      elseif( pTable%iTableType == iETABLE ) then
        do j=1,size(pTable%tTableData)
        write(LU_PEST_CONTROL_FILE,fmt="(a,t22,a,t40,g12.6,2x,a)") &
          trim(pTable%sSeriesName)//"_"//trim(asChar(j)), &
          trim(pTable%tTableData(j)%sValue(3)), &
          TS%tTableComparison(i)%rWeightValue(j),trim(pTable%sSeriesName)
        enddo

      elseif( pTable%iTableType == iVTABLE ) then
        do j=1,size(pTable%tTableData)
        write(LU_PEST_CONTROL_FILE,fmt="(a,t22,a,t40,g12.6,2x,a)") &
          trim(pTable%sSeriesName)//"_"//trim(asChar(j)),trim(pTable%tTableData(j)%sValue(1)), &
          TS%tTableComparison(i)%rWeightValue(j),trim(pTable%sSeriesName)
        enddo

      elseif( pTable%iTableType == iITABLE ) then
        do j=1,size(pTable%tTableData)
        write(LU_PEST_CONTROL_FILE,fmt="(a,t22,a,t40,g12.6,2x,a)") &
          trim(pTable%sSeriesName)//"_"//trim(asChar(j)),trim(pTable%tTableData(j)%sValue(1)), &
          TS%tTableComparison(i)%rWeightValue(j),trim(pTable%sSeriesName)
        enddo

      endif
!              trim(aname),series_g(io)%val(j),dval,trim(series_g(im)%name)
! 1900       format(a,t22,1pg14.7,t40,1pg12.6,2x,a)

    enddo


! ! -- Now we handle S_TABLE observations.
!
! 2100   continue
!        if(iostable == 0) go to 2200
!        siout=0
!        do i=1,iostable
!          siout=siout+1
!          im=iOutStable_g(i)
!          do j=1,iostable
!            if(im == modstable(j)) go to 2120
!          end do
!          write(amessage,1110) 's',trim(stable_g(im)%name),'S'
!          go to 9800
! 2120     io=obsstable(j)
!          aname=stable_g(im)%name
!          sbasename(siout)=aname(1:12)
!          weightmin=max(stweightmin(j),0.0)
!          weightmax=min(stweightmax(j),1.0e36)
!          eqntext=stequation(j)
!          call prepare_eqn(ierr,nterm,stequation(j),0)
!          if(ierr.ne.0) then
!            ieqnerr=1
!            go to 9800
!          end if
!          nnterm=nterm
!          do iterm=1,nterm
!            cterm(iterm)=aterm(iterm)
!          end do
!          do iterm=1,nterm
!            qterm(iterm)=rterm(iterm)
!          end do
!
!          if(stable_g(io)%maximum > -1.0e36)then
!            aname=trim(sbasename(siout))//OBSCHAR//'max'
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(stable_g(io)%maximum)
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),stable_g(io)%maximum,dval,trim(stable_g(im)%name)
!          end if
!
!          if(stable_g(io)%minimum > -1.0e36)then
!            aname=trim(sbasename(siout))//OBSCHAR//'min'
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(stable_g(io)%minimum)
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),stable_g(io)%minimum,dval,trim(stable_g(im)%name)
!          end if
!
!          if(stable_g(io)%range > -1.0e36)then
!            aname=trim(sbasename(siout))//OBSCHAR//'range'
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(stable_g(io)%range)
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),stable_g(io)%range,dval,trim(stable_g(im)%name)
!          end if
!
!          if(stable_g(io)%total > -1.0e36)then
!            aname=trim(sbasename(siout))//OBSCHAR//'sum'
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(stable_g(io)%total)
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),stable_g(io)%total,dval,trim(stable_g(im)%name)
!          end if
!
!          if(stable_g(io)%mean > -1.0e36)then
!            aname=trim(sbasename(siout))//OBSCHAR//'mean'
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(stable_g(io)%mean)
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),stable_g(io)%mean,dval,trim(stable_g(im)%name)
!          end if
!
!          if(stable_g(io)%stddev > -1.0e36)then
!            aname=trim(sbasename(siout))//OBSCHAR//'sd'
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(stable_g(io)%stddev)
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),stable_g(io)%stddev,dval,trim(stable_g(im)%name)
!          end if
!
!        end do
!
! ! -- Next the V_TABLE observations are handled.
!
! 2200   continue
!        if(iovtable == 0) go to 2300
!        do i=1,iovtable
!          iout=iout+1
!          im=iOutVtable_g(i)
!          do j=1,iovtable
!            if(im == modvtable(j)) go to 2220
!          end do
!          write(amessage,1110) 'v',trim(vtable_g(im)%name),'V'
!          go to 9800
! 2220     io=obsvtable(j)
!          nsterm=vtable_g(io)%nterm
!          aname=vtable_g(im)%name
!          call make_basename(ierr,iout,nsterm,aname,basename)
!          if(ierr.ne.0) go to 9800
!          atemp=basename(iout)
!          weightmin=max(vtweightmin(j),0.0)
!          weightmax=min(vtweightmax(j),1.0e36)
!          eqntext=vtequation(j)
!          call prepare_eqn(ierr,nterm,vtequation(j),0)
!          if(ierr.ne.0) then
!            ieqnerr=1
!            go to 9800
!          end if
!          nnterm=nterm
!          do iterm=1,nterm
!            cterm(iterm)=aterm(iterm)
!          end do
!          do iterm=1,nterm
!            qterm(iterm)=rterm(iterm)
!          end do
!          do j=1,nsterm
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            call num2char(j,anum)
!            aname=trim(atemp)//trim(anum)
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(vtable_g(io)%vol(j))
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),vtable_g(io)%vol(j),dval,trim(vtable_g(im)%name)
!          end do
!        end do
!
! ! -- Next the E_TABLE observations are handled.
!
! 2300   continue
!        if(iodtable == 0) go to 2400
!        do i=1,iodtable
!          iout=iout+1
!          im=iOutDtable_g(i)
!          do j=1,iodtable
!            if(im == moddtable(j)) go to 2320
!          end do
!          write(amessage,1110) 'e',trim(vtable_g(im)%name),'E'
!          go to 9800
! 2320     io=obsdtable(j)
!          totim=dtable_g(io)%total_time
!          nsterm=dtable_g(io)%nterm
!          aname=dtable_g(im)%name
!          call make_basename(ierr,iout,nsterm,aname,basename)
!          if(ierr.ne.0) go to 9800
!          atemp=basename(iout)
!          weightmin=max(dtweightmin(j),0.0)        !chek
!          weightmax=min(dtweightmax(j),1.0e36)
!          eqntext=dtequation(j)
!          call prepare_eqn(ierr,nterm,dtequation(j),0)
!          if(ierr.ne.0) then
!            ieqnerr=1
!            go to 9800
!          end if
!          nnterm=nterm
!          do iterm=1,nterm
!            cterm(iterm)=aterm(iterm)
!          end do
!          do iterm=1,nterm
!            qterm(iterm)=rterm(iterm)
!          end do
!          do j=1,nsterm
!            nterm=nnterm
!            do iterm=1,nterm
!              aterm(iterm)=cterm(iterm)
!            end do
!            do iterm=1,nterm
!              rterm(iterm)=qterm(iterm)
!            end do
!            call num2char(j,anum)
!            aname=trim(atemp)//trim(anum)
!            do iterm =1,nterm
!              if(aterm(iterm)(1:3) == '@_2') then
!                rterm(iterm)=abs(dtable_g(io)%time(j)/totim)
!                aterm(iterm)='~!~'
!              end if
!            end do
!            call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
!            OPERAT,FUNCT,IORDER,DVAL,rterm)
!            if(dval < weightmin)dval=weightmin
!            if(dval > weightmax)dval=weightmax
!            write(LU_PEST_CONTROL_FILE,1900) trim(aname),dtable_g(io)%time(j)/totim,dval,trim(dtable_g(im)%name)
!          end do
!        end do
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ! -- The "* model command line" section of the PEST control file is written.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! 2400   continue
!
        write(LU_PEST_CONTROL_FILE,fmt="('* model command line')")
! 2410   format('* model command line')
        if(modcomline == ' ')modcomline='model'
! !       call addquote(modcomline,bstring)
       write(LU_PEST_CONTROL_FILE,fmt="(a)") quote(modcomline)
! 2420   format(a)
!
! ! -- The "* model input/output" section of the PEST control file is written.
!
        write(LU_PEST_CONTROL_FILE,fmt="('* model input/output')")
! 2430   format('* model input/output')
        do i=1,numtempfile
          if(modfile(i) == ' ')then
!            call num2char(i,anum)
            modfile(i)='model'//trim(asChar(i))//'.in'
          end if
!          call addquote(tempfile(i),bstring)
!          call addquote(modfile(i),cstring)
          write(LU_PEST_CONTROL_FILE,fmt="(a,3x,a)") quote(tempfile(i)),quote(modfile(i))
! 2440     format(a,3x,a)
        end do
!        call addquote(instructfile,bstring)
!        call addquote(sListOutputFile_g,cstring)
        write(LU_PEST_CONTROL_FILE,fmt="(a,3x,a)") quote(sMostRecentInstructionFile), &
            quote(sMostRecentListOutputFile)
        close(unit=LU_PEST_CONTROL_FILE)
!
!        write(*,2460) trim(sString)
!        write(LU_REC,2460) trim(sString)
! 2460   format(t5,'- file ',a,' written ok.')
!
! ! -- If a MICA control file was requested, it is now written.
!
!        if(micactlfile.ne.' ')then
!          call addquote(micactlfile,sString)
!          write(*,2470) trim(sString)
!          write(LU_REC,2470) trim(sString)
! 2470     format(t5,'Writing MICA control file ',a,' ....')
!          inquire(file=micactlfile,exist=lexist)
!          if(lexist)then
! 2471       write(6,*)
! 2472       write(*,815,advance='no') trim(sString)
!            read(5,'(a)') aa
!            call casetrans(aa,'lo')
!            if((aa.ne.'y').and.(aa.ne.'n')) go to 2471
!            if(aa == 'n') go to 2495
!          end if
!          itempunit=nextunit()
!          open(unit=itempunit,file=micactlfile,status='old',iostat=ierr)
!          if(ierr == 0)then
!            close(unit=itempunit,status='delete')
!          end if
!          itempunit=nextunit()
!          open(unit=itempunit,file='t###.###')
!          call addquote(pestctlfile,cstring)
!          write(itempunit,'(a)') trim(cstring)
!          write(itempunit,'(a)') '1'
!          write(itempunit,'(a)') trim(sString)
!          close(unit=itempunit)
!          ierr = system(trim(pest2micacom)//' < t###.### > nul')
!          inquire(file=micactlfile,exist=lexist)
!          if(.not.lexist)then
!            write(amessage,2480)
! 2480       format('could not write MICA control file - check PEST2MICA command.')
!            go to 9800
!          else
!            write(*,2460) trim(sString)
!            write(LU_REC,2460) trim(sString)
!          end if
!        end if
! 2495   continue
!
!        go to 9900
!
! 9000   call num2char(ILine_g,aline)
!        call addquote(sInfile,sString)
!        write(amessage,9010) trim(aline), trim(sString)
! 9010   format('cannot read line ',a,' of TSPROC input file ',a)
!        go to 9800
! 9100   continue
!        call addquote(sInfile,sString)
!        write(amessage,9110) trim(sString),trim(CurrentBlock_g)
! 9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
!        ' reading ',a,' block.')
!        go to 9800
! 9200   write(amessage,9210)
! 9210   format('cannot allocate sufficient memory to continue execution.')
!        go to 9800
! 9300   call num2char(ILine_g,aline)
!        call addquote(sInfile,sString)
!        write(amessage,9310) trim(aoption),trim(aline),trim(sString),trim(correct_keyword)
! 9310   format(a,' keyword at line ',a,' of TSPROC input file ',a,' should immediately ', &
!        'follow ',a,' keyword.')
!        go to 9800
! 9350   write(amessage,9360) trim(sString)
! 9360   format('cannot rewind file ',a)
!        go to 9800
! 9400   call num2char(jline,aline)
!        write(amessage,9410) trim(aline),trim(sString)
! 9410   format('cannot read line ',a,' of file ',a)
!        go to 9800
! 9450   call num2char(jline,aline)
!        write(amessage,9460) trim(aline),trim(sString)
! 9460   format('insufficient entries on line ',a,' of file ',a)
!        go to 9800
 9500   write(amessage,9510) trim(sString)
 9510   format('cannot close file ',a)
!        go to 9800
! 9600   write(amessage,9610) trim(aoname),trim(amname),trim(avariable)
! 9610   format('OBSERVATION_S_TABLE "',a,'"  has been matched to ', &
!        'MODEL_S_TABLE "',a,'". However the ',a,' has been computed ', &
!        'for one and not for the other.')
!        go to 9800
!
!
! 9800   call write_message(leadspace='yes',error='yes')
!        call write_message(iunit=LU_REC,leadspace='yes')
!        if(ieqnerr.ne.0)then
!          write(amessage,9810)
! 9810     format(' Offending equation follows:-')
!          call write_message()
!          call write_message(iunit=LU_REC)
!          do i=1,len_trim(eqntext)
!            if(eqntext(i:i) == char(196)) eqntext(i:i)='/'
!          end do
!          write(*,9820) trim(eqntext)
!          write(LU_REC,9820) trim(eqntext)
! 9820     format(' "',a,'"')
!        end if
!        ifail=1
       if(iunit.ne.0)close(unit=iunit,iostat=ierr)

9900   deallocate(f_pargpnme,f_inctyp,f_derinc,f_derinclb,f_forcen,f_derincmul, &
                  f_dermthd,stat=ierr)
       deallocate(f_parnme,f_partrans,f_parchglim,f_parval1,f_parlbnd,f_parubnd, &
                  f_pargp,f_scale,f_offset,stat=ierr)
       deallocate(partrans,parchglim,parval1,parlbnd,parubnd,pargp,scale,offset, stat=ierr)
       deallocate(pargpnme,inctyp,derinc,derinclb,forcen,derincmul,dermthd,stat=ierr)


       return

end subroutine pest_files

SUBROUTINE FIXED(NDAYS,DIS,INTRVL,LSTFLG,START,GDIS)
!
!     + + + PURPOSE + + +
!     Hydrograph separation by the fixed interval method
!     adapted from Pettyjohn and Henning (1979)
!     by R. A. Sloto
!     04/01/88   3.2
!
!     Code modified by AML for cases of missing data at beginning of
!     record and transition between years.  Missing data values are
!     defined by negative values.  9/16/91
!
!     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDAYS, INTRVL, START, LSTFLG
      REAL      DIS(*), GDIS(*)
!
!     + + + ARGUMENT DEFINITION + + +
!     NDAYS  - number of days for processing
!     DIS    - daily streamflow
!     INTRVL - time period in days for finding minimum
!     LSTFLG - flag set equal to 1 if last year being processed
!     START  - position (day) in DIS after which initial interval should begin
!              (index of last processed value/last bad value)
!     GDIS   - computed daily baseflow
!
!     + + + LOCAL VARIABLES + + +
      INTEGER   I, J, K, L1, L2, LD, LA, M, GOODV, FXDAYS
      REAL      PMIN
!
!     + + + END SPECIFICATIONS + + +
!
!     initialize variables and find start of good data--
!     initialize GDIS to 0.0 for days of good data and set equal
!     to DIS for days of negative data
      GOODV = 0
      DO 10 I = 1,NDAYS
        IF (DIS(I) .GE. 0.0) THEN
          GDIS(I) = 0.0
          GOODV = 1
        ELSE
          GDIS(I) = DIS(I)
!         reset START only if a good value hasn't been found after initial START
          IF (GOODV.EQ.0 .AND. I.GT.START)  START = I
        END IF
 10   CONTINUE
!
      FXDAYS = NDAYS
      IF (LSTFLG .EQ. 1) THEN
!       last year to process; reduce total days for processing by the
!       number of missing values within extra 11 days at end
        DO 15 I = NDAYS, NDAYS-10, -1
          IF (DIS(I) .LT. 0) THEN
            FXDAYS = FXDAYS - 1
          END IF
 15     CONTINUE
      END IF
!
      K = (FXDAYS - START)/INTRVL
!     for beginning of period
      IF (K .GE. 1) THEN
        DO 40 I = 1,K
          PMIN = 100000.
          L1 = ((I-1)*INTRVL) + START + 1
          L2 = I*INTRVL + START
          DO 20 J = L1,L2
            IF(DIS(J) .LT. PMIN)  PMIN = DIS(J)
 20       CONTINUE
          DO 30 J = L1,L2
            GDIS(J) = PMIN
 30       CONTINUE
 40     CONTINUE
      END IF
!
      IF (LSTFLG .NE. 1) THEN
!       set START for next year so that first few days of next year
!       use some of last few days of this year to determine base flow
!
!       find last day processed
        LD = START + K*INTRVL
!       find last day of year in array
        LA = NDAYS - 11
!       move START back in INTRVL increments
        M = LD
 70     CONTINUE
          M = M - INTRVL
        IF (M .GT. LA) GOTO 70
        START = 11 - (LA - M)
      ELSE
!       last year to process
        IF (L2 .LT. FXDAYS) THEN
!         extra days left over after processing K intervals;
!         do base-flow separation on those days by themselves
          PMIN = 100000.
          DO 80 J = L2+1,FXDAYS
            IF(DIS(J) .LT. PMIN)  PMIN = DIS(J)
 80       CONTINUE
          DO 90 J = L2+1,FXDAYS
            GDIS(J) = PMIN
 90       CONTINUE
        END IF
      END IF
!
      RETURN
END SUBROUTINE FIXED

SUBROUTINE SLIDE(NDAYS,DIS,INTRVL,GDIS)
!
!     + + + PURPOSE + + +
!     Hydrograph separation by the sliding interval method
!     adapted from Pettyjohn and Henning (1979)
!     by R. A. Sloto
!     04/05/88   3.1
!
!     Code modified by AML for cases of missing data.  Missing data values
!     are identified by negative values.  Code also converted to
!     IF-THEN-ELSE structure.  9/16/91
!
!     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDAYS, INTRVL
      REAL      DIS(*), GDIS(*)
!
!     + + + ARGUMENT DEFINITION + + +
!     NDAYS  - number of days for processing
!     DIS    - daily streamflow
!     GDIS   - computed daily baseflow
!     INTRVL - time period in days for finding minimum
!
!
!     + + + LOCAL VARIABLES + + +
      INTEGER   I, J, INT, K1, K2, GOODV, START, S1, DAY
      REAL      PMIN
!
!     + + + END SPECIFICATIONS + + +
!
!     initialize variables and find start of good data--
!     initialize GDIS to 0.0 for days of good data and set equal
!     to DIS for days of negative data
      START = 0
      GOODV = 0
      DO 10 I = 1,NDAYS
        IF (DIS(I) .GE. 0.0) THEN
          GDIS(I) = 0.0
          GOODV = 1
        ELSE
          GDIS(I) = DIS(I)
!         reset START only if a good value hasn't been found yet
          IF (GOODV .EQ. 0) START = I
        END IF
 10   CONTINUE
!
      INT=(INTRVL-1)/2
      S1 = START + 1
      DO 50 I = S1, NDAYS
        IF(DIS(I).GE.0.0) THEN
!         set DAY equal to index of current day in year
          DAY = I - START
          IF (DAY .LE. INT) THEN
!           when day near beginning
            PMIN = 100000.
            K2 = I+INT
            DO 20 J = S1,K2
              IF(DIS(J).LT.PMIN) PMIN = DIS(J)
 20         CONTINUE
            GDIS(I) = PMIN
          ELSE IF (NDAYS-I .LE. INT) THEN
!           when day near end
            PMIN=100000.
            K1=I-INT
            DO 30 J=K1,NDAYS
              IF(DIS(J).LT.PMIN) PMIN=DIS(J)
 30         CONTINUE
            GDIS(I)=PMIN
          ELSE
!           when day not near beginning or end
            PMIN=100000.
            K1=I-INT
            K2=I+INT
            DO 40 J=K1,K2
              IF(DIS(J).LT.PMIN) PMIN=DIS(J)
 40         CONTINUE
            GDIS(I)=PMIN
          END IF
        END IF
 50   CONTINUE
!
      RETURN
END SUBROUTINE SLIDE

SUBROUTINE LOCMIN(NDAYS,DIS,INTRVL,GDIS)
!
!     + + + PURPOSE + + +
!     Hydrograph separation by the local minimum method
!     adapted from Pettyjohn and Henning (1979)
!     by R. A. Sloto
!     04/05/88   2.1
!
!     Code modified by AML for cases of missing data.  Missing data values
!     are identified by negative values.  Code also converted to
!     IF-THEN-ELSE structure.  9/16/91
!
!     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDAYS, INTRVL
      REAL, dimension(:) ::  DIS, GDIS
!
!     + + + ARGUMENT DEFINITION + + +
!     NDAYS  - Number of days in the year
!     DIS    - streamflow
!     GDIS   - baseflow
!     INTRVL - interval
!
!     + + + LOCAL VARIABLES + + +

      integer, allocatable, dimension(:) :: IPOINT
      INTEGER   I, J, K, NUMPT, L, ID, PFLAG, END, &
                IJ, IP1, IP2, ISTART, IEND, IJK, S, GOODV, START
      REAL      X, Y
!
!     + + + INTRINSCIS + + +
      INTRINSIC   ALOG10
!
!     + + + END SPECIFICATIONS + + +
!
      allocate(IPOINT(size(DIS)))

      ID = 0
 10   CONTINUE
!       loop for periods of good data
        NUMPT = 0
        GOODV = 0
        PFLAG = 0
 20     CONTINUE
!         find start and end of good values
          ID = ID + 1
          IF (DIS(ID) .GE. 0.0) THEN
!           good value
            GDIS(ID) = 0.0
            IF (GOODV .EQ. 0) START = ID
            GOODV = GOODV + 1
            END = ID
          ELSE
!           bad value
            IF (GOODV .EQ. 0) THEN
!             no good values yet
              GDIS(ID) = DIS(ID)
            ELSE IF (GOODV .LT. INTRVL) THEN
!             not enough good values to process
              DO 25 J = 1, GOODV
                K = ID - J
                GDIS(K) = -999.0
 25           CONTINUE
              GOODV = 0
            ELSE
!             found good period to process
              PFLAG = 1
              GDIS(ID) = -999.0
            END IF
          END IF
        IF (ID.LT.NDAYS .AND. PFLAG.EQ.0) GO TO 20
!
        IF (GOODV .GE. INTRVL) THEN
!         have good period to process
          IF(INTRVL.EQ.3) THEN
            L=END-1
            S = START + 1
            DO 30 I = S, L
              IF(DIS(I).LE.DIS(I+1).AND.DIS(I).LE.DIS(I-1)) THEN
                NUMPT=NUMPT+1
                IPOINT(NUMPT)=I
              END IF
 30         CONTINUE
          ELSE IF(INTRVL.EQ.5) THEN
            L=END-2
            S = START + 2
            DO 35 I = S, L
              IF(DIS(I).LE.DIS(I+1).AND.DIS(I).LE.DIS(I-1).AND. &
                 DIS(I).LE.DIS(I+2).AND.DIS(I).LE.DIS(I-2)) THEN
                NUMPT=NUMPT+1
                IPOINT(NUMPT)=I
              END IF
 35         CONTINUE
          ELSE IF(INTRVL.EQ.7) THEN
            L=END-3
            S = START + 3
            DO 40 I = S, L
              IF(DIS(I).LE.DIS(I+1).AND.DIS(I).LE.DIS(I+2).AND. &
                 DIS(I).LE.DIS(I+3).AND.DIS(I).LE.DIS(I-1).AND. &
                 DIS(I).LE.DIS(I-2).AND.DIS(I).LE.DIS(I-3)) THEN
                NUMPT=NUMPT+1
                IPOINT(NUMPT)=I
              END IF
 40         CONTINUE
          ELSE IF(INTRVL.EQ.9) THEN
            L=END-4
            S = START + 4
            DO 45 I = S, L
              IF(DIS(I).LE.DIS(I+1).AND.DIS(I).LE.DIS(I+2).AND. &
                 DIS(I).LE.DIS(I+3).AND.DIS(I).LE.DIS(I-1).AND. &
                 DIS(I).LE.DIS(I-2).AND.DIS(I).LE.DIS(I-3).AND. &
                 DIS(I).LE.DIS(I+4).AND.DIS(I).LE.DIS(I-4)) THEN
                NUMPT=NUMPT+1
                IPOINT(NUMPT)=I
              END IF
 45         CONTINUE
          ELSE IF(INTRVL.GE.11) THEN
            L=END-5
            S = START + 5
            DO 50 I = S, L
              IF(DIS(I).LE.DIS(I+1).AND.DIS(I).LE.DIS(I+2).AND. &
                 DIS(I).LE.DIS(I+3).AND.DIS(I).LE.DIS(I-1).AND. &
                 DIS(I).LE.DIS(I-2).AND.DIS(I).LE.DIS(I-3).AND. &
                 DIS(I).LE.DIS(I+4).AND.DIS(I).LE.DIS(I-4).AND. &
                 DIS(I).LE.DIS(I+5).AND.DIS(I).LE.DIS(I-5)) THEN
                NUMPT=NUMPT+1
                IPOINT(NUMPT)=I
              END IF
 50         CONTINUE
          END IF
!
          IF (NUMPT .GT. 0) THEN
!           at least one local minimum found in good period being analyzed
            K=NUMPT-1
            J=IPOINT(1)
            L=IPOINT(NUMPT)
!           set beginning values to first local minimum
            DO 60 IJ=START,J
              GDIS(IJ)=DIS(J)
 60         CONTINUE
!           set ending values to last local minimum
            DO 65 IJ=L,END
              GDIS(IJ)=DIS(L)
 65         CONTINUE
!           set all the values in the middle
            DO 75 I=1,K
              IP1=IPOINT(I)
              IP2=IPOINT(I+1)
              GDIS(IP1)=DIS(IP1)
              GDIS(IP2)=DIS(IP2)
              ISTART=IP1
              IEND=IP2
              DO 70 J=ISTART,IEND
                X=J-IP1
                Y=IP2-IP1
                IF(GDIS(IP1).LE.0.0) GDIS(IP1)=0.01
                IF(GDIS(IP2).LE.0.0) GDIS(IP2)=0.01
                GDIS(J)=10.**((X/Y)*(ALOG10(GDIS(IP2))- &
                        ALOG10(GDIS(IP1)))+ALOG10(GDIS(IP1)))
 70           CONTINUE
 75         CONTINUE
          ELSE
!           no local minimum found in period analyzed
            DO 80 I = START, END
              GDIS(I) = -999.0
 80         CONTINUE
          END IF
        END IF
      IF (END.LT.NDAYS .AND. ID.LT.NDAYS) GO TO 10
!
      DO 90 IJK=1,NDAYS
        IF(GDIS(IJK).GT.DIS(IJK)) GDIS(IJK)=DIS(IJK)
 90   CONTINUE
!
      RETURN
END SUBROUTINE LOCMIN


end module tsp_legacy_code
