program test_hydrologic_indices

  use tsp_data_structures
  use tsp_utilities
  use tsp_command_processors
  use tsp_hydrologic_indices
  use wsc_additions
  implicit none

  ! [ LOCALS ]
  type(T_USGS_NWIS_GAGE),dimension(:),pointer :: pNWIS_Gage
  type(T_USGS_NWIS_DAILY),dimension(:),pointer :: pNWIS_Data
  type(T_USGS_NWIS_DAILY),dimension(:),pointer :: pNWIS_Data_Subset

  real(kind=T_SGL), dimension(:), allocatable :: rData
  integer(kind=T_INT), dimension(:), allocatable :: iJD
  integer(kind=T_INT) :: i, j, k

  type(T_HI), dimension(:), pointer :: MA
  type (T_STATS_COLLECTION), pointer :: pStats

  character (len=256) :: sBuf

!  pNWIS_Gage => read_USGS_NWIS("nwis_tab-delim_BEC.txt")
  pNWIS_Gage => read_USGS_NWIS("nwis_tab-delim_BEC_multiple_gages.txt")

  do k=1,size(pNWIS_Gage)

    write(*,fmt="('now calculating stats for ',a,/,a,2x,a)") &
       trim(pNWIS_Gage(k)%sDescription), trim(pNWIS_Gage(k)%sAgencyCode), &
       trim(pNWIS_Gage(k)%sSiteNumber)

    pNWIS_Data => pNWIS_Gage(k)%pGageData

    pStats => create_stats_object(pNWIS_Data%rMeanDischarge, pNWIS_Data%iMonth, &
                pNWIS_Data%iWaterYear, pNWIS_Data%iJulianDay)

    do i=MINVAL(pNWIS_Data%iWaterYear), MAXVAL(pNWIS_Data%iWaterYear)

      write(sBuf,"(a,i4,' - 'a)") 'Statistics for water year ',i, &
        trim(pNWIS_Gage(k)%sDescription)
      call write_base_stats(pStats%pByYear(i), TRIM(sBuf))

    end do


    MA => compute_hyd_indices_MA(pStats)

    do i=1,size(MA%rValue)
      write(*,fmt="('MA('i3,')',t8,f12.3,3x,a)") i, MA(i)%rValue, TRIM(MA(i)%sHydrologicIndex)
    end do

    deallocate(MA)
    deallocate(pStats)

  end do


end program test_hydrologic_indices
