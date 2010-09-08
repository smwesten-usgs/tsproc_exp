module tsp_statistics

   use tsp_data_structures
   use tsp_utilities
   use tsp_datetime_class
   implicit none

   ! Define enumerated items for PERCENTILE indices
   enum, bind(c)
     enumerator :: P01 = 1
     enumerator P05, P10, P15, P20, P25, P30, P35, P40, P45, P50, P55, P60, P65, &
                P70, P75, P80, P85, P90, P95, P99
   end enum

   ! Enumerated constants for DAY AVERAGING indices
   enum, bind(c)
     enumerator :: D1 = 1
     enumerator D3, D7, D30, D90
   end enum

   ! Enumerated constants for MONTH indices
   enum, bind(c)
     enumerator :: JAN = 1
     enumerator FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
   end enum

   type T_AVERAGING_PERIOD
     integer (kind=T_INT) :: iDaysInPeriod
     character (len=256) :: sDescription
   end type T_AVERAGING_PERIOD

   type T_HI
     integer (kind=T_INT) :: iUnits
     character (len=80)   :: sHydrologicIndex
     real (kind=T_SGL)    :: rValue
   end type T_HI

   integer (kind=T_INT), parameter :: iNUM_QUANTILES = 21
   integer (kind=T_INT), parameter :: iNUM_PERIODS = 5

   type T_HI_STATS
      integer (kind=T_INT) :: iCount = 0
      real (kind=T_SGL) :: rMean = rZERO
      real (kind=T_SGL) :: rMedian = rZERO
      real (kind=T_SGL) :: rVariance = rZERO
      real (kind=T_SGL) :: rStddev = rZERO
      real (kind=T_SGL) :: rCV = rZERO
      real (kind=T_SGL), dimension(iNUM_QUANTILES) :: rQuantile = rZERO
      real (kind=T_SGL), dimension(iNUM_QUANTILES) :: rExceedance = rZERO
      real (kind=T_SGL) :: rMin = rZERO
      real (kind=T_SGL) :: rMax = rZERO
      integer (kind=T_INT) :: iDayOfYearMin
      integer (kind=T_INT) :: iDayOfYearMax
      real (kind=T_SGL), dimension(iNUM_PERIODS) :: rPeriodMin = rZERO
      real (kind=T_SGL), dimension(iNUM_PERIODS) :: rPeriodMax = rZERO
      logical (kind=T_LOGICAL) :: lValid = lFALSE
   end type T_HI_STATS

   type T_STATS_COLLECTION
     type (T_HI_STATS), dimension(:), pointer :: pByYear
     type (T_HI_STATS), dimension(:), pointer :: pByMonth
     type (T_HI_STATS), dimension(:,:), pointer :: pByYearAndMonth
     type (T_HI_STATS), pointer :: pAllRecords
   end type T_STATS_COLLECTION

   real(kind=T_SGL), dimension(iNUM_QUANTILES), parameter :: rSTD_PROBABILITIES = (/ &
     0.01_T_SGL, 0.05_T_SGL, 0.1_T_SGL, 0.15_T_SGL, 0.2_T_SGL, 0.25_T_SGL, &
     0.3_T_SGL, 0.35_T_SGL, 0.4_T_SGL, 0.45_T_SGL, 0.5_T_SGL, 0.55_T_SGL, &
     0.6_T_SGL, 0.65_T_SGL, 0.7_T_SGL, 0.75_T_SGL, 0.8_T_SGL, 0.85_T_SGL, &
     0.9_T_SGL, 0.95_T_SGL, 0.99_T_SGL /)

   type (T_AVERAGING_PERIOD), dimension(iNUM_PERIODS) :: AVERAGING_PERIOD = (/ &
     T_AVERAGING_PERIOD(1, '1-day'), &
     T_AVERAGING_PERIOD(3, '3-day'), &
     T_AVERAGING_PERIOD(7, '7-day'), &
     T_AVERAGING_PERIOD(30, '30-day'), &
     T_AVERAGING_PERIOD(90, '90-day') &
     /)

   contains

function create_stats_object(rData, iMonth, iYear, iJulianDay)  result(pStats)

   real (kind=T_SGL), dimension(:) :: rData
   integer (kind=T_INT), dimension(:) :: iMonth
   integer (kind=T_INT), dimension(:) :: iYear
   integer (kind=T_INT), dimension(:) :: iJulianDay
   type (T_STATS_COLLECTION), pointer :: pStats

   ! [ LOCALS ]
   integer (kind=T_INT) :: iNumRecs
   integer (kind=T_INT) :: iStat
   integer (kind=T_INT) :: iFirstYear, iLastYear
   integer (kind=T_INT) :: i, j
   real (kind=T_SGL), dimension(:), allocatable :: rSubset
   integer (kind=T_INT), dimension(:), allocatable :: iJD

   iNumRecs = size(rData)
   iFirstYear = MINVAL(iYear)
   iLastYear = MAXVAL(iYear)

   allocate(pStats, stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for statistics collections data object")

   allocate(pStats%pByYear(iFirstYear:iLastYear), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'ByYear' statistics data object")

   allocate(pStats%pByMonth(12), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'ByMonth' statistics data object")

   do i=1,12
     allocate(rSubset(count(iMonth==i)))
     allocate(iJD(count(iMonth==i)))

     ! create a subset of the data for a given month
     rSubset = PACK(rData,iMonth==i)
     iJD = PACK(iJulianDay,iMonth==i)

     if(size(rSubset) < 30) then
       pStats%pByMonth(i)%lValid = lFALSE
     else
       pStats%pByMonth(i) = calc_base_stats(rSubset, iJD)
     endif

     deallocate(rSubset)
     deallocate(iJD)
   enddo

   allocate(pStats%pByYearAndMonth(iFirstYear:iLastYear,12), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'ByYearAndMonth' statistics data object")

   do i=iFirstYear,iLastYear

     allocate(rSubset(count( iYear==i )))
     allocate(iJD(count( iYear==i )))

     ! create a subset of the data for a given month/year combination
     rSubset = PACK(rData, iYear==i)
     iJD = PACK(iJulianDay, iYear==i)

     if(size(rSubset) < 350) then
         pStats%pByYear(i)%lValid = lFALSE
       else
         pStats%pByYear(i) = calc_base_stats(rSubset, iJD)
     endif

     deallocate(rSubset)
     deallocate(iJD)

     do j=1,12

       allocate(rSubset(count(iMonth==j .and. iYear==i )))
       allocate(iJD(count(iMonth==j .and. iYear==i )))

       ! create a subset of the data for a given month/year combination
       rSubset = PACK(rData,iMonth==j .and. iYear==i)
       iJD = PACK(iJulianDay,iMonth==j .and. iYear==i)

       if(size(rSubset) < 30) then
         pStats%pByYearAndMonth(i,j)%lValid = lFALSE
       else
         pStats%pByYearAndMonth(i,j) = calc_base_stats(rSubset, iJD)
       endif

       deallocate(rSubset)
       deallocate(iJD)
     enddo
   enddo

   allocate(pStats%pAllRecords, stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'AllRecords' statistics data object")

   pStats%pAllRecords = calc_base_stats(rData, iJulianDay)

   return

end function create_stats_object

!------------------------------------------------------------------------------

function calc_base_stats(rData, iJulianDay)   result(pBaseStats)

   real (kind=T_SGL), dimension(:) :: rData
   integer (kind=T_INT), dimension(:) :: iJulianDay
   type (T_HI_STATS) :: pBaseStats

   ! [ LOCALS ]
   integer (kind=T_INT) :: i,j,k
   integer (kind=T_INT), dimension(1) :: iLocMin, iLocMax
   real (kind=T_SGL) :: rMean
   integer (kind=T_INT) :: iMaxPeriodIndex
   integer (kind=T_INT) ,dimension(size(rData)):: iOriginalOrder
   real (kind=T_SGL), dimension(size(rData)) :: rSortedData

   rSortedData = rData

   call quick_sort(rSortedData, iOriginalOrder)

   pBaseStats%iCount = size(rData)

   pBaseStats%rMedian = median(rData)

   pBaseStats%rMean = mean(rData)
   pBaseStats%rVariance = variance(rData)

   pBaseStats%rStddev = sqrt(pBaseStats%rVariance)
   if(pBaseStats%rMean /= 0.) pBaseStats%rCV = pBaseStats%rStddev / pBaseStats%rMean

   do i=1,iNUM_QUANTILES
     pBaseStats%rQuantile(i) = quantile(rSTD_PROBABILITIES(i), rSortedData)
     pBaseStats%rExceedance(iNUM_QUANTILES - i + 1) = pBaseStats%rQuantile(i)
   end do

   pBaseStats%rMin = MINVAL(rData)
   pBaseStats%rMax = MAXVAL(rData)
   iLocMin = MINLOC(rData)
   iLocMax = MAXLOC(rData)

   pBaseStats%iDayOfYearMin = day_of_year(iJulianDay(iLocMin(1)))
   pBaseStats%iDayOfYearMax = day_of_year(iJulianDay(iLocMax(1)))

   pBaseStats%rPeriodMax = 1.e-20
   pBaseStats%rPeriodMin = 1.e+20

   if(size(rData) >= 365) then
     iMaxPeriodIndex = D90
   else if(size(rData) >= 30) then
     iMaxPeriodIndex = D30
   else
     iMaxPeriodIndex = D1
   endif

   do i=1,iMaxPeriodIndex
     do j=1,size(rData) - AVERAGING_PERIOD(i)%iDaysInPeriod + 1
       k = j + AVERAGING_PERIOD(i)%iDaysInPeriod - 1
       rMean = SUM(rData(j:k)) / AVERAGING_PERIOD(i)%iDaysInPeriod
       if (rMean > pBaseStats%rPeriodMax(i)) pBaseStats%rPeriodMax(i) = rMean
       if (rMean < pBaseStats%rPeriodMin(i)) pBaseStats%rPeriodMin(i) = rMean
     end do
   end do

   pBaseStats%lValid = lTRUE

   return

end function calc_base_stats

!------------------------------------------------------------------------------

function compute_hyd_indices_MA(pStats)  result(MA)

  type (T_STATS_COLLECTION), pointer :: pStats
  type(T_HI), dimension(:), pointer :: MA

   ! [ LOCALS ]
   integer(kind=T_INT) :: i
   real (kind=T_SGL) :: rTempVal
   real (kind=T_SGL), dimension(iNUM_QUANTILES) :: &
          rQuantilesOfMonthlyMeanFlow, rQuantilesOfAnnualMeanFlow
   real (kind=T_SGL), dimension(:), allocatable :: rMeanMonthlyFlowTS
   real (kind=T_SGL), dimension(:), allocatable :: rMeanAnnualFlowTS

   allocate(MA(45) )

   MA = (/ &
     T_HI( 1,'Mean, all daily flows',rZERO), &
     T_HI( 1,'Median, all daily flows',rZERO), &
     T_HI( 6,'CV, all daily flows',rZERO), &
     T_HI( 6,'CV, log of all daily flows',rZERO), &
     T_HI( 6,'Mean daily flow / median daily flow',rZERO), &
     T_HI( 6,'Ratio, Q10 / Q90 for all daily flows',rZERO), &
     T_HI( 6,'Ratio, Q20 / Q80 for all daily flows',rZERO), &
     T_HI( 6,'Ratio, Q25 / Q75 for all daily flows',rZERO), &
     T_HI( 2,'(Q10 - Q90) / median daily flow',rZERO), &
     T_HI( 2,'(Q20 - Q80) / median daily flow',rZERO), &
     T_HI( 2,'(Q25 - Q75) / median daily flow',rZERO), &
     T_HI( 2,'Mean monthly flow, January',rZERO), &
     T_HI( 2,'Mean monthly flow, February',rZERO), &
     T_HI( 2,'Mean monthly flow, March',rZERO), &
     T_HI( 2,'Mean monthly flow, April',rZERO), &
     T_HI( 2,'Mean monthly flow, May',rZERO), &
     T_HI( 2,'Mean monthly flow, June',rZERO), &
     T_HI( 2,'Mean monthly flow, July',rZERO), &
     T_HI( 2,'Mean monthly flow, August',rZERO), &
     T_HI( 2,'Mean monthly flow, September',rZERO), &
     T_HI( 2,'Mean monthly flow, October',rZERO), &
     T_HI( 2,'Mean monthly flow, November',rZERO), &
     T_HI( 2,'Mean monthly flow, December',rZERO), &
     T_HI( 2,'CV of monthly flow, January',rZERO), &
     T_HI( 2,'CV of monthly flow, February',rZERO), &
     T_HI( 2,'CV of monthly flow, March',rZERO), &
     T_HI( 2,'CV of monthly flow, April',rZERO), &
     T_HI( 2,'CV of monthly flow, May',rZERO), &
     T_HI( 2,'CV of monthly flow, June',rZERO), &
     T_HI( 2,'CV of monthly flow, July',rZERO), &
     T_HI( 2,'CV of monthly flow, August',rZERO), &
     T_HI( 2,'CV of monthly flow, September',rZERO), &
     T_HI( 2,'CV of monthly flow, October',rZERO), &
     T_HI( 2,'CV of monthly flow, November',rZERO), &
     T_HI( 2,'CV of monthly flow, December',rZERO), &
     T_HI( 2,'Range mean monthly / median monthly flow',rZERO), &
     T_HI( 2,'IQR mean monthly / median monthly flow',rZERO), &
     T_HI( 2,'(Q10 - Q90)[monthly] / median monthly flow',rZERO), &
     T_HI( 2,'CV, monthly mean flows',rZERO), &
     T_HI( 2,'Skewness in monthly flows',rZERO), &
     T_HI( 2,'Mean annual runoff',rZERO), &
     T_HI( 2,'Range mean annual / median annual flow',rZERO), &
     T_HI( 2,'IQR mean annual / median annual flow',rZERO), &
     T_HI( 2,'(Q10 - Q90)[annual] / median annual flow',rZERO), &
     T_HI( 2,'Skewness in annual flows',rZERO) &
     /)

   MA(1)%rValue = pStats%pAllRecords%rMean
   MA(2)%rValue = pStats%pAllRecords%rMedian

   if(pStats%pAllRecords%rMean /= 0.) &
        MA(3)%rValue = mean(pStats%pByYear%rStdDev) / pStats%pAllRecords%rMean * 100.


   if(MA(2)%rValue /=0. ) &
        MA(5)%rValue = pStats%pAllRecords%rMean / pStats%pAllRecords%rMedian

   rTempVal = mean(pStats%pAllRecords%rQuantile(P05:P95))
   if(rTempVal /= 0.) &
        MA(4)%rValue = stddev(pStats%pAllRecords%rQuantile(P05:P95)) / rTempVal * 100.

   MA(6)%rValue = pStats%pAllRecords%rExceedance(P10) / pStats%pAllRecords%rExceedance(P90)
   MA(7)%rValue = pStats%pAllRecords%rExceedance(p20) / pStats%pAllRecords%rExceedance(P80)
   MA(8)%rValue = pStats%pAllRecords%rExceedance(p25) / pStats%pAllRecords%rExceedance(p75)

   if(MA(2)%rValue > 0.) then
     MA(9)%rValue  = (pStats%pAllRecords%rExceedance(P10) - pStats%pAllRecords%rExceedance(P90)) / MA(2)%rValue
     MA(10)%rValue = (pStats%pAllRecords%rExceedance(P20) - pStats%pAllRecords%rExceedance(P80)) / MA(2)%rValue
     MA(11)%rValue = (pStats%pAllRecords%rExceedance(P25) - pStats%pAllRecords%rExceedance(P75)) / MA(2)%rValue
   end if

   do i=1,12
     MA(11+i)%rValue = mean(PACK(pStats%pByYearAndMonth(:,i)%rMean, &
                              pStats%pByYearAndMonth(:,i)%lValid))

     MA(23+i)%rValue = mean(PACK(pStats%pByYearAndMonth(:,i)%rCV * 100., &
                              pStats%pByYearAndMonth(:,i)%lValid))
   end do

   allocate(rMeanMonthlyFlowTS(COUNT(pStats%pByYearAndMonth(:,:)%lValid)))

   rMeanMonthlyFlowTS = PACK(pStats%pByYearAndMonth(:,:)%rMean, &
                               pStats%pByYearAndMonth(:,:)%lValid)

   allocate(rMeanAnnualFlowTS(COUNT(pStats%pByYear%lValid)))

   rMeanAnnualFlowTS = PACK(pStats%pByYear(:)%rMean, &
                               pStats%pByYear(:)%lValid)


   write(*,*) "      PROB  Q(mon mean TS) Q(ann mean TS)"
   write (*,*)"---------------------------------------------"

   do i=1,iNUM_QUANTILES
     rQuantilesOfMonthlyMeanFlow(i) = quantile(rSTD_PROBABILITIES(i), rMeanMonthlyFlowTS)
     rQuantilesOfAnnualMeanFlow(i) = quantile(rSTD_PROBABILITIES(i), rMeanAnnualFlowTS)
     write(*,fmt="(i3,') ',f6.2,2f12.3)")  i,rSTD_PROBABILITIES(i), &
         rQuantilesOfMonthlyMeanFlow(i),rQuantilesOfAnnualMeanFlow(i)
   end do


   MA(36)%rValue = (MAXVAL(pStats%pByYearAndMonth(:,:)%rMean) &
                     - MINVAL(pStats%pByYearAndMonth(:,:)%rMean )) &
                     / rQuantilesOfMonthlyMeanFlow(P50)

   MA(37)%rValue = (rQuantilesOfMonthlyMeanFlow(P75) &
                     - rQuantilesOfMonthlyMeanFlow(P25)) &
                     / rQuantilesOfMonthlyMeanFlow(P50)

   MA(38)%rValue = (rQuantilesOfMonthlyMeanFlow(P90) &
                     - rQuantilesOfMonthlyMeanFlow(P10)) &
                     / rQuantilesOfMonthlyMeanFlow(P50)

   MA(39)%rValue = stddev(rMeanMonthlyFlowTS) / mean(rMeanMonthlyFlowTS) * 100.

   MA(40)%rValue =  (mean(rMeanMonthlyFlowTS) -  median(rMeanMonthlyFlowTS)) &
                      / median(rMeanMonthlyFlowTS)

   MA(42)%rValue = (MAXVAL(pStats%pByYear%rMean) &
                     - MINVAL(pStats%pByYear%rMean )) &
                     / rQuantilesOfAnnualMeanFlow(P50)

   MA(43)%rValue = ( rQuantilesOfAnnualMeanFlow(P75) &
                     - rQuantilesOfAnnualMeanFlow(P25)) &
                     / rQuantilesOfAnnualMeanFlow(P50)

   MA(44)%rValue = ( rQuantilesOfAnnualMeanFlow(P90) &
                     - rQuantilesOfAnnualMeanFlow(P10)) &
                     / rQuantilesOfAnnualMeanFlow(P50)

   MA(45)%rValue =  (mean(rMeanAnnualFlowTS) -  median(rMeanAnnualFlowTS)) &
                      / median(rMeanAnnualFlowTS)

   deallocate(rMeanMonthlyFlowTS)
   deallocate(rMeanAnnualFlowTS)

   return

end function compute_hyd_indices_MA

!------------------------------------------------------------------------------

subroutine write_base_stats(pBaseStats, sDescription)

  type (T_HI_STATS) :: pBaseStats
  character (len=*), optional :: sDescription

  if(present(sDescription)) then
    write(*,fmt="(/,a,/,a,/,a,/)") repeat("-",80),TRIM(sDescription),repeat("-",80)
  end if

  if(pBaseStats%lValid) then

    write(*,fmt="('  count:',t15,i8)") pBaseStats%iCount
    write(*,fmt="('  mean:',t15,f12.3)") pBaseStats%rMean
    write(*,fmt="('  median:',t15,f12.3)") pBaseStats%rMedian
    write(*,fmt="('  variance:',t15,f12.3)") pBaseStats%rVariance
    write(*,fmt="('  min:',t15,f12.3)") pBaseStats%rMin
    write(*,fmt="('  max:',t15,f12.3)") pBaseStats%rMax
    write(*,fmt="('  DOY, minimum:',t15,i8)") pBaseStats%iDayOfYearMin
    write(*,fmt="('  DOY, maximum:',t15,i8)") pBaseStats%iDayOfYearMax
    write(*,fmt="(/)")

  else

      write(*,fmt="('  ===> NOT ENOUGH VALID DATA FOR THIS TIME SUBSET... NO STATS CALCULATED <===',/)")

  endif

end subroutine write_base_stats

!-------------------------------------------------------------------------------------------

  function mean(rData)   result(rMean)

    real (kind=T_SGL), dimension(:), intent(in) :: rData
    real (kind=T_SGL) :: rMean

    rMean = SUM(rData) / real(size(rData),kind=T_SGL)

  end function mean

!------------------------------------------------------------------------------

  function variance(rData)   result(rVariance)

    real (kind=T_SGL), dimension(:), intent(in) :: rData
    real (kind=T_SGL) :: rVariance

    ! [ LOCALS ]
    real (kind=T_SGL) :: rSum
    real (kind=T_SGL) :: rMean

    rMean = SUM(rData) / real(size(rData),kind=T_SGL)

    rSum = SUM((rData - rMean)**2)

    rVariance = rSum / real(size(rData)-1,kind=T_SGL)

  end function variance

!------------------------------------------------------------------------------

  function stddev(rData)   result(rStdDev)

    real (kind=T_SGL), dimension(:), intent(in) :: rData
    real (kind=T_SGL) :: rStdDev
    real ( kind=T_SGL) :: rVariance

    ! [ LOCALS ]

    rVariance = variance(rData)
    rStdDev = sqrt(rVariance)

  end function stddev

!-------------------------------------------------------------------------------------------

  function quantile( rQuantile, rData)  result(rValue)

    real (kind=T_SGL), intent(in) :: rQuantile
    real (kind=T_SGL), dimension(:), intent(in) :: rData
    real (kind=T_SGL) :: rValue

    ! [ LOCALS ]
    integer (kind=T_INT) :: iNumRecords
    integer (kind=T_INT) :: iInitialIndex
    real (kind=T_SGL) :: rRealIndex
    real (kind=T_SGL) :: rFractionalIndex
    real (kind=T_SGL), dimension(size(rData)) :: rDatacp
    integer (kind=T_INT), dimension(size(rData)) :: iOriginalOrder

    rDatacp = rData

    CALL quick_sort(rDatacp, iOriginalOrder)

    rValue = rZERO

    iNumRecords = size(rDatacp)

    if(iNumRecords > 0) then
      rRealIndex = rQuantile * REAL(iNumRecords,kind=T_SGL)
      iInitialIndex = max(INT(rRealIndex,kind=T_INT),1)
      rFractionalIndex = rRealIndex - REAL(iInitialIndex,kind=T_SGL)
      if(iInitialIndex < iNumRecords) then
        rValue = rDatacp(iInitialIndex) &
          + (rDatacp(iInitialIndex+1) - rDatacp(iInitialIndex)) &
          * rFractionalIndex
      else
        rValue = rDatacp(iNumRecords)
      endif

    endif

  end function quantile

!------------------------------------------------------------------------------

  function quantiles( rQuantiles, rData)  result(rValues)

    real (kind=T_SGL), intent(in), dimension(:) :: rQuantiles
    real (kind=T_SGL), dimension(:), intent(in) :: rData
    real (kind=T_SGL), dimension(size(rQuantiles)) :: rValues

    ! [ LOCALS ]
    integer (kind=T_INT) :: iNumRecords
    integer (kind=T_INT) :: iInitialIndex
    real (kind=T_SGL) :: rRealIndex
    real (kind=T_SGL) :: rFractionalIndex
    real (kind=T_SGL), dimension(size(rData)) :: rDatacp
    integer (kind=T_INT), dimension(size(rData)) :: iOriginalOrder
    integer (kind=T_INT) :: i
    integer (kind=T_INT) :: iStat
    integer (kind=T_INT) :: iNumQuantiles

    rDatacp = rData
    iNumQuantiles = size(rQuantiles)
    iNumRecords = size(rData)

    ! for this algorithm to work, data mush be sorted in ascending order
    CALL quick_sort(rDatacp, iOriginalOrder)

    rValues = rZERO

!    allocate(rValues(iNumQuantiles), stat=iStat)
!    call Assert(iStat == 0, "Problem allocating memory for quantiles", &
!        trim(__FILE__), __LINE__)

    if(iNumRecords > 0) then

      do i=1,iNumQuantiles

        ! find approximate index value for given quantile
        ! i.e. if quantile is 0.25 and n=100, index is approx 25
        rRealIndex = rQuantiles(i) * REAL(iNumRecords,kind=T_SGL)
        iInitialIndex = max(INT(rRealIndex,kind=T_INT),1)
        rFractionalIndex = rRealIndex - REAL(iInitialIndex,kind=T_SGL)
        ! perform linear interpolation to find the approximate value
        ! associated with this quantile
        if(iInitialIndex < iNumRecords) then
          rValues(i) = rDatacp(iInitialIndex) &
            + (rDatacp(iInitialIndex+1) - rDatacp(iInitialIndex)) &
            * rFractionalIndex
        else
          rValues(i) = rDatacp(iNumRecords)
        endif

      enddo

    else

      rValues = -99999.

    endif

  end function quantiles

!-------------------------------------------------------------------------------------------

  recursive subroutine quick_sort(list, order)

  ! Quick sort routine from:
  ! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
  ! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
  ! Modified by Alan Miller to include an associated integer array which gives
  ! the positions of the elements in the original order.

    implicit none
    real, dimension (:), intent(in out)  :: list
    integer, dimension (:), intent(out)  :: order

    ! LOCAL VARIABLE
    integer :: i

    do i = 1, size(list)
      order(i) = i
    end do

    call quick_sort_1(1, size(list))

  contains

    recursive subroutine quick_sort_1(left_end, right_end)

      integer, intent(in) :: left_end, right_end

      ! LOCAL VARIABLES
      integer             :: i, j, itemp
      real                :: reference, temp
      integer, parameter  :: max_simple_sort_size = 6

      if (right_end < left_end + max_simple_sort_size) then
        ! use interchange sort for small lists
        call interchange_sort(left_end, right_end)

      else ! use partition ("quick") sort

        reference = list((left_end + right_end)/2)
        i = left_end - 1; j = right_end + 1

        do ! scan list from left end until element >= reference is found
          do
            i = i + 1
            if (list(i) >= reference) exit
          end do
          do ! scan list from right end until element <= reference is found
            j = j - 1
            if (list(j) <= reference) exit
          end do

          if (i < j) then
            ! swap two out-of-order elements
            temp = list(i); list(i) = list(j); list(j) = temp
            itemp = order(i); order(i) = order(j); order(j) = itemp
          else if (i == j) then
            i = i + 1
            exit
          else
            exit
          end if
        end do

        if (left_end < j) call quick_sort_1(left_end, j)
        if (i < right_end) call quick_sort_1(i, right_end)
      end if

  end subroutine quick_sort_1

!-------------------------------------------------------------------------------------------

  subroutine interchange_sort(left_end, right_end)

    integer, intent(in) :: left_end, right_end

    ! LOCAL VARIABLES
    integer             :: i, j, itemp
    real                :: temp

    do i = left_end, right_end - 1
      do j = i+1, right_end
        if (list(i) > list(j)) then
          temp = list(i); list(i) = list(j); list(j) = temp
          itemp = order(i); order(i) = order(j); order(j) = itemp
        end if
      end do
    end do

  end subroutine interchange_sort

end subroutine quick_sort

!-------------------------------------------------------------------------------------------

function median(rData)   result(rMedian)
!  *                                           *
!  *********************************************
!  * Returns the median value of the elements in
!  * the vector rData(:)
!  *********************************************

    Real (kind=T_SGL), Intent (in) :: rData(:)
    real (kind=T_SGL) :: rMedian


    ! [ LOCALS ]
    Real (kind=T_SGL) :: rDatacp(Size(rData))
    Integer :: Ns, Nsd2
    integer (kind=T_INT),dimension(size(rData)) :: iOriginalOrder

    ! make a copy of the incoming data
    rDatacp = rData
    Ns = Size(rData)
    Nsd2 = Int(Ns/2)

    if(Ns > 1) then

      call quick_sort(rDatacp, iOriginalOrder)
      if (mod(Ns,2) == 1) Then
         rMedian = rDatacp(Nsd2+1)
      else
         rMedian = (rDatacp(Nsd2) + rDatacp(Nsd2+1))/2.0_T_SGL
      endif

    else

      rMedian = rDatacp(1)

    endif

    Return

end function median

!------------------------------------------------------------------------------

function calc_period_min_mean(rData, iPeriod)                 result(rPeriodMinMean)

   real (kind=T_SGL), dimension(:), intent(in) :: rData
   integer (kind=T_INT), intent(in) :: iPeriod
   real (kind=T_SGL) :: rPeriodMinMean

   ! [ LOCALS ]
   integer (kind=T_INT) :: i, j
   real (kind=T_SGL) :: rMean

   rPeriodMinMean = huge(rPeriodMinMean)

  ! iterate over all data; assumption is that this is daily data, continuous
   do i=1,size(rData) - iPeriod + 1
     j = i + iPeriod - 1
     rMean = SUM(rData(i:j)) / iPeriod
     if (rMean < rPeriodMinMean) rPeriodMinMean = rMean
   end do

end function calc_period_min_mean

!------------------------------------------------------------------------------

function calc_period_max_mean(rData, iPeriod)               result(rPeriodMaxMean)

   real (kind=T_SGL), dimension(:), intent(in) :: rData
   integer (kind=T_INT), intent(in) :: iPeriod
   real (kind=T_SGL) :: rPeriodMaxMean

   ! [ LOCALS ]
   integer (kind=T_INT) :: i, j
   real (kind=T_SGL) :: rMean

   rPeriodMaxMean = -huge(rPeriodMaxMean)

  ! iterate over all data; assumption is that this is daily data, continuous
   do i=1,size(rData) - iPeriod + 1
     j = i + iPeriod - 1
     rMean = SUM(rData(i:j)) / iPeriod
     if (rMean > rPeriodMaxMean) rPeriodMaxMean = rMean
   end do

end function calc_period_max_mean

!------------------------------------------------------------------------------

!> @ brief The code for performing a 1-D Akima interpolation is taken from a package
!> called SOSIE: http://sosie.sourceforge.net/
!>
!> From the SOSIE documentation:
!> The main interpolation procedure of SOSIE is based on the method of Akima (1970):
!>  "A New Method of Interpolation and Smooth Surface Fitting Based On Local Procedures,
!>  J.of Applied Comput. Math., 17, 589-602."
!>
!> Original code developed by Laurent Brodeau, brodeau@gmail.com
!> and by Jean-Marc Molines, jean-marc.molines@hmq.inpq.fr  (amail adresses valid as of 3/10/2010)

  subroutine interp_1d( vx1, vy1, vx2, vy2)

    real (kind=T_SGL), DIMENSION(:), INTENT(in)  :: vx1, vy1
    real (kind=T_SGL), DIMENSION(:), INTENT(in)  :: vx2
    real (kind=T_SGL), DIMENSION(size(vx2)), INTENT(out) :: vy2

    ! [ LOCALS ]
    real (kind=T_DBL), dimension(:), allocatable  :: vx1p4, vy1p4, vslp

    real (kind=T_DBL) :: a0, a1, a2, a3, rr, dx
    integer (kind=T_INT)  :: n1, n2, n4, j1, j2, p1, p2, j1p4
    logical (kind=T_LOGICAL)  :: lfnd

    n1 = size(vx1)
    n2 = size(vx2)

    n4 = n1 + 4

    allocate ( vx1p4(n4), vy1p4(n4), vslp(n4) )

    vx1p4(3:n1+2) = real(vx1(:), T_DBL)
    vy1p4(3:n1+2) = real(vy1(:), T_DBL)

    !! Extending input X array :
    !! =========================
    !! - if vx is regularly spaced, it's not a big deal, otherwise we use
    !!   what's been proposed by Akima (1970) :

    !! Bottom (or West) :
    vx1p4(2) =  real(vx1p4(4) - (vx1p4(5) - vx1p4(3)), T_DBL)
    vx1p4(1) =  real(vx1p4(3) - (vx1p4(5) - vx1p4(3)), T_DBL)

    !! Top (or East) :
    vx1p4(n4-1) =  real(vx1p4(n4-3) + vx1p4(n4-2) - vx1p4(n4-4), T_DBL)
    vx1p4(n4)   =  real(vx1p4(n4-2) + vx1p4(n4-2) - vx1p4(n4-4), T_DBL)


    !! Now extrapolating input Y values on these 4 extra points :
    !! ==========================================================

    !! Bottom (or West) :
    call extra_2_west(vx1p4(5), vx1p4(4), vx1p4(3), vx1p4(2), vx1p4(1), &
         &            vy1p4(5), vy1p4(4), vy1p4(3), vy1p4(2), vy1p4(1) )

    !! Top (or East) :
    call extra_2_east(vx1p4(n4-4), vx1p4(n4-3), vx1p4(n4-2), vx1p4(n4-1), vx1p4(n4), &
         &            vy1p4(n4-4), vy1p4(n4-3), vy1p4(n4-2), vy1p4(n4-1), vy1p4(n4) )

    !! Computing slopes :
    !! ==================
    call SLOPES(n4, vx1p4, vy1p4, vslp)

 mainloop: do j2 = 1, n2
       !!
       j1   = 1
       lfnd = .FALSE.

       !! Persistence: if point of output grid is shallower
       !!              than first point of input grid
       !!              (should occur only when j2 = 1)
       if ( vx2(j2) < vx1(1) ) then
          vy2(j2) = vy1(1)
           cycle
       endif

       do WHILE ( (j1 < n1).and.(.not. lfnd) )

          if ( (vx2(j2) > vx1(j1)).and.(vx2(j2) < vx1(j1+1)) ) then
             j1p4  = j1 + 2
             p1    = j1p4
             p2    = j1p4 + 1
             lfnd = .TRUE.

          else

             if ( vx2(j2) == vx1(j1) ) then
                vy2(j2) = vy1(j1)
                 cycle mainloop
             else
                if ( vx2(j2) == vx1(j1+1) ) then
                   vy2(j2) = vy1(j1+1)
                    cycle mainloop
                endif
             endif

          endif

          j1 = j1 + 1

       end do
        !!  (PM) Take bottom value below the last data
       if (( j1 == n1) .and. (.not. lfnd)) then
          vy2(j2) = vy1(n1)
           cycle
       elseif ( .not. lfnd ) then
          vy2(j2) = -6666.0
           cycle
       endif

       !! Estimating vy2(j2) with a third order polynome :
       !! -----------------------------------------------
       !! Coefficients of the polynome
       !! MIND ! : p1 and p2 are given in 'vp4'
       a0 = vy1p4(p1)
       a1 = vslp(p1)
       a2 = (3*(vy1p4(p2) - vy1p4(p1))/(vx1p4(p2) - vx1p4(p1)) - 2*vslp(p1) - vslp(p2)) &
            & /(vx1p4(p2) - vx1p4(p1))
       a3 = (vslp(p1) + vslp(p2) - 2*(vy1p4(p2)-vy1p4(p1))/(vx1p4(p2)-vx1p4(p1)))  &
            &         /(vx1p4(p2) - vx1p4(p1))**2

       dx = real(vx2(j2), T_DBL) - vx1p4(p1)

       rr = a0 + a1*dx + a2*dx**2 + a3*dx**3
       vy2(j2) = real(rr, 4)

    end do mainloop

    deALLOCATE ( vx1p4, vy1p4, vslp )

  end subroutine INTERP_1D

!------------------------------------------------------------------------------

  subroutine SLOPES(n, x, y, slope)
    !!
    !!   C o m p u t a t i o n   o f   s l o p e s  :
    !!   --------------------------------------------
    !! * For each given point, we compute its local slope, from the four
    !!   surrounding points and itself; following Akima 1970, p.590, eq.(1)
    !! * The slopes are stored into an array 'slp(:)' of length n
    !!
    !! INPUT :
    !! -------
    !!          - n       = length dimension of the array contening the
    !!                      input data
    !!          - x       = 1D array (n) contening input data coordinates
    !!          - y       = 1D array (n) contains values y(x)
    !!
    !! OUTPUT :
    !! --------
    !!          - slope  = 1D array (n) contening the slopes
    !!
    !!
    !! Author : Laurent Brodeau, dec. 2004
    !!
    !!-------------------------------------------------------------------
    !!
    integer (kind=T_INT),  INTENT(in) :: n
    real (kind=T_DBL), DIMENSION(n), INTENT(in)  :: x, y
    real (kind=T_DBL), DIMENSION(n), INTENT(out) :: slope
    !!_______________________________________________________
    !!
    !! Local :
    integer (kind=T_INT)  :: k
    real (kind=T_DBL) :: m1, m2, m3, m4
    !!
    !!
    !! Treating middle of array ( at least to points away from the bordures ) :
    !!-------------------------------------------------------------------------
    do k=3, n-2
       m1 =  ( y(k-1) - y(k-2) ) / ( x(k-1) - x(k-2) )
       m2 =  ( y(k)   - y(k-1) ) / ( x(k)   - x(k-1) )
       m3 =  ( y(k+1) - y(k)   ) / ( x(k+1) - x(k)   )
       m4 =  ( y(k+2) - y(k+1) ) / ( x(k+2) - x(k+1) )
       !!
       if ( (m1 == m2).and.(m3 == m4) ) then
          slope(k) = 0.5*(m2 + m3)
       else
          slope(k) =   ( ABS(m4-m3)*m2 + ABS(m2-m1)*m3 ) &
               &     / ( ABS(m4-m3)    + ABS(m2-m1) )
       endif
       !!
    end do
    !!
    !!
    !! Treating 'second' and 'before last' points :
    !! --------------------------------------------
    !! Point k=2
    m2 =  ( y(2)   - y(1)   ) / ( x(2)   - x(1)   )
    m3 =  ( y(3)   - y(2)   ) / ( x(3)   - x(2)   )
    slope(2)   = 0.5*( m2 + m3 )
    !! Point k=n-1
    m2 =  ( y(n-1) - y(n-2) ) / ( x(n-1) - x(n-2) )
    m3 =  ( y(n)   - y(n-1) ) / ( x(n)   - x(n-1) )
    slope(n-1) = 0.5*( m2 + m3 )
    !!
    !! Treating 'first' and 'last' points :
    !! --------------------------------------------
    !! Point k = 1
    m3 =  ( y(2)   - y(1)   ) / ( x(2)   - x(1)   )
    slope(1)   = m3
    !! Point k = n
    m2 =  ( y(n) - y(n-1) ) / ( x(n) - x(n-1) )
    slope(n)   = m2
    !!
    !!
  end subroutine SLOPES

  subroutine extra_2_east(x1, x2, x3, x4, x5, y1, y2, y3, y4, y5)
    !!
    !!============================================================================
    !!
    !! Extrapolates 2 extra east (or north) points of a curve with Akima's 1D method
    !!
    !! Input  : x1, x2, x3, x4, x5, y1, y2, y3
    !! Output : y4, y5
    !!
    !!                       Author : Laurent BRODEAU, 2007
    !!============================================================================
    !!
    !!
    real (kind=T_DBL), INTENT(in)  :: x1, x2, x3, x4, x5, y1, y2, y3
    real (kind=T_DBL), INTENT(out) :: y4, y5
    !!
    !! Local :
    real (kind=T_DBL) :: A, B, C, D, ALF, BET
    !!
    !!
    A    = x2 - x1
    B    = x3 - x2
    C    = x4 - x3
    D    = x5 - x4
    !!
    ALF  = y2 - y1
    BET  = y3 - y2
    !!
    if ( (A == 0.).OR.(B == 0.).OR.(C == 0.) ) then
       y4 = y3 ; y5 = y3
    else
       y4   = C*(2*BET/B - ALF/A) + y3
       y5   = y4 + y4*D/C + BET*D/B - ALF*D/A - y3*D/C
    endif
    !!
    !!
  end subroutine extra_2_east

!------------------------------------------------------------------------------

  subroutine extra_2_west(x5, x4, x3, x2, x1, y5, y4, y3, y2, y1)
    !!
    !!============================================================================
    !!
    !! Extrapolates 2 extra west (or south) points of a curve with Akima's 1D method
    !!
    !! Input  : x1, x2, x3, x4, x5, y1, y2, y3
    !! Output : y4, y5
    !!
    !!                       Author : Laurent BRODEAU, 2007
    !!============================================================================
    !!
    !!
    real (kind=T_DBL), INTENT(in)  :: x1, x2, x3, x4, x5, y5, y4, y3
    real (kind=T_DBL), INTENT(out) :: y1, y2
    real (kind=T_DBL) :: A, B, C, D, ALF, BET
    !!
    !! x1 -> x5
    !! x2 -> x4
    !! x3 -> x3
    !! x4 -> x2
    !! x5 -> x1
    !!
    A    = x4 - x5
    B    = x3 - x4
    C    = x2 - x3
    D    = x1 - x2
    !!
    ALF  = y4 - y5
    BET  = y3 - y4
    !!
    if ( (A == 0.).OR.(B == 0.).OR.(C == 0.) ) then
       y2 = y3; y1 = y3
    else
       y2   = C*(2*BET/B - ALF/A) + y3
       y1   = y2 + y2*D/C + BET*D/B - ALF*D/A - y3*D/C
    endif
    !!
    !!
  end subroutine extra_2_west


end module tsp_statistics
