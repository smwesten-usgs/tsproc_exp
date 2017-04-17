tsproc_exp
==========

TSPROC - experimental branch

TSPROC is a Time Series PROCessor originally written by John Doherty; John is founder of Watermark Numerical Computing and is a Professor at the National Centre for Groundwater Research and Training at Flinders University, Australia. 

This version of TSPROC is INCOMPLETE. It was begun as a complete re-write of the TSPROC code. About 90% of the functionality of the original TSPROC is present in the new TSPROC code. The table below compares the basic functionality as of December 2013:

<table>
  <tr>
    <th>TSPROC Block Name</th><th>tsproc</th><th>tsproc_exp</th>  
  </tr>
  <tr>
    <th>DIGITAL_FILTER</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>Butterworth Filter</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>Baseflow Separation Filter</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>Clipping</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>Settling Time</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>Reverse Filtering</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>ERASE_ENTITY</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>EXCEEDENCE_TIME</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>GET_MUL_SERIES_GENERAL</th><th></th><th></th>
  </tr>
  <tr>
    <th>GET_MUL_SERIES_GSFLOW_GAGE</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>GET_MUL_SERIES_NWIS</th><th></th><th>*</th>
  </tr>
  <tr>
    <th>GET_MUL_SERIES_SSF</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>GET_MUL_SERIES_STATVAR</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>GET_SERIES_PLOTGEN</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>GET_SERIES_SSF</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>GET_SERIES_TETRAD</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>GET_SERIES_UFORE_HYDRO</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>GET_SERIES_WDM</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>HYDRO_EVENTS</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>HYDROLOGIC_INDICES</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>LIST_OUTPUT</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>NEW_SERIES_UNIFORM</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>NEW_TIME_BASE</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>PERIOD_STATISTICS</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>REDUCE_TIME_SPAN</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>SERIES_BASE_LEVEL</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>SERIES_CLEAN</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>SERIES_COMPARE</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>SERIES_DIFFERENCE</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>SERIES_DISPLACE</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>SERIES_EQUATION</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>SERIES_STATISTICS</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>SETTINGS</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>USGS_HYSEP</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>V_TABLE_TO_SERIES</th><th>*</th><th></th>
  </tr>
  <tr>
    <th>VOLUME_CALCULATION</th><th>*</th><th>*</th>
  </tr>
  <tr>
    <th>WRITE_PEST_FILES</th><th>*</th><th>*</th>
  </tr>
</table>

In addition, the functionality that *is* coded into tsproc_exp needs further testing before use in any project work.
