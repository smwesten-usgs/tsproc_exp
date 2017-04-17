
if(TARGET__TSPROC_EXECUTABLE)

  add_executable(tsproc2
            tsp_utilities.F90
            tsp_data_structures.F90
            tsp_control_file_ops.F90
            tsp_time_series_manager.F90
            tsp_file_readers.F90
            tsp_table_manager.F90
            tsp_collections.F90
            tsp_statistics.F90
            tsp_file_writers.F90
            tsp_datetime_class.F90
            tsp_equations_interpreter.F90
            tsp_legacy_code.F90
            tsp_main_loop.F90
            tsp_fortran_main.F90

            generated/version_control.F90

            wdm_support/DTTM90.FOR
            wdm_support/TSBUFR.FOR
            wdm_support/MSEXPT.FOR
            wdm_support/UTCHAR.FOR
            wdm_support/UTCP90.FOR
            wdm_support/UTDATE.FOR
            wdm_support/UTNUMB.FOR
            wdm_support/UTWDMD.FOR
            wdm_support/UTWDMF.FOR
            wdm_support/UTWDT1.FOR
            wdm_support/WDATM1.FOR
            wdm_support/WDATM2.FOR
            wdm_support/WDATRB.FOR
            wdm_support/WDBTCH.FOR
            wdm_support/WDDLG.FOR
            wdm_support/WDEXPT.FOR
            wdm_support/WDMESS.FOR
            wdm_support/WDMID.FOR
            wdm_support/WDOP.FOR
            wdm_support/WDTMS1.FOR
            wdm_support/WDTMS2.FOR
            wdm_support/WDTBLE.FOR )

   set_target_properties( tsproc2 PROPERTIES LINKER_LANGUAGE CXX )

   install(TARGETS tsproc2
           RUNTIME
           DESTINATION bin
           )

endif()

if(TARGET__TSPROC_LIBRARY)

  add_library(tsproc2lib SHARED
            tsp_utilities.F90
            tsp_data_structures.F90
            tsp_control_file_ops.F90
            tsp_time_series_manager.F90
            tsp_file_readers.F90
            tsp_table_manager.F90
            tsp_collections.F90
            tsp_statistics.F90
            tsp_file_writers.F90
            tsp_datetime_class.F90
            tsp_equations_interpreter.F90
            tsp_legacy_code.F90

            generated/version_control.F90

            wdm_support/DTTM90.FOR
            wdm_support/TSBUFR.FOR
            wdm_support/MSEXPT.FOR
            wdm_support/UTCHAR.FOR
            wdm_support/UTCP90.FOR
            wdm_support/UTDATE.FOR
            wdm_support/UTNUMB.FOR
            wdm_support/UTWDMD.FOR
            wdm_support/UTWDMF.FOR
            wdm_support/UTWDT1.FOR
            wdm_support/WDATM1.FOR
            wdm_support/WDATM2.FOR
            wdm_support/WDATRB.FOR
            wdm_support/WDBTCH.FOR
            wdm_support/WDDLG.FOR
            wdm_support/WDEXPT.FOR
            wdm_support/WDMESS.FOR
            wdm_support/WDMID.FOR
            wdm_support/WDOP.FOR
            wdm_support/WDTMS1.FOR
            wdm_support/WDTMS2.FOR
            wdm_support/WDTBLE.FOR )

   set_target_properties( tsproc2lib PROPERTIES LINKER_LANGUAGE CXX )

   install( TARGETS tsproc2lib
            ARCHIVE DESTINATION lib
            LIBRARY DESTINATION lib
            COMPONENT library
          )

endif()
