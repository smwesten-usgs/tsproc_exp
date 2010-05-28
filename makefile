#
# Makefile for compiling under MSYS/gfortran on Windows-32 platform
#
#FFLAGS	= -O2 -x f95-cpp-input -fopenmp
#FFLAGS	= -O2 -cpp
FFLAGS	= -O2 -cpp -fbounds-check

F95		= gfortran
#F95		= g95
GCC     = gcc

#LIBPATH	= -L/cygdrive/c/usr/local/lib/gcc-lib/i686-pc-mingw/4.0.3
LIB		= -lgfortran

# -luser32 -lgdi32

all:	main tsp_test

main:	tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o tsproc_main.o wsc_additions.o \
		tsp_command_processors.o
		$(F95) -o tsproc \
		tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o tsproc_main.o wsc_additions.o \
		tsp_command_processors.o \
		$(LIBPATH) $(LIB) $(FFLAGS)

tsp_test:	tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o wsc_additions.o tsp_command_processors.o \
		tsp_hydrologic_indices.o test_hydrologic_indices.F90
		$(F95) test_hydrologic_indices.F90 -o tsp_hyd_indices \
		tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o wsc_additions.o tsp_command_processors.o \
		tsp_hydrologic_indices.o \
		$(LIBPATH) $(LIB) $(FFLAGS)

tsp_data_structures.o: tsp_data_structures.F90
		$(F95) $(FFLAGS) -c tsp_data_structures.F90 -o tsp_data_structures.o

tsp_hydrologic_indices.o: tsp_hydrologic_indices.F90
		$(F95) $(FFLAGS) -c tsp_hydrologic_indices.F90 -o tsp_hydrologic_indices.o

tsp_utilities.o: tsp_utilities.F90 tsp_data_structures.o
		$(F95) $(FFLAGS) -c tsp_utilities.F90 -o tsp_utilities.o

tsp_time_series_processors.o: tsp_time_series_processors.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c tsp_time_series_processors.F90 -o tsp_time_series_processors.o

tsproc_main.o: tsproc_main.F90  tsp_data_structures.o tsp_utilities.o tsp_time_series_processors.o \
		wsc_additions.o tsp_command_processors.o
		$(F95) $(FFLAGS) -c tsproc_main.F90 -o tsproc_main.o

tsp_main.o: tsp_main.F90  tsp_data_structures.o tsp_utilities.o tsp_time_series_processors.o \
		wsc_additions.o tsp_command_processors.o
		$(F95) $(FFLAGS) -c tsp_main.F90 -o tsp_main.o

tsp_equation_parser.o: tsp_equation_parser.F90  tsp_data_structures.o tsp_utilities.o \
		tsp_command_processors.o
		$(F95) $(FFLAGS) -c tsp_equation_parser.F90 -o tsp_equation_parser.o

tsp_input.o: tsp_input.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c tsp_input.F90 -o tsp_input.o

tsp_output.o: tsp_output.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c tsp_output.F90 -o tsp_output.o

tsp_command_processors.o: tsp_command_processors.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c tsp_command_processors.F90 -o tsp_command_processors.o

wsc_additions.o: wsc_additions.F90  tsp_data_structures.o tsp_utilities.o tsp_time_series_processors.o
		$(F95) $(FFLAGS) -c wsc_additions.F90 -o wsc_additions.o
clean :
		rm -f *.o
		rm -f tsproc.exe
		rm -f tsproc
		rm -f *.mod

copy:
		sudo chmod 755 tsproc
		sudo cp tsproc /usr/local/bin/tsproc

copy_win:
		cp tsproc.exe d:/DOS/tsproc.exe
