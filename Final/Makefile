# Makefile for Tsunami Simulation Project
# Requires: netcdf-fortran library

# Compiler
FC = gfortran

# Compiler flags
FFLAGS = -O2 -Wall -fcheck=all -fbacktrace

# Optional: Use DC3D Okada implementation (finite fault)
# Set USE_DC3D=1 to use DC3D instead of default empirical formula
USE_DC3D ?= 0

# Optional: Use COMCOT SWE solver (requires COMCOT source code)
# Set USE_COMCOT=1 to use COMCOT instead of hand-written solver
USE_COMCOT ?= 0

# Conditional compilation flags
ifeq ($(USE_DC3D),1)
    FFLAGS += -DUSE_DC3D
endif

ifeq ($(USE_COMCOT),1)
    FFLAGS += -DUSE_COMCOT
    # TODO: Add COMCOT source directory when available
    # COMCOT_DIR = ./comcot
    # FFLAGS += -I$(COMCOT_DIR)
endif

# NetCDF library flags (adjust paths as needed)
# On macOS with Homebrew: 
#   NETCDF_INC = -I/opt/homebrew/include
#   NETCDF_LIB = -L/opt/homebrew/lib -lnetcdff -lnetcdf
# On Linux with system packages:
#   NETCDF_INC = 
#   NETCDF_LIB = -lnetcdff -lnetcdf

# Try to auto-detect NetCDF paths
NETCDF_INC ?= $(shell nc-config --fflags 2>/dev/null || echo "")
NETCDF_LIB ?= $(shell nc-config --flibs 2>/dev/null || echo "-lnetcdff -lnetcdf")

# If nc-config is not available, try pkg-config
ifeq ($(NETCDF_INC),)
    NETCDF_INC = $(shell pkg-config --cflags netcdf-fortran 2>/dev/null || echo "")
endif
ifeq ($(NETCDF_LIB),)
    NETCDF_LIB = $(shell pkg-config --libs netcdf-fortran 2>/dev/null || echo "-lnetcdff -lnetcdf")
endif

# Default target
all: tsunami_sim

# Object files (dependency order matters)
mod_netcdf_io.o: mod_netcdf_io.f90
	$(FC) $(FFLAGS) $(NETCDF_INC) -c mod_netcdf_io.f90

mod_okada.o: mod_okada.f90
	$(FC) $(FFLAGS) -c mod_okada.f90

# DC3D Okada implementation (FORTRAN 77, compiled separately)
dc3d.o: DC3D.f
	$(FC) -O2 -w -std=legacy -c DC3D.f -o dc3d.o

DC3D.f: DC3D.txt
	cp DC3D.txt DC3D.f

mod_okada_dc3d.o: mod_okada_dc3d.f90 mod_okada.o
	$(FC) $(FFLAGS) -c mod_okada_dc3d.f90

mod_swe_solver.o: mod_swe_solver.f90
	$(FC) $(FFLAGS) -c mod_swe_solver.f90

# COMCOT SWE solver (template - requires COMCOT source)
mod_swe_comcot.o: mod_swe_comcot.f90 mod_swe_solver.o
	$(FC) $(FFLAGS) -c mod_swe_comcot.f90
	@echo "Note: mod_swe_comcot is a template. Full integration requires COMCOT source code."

mod_netcdf_output.o: mod_netcdf_output.f90 mod_netcdf_io.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c mod_netcdf_output.f90

# Conditional compilation for tsunami_sim based on USE_DC3D and USE_COMCOT
ifeq ($(USE_COMCOT),1)
  ifeq ($(USE_DC3D),1)
tsunami_sim.o: tsunami_sim.f90 mod_netcdf_io.o mod_okada.o mod_okada_dc3d.o mod_swe_solver.o mod_swe_comcot.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c tsunami_sim.f90
  else
tsunami_sim.o: tsunami_sim.f90 mod_netcdf_io.o mod_okada.o mod_swe_solver.o mod_swe_comcot.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c tsunami_sim.f90
  endif
else
  ifeq ($(USE_DC3D),1)
tsunami_sim.o: tsunami_sim.f90 mod_netcdf_io.o mod_okada.o mod_okada_dc3d.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c tsunami_sim.f90
  else
tsunami_sim.o: tsunami_sim.f90 mod_netcdf_io.o mod_okada.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c tsunami_sim.f90
  endif
endif

test_netcdf_io.o: test_netcdf_io.f90 mod_netcdf_io.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c test_netcdf_io.f90

# Executables
ifeq ($(USE_DC3D),1)
tsunami_sim: tsunami_sim.o mod_netcdf_io.o mod_okada.o mod_okada_dc3d.o dc3d.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o tsunami_sim tsunami_sim.o mod_netcdf_io.o mod_okada.o \
	    mod_okada_dc3d.o dc3d.o mod_swe_solver.o mod_netcdf_output.o $(NETCDF_LIB)
else
tsunami_sim: tsunami_sim.o mod_netcdf_io.o mod_okada.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o tsunami_sim tsunami_sim.o mod_netcdf_io.o mod_okada.o \
	    mod_swe_solver.o mod_netcdf_output.o $(NETCDF_LIB)
endif

test_netcdf_io: test_netcdf_io.o mod_netcdf_io.o
	$(FC) $(FFLAGS) -o test_netcdf_io test_netcdf_io.o mod_netcdf_io.o $(NETCDF_LIB)

# COMCOT version executable
tsunami_sim_comcot.o: tsunami_sim_comcot.f90 mod_netcdf_io.o mod_okada.o mod_swe_comcot.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -cpp $(CPPFLAGS) -c tsunami_sim_comcot.f90

ifeq ($(USE_DC3D),1)
tsunami_sim_comcot: tsunami_sim_comcot.o mod_netcdf_io.o mod_okada.o mod_okada_dc3d.o dc3d.o mod_swe_comcot.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o tsunami_sim_comcot tsunami_sim_comcot.o mod_netcdf_io.o mod_okada.o \
	    mod_okada_dc3d.o dc3d.o mod_swe_comcot.o mod_swe_solver.o mod_netcdf_output.o $(NETCDF_LIB)
else
tsunami_sim_comcot: tsunami_sim_comcot.o mod_netcdf_io.o mod_okada.o mod_swe_comcot.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o tsunami_sim_comcot tsunami_sim_comcot.o mod_netcdf_io.o mod_okada.o \
	    mod_swe_comcot.o mod_swe_solver.o mod_netcdf_output.o $(NETCDF_LIB)
endif

# PGPLOT visualization (optional, requires PGPLOT library)
plot_tsunami_pgplot.o: plot_tsunami_pgplot.f90 mod_netcdf_io.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c plot_tsunami_pgplot.f90

plot_tsunami_pgplot: plot_tsunami_pgplot.o mod_netcdf_io.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o plot_tsunami_pgplot plot_tsunami_pgplot.o \
	    mod_netcdf_io.o mod_netcdf_output.o $(NETCDF_LIB) -lpgplot -lX11

# Clean
clean:
	rm -f *.o *.mod tsunami_sim tsunami_sim_comcot test_netcdf_io plot_tsunami_pgplot \
	      compare_okada tsunami_output.nc tsunami_output_comcot.nc DC3D.f

# Test run
run-test: test_netcdf_io
	./test_netcdf_io

run: tsunami_sim
	./tsunami_sim

# Comparison program
compare_okada.o: compare_okada.f90 mod_netcdf_io.o mod_okada.o mod_okada_dc3d.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c compare_okada.f90

compare_okada: compare_okada.o mod_netcdf_io.o mod_okada.o mod_okada_dc3d.o dc3d.o
	$(FC) $(FFLAGS) -o compare_okada compare_okada.o mod_netcdf_io.o mod_okada.o \
	    mod_okada_dc3d.o dc3d.o $(NETCDF_LIB)

compare: compare_okada
	./compare_okada

# Help
help:
	@echo "Available targets:"
	@echo "  all          - Build the tsunami simulation program (default)"
	@echo "  tsunami_sim  - Build the main tsunami simulation"
	@echo "  test_netcdf_io - Build the NetCDF I/O test program"
	@echo "  plot_tsunami_pgplot - Build PGPLOT visualization (requires PGPLOT)"
	@echo "  clean        - Remove object files and executables"
	@echo "  run          - Build and run the tsunami simulation"
	@echo "  run-test     - Build and run the NetCDF I/O test"
	@echo "  compare      - Build and run Okada comparison program"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Compilation options:"
	@echo "  USE_DC3D=1   - Use DC3D finite fault Okada implementation"
	@echo "                 Example: make USE_DC3D=1"
	@echo "  USE_DC3D=0   - Use default empirical Okada approximation (default)"
	@echo ""
	@echo "  USE_COMCOT=1 - Use COMCOT SWE solver (requires COMCOT source)"
	@echo "                 Example: make USE_COMCOT=1"
	@echo "  USE_COMCOT=0 - Use hand-written SWE solver (default)"
	@echo ""
	@echo "  Combined:    - Use both reference implementations"
	@echo "                 Example: make USE_DC3D=1 USE_COMCOT=1"
	@echo ""
	@echo "Note: Make sure netcdf-fortran is installed."
	@echo "  macOS: brew install netcdf"
	@echo "  Linux: sudo apt-get install libnetcdff-dev (Ubuntu/Debian)"
	@echo "         sudo yum install netcdf-fortran-devel (RHEL/CentOS)"

.PHONY: all clean run run-test help


