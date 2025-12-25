# Makefile for Tsunami Simulation Project
# Requires: netcdf-fortran library

# Compiler
FC = gfortran

# Compiler flags
FFLAGS = -O2 -Wall -fcheck=all -fbacktrace

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

mod_swe_solver.o: mod_swe_solver.f90
	$(FC) $(FFLAGS) -c mod_swe_solver.f90

mod_netcdf_output.o: mod_netcdf_output.f90 mod_netcdf_io.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c mod_netcdf_output.f90

tsunami_sim.o: tsunami_sim.f90 mod_netcdf_io.o mod_okada.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c tsunami_sim.f90

test_netcdf_io.o: test_netcdf_io.f90 mod_netcdf_io.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c test_netcdf_io.f90

# Executables
tsunami_sim: tsunami_sim.o mod_netcdf_io.o mod_okada.o mod_swe_solver.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o tsunami_sim tsunami_sim.o mod_netcdf_io.o mod_okada.o \
	    mod_swe_solver.o mod_netcdf_output.o $(NETCDF_LIB)

test_netcdf_io: test_netcdf_io.o mod_netcdf_io.o
	$(FC) $(FFLAGS) -o test_netcdf_io test_netcdf_io.o mod_netcdf_io.o $(NETCDF_LIB)

# PGPLOT visualization (optional, requires PGPLOT library)
plot_tsunami_pgplot.o: plot_tsunami_pgplot.f90 mod_netcdf_io.o mod_netcdf_output.o
	$(FC) $(FFLAGS) $(NETCDF_INC) -c plot_tsunami_pgplot.f90

plot_tsunami_pgplot: plot_tsunami_pgplot.o mod_netcdf_io.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o plot_tsunami_pgplot plot_tsunami_pgplot.o \
	    mod_netcdf_io.o mod_netcdf_output.o $(NETCDF_LIB) -lpgplot -lX11

# Clean
clean:
	rm -f *.o *.mod tsunami_sim test_netcdf_io plot_tsunami_pgplot tsunami_output.nc

# Test run
run-test: test_netcdf_io
	./test_netcdf_io

run: tsunami_sim
	./tsunami_sim

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
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Note: Make sure netcdf-fortran is installed."
	@echo "  macOS: brew install netcdf"
	@echo "  Linux: sudo apt-get install libnetcdff-dev (Ubuntu/Debian)"
	@echo "         sudo yum install netcdf-fortran-devel (RHEL/CentOS)"

.PHONY: all clean run run-test help


