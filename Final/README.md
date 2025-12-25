# Tsunami Simulation Project

**Author**: Bowen Chen  
**Date**: December 2025  
**Course**: Earth Science Programming

---

## Overview

This project implements a comprehensive numerical simulation framework for tsunami generation and propagation, using the 2011 Tohoku earthquake (Mw 9.0) as a case study. The implementation combines seismic source modeling with wave propagation simulation, written in Fortran 90 with modular architecture.

### Key Features

- **Dual Okada Model Implementation**: Empirical approximation and DC3D finite-fault model
- **Dual SWE Solver Implementation**: Hand-written nonlinear and COMCOT linear solvers
- **High-Resolution Bathymetry**: GEBCO 2025 data (1920 x 1800 grid points)
- **NetCDF I/O**: Standard format for input/output data
- **Modular Architecture**: Easy to extend and modify
- **Conditional Compilation**: Select implementation variants at build time

---

## Implementation Variants

Four implementation combinations are available:

| Variant | Okada Model | SWE Solver | Compile Command | Output File |
|---------|-------------|------------|-----------------|-------------|
| 1. Baseline | Empirical | Hand-written | `make` | `tsunami_output.nc` |
| 2. High-accuracy | DC3D | Hand-written | `make USE_DC3D=1` | `tsunami_output.nc` |
| 3. Industry-standard | Empirical | COMCOT | `make tsunami_sim_comcot` | `tsunami_output_comcot.nc` |
| 4. Best-of-both | DC3D | COMCOT | `make tsunami_sim_comcot USE_DC3D=1` | `tsunami_output_comcot.nc` |

**Recommended**: Variant 4 (DC3D + COMCOT) provides the best balance of accuracy and computational efficiency.

---

## Project Structure

```
Final/
├── Core Modules
│   ├── mod_netcdf_io.f90           # NetCDF bathymetry I/O
│   ├── mod_okada.f90                # Empirical Okada model
│   ├── mod_okada_dc3d.f90           # DC3D finite-fault Okada
│   ├── mod_swe_solver.f90           # Hand-written nonlinear SWE solver
│   ├── mod_swe_comcot.f90           # COMCOT linear SWE solver
│   └── mod_netcdf_output.f90        # NetCDF results output
│
├── Main Programs
│   ├── tsunami_sim.f90              # Standard version
│   └── tsunami_sim_comcot.f90       # COMCOT version
│
├── Comparison Tools
│   ├── compare_okada.f90            # Compare Okada implementations
│   └── compare_swe.f90              # Compare SWE solvers (template)
│
├── Visualization
│   ├── plot_tsunami.py              # Python plotting and analysis
│   └── plot_tsunami_pgplot.f90      # Fortran PGPLOT visualization
│
├── External Sources
│   ├── DC3D.txt                     # Okada (1985) original code
│   └── comcot-gfortran/             # COMCOT reference (in ~/Documents/)
│
├── Input Data
│   ├── GEBCO_21_Dec_2025_d9303d544c3e/gebco_2025_n41.5_s34.0_w138.0_e146.0.nc
│   └── fault_params.txt             # 2011 Tohoku earthquake parameters
│
├── Output
│   ├── tsunami_output.nc            # Standard version output
│   ├── tsunami_output_comcot.nc     # COMCOT version output
│   └── plots/                       # Generated figures
│
├── Build System
│   ├── Makefile                     # Complete build configuration
│   └── .gitignore                   # Version control ignore patterns
│
├── Documentation
│   ├── README.md                    # This file
│   ├── guide.md                     # Original project specifications
│   ├── netcdf_spec.md               # GEBCO NetCDF format documentation
│   ├── README_DC3D.md               # DC3D integration guide
│   ├── README_COMCOT.md             # COMCOT integration guide
│   ├── QUICK_START_DC3D_COMCOT.md   # Quick start for best combination
│   ├── README_FINAL_STATUS.md       # Complete project status
│   ├── REPORT_UPDATE_SUMMARY.md     # LaTeX report update log
│   └── report.tex                   # Academic report (LaTeX)
│
└── Test Programs
    └── test_netcdf_io.f90           # NetCDF I/O testing
```

---

## Dependencies

### Required

- **gfortran** (GCC 4.8 or later)
- **NetCDF-Fortran library** (4.x or later)
- **GNU Make** (3.82 or later)

### Optional

- **Python 3.x** with numpy, matplotlib, netCDF4 (for visualization)
- **PGPLOT library** (for Fortran plotting, optional)

### Installation (macOS with Homebrew)

```bash
brew install gcc netcdf netcdf-fortran
```

### Installation (Ubuntu/Debian)

```bash
sudo apt-get install gfortran libnetcdf-dev libnetcdff-dev
```

---

## Quick Start

### Standard Simulation (Empirical Okada + Hand-written SWE)

```bash
# Compile
make

# Run
./tsunami_sim

# Visualize
python plot_tsunami.py
```

### Recommended Simulation (DC3D + COMCOT)

```bash
# Compile DC3D modules first
make dc3d.o mod_okada_dc3d.o

# Compile COMCOT version with DC3D
make tsunami_sim_comcot USE_DC3D=1

# Run
./tsunami_sim_comcot

# Visualize
python plot_tsunami.py
```

---

## Compilation Options

### Basic Compilation

```bash
make                    # Standard version (empirical Okada + hand-written SWE)
make clean              # Clean all object files and executables
```

### With DC3D Okada Model

```bash
make USE_DC3D=1                    # Standard version with DC3D
make tsunami_sim_comcot USE_DC3D=1 # COMCOT version with DC3D
```

### Comparison Tools

```bash
make compare_okada      # Compare Okada implementations
make test_netcdf_io     # Test NetCDF I/O
```

### All Targets

```bash
make all                # Build all main executables
```

### Help

```bash
make help               # Display all available targets
```

---

## Input Data

### Bathymetry (GEBCO 2025)

- **File**: `gebco_2025_n41.5_s34.0_w138.0_e146.0.nc`
- **Region**: Japan Trench (138°E-146°E, 34°N-41.5°N)
- **Resolution**: 15 arc-seconds (~450 meters)
- **Grid Size**: 1920 (longitude) × 1800 (latitude)
- **Format**: NetCDF with `elevation` variable (negative for ocean)

### Fault Parameters (2011 Tohoku Earthquake)

File: `fault_params.txt`

```
Strike:  203.0°    (Azimuth of fault trace)
Dip:     10.0°     (Fault plane angle)
Rake:    90.0°     (Slip direction - pure thrust)
Slip:    50.0 m    (Fault displacement)
Depth:   20.0 km   (Top of fault plane)
Length:  500.0 km  (Along-strike dimension)
Width:   200.0 km  (Along-dip dimension)
Center:  142.0°E, 38.0°N (Fault center location)
```

---

## Simulation Parameters

### Default Settings

- **Duration**: 3 hours (10,800 seconds)
- **Output Interval**: 5 minutes (300 seconds)
- **Time Step**: Automatically computed (CFL condition), typically 0.1-0.2 seconds
- **Total Steps**: ~36,000-54,000 depending on variant
- **Computational Time**: 
  - Hand-written solver: ~45 minutes
  - COMCOT solver: ~40 minutes (after bug fix)

### CFL Stability Condition

Time step is automatically computed to satisfy:

```
dt <= 0.3 * min(dx, dy) / (c_max + max(|u|, |v|))
```

where `c_max = sqrt(g * h_max)` is the maximum wave speed.

---

## Output Data

### NetCDF Format

Both output files contain:

- **Dimensions**: `time`, `lon`, `lat`
- **Variables**:
  - `time(time)`: Time in seconds since simulation start
  - `lon(lon)`: Longitude in degrees East
  - `lat(lat)`: Latitude in degrees North
  - `eta(time, lon, lat)`: Surface elevation in meters

### Expected Results

#### Initial Displacement

- **Empirical Okada**: ±5.0 m (symmetric pattern)
- **DC3D Okada**: +12.86 m uplift, -10.83 m subsidence (asymmetric)

#### Wave Propagation

- **Maximum wave heights**: 5-15 meters near source region
- **Coastal amplification**: Factor of 2-4 in shallow water
- **Propagation speed**: ~200-280 m/s in deep ocean (depth-dependent)
- **First arrival at coast**: 10-30 minutes after earthquake

---

## Visualization

### Python Plotting

```bash
python plot_tsunami.py
```

Generates in `plots/` directory:
- `initial_displacement.png`: Okada model output
- `wave_propagation_tXXXXs.png`: Wave snapshots
- `maximum_amplitude.png`: Maximum wave height distribution
- `wave_energy.png`: Energy distribution over time
- `statistics.txt`: Numerical statistics

### Fortran PGPLOT (Optional)

```bash
make plot_tsunami_pgplot
./plot_tsunami_pgplot
```

Requires PGPLOT library installation.

---

## Key Implementation Details

### Okada Model

#### Empirical Version (mod_okada.f90)
- Simplified point-source approximation
- Distance-dependent decay (10% of slip)
- Fast computation (< 1 second)
- Suitable for preliminary studies

#### DC3D Version (mod_okada_dc3d.f90)
- Rigorous finite-fault implementation
- Based on Okada (1985) analytical solution
- Numerical integration over fault plane
- Accurate displacement field (~30 seconds)
- Recommended for research applications

### SWE Solver

#### Hand-written Nonlinear (mod_swe_solver.f90)
- Full nonlinear shallow water equations
- Arakawa C-grid (staggered grid)
- Leap-frog time stepping (three-level)
- Explicit forward Euler initialization
- Wet/dry boundary treatment
- NaN/Infinity checks for stability

#### COMCOT Linear (mod_swe_comcot.f90)
- Linear shallow water equations
- Explicit finite difference
- Radiation boundary conditions
- Based on Cornell Multi-grid model
- Industry-standard formulation
- Tested on 2011 Tohoku event

### Critical Bug Fix

**Issue**: COMCOT version initially produced numerical instability (wave heights exceeding 300 m).

**Root Cause**: GEBCO bathymetry uses elevation (negative for ocean), but SWE requires positive water depth. Missing sign conversion caused negative depth values, leading to imaginary wave speeds.

**Solution**: Added conversion in `mod_swe_comcot.f90`:
```fortran
grid%bath = -bathymetry  ! Convert elevation to depth
```

**Status**: Fixed and validated. Post-fix results show physically realistic wave heights.

---

## Comparison of Implementations

### Okada Models

| Feature | Empirical | DC3D |
|---------|-----------|------|
| Displacement | ±5.0 m | +12.86 / -10.83 m |
| Spatial pattern | Symmetric | Asymmetric (realistic) |
| Computation time | < 1 second | ~30 seconds |
| Accuracy | Approximate | High (analytical) |
| Use case | Testing | Research |

### SWE Solvers

| Feature | Hand-written | COMCOT |
|---------|--------------|--------|
| Equations | Nonlinear | Linear |
| Advection terms | Included | Omitted |
| Time stepping | Leap-frog | Forward Euler |
| Boundary conditions | Reflection + absorption | Radiation |
| Stability | Requires initialization | Simpler |
| Computation speed | ~45 min | ~40 min |
| Accuracy | High (nonlinear) | Good (linear approx.) |
| Validation | Hand-coded | 2011 Tohoku tested |

---

## Testing and Validation

### Unit Tests

```bash
# Test NetCDF I/O
make test_netcdf_io
./test_netcdf_io

# Compare Okada implementations
make compare_okada
./compare_okada
```

### Validation Methods

1. **Mass Conservation**: Check total water volume remains constant
2. **Energy Conservation**: Monitor energy dissipation (should be minimal)
3. **CFL Stability**: Verify time step satisfies stability condition
4. **Physical Ranges**: Ensure wave heights are realistic (< 20 m in most areas)

### Future Validation

Integration with IOC Sea Level Monitoring Facility (http://www.ioc-sealevelmonitoring.org/):
- Compare with tide gauge records from 2011 Tohoku event
- Validate arrival times and wave amplitudes
- Quantitative metrics (RMSE, correlation)

---

## Performance

### Computational Complexity

- **Grid Points**: 1920 × 1800 = 3,456,000
- **Time Steps**: ~50,000 for 3-hour simulation
- **Operations per Step**: ~6.5 million FLOPs
- **Total Operations**: ~350 billion FLOPs
- **Memory Usage**: ~200 MB for grid arrays

### Optimization

- Fortran 90 with `-O2` optimization
- No parallelization (single-threaded)
- NetCDF I/O overhead: ~15%

### Potential Improvements

- OpenMP threading for multi-core CPUs
- MPI parallelization for distributed systems
- GPU acceleration (CUDA/OpenCL)
- Adaptive time stepping
- Nested grids for higher coastal resolution

---

## Limitations

### Current Implementation

1. **Uniform Fault Slip**: Real earthquakes have heterogeneous slip distributions
2. **Fixed Grid**: No adaptive mesh refinement
3. **2D Approximation**: Shallow water equations neglect vertical accelerations
4. **No Bottom Friction**: Manning's formula not implemented
5. **No Coriolis Effect**: Not significant for short-duration tsunamis
6. **Linear COMCOT**: May underestimate amplitudes in very shallow water

### Data Limitations

1. **Bathymetry Resolution**: 450 m may miss small coastal features
2. **Simplified Fault Model**: Single rectangular fault
3. **No Post-Earthquake Deformation**: Only initial displacement

---

## Troubleshooting

### Compilation Errors

**Problem**: Cannot find netcdf-fortran libraries

**Solution**:
```bash
# Check installation
pkg-config --libs netcdf-fortran

# Set paths manually if needed
export NETCDF_INC=/path/to/netcdf/include
export NETCDF_LIB=/path/to/netcdf/lib
```

**Problem**: `mod_okada_dc3d.mod` not found when compiling COMCOT with DC3D

**Solution**:
```bash
# Compile DC3D modules first
make dc3d.o mod_okada_dc3d.o
make tsunami_sim_comcot USE_DC3D=1
```

### Runtime Errors

**Problem**: Numerical instability (NaN or extremely large values)

**Solution**:
- Check bathymetry data units
- Verify CFL condition is satisfied
- Reduce time step if needed
- Check for negative water depths

**Problem**: Simulation runs very slowly

**Solution**:
- Reduce simulation duration (`t_end` in source code)
- Increase output interval
- Use COMCOT solver (slightly faster)
- Check system resources

---

## Version Control

A `.gitignore` file is provided to exclude compilation artifacts and output files from version control:

**Ignored Files**:
- Compilation: `*.o`, `*.mod`, executables
- Output: `*.nc`, `plots/`, `*.log`
- Temporary: `DC3D.f`
- System: `.DS_Store`, `Thumbs.db`
- Editor: `*.swp`, `.vscode/`, `.idea/`

**What to track**:
- All `.f90` source files
- `Makefile`, configuration files
- Documentation (`.md`, `.tex`)
- Input data (if small enough)

See `GITIGNORE_EXPLANATION.md` for detailed documentation.

---

## Documentation

- **README_DC3D.md**: DC3D Okada model integration guide
- **README_COMCOT.md**: COMCOT solver integration details
- **README_FINAL_STATUS.md**: Comprehensive project status and features
- **report.tex**: Academic report (LaTeX source)

---

## References

### Scientific Literature

1. Okada, Y. (1985). Surface deformation due to shear and tensile faults in a half-space. *Bulletin of the Seismological Society of America*, 75(4), 1135-1154.

2. Satake, K., & Fujii, Y. (2013). Review: Source models of the 2011 Tohoku earthquake and long-term forecast of large earthquakes. *Earth, Planets and Space*, 65(10), 1193-1199.

### Data Sources

- **GEBCO 2025**: https://www.gebco.net/
- **USGS Earthquake Data**: https://earthquake.usgs.gov/
- **IOC Sea Level Monitoring**: http://www.ioc-sealevelmonitoring.org/

### Software

- **COMCOT gfortran**: https://github.com/AndybnACT/comcot-gfortran
  - Developed by Tsunami Research Group, IHOS, National Central University, Taiwan
  - Used as reference for linear SWE solver implementation

- **DC3D Source**: Original Okada (1985) FORTRAN 77 code
  - Included as `DC3D.txt` in this repository
  - Compiled separately due to fixed-format legacy syntax

---

## License

This project is developed for educational purposes as part of Earth Science Programming coursework.

The DC3D code is based on Okada (1985) analytical solution, widely used in the geophysics community.

The COMCOT solver implementation is inspired by the open-source comcot-gfortran project by NCU Taiwan.

GEBCO bathymetry data is publicly available under GEBCO terms of use.

---

## Acknowledgments

- **Professor and Course Staff**: For project guidance and support
- **NCU Tsunami Research Group**: For the comcot-gfortran implementation
- **GEBCO**: For providing high-quality global bathymetry data
- **IOC**: For maintaining the global sea level monitoring network
- **Okada (1985)**: For the foundational elastic dislocation theory

---

## Contact

**Author**: Bowen Chen  
**Course**: Earth Science Programming  
**Date**: December 2025

For questions or issues, please refer to the documentation files or consult the course materials.

---

## Version History

- **v1.0** (Dec 2025): Initial implementation with empirical Okada and hand-written SWE
- **v1.1** (Dec 2025): Added DC3D finite-fault Okada model
- **v1.2** (Dec 2025): Integrated COMCOT linear SWE solver
- **v1.3** (Dec 2025): Fixed COMCOT bathymetry unit conversion bug
- **v1.4** (Dec 2025): Complete documentation and LaTeX report

---

**Status**: Production-ready for educational and research applications

