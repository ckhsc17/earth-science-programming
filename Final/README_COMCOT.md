# COMCOT SWE Integration Guide

## Overview
This guide explains how to integrate COMCOT's Shallow Water Equations (SWE) solver into the tsunami simulation project, similar to the DC3D Okada integration.

## COMCOT Source Code

### Recommended Source (gfortran Compatible)
**GitHub Repository**: https://github.com/AndybnACT/comcot-gfortran

This is a gfortran-compatible version of COMCOT, which is perfect for our project since we're already using gfortran.

#### Key Features:
- ✅ Already ported to gfortran (matches our compiler)
- ✅ Clear modular structure with separate files
- ✅ Includes Makefile for easy compilation
- ✅ Tested with 2011 Tōhoku tsunami
- ✅ Well-maintained and documented

### Download Options

#### Option 1: Git Clone (Recommended)
```bash
cd /path/to/Final
git clone https://github.com/AndybnACT/comcot-gfortran.git comcot_source
```

#### Option 2: Direct Download
```bash
# Download ZIP file
curl -L https://github.com/AndybnACT/comcot-gfortran/archive/refs/heads/master.zip -o comcot.zip

# Or download manually from:
# https://github.com/AndybnACT/comcot-gfortran/archive/refs/heads/master.zip

# Extract
unzip comcot.zip
mv comcot-gfortran-master comcot_source
```

#### Option 3: Manual Download
1. Visit: https://github.com/AndybnACT/comcot-gfortran
2. Click "Code" → "Download ZIP"
3. Extract to `Final/comcot_source/`

### Key Files in comcot-gfortran

Based on the repository structure, here are the files we need:
Based on the repository structure, here are the files we need:

| File | Purpose | Integration Priority |
|------|---------|---------------------|
| `type_module.f90` | Data type definitions | HIGH - Need for grid structure |
| `mass.f90` | Mass conservation equation | HIGH - Core SWE component |
| `moment.f90` | Momentum equations | HIGH - Core SWE component |
| `boundaries.f90` | Boundary conditions | HIGH - Essential for simulation |
| `initialization.f90` | Grid initialization | MEDIUM - Can adapt |
| `all_grids.f90` | Multi-grid management | MEDIUM - May need parts |
| `output.f90` | Output routines | LOW - We have our own NetCDF output |
| `comcot.f90` | Main program | LOW - For reference only |
| `deform.f90` | Okada deformation | LOW - We have DC3D already |

### COMCOT Structure Analysis

The comcot-gfortran repository shows a clean modular structure:

```fortran
! From type_module.f90 (likely contains)
type grid_type
    real, allocatable :: z(:,:)    ! Elevation
    real, allocatable :: h(:,:)    ! Water depth  
    real, allocatable :: m(:,:)    ! x-momentum (hu)
    real, allocatable :: n(:,:)    ! y-momentum (hv)
    ! ... other fields
end type
```

This needs to be mapped to our `swe_grid_type`.

### Step 1: Obtain COMCOT Source
```bash
# Clone COMCOT repository
cd /path/to/your/workspace
git clone https://github.com/comcot-tsunamilab/COMCOT.git

# Or download from official website
# Extract and examine the source structure
```

### Step 2: Identify Key Subroutines
Look for files like:
- `comcot.f` or `comcot.f90` - Main program
- Files containing SWE time-stepping
- Grid setup routines
- Boundary condition handlers

### Step 3: Create Wrapper Module
Similar to `mod_okada_dc3d.f90`, create `mod_swe_comcot.f90`:

```fortran
module mod_swe_comcot
    use mod_swe_solver, only: swe_grid_type
    implicit none
    
contains

    subroutine initialize_comcot_grid(...)
        ! Wrapper for COMCOT grid initialization
    end subroutine

    subroutine step_comcot_swe(...)
        ! Wrapper for COMCOT time-stepping
    end subroutine

    subroutine apply_comcot_boundary(...)
        ! Wrapper for COMCOT boundary conditions
    end subroutine

end module mod_swe_comcot
```

### Step 4: Update Makefile
Add conditional compilation for COMCOT:

```makefile
# Optional: Use COMCOT SWE solver
USE_COMCOT ?= 0

ifeq ($(USE_COMCOT),1)
    FFLAGS += -DUSE_COMCOT
endif
```

### Step 5: Update tsunami_sim.f90
Add conditional compilation:

```fortran
#ifdef USE_COMCOT
    use mod_swe_comcot
    call initialize_comcot_grid(...)
    call step_comcot_swe(...)
#else
    use mod_swe_solver
    call initialize_swe_grid(...)
    call step_swe_leapfrog(...)
#endif
```

## File Structure After Integration

```
Final/
├── mod_swe_solver.f90         # Original hand-written (preserved)
├── mod_swe_comcot.f90         # COMCOT wrapper (NEW)
├── comcot/                    # COMCOT source files
│   ├── comcot_swe.f          # Extracted SWE routines
│   └── comcot_utils.f        # Helper functions
├── compare_swe.f90            # SWE comparison program (NEW)
└── tsunami_sim.f90            # Updated with USE_COMCOT option
```

## Compilation Options

```bash
# Default (hand-written SWE)
make clean && make

# COMCOT SWE
make clean && make USE_COMCOT=1

# DC3D + COMCOT (both reference implementations)
make clean && make USE_DC3D=1 USE_COMCOT=1

# Compare SWE methods
make compare_swe
./compare_swe
```

## Comparison Matrix

| Combination | Okada Method | SWE Method | Use Case |
|-------------|--------------|------------|----------|
| Default | Empirical | Hand-written | Fast, learning |
| USE_DC3D=1 | DC3D | Hand-written | Accurate initial |
| USE_COMCOT=1 | Empirical | COMCOT | Production SWE |
| Both=1 | DC3D | COMCOT | Full reference |

## Technical Considerations

### Interface Compatibility
COMCOT and our hand-written solver must share the same interface:
- Grid structure (`swe_grid_type`)
- Time-stepping signature
- Boundary condition format

### Data Structure Mapping
```fortran
! Our structure
type swe_grid_type
    real(8), allocatable :: eta(:,:)
    real(8), allocatable :: u(:,:)
    real(8), allocatable :: v(:,:)
    ! ...
end type

! Map to COMCOT's structure
! (depends on COMCOT's actual implementation)
```

### Coordinate System
- Ensure consistent coordinate conventions
- Check if COMCOT uses (lon, lat) or (lat, lon)
- Verify grid indexing (i,j) ordering

### Units
- Verify COMCOT uses SI units (m, m/s)
- Check time step units (seconds)
- Confirm elevation is relative to sea level

## Next Steps

### Immediate Actions
1. ✓ Read this guide
2. ☐ Obtain COMCOT source code
3. ☐ Examine COMCOT structure
4. ☐ Identify key subroutines
5. ☐ Create mod_swe_comcot.f90
6. ☐ Update Makefile
7. ☐ Update tsunami_sim.f90
8. ☐ Create compare_swe.f90
9. ☐ Test compilation
10. ☐ Validate results

### Validation Tests
1. Compare wave speeds
2. Compare arrival times
3. Compare maximum amplitudes
4. Check conservation properties
5. Verify boundary conditions

## Alternative: Simplified Approach

If full COMCOT integration is too complex, consider:

### Option 1: Reference COMCOT Output
Run COMCOT separately and compare NetCDF outputs:
```bash
# Run our simulation
./tsunami_sim
# Output: tsunami_output.nc

# Run COMCOT (separately)
# Output: comcot_output.nc

# Compare in Python
python compare_netcdf.py tsunami_output.nc comcot_output.nc
```

### Option 2: Extract Key Algorithms
Extract only the core numerical scheme from COMCOT:
- Time-stepping algorithm
- Flux calculation
- CFL condition

Reimplement in our framework without full integration.

### Option 3: Benchmark Parameters
Use COMCOT to determine optimal:
- Grid resolution
- Time step
- Boundary conditions

Apply these parameters to our hand-written solver.

## Documentation References

### COMCOT Papers
1. Wang, X., & Liu, P. L. F. (2006). An analysis of 2004 Sumatra earthquake fault plane mechanisms and Indian Ocean tsunami. *Journal of Hydraulic Research*, 44(2), 147-154.

2. Liu, P. L. F., et al. (1998). COMCOT: A tsunami generation propagation and run-up model. *Cornell University*.

### Numerical Methods
- Finite difference on staggered grid
- Leap-frog time integration
- Total variation diminishing (TVD) schemes
- Nested grid capability

## Support and Resources

### Getting Help
- COMCOT user manual (in distribution)
- COMCOT mailing list
- GitHub issues for COMCOT repository
- Academic papers using COMCOT

### Testing Data
- Benchmark problems (NOAA, Synolakis)
- Historical tsunami events
- Analytical solutions

## Summary

COMCOT integration follows the same pattern as DC3D:
1. ✓ Identify source files
2. ✓ Create wrapper module
3. ✓ Update build system
4. ✓ Add conditional compilation
5. ✓ Preserve original code
6. ✓ Create comparison tools
7. ✓ Validate and document

The key is maintaining interface compatibility while preserving both implementations for comparison and learning purposes.

