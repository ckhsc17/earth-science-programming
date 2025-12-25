# DC3D Okada Model Integration

## Overview
This document describes the integration of the DC3D (Okada 1985) finite fault implementation into the tsunami simulation project. The integration allows users to choose between two Okada Model implementations:

1. **Empirical approximation** (default): Fast point-source approximation suitable for far-field tsunami modeling
2. **DC3D finite fault** (optional): Complete integration over rectangular fault plane for higher accuracy

## Project Structure

```
Final/
├── mod_okada.f90              # Original empirical implementation (preserved)
├── mod_okada_dc3d.f90         # DC3D wrapper module (NEW)
├── DC3D.txt / DC3D.f          # Original Okada DC3D subroutines
├── tsunami_sim.f90            # Main program (updated with conditional compilation)
├── compare_okada.f90          # Comparison program (NEW)
├── Makefile                   # Updated with USE_DC3D option
└── fault_params.txt           # 2011 Tohoku earthquake parameters
```

## Compilation Options

### Default (Empirical Approximation)
```bash
make clean
make
```

### DC3D Finite Fault
```bash
make clean
make USE_DC3D=1
```

### Comparison Program
```bash
make clean
make compare_okada
./compare_okada
```

## Technical Details

### Conditional Compilation
The main program `tsunami_sim.f90` uses preprocessor directives to select the appropriate implementation:

```fortran
#ifdef USE_DC3D
    use mod_okada_dc3d
    call compute_initial_displacement_dc3d(...)
#else
    use mod_okada
    call compute_initial_displacement(...)
#endif
```

### Module Structure

#### `mod_okada.f90` (Original)
- **Method**: Empirical formula with distance-dependent decay
- **Characteristics**: 
  - Fast computation
  - ~10% of slip as maximum vertical displacement
  - Suitable for tsunami simulations (focus on far-field propagation)
- **Status**: Completely preserved, no modifications to core logic

#### `mod_okada_dc3d.f90` (New)
- **Method**: Complete DC3D implementation from Okada (1985)
- **Characteristics**:
  - Integrates over rectangular fault plane (AL1-AL2 × AW1-AW2)
  - Accurate near-field and far-field calculations
  - Uses original FORTRAN 77 DC3D subroutines
- **Interface**: Compatible with existing `fault_params_type`

#### `compare_okada.f90` (New)
- Computes displacement using both methods
- Generates comparison statistics:
  - Min, max, mean values for each method
  - Difference (DC3D - Empirical)
  - RMSE (Root Mean Square Error)
  - Relative differences

### Makefile Updates

#### Compile Flags
- `USE_DC3D=0` (default): `-DUSE_DC3D` not defined
- `USE_DC3D=1`: Adds `-DUSE_DC3D` preprocessor flag

#### Dependencies
- DC3D subroutines compiled separately as `dc3d.o` using `-std=legacy` for FORTRAN 77 compatibility
- Automatic conversion of `DC3D.txt` to `DC3D.f` when needed

## Usage Examples

### Run Default Simulation
```bash
make clean
make
./tsunami_sim
```

### Run with DC3D
```bash
make clean
make USE_DC3D=1
./tsunami_sim
```

### Compare Implementations
```bash
make compare_okada
./compare_okada
```

Expected output:
```
============================================================
COMPARISON REPORT
============================================================

Empirical Approximation:
  Min:   X.XXX m
  Max:   X.XXX m
  Mean:  X.XXX m

DC3D Finite Fault:
  Min:   X.XXX m
  Max:   X.XXX m
  Mean:  X.XXX m

Difference (DC3D - Empirical):
  Min:   X.XXX m
  Max:   X.XXX m
  Mean:  X.XXX m
  RMSE:  X.XXX m

Relative Difference:
  Max difference / Max empirical:  XX.XX %
  Mean difference / Mean empirical: XX.XX %
============================================================
```

## Theoretical Background

### Empirical Approximation (mod_okada)
- Based on empirical observations: vertical displacement ≈ 5-15% of slip for thrust faults
- Uses distance-dependent decay functions:
  - Near field (R < 0.5 × characteristic length): constant
  - Mid field (0.5-2×): linear decay
  - Far field (2-5×): exponential decay
- **Advantage**: Fast, numerically stable
- **Limitation**: Less accurate in near field

### DC3D Finite Fault (mod_okada_dc3d)
- Complete Okada (1985) formulation
- Integrates displacement field over rectangular fault using:
  - Real-source contribution
  - Image-source contribution (for free surface)
  - Four corner integration
- **Advantage**: Accurate for all distances
- **Limitation**: Slower computation

## References

1. Okada, Y. (1985). Surface deformation due to shear and tensile faults in a half-space. *Bulletin of the Seismological Society of America*, 75(4), 1135-1154.

2. COMCOT (Cornell Multi-grid Coupled Tsunami Model) - Reference for best practices

## Notes

- Both implementations are fully functional and tested
- The original hand-written code (`mod_okada.f90`) is completely preserved
- Users can switch between implementations at compile time
- The DC3D subroutines maintain their original FORTRAN 77 format for authenticity
- Both implementations use the same `fault_params.txt` input format

## Future Work

- Extend comparison to include COMCOT SWE solver integration
- Add runtime parameter file for method selection
- Create visualization comparing displacement fields
- Benchmark performance differences

