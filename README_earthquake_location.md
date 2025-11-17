# 4D Earthquake Location Program

## Overview
This Fortran program implements a 4-dimensional earthquake location algorithm using the Gauss-Newton iterative method. It locates earthquakes in space (X, Y, Z) and time (T) using observed P-wave travel times from seismic stations.

## Features
- **4D Location**: Solves for earthquake coordinates (X, Y, Z) and origin time (T)
- **Half-space Model**: Constrains earthquake depth to be >= 0 km
- **Weighted Residuals**: Uses 1/r² weighting to reduce influence of outliers
- **Robust Matrix Operations**: Includes LU decomposition for stable matrix inversion
- **Coordinate Conversion**: Converts latitude/longitude to km using the `delaz` subroutine

## Input Files

### 1. `ppfile.txt`
Contains observed P-wave arrival times for each station:
- Header line: Year, month, day, hour, minute, second of earthquake origin
- Data lines: Station code, epicentral distance, arrival time (minutes, seconds)

Format example:
```
 2001 9 32240  9.232350.56121 3.35 27.303.0030 16.5 80 .31  .6  .6 F 56A
 SML   16.5 285 142- 39 75.13  .18  .00 79.37  .23 2.00  .00  .00 3.28  .00
```

### 2. `nsta.dat`
Contains static station information (coordinates, elevation):
- Station code (4 characters)
- Latitude: degrees, minutes
- Longitude: degrees, minutes  
- Elevation (meters)

Format: `read(2,'(A4,I2,F5.2,1x,I3,F5.2,1x,F6.1)')`

Example:
```
TAP 2502.35N12131.35E   5.5 001057030.013   0622 0597 0000 -1-1-1 000000 992431
```

## Algorithm Details

### Theoretical Model
- **Velocity Model**: Constant P-wave velocity of 6.5 km/s
- **Travel Time**: T = R/V, where R is distance and V is velocity
- **Distance**: R = √[(X-Xi)² + (Y-Yi)² + (Z-Zi)²]

### Gauss-Newton Method
The program solves the nonlinear system:
```
t_obs = t_calc + t_origin
```

Where:
- `t_obs`: Observed travel time
- `t_calc`: Theoretical travel time = distance/velocity
- `t_origin`: Earthquake origin time

### Jacobian Matrix
The 4×n Jacobian matrix G contains partial derivatives:
```
G[i,1] = ∂t/∂X = (X-Xi)/(V×R)
G[i,2] = ∂t/∂Y = (Y-Yi)/(V×R)  
G[i,3] = ∂t/∂Z = (Z-Zi)/(V×R)
G[i,4] = ∂t/∂T = -1
```

### Weighting Scheme
Residuals are weighted by 1/r² to reduce the influence of distant stations and potential outliers.

## Usage

### Compilation
```bash
gfortran -o earthquake_location_4d earthquake_location_4d.f90
```

### Execution
```bash
./earthquake_location_4d
```

### Input Requirements
The program will prompt for initial estimates:
1. **X (km)**: Easting coordinate
2. **Y (km)**: Northing coordinate  
3. **Z (km)**: Depth (must be ≥ 0)
4. **T (seconds)**: Origin time

### Suggested Initial Values
For Taiwan region earthquakes:
- X: 0 km (relative to 121°E)
- Y: 0 km (relative to 24°N)
- Z: 10 km (typical crustal earthquake depth)
- T: 0 seconds (relative to header time)

### Example Results
With the provided test data, the program successfully converges to:
```
Final earthquake location:
  X (Easting)  =        4.291 km
  Y (Northing) =      -16.746 km
  Z (Depth)    =       24.496 km
  T (Origin)   =        1.530 seconds

Geographic coordinates:
  Latitude  =  23.848797°
  Longitude = 121.042180°

  Latitude  = 23° 50' 55.669" N
  Longitude = 121°  2' 31.848" E

RMS residual =    0.470 seconds
```

This represents an earthquake located:
- **Geographic**: 23.8488°N, 121.0422°E (23°50'55.7"N, 121°2'31.8"E)
- **Cartesian**: 4.3 km east, 16.7 km south of reference point (24°N, 121°E)
- **Depth**: 24.5 km
- **Quality**: Good fit (RMS = 0.47 seconds)

## Output

### Convergence Information
- Iteration progress with convergence metrics
- Final parameter values
- Convergence status

### Location Results
```
Final earthquake location:
  X (Easting)  =    XX.XXX km
  Y (Northing) =    XX.XXX km  
  Z (Depth)    =    XX.XXX km
  T (Origin)   =    XX.XXX seconds
```

### Residual Analysis
- Station-by-station residuals
- RMS residual for quality assessment
- Distance and travel time comparisons

## Coordinate System

### Reference Point
- Latitude: 24°N (approximate Taiwan center)
- Longitude: 121°E (approximate Taiwan center)

### Conversion
The `delaz` subroutine converts geographic coordinates to km:
- **X (Easting)**: Positive eastward from 121°E
- **Y (Northing)**: Positive northward from 24°N
- **Z (Depth)**: Positive downward from sea level

## Error Handling
- Checks for singular matrices
- Validates input file formats
- Enforces half-space constraint (Z ≥ 0)
- Handles missing station data gracefully

## Limitations
- Assumes constant velocity (6.5 km/s)
- Uses simple half-space Earth model
- Requires at least 4 stations for 4D location
- Local coordinate system (suitable for Taiwan region)

## Files Generated
- `earthquake_location_4d`: Compiled executable
- `test_earthquake_location.sh`: Test script

## Testing
Use the provided test script:
```bash
./test_earthquake_location.sh
```

This will check for required files and run the location program with guidance on initial parameter selection.
