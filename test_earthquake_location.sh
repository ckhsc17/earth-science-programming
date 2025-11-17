#!/bin/bash

# Test script for 4D earthquake location program
echo "Testing 4D Earthquake Location Program"
echo "======================================"

# Check if required files exist
if [ ! -f "ppfile.txt" ]; then
    echo "Error: ppfile.txt not found"
    exit 1
fi

if [ ! -f "nsta.dat" ]; then
    echo "Error: nsta.dat not found"
    exit 1
fi

if [ ! -f "earthquake_location_4d" ]; then
    echo "Error: earthquake_location_4d executable not found"
    echo "Please compile first: gfortran -o earthquake_location_4d earthquake_location_4d.f90"
    exit 1
fi

echo "All required files found."
echo ""
echo "Running earthquake location program..."
echo "You will need to provide initial estimates for:"
echo "  X (km) - Easting coordinate"
echo "  Y (km) - Northing coordinate" 
echo "  Z (km) - Depth (>= 0)"
echo "  T (s)  - Origin time"
echo ""
echo "Suggested initial values based on Taiwan region:"
echo "  X: 0 (km)"
echo "  Y: 0 (km)"
echo "  Z: 10 (km)"
echo "  T: 0 (seconds)"
echo ""

./earthquake_location_4d
