!===============================================================================
! Program: test_netcdf_io.f90
! Purpose: Test program for NetCDF I/O module
!===============================================================================

program test_netcdf_io
    use mod_netcdf_io
    implicit none
    
    character(len=256) :: filename
    integer :: nx, ny, ierr
    real(8) :: lon_min, lon_max, lat_min, lat_max
    real(8), allocatable :: elevation(:,:), lon(:), lat(:)
    real(8) :: elev_min, elev_max, elev_mean
    integer :: i, j
    
    ! Set filename
    filename = 'GEBCO_21_Dec_2025_d9303d544c3e/gebco_2025_n41.5_s34.0_w138.0_e146.0.nc'
    
    write(*,*) '============================================================'
    write(*,*) 'NetCDF I/O Module Test'
    write(*,*) '============================================================'
    write(*,*)
    
    ! Test 1: Get file information
    write(*,*) 'Test 1: Getting file information...'
    call get_netcdf_info(filename, nx, ny, lon_min, lon_max, lat_min, lat_max, ierr)
    
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'ERROR: Failed to get file information'
        stop 1
    end if
    
    write(*,*) '  Success!'
    write(*,*) '  Grid size: ', nx, ' x ', ny
    write(*,*) '  Longitude range: ', lon_min, ' to ', lon_max, ' degrees'
    write(*,*) '  Latitude range: ', lat_min, ' to ', lat_max, ' degrees'
    write(*,*)
    
    ! Test 2: Read bathymetry data
    write(*,*) 'Test 2: Reading bathymetry data...'
    ! Ensure arrays are deallocated before call
    if (allocated(elevation)) deallocate(elevation)
    if (allocated(lon)) deallocate(lon)
    if (allocated(lat)) deallocate(lat)
    call read_gebco_bathymetry(filename, elevation, lon, lat, nx, ny, ierr)
    
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'ERROR: Failed to read bathymetry data'
        stop 1
    end if
    
    write(*,*) '  Success!'
    ! Try to access the array directly - if it fails, we'll get a runtime error
    ! which will tell us more about what's happening
    write(*,*) '  Elevation array shape: ', size(elevation, 1), ' x ', size(elevation, 2)
    write(*,*)
    
    ! Test 3: Calculate statistics
    write(*,*) 'Test 3: Calculating elevation statistics...'
    elev_min = minval(elevation)
    elev_max = maxval(elevation)
    elev_mean = sum(elevation) / real(size(elevation), kind=8)
    
    write(*,*) '  Elevation statistics:'
    write(*,*) '    Minimum: ', elev_min, ' m'
    write(*,*) '    Maximum: ', elev_max, ' m'
    write(*,*) '    Mean: ', elev_mean, ' m'
    write(*,*)
    
    ! Test 4: Verify coordinate arrays
    write(*,*) 'Test 4: Verifying coordinate arrays...'
    write(*,*) '  Longitude array:'
    write(*,*) '    First value: ', lon(1), ' degrees'
    write(*,*) '    Last value: ', lon(nx), ' degrees'
    write(*,*) '    Size: ', size(lon)
    write(*,*) '  Latitude array:'
    write(*,*) '    First value: ', lat(1), ' degrees'
    write(*,*) '    Last value: ', lat(ny), ' degrees'
    write(*,*) '    Size: ', size(lat)
    write(*,*)
    
    ! Test 5: Sample data points
    write(*,*) 'Test 5: Sample data points (center region):'
    i = nx / 2
    j = ny / 2
    write(*,*) '  Center point (i=', i, ', j=', j, '):'
    write(*,*) '    Longitude: ', lon(i), ' degrees'
    write(*,*) '    Latitude: ', lat(j), ' degrees'
    write(*,*) '    Elevation: ', elevation(i, j), ' m'
    write(*,*)
    
    ! Test 6: Check for land/sea
    write(*,*) 'Test 6: Land/Sea distribution:'
    write(*,*) '  Points below sea level (elevation < 0): ', count(elevation < 0.0d0)
    write(*,*) '  Points at/above sea level (elevation >= 0): ', count(elevation >= 0.0d0)
    write(*,*)
    
    write(*,*) '============================================================'
    write(*,*) 'All tests completed successfully!'
    write(*,*) '============================================================'
    
end program test_netcdf_io


