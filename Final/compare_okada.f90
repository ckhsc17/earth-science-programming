!===============================================================================
! Program: compare_okada.f90
! Purpose: Compare two Okada Model implementations
!          1. Empirical point-source approximation (mod_okada)
!          2. DC3D finite fault integration (mod_okada_dc3d)
!===============================================================================

program compare_okada
    use mod_netcdf_io
    use mod_okada
    use mod_okada_dc3d
    implicit none
    
    ! File names
    character(len=256) :: bathymetry_file, fault_params_file
    
    ! Grid data
    integer :: nx, ny, ierr
    real(8), allocatable :: bathymetry(:,:), lon(:), lat(:)
    real(8), allocatable :: disp_empirical(:,:), disp_dc3d(:,:)
    real(8), allocatable :: difference(:,:)
    
    ! Fault parameters
    type(fault_params_type) :: fault
    
    ! Statistics
    real(8) :: min_emp, max_emp, mean_emp
    real(8) :: min_dc3d, max_dc3d, mean_dc3d
    real(8) :: min_diff, max_diff, mean_diff, rmse
    integer :: i, j
    
    write(*,*) '============================================================'
    write(*,*) 'Okada Model Comparison: Empirical vs. DC3D'
    write(*,*) '============================================================'
    write(*,*)
    
    ! Set file names
    bathymetry_file = 'GEBCO_21_Dec_2025_d9303d544c3e/gebco_2025_n41.5_s34.0_w138.0_e146.0.nc'
    fault_params_file = 'fault_params.txt'
    
    ! Read bathymetry
    write(*,*) 'Reading bathymetry data...'
    call read_gebco_bathymetry(bathymetry_file, bathymetry, lon, lat, nx, ny, ierr)
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'ERROR: Failed to read bathymetry'
        stop 1
    end if
    write(*,*) '  Grid size: ', nx, ' x ', ny
    write(*,*)
    
    ! Read fault parameters
    write(*,*) 'Reading fault parameters...'
    call read_fault_params(fault_params_file, fault, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to read fault parameters'
        stop 1
    end if
    write(*,*)
    
    ! Compute initial displacement using empirical approximation
    write(*,*) 'Computing displacement (empirical approximation)...'
    allocate(disp_empirical(nx, ny))
    call compute_initial_displacement(lon, lat, nx, ny, fault, disp_empirical, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to compute empirical displacement'
        stop 1
    end if
    write(*,*)
    
    ! Compute initial displacement using DC3D
    write(*,*) 'Computing displacement (DC3D finite fault)...'
    allocate(disp_dc3d(nx, ny))
    call compute_initial_displacement_dc3d(lon, lat, nx, ny, fault, disp_dc3d, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to compute DC3D displacement'
        stop 1
    end if
    write(*,*)
    
    ! Compute difference
    allocate(difference(nx, ny))
    difference = disp_dc3d - disp_empirical
    
    ! Calculate statistics
    min_emp = minval(disp_empirical)
    max_emp = maxval(disp_empirical)
    mean_emp = sum(disp_empirical) / real(nx * ny, kind=8)
    
    min_dc3d = minval(disp_dc3d)
    max_dc3d = maxval(disp_dc3d)
    mean_dc3d = sum(disp_dc3d) / real(nx * ny, kind=8)
    
    min_diff = minval(difference)
    max_diff = maxval(difference)
    mean_diff = sum(difference) / real(nx * ny, kind=8)
    
    ! RMSE
    rmse = 0.0d0
    do j = 1, ny
        do i = 1, nx
            rmse = rmse + difference(i, j)**2
        end do
    end do
    rmse = sqrt(rmse / real(nx * ny, kind=8))
    
    ! Print comparison report
    write(*,*) '============================================================'
    write(*,*) 'COMPARISON REPORT'
    write(*,*) '============================================================'
    write(*,*)
    write(*,*) 'Empirical Approximation:'
    write(*,*) '  Min:  ', min_emp, ' m'
    write(*,*) '  Max:  ', max_emp, ' m'
    write(*,*) '  Mean: ', mean_emp, ' m'
    write(*,*)
    write(*,*) 'DC3D Finite Fault:'
    write(*,*) '  Min:  ', min_dc3d, ' m'
    write(*,*) '  Max:  ', max_dc3d, ' m'
    write(*,*) '  Mean: ', mean_dc3d, ' m'
    write(*,*)
    write(*,*) 'Difference (DC3D - Empirical):'
    write(*,*) '  Min:  ', min_diff, ' m'
    write(*,*) '  Max:  ', max_diff, ' m'
    write(*,*) '  Mean: ', mean_diff, ' m'
    write(*,*) '  RMSE: ', rmse, ' m'
    write(*,*)
    write(*,*) 'Relative Difference:'
    if (abs(max_emp) > 1.0d-10) then
        write(*,*) '  Max difference / Max empirical: ', &
                    100.0d0 * max_diff / max_emp, ' %'
    end if
    if (abs(mean_emp) > 1.0d-10) then
        write(*,*) '  Mean difference / Mean empirical: ', &
                    100.0d0 * mean_diff / mean_emp, ' %'
    end if
    write(*,*)
    write(*,*) '============================================================'
    
    ! Clean up
    deallocate(bathymetry, lon, lat)
    deallocate(disp_empirical, disp_dc3d, difference)
    
    write(*,*) 'Comparison complete!'
    
end program compare_okada

