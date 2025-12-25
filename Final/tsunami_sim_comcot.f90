!===============================================================================
! Program: tsunami_sim_comcot.f90
! Purpose: Tsunami simulation using COMCOT SWE solver
! 
! This version uses COMCOT's numerical methods instead of hand-written solver
! For comparison with the original tsunami_sim.f90
!===============================================================================

program tsunami_sim_comcot
    use mod_netcdf_io
    use mod_okada
#ifdef USE_DC3D
    use mod_okada_dc3d
#endif
    use mod_swe_comcot  ! Use COMCOT solver instead of mod_swe_solver
    use mod_netcdf_output
    implicit none

    ! File paths
    character(len=256) :: gebco_file = 'GEBCO_21_Dec_2025_d9303d544c3e/gebco_2025_n41.5_s34.0_w138.0_e146.0.nc'
    character(len=256) :: fault_file = 'fault_params.txt'
    character(len=256) :: output_file = 'tsunami_output_comcot.nc'

    ! NetCDF variables
    integer :: nlat, nlon, ncid_out
    real(8), allocatable :: lat(:), lon(:)
    real(8), allocatable :: elevation(:,:)  ! (lon, lat) order

    ! Simulation variables
    type(swe_grid_type) :: grid
    type(fault_params_type) :: fault
    real(8), allocatable :: initial_displacement(:,:)
    real(8) :: dt, t, t_end, t_output
    integer :: ierr, step, output_step

    ! Timing
    real :: start_cpu, end_cpu

    ! Constants
    real(8), parameter :: output_interval = 300.0d0  ! Output every 5 minutes
    t_end = 10800.0d0  ! 3 hours simulation

    call cpu_time(start_cpu)

    write(*,*) '=========================================='
    write(*,*) 'Tsunami Simulation (COMCOT Solver)'
    write(*,*) '=========================================='
    write(*,*)

    !---------------------------------------------------------------------------
    ! STEP 1: Read GEBCO bathymetry data
    !---------------------------------------------------------------------------
    write(*,*) 'Step 1: Reading GEBCO bathymetry...'
    call read_gebco_bathymetry(gebco_file, elevation, lon, lat, nlon, nlat, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to read bathymetry'
        stop 1
    end if

    write(*,'(A,I5,A,I5)') '  Grid size: ', nlon, ' x ', nlat
    write(*,'(A,F8.3,A,F8.3)') '  Lon range: ', lon(1), ' to ', lon(nlon)
    write(*,'(A,F8.3,A,F8.3)') '  Lat range: ', lat(1), ' to ', lat(nlat)
    write(*,*)

    !---------------------------------------------------------------------------
    ! STEP 2: Read fault parameters and compute initial displacement
    !---------------------------------------------------------------------------
    write(*,*) 'Step 2: Computing initial seafloor displacement...'
    call read_fault_params(fault_file, fault, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to read fault parameters'
        stop 1
    end if

    write(*,*) '  Fault parameters:'
    write(*,'(A,F8.2,A)') '    Strike: ', fault%strike, ' deg'
    write(*,'(A,F8.2,A)') '    Dip: ', fault%dip, ' deg'
    write(*,'(A,F8.2,A)') '    Rake: ', fault%rake, ' deg'
    write(*,'(A,F8.2,A)') '    Slip: ', fault%slip, ' m'
    write(*,'(A,F8.2,A)') '    Depth: ', fault%depth, ' km'
    write(*,'(A,F8.2,A,F8.2,A)') '    Dimensions: ', fault%length, ' x ', fault%width, ' km'
    write(*,*)

    ! Compute initial displacement
    allocate(initial_displacement(nlon, nlat))
    
#ifdef USE_DC3D
    write(*,*) '  Using DC3D (finite fault) Okada Model...'
    call compute_initial_displacement_dc3d(lon, lat, nlon, nlat, fault, initial_displacement, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to compute DC3D displacement'
        stop 1
    end if
#else
    write(*,*) '  Using empirical Okada Model...'
    block
        integer :: i, j
        do j = 1, nlat
            do i = 1, nlon
                initial_displacement(i,j) = okada_vertical_displacement(lon(i), lat(j), fault)
            end do
        end do
    end block
#endif

    write(*,'(A,F10.6,A)') '  Max displacement: ', maxval(initial_displacement), ' m'
    write(*,'(A,F10.6,A)') '  Min displacement: ', minval(initial_displacement), ' m'
    write(*,*)

    !---------------------------------------------------------------------------
    ! STEP 3: Initialize COMCOT SWE grid
    !---------------------------------------------------------------------------
    write(*,*) 'Step 3: Initializing COMCOT SWE grid...'
    call initialize_comcot_grid(elevation, initial_displacement, lon, lat, &
                                nlon, nlat, grid, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to initialize grid'
        stop 1
    end if
    write(*,*)

    !---------------------------------------------------------------------------
    ! STEP 4: Create output NetCDF file
    !---------------------------------------------------------------------------
    write(*,*) 'Step 4: Creating output file...'
    call create_output_netcdf(output_file, lon, lat, nlon, nlat, ncid_out, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to create output file'
        stop 1
    end if
    write(*,*)

    !---------------------------------------------------------------------------
    ! STEP 5: Time-stepping loop (COMCOT solver)
    !---------------------------------------------------------------------------
    write(*,*) 'Step 5: Running simulation with COMCOT solver...'
    write(*,'(A,F10.2,A)') '  Total time: ', t_end, ' seconds'
    write(*,'(A,F10.2,A)') '  Output interval: ', output_interval, ' seconds'
    write(*,*)

    t = 0.0d0
    step = 0
    output_step = 0
    t_output = 0.0d0

    ! Write initial condition
    call write_time_snapshot(ncid_out, grid%eta, t, output_step, ierr)
    output_step = output_step + 1

    ! Main time loop
    do while (t < t_end)
        ! Compute time step (CFL condition)
        call compute_comcot_timestep(grid, dt)
        
        ! Don't overshoot end time
        if (t + dt > t_end) dt = t_end - t
        
        ! Advance one time step using COMCOT method
        call step_comcot_swe(grid, dt)
        
        t = t + dt
        step = step + 1
        
        ! Output at specified intervals
        if (t >= t_output + output_interval .or. abs(t - t_end) < 1.0d-6) then
            call write_time_snapshot(ncid_out, grid%eta, t, output_step, ierr)
            output_step = output_step + 1
            t_output = t
            
            write(*,'(A,F10.2,A,F8.2,A,I8,A,F10.6,A,F10.6,A)') &
                '  t = ', t, ' s (', t/60.0d0, ' min), step ', step, &
                ', eta: [', minval(grid%eta), ', ', maxval(grid%eta), '] m'
        end if
    end do

    write(*,*)
    write(*,'(A,I8,A)') '  Total steps: ', step
    write(*,'(A,I8,A)') '  Output snapshots: ', output_step
    write(*,*)

    !---------------------------------------------------------------------------
    ! STEP 6: Close output file
    !---------------------------------------------------------------------------
    call close_output_netcdf(ncid_out, ierr)

    call cpu_time(end_cpu)
    write(*,*) '=========================================='
    write(*,*) 'Simulation completed successfully!'
    write(*,'(A,F10.2,A)') 'CPU time: ', end_cpu - start_cpu, ' seconds'
    write(*,'(A,A)') 'Output file: ', trim(output_file)
    write(*,*) '=========================================='

    ! Clean up
    deallocate(lat, lon, elevation, initial_displacement)

end program tsunami_sim_comcot

