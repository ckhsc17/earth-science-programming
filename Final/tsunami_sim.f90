!===============================================================================
! Program: tsunami_sim.f90
! Purpose: Main program for tsunami simulation
!          Integrates Okada Model, SWE solver, and I/O modules
!===============================================================================

program tsunami_sim
    use mod_netcdf_io
    use mod_okada
    use mod_swe_solver
    use mod_netcdf_output
    implicit none
    
    ! File names
    character(len=256) :: bathymetry_file, fault_params_file, output_file
    
    ! Grid data
    integer :: nx, ny, ierr
    real(8), allocatable :: bathymetry(:,:), lon(:), lat(:)
    real(8), allocatable :: initial_displacement(:,:)
    
    ! Fault parameters
    type(fault_params_type) :: fault
    
    ! SWE grid
    type(swe_grid_type) :: grid
    
    ! Simulation parameters
    real(8) :: dt, t, t_end, t_output
    real(8) :: output_interval
    integer :: n_steps, output_step, step
    integer :: output_ncid, time_index
    
    write(*,*) '============================================================'
    write(*,*) 'Tsunami Simulation: 2011 Tohoku Earthquake'
    write(*,*) '============================================================'
    write(*,*)
    
    ! Set file names
    bathymetry_file = 'GEBCO_21_Dec_2025_d9303d544c3e/gebco_2025_n41.5_s34.0_w138.0_e146.0.nc'
    fault_params_file = 'fault_params.txt'
    output_file = 'tsunami_output.nc'
    
    ! Step 1: Read bathymetry
    write(*,*) 'Step 1: Reading bathymetry data...'
    call read_gebco_bathymetry(bathymetry_file, bathymetry, lon, lat, nx, ny, ierr)
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'ERROR: Failed to read bathymetry'
        stop 1
    end if
    write(*,*) '  Success: Grid size = ', nx, ' x ', ny
    write(*,*)
    
    ! Step 2: Read fault parameters
    write(*,*) 'Step 2: Reading fault parameters...'
    call read_fault_params(fault_params_file, fault, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to read fault parameters'
        stop 1
    end if
    write(*,*)
    
    ! Step 3: Compute initial displacement
    write(*,*) 'Step 3: Computing initial seafloor displacement...'
    allocate(initial_displacement(nx, ny))
    call compute_initial_displacement(lon, lat, nx, ny, fault, initial_displacement, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to compute initial displacement'
        stop 1
    end if
    write(*,*)
    
    ! Step 4: Initialize SWE grid
    write(*,*) 'Step 4: Initializing SWE grid...'
    call initialize_swe_grid(bathymetry, initial_displacement, lon, lat, &
                            nx, ny, grid, ierr)
    if (ierr /= 0) then
        write(*,*) 'ERROR: Failed to initialize SWE grid'
        stop 1
    end if
    write(*,*)
    
    ! Step 5: Set simulation parameters
    write(*,*) 'Step 5: Setting simulation parameters...'
    t = 0.0d0
    t_end = 3600.0d0  ! 1 hour simulation
    output_interval = 300.0d0  ! Output every 5 minutes
    
    ! Calculate time step
    call compute_time_step(grid, dt)
    write(*,*) '  Time step: ', dt, ' seconds'
    write(*,*) '  Simulation duration: ', t_end, ' seconds (', t_end/3600.0d0, ' hours)'
    write(*,*) '  Output interval: ', output_interval, ' seconds'
    
    n_steps = int(t_end / dt) + 1
    output_step = int(output_interval / dt)
    if (output_step < 1) output_step = 1
    
    write(*,*) '  Total steps: ', n_steps
    write(*,*) '  Output every ', output_step, ' steps'
    write(*,*)
    
    ! Step 6: Initialize leap-frog scheme (needs two time steps)
    write(*,*) 'Step 6: Initializing leap-frog scheme...'
    ! Leap-frog requires eta at t=-dt and t=0
    ! Use forward Euler for first step to get eta at t=dt
    block
        real(8), allocatable :: eta_old(:,:), u_old(:,:), v_old(:,:)
        
        ! Store initial condition
        allocate(eta_old(nx, ny))
        allocate(u_old(nx+1, ny))
        allocate(v_old(nx, ny+1))
        
        eta_old = grid%eta
        u_old = grid%u
        v_old = grid%v
        
        ! Take one forward Euler step to initialize leap-frog
        ! This gives us eta at t=dt, so leap-frog can proceed from t=0
        call step_swe_leapfrog(grid, dt)
        
        ! Now we have eta at t=0 (from initial condition) and t=dt (from first step)
        ! Leap-frog can now proceed normally
        write(*,*) '  Leap-frog initialized (first step completed)'
        
        deallocate(eta_old, u_old, v_old)
    end block
    t = dt  ! Update time to dt after first step
    write(*,*)
    
    ! Step 7: Create output NetCDF file
    write(*,*) 'Step 7: Creating output NetCDF file...'
    call create_output_netcdf(output_file, lon, lat, nx, ny, output_ncid, ierr)
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'ERROR: Failed to create output file'
        stop 1
    end if
    write(*,*) '  Output file: ', trim(output_file)
    write(*,*)
    
    ! Step 8: Write initial condition (t=0)
    time_index = 1
    ! Restore initial condition for output (we already advanced one step)
    block
        real(8), allocatable :: eta_init(:,:)
        allocate(eta_init(nx, ny))
        eta_init = initial_displacement  ! Use original initial displacement
        call write_time_snapshot(output_ncid, eta_init, 0.0d0, time_index, ierr)
        deallocate(eta_init)
    end block
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'WARNING: Failed to write initial condition'
    end if
    write(*,*) '  Written initial condition (t = 0.0 s)'
    time_index = time_index + 1
    
    ! Step 9: Run simulation
    write(*,*) 'Step 9: Running simulation...'
    write(*,*) '============================================================'
    
    t_output = output_interval
    
    do step = 1, n_steps
        ! Advance one time step
        call step_swe_leapfrog(grid, dt)
        t = t + dt
        
        ! Output if needed
        if (t >= t_output .or. step == n_steps) then
            call write_time_snapshot(output_ncid, grid%eta, t, time_index, ierr)
            if (ierr == NC_SUCCESS) then
                write(*,'(A,F10.1,A,F8.2,A,F10.2,A,F10.2)') &
                    '  t = ', t, ' s (', t/60.0d0, ' min) | ', &
                    minval(grid%eta), ' <= eta <= ', maxval(grid%eta), ' m'
            end if
            time_index = time_index + 1
            t_output = t_output + output_interval
        end if
        
        ! Progress indicator
        if (mod(step, 100) == 0) then
            write(*,'(A,I6,A,F6.1,A)') '  Progress: step ', step, ' / ', &
                real(step)/real(n_steps)*100.0d0, '%'
        end if
    end do
    
    write(*,*) '============================================================'
    write(*,*) 'Simulation completed!'
    write(*,*)
    
    ! Step 10: Close output file
    call close_output_netcdf(output_ncid, ierr)
    if (ierr == NC_SUCCESS) then
        write(*,*) 'Output file closed successfully'
    end if
    
    ! Cleanup
    deallocate(bathymetry, lon, lat, initial_displacement)
    deallocate(grid%h, grid%eta, grid%bath, grid%u, grid%v, grid%h_u, grid%h_v)
    
    write(*,*) '============================================================'
    write(*,*) 'All done!'
    write(*,*) '============================================================'
    
end program tsunami_sim

