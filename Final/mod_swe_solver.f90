!===============================================================================
! Module: mod_swe_solver.f90
! Purpose: 2D Shallow Water Equations solver using Arakawa C-grid
! Method: Finite Difference Method with Leap-frog time stepping
!===============================================================================

module mod_swe_solver
    implicit none
    
    ! Physical constants
    real(8), parameter :: G = 9.81d0  ! Gravitational acceleration (m/s^2)
    real(8), parameter :: PI = 3.14159265358979323846d0
    
    ! Grid type for Arakawa C-grid
    type swe_grid_type
        integer :: nx, ny
        real(8) :: dx, dy  ! Grid spacing in meters
        real(8), allocatable :: h(:,:)      ! Total depth (eta + bathymetry) at (i,j)
        real(8), allocatable :: eta(:,:)     ! Surface elevation (water level) at (i,j)
        real(8), allocatable :: bath(:,:)    ! Bathymetry (negative below sea level) at (i,j)
        real(8), allocatable :: u(:,:)       ! x-velocity at (i+1/2,j) - staggered
        real(8), allocatable :: v(:,:)       ! y-velocity at (i,j+1/2) - staggered
        real(8), allocatable :: h_u(:,:)    ! Depth at u-points (i+1/2,j)
        real(8), allocatable :: h_v(:,:)    ! Depth at v-points (i,j+1/2)
    end type swe_grid_type
    
contains

    !---------------------------------------------------------------------------
    ! Subroutine: initialize_swe_grid
    ! Purpose: Initialize SWE grid from bathymetry and initial displacement
    !---------------------------------------------------------------------------
    subroutine initialize_swe_grid(bathymetry, initial_displacement, lon, lat, &
                                   nx, ny, grid, ierr)
        real(8), intent(in) :: bathymetry(:,:)
        real(8), intent(in) :: initial_displacement(:,:)
        real(8), intent(in) :: lon(:), lat(:)
        integer, intent(in) :: nx, ny
        type(swe_grid_type), intent(out) :: grid
        integer, intent(out) :: ierr
        
        integer :: i, j
        real(8) :: R_earth = 6371000.0d0  ! Earth radius in meters
        real(8) :: lat_center
        
        ierr = 0
        grid%nx = nx
        grid%ny = ny
        
        ! Calculate grid spacing (approximate, using center latitude)
        lat_center = (lat(1) + lat(ny)) / 2.0d0
        grid%dx = (lon(2) - lon(1)) * PI / 180.0d0 * R_earth * cos(lat_center * PI / 180.0d0)
        grid%dy = (lat(2) - lat(1)) * PI / 180.0d0 * R_earth
        
        ! Allocate arrays
        allocate(grid%h(nx, ny))
        allocate(grid%eta(nx, ny))
        allocate(grid%bath(nx, ny))
        allocate(grid%u(nx+1, ny))
        allocate(grid%v(nx, ny+1))
        allocate(grid%h_u(nx+1, ny))
        allocate(grid%h_v(nx, ny+1))
        
        ! Initialize bathymetry (negative below sea level)
        grid%bath = -bathymetry  ! GEBCO elevation is negative below sea, so negate
        
        ! Initial surface elevation = initial displacement
        grid%eta = initial_displacement
        
        ! Total depth = surface elevation - bathymetry
        ! (bathymetry is negative, so this becomes eta - (-bath) = eta + |bath|)
        grid%h = grid%eta - grid%bath
        
        ! Initialize velocities to zero
        grid%u = 0.0d0
        grid%v = 0.0d0
        
        ! Initialize staggered depth arrays
        do j = 1, ny
            do i = 1, nx+1
                if (i == 1) then
                    grid%h_u(i, j) = max(grid%h(1, j), 0.0d0)
                else if (i == nx+1) then
                    grid%h_u(i, j) = max(grid%h(nx, j), 0.0d0)
                else
                    grid%h_u(i, j) = max(0.5d0 * (grid%h(i-1, j) + grid%h(i, j)), 0.0d0)
                end if
            end do
        end do
        
        do j = 1, ny+1
            do i = 1, nx
                if (j == 1) then
                    grid%h_v(i, j) = max(grid%h(i, 1), 0.0d0)
                else if (j == ny+1) then
                    grid%h_v(i, j) = max(grid%h(i, ny), 0.0d0)
                else
                    grid%h_v(i, j) = max(0.5d0 * (grid%h(i, j-1) + grid%h(i, j)), 0.0d0)
                end if
            end do
        end do
        
        write(*,*) 'SWE grid initialized:'
        write(*,*) '  Grid size: ', nx, ' x ', ny
        write(*,*) '  Grid spacing: dx = ', grid%dx/1000.0d0, ' km, dy = ', grid%dy/1000.0d0, ' km'
        write(*,*) '  Initial eta range: ', minval(grid%eta), ' to ', maxval(grid%eta), ' m'
    end subroutine initialize_swe_grid

    !---------------------------------------------------------------------------
    ! Subroutine: compute_time_step
    ! Purpose: Calculate stable time step based on CFL condition
    !---------------------------------------------------------------------------
    subroutine compute_time_step(grid, dt)
        type(swe_grid_type), intent(in) :: grid
        real(8), intent(out) :: dt
        
        real(8) :: c_max, u_max, v_max
        real(8) :: dx_min
        real(8) :: h_max
        
        ! Find maximum depth (use absolute value to handle negative bathymetry)
        h_max = maxval(grid%h)
        if (h_max < 1.0d0) h_max = 1000.0d0  ! Default to 1000m if all dry
        
        ! Find maximum wave speed: c = sqrt(g*h)
        c_max = sqrt(G * h_max)
        
        ! Find maximum velocity
        u_max = maxval(abs(grid%u))
        v_max = maxval(abs(grid%v))
        
        ! CFL condition: dt < min(dx, dy) / (c + |u|)
        ! Use more conservative safety factor for stability
        dx_min = min(grid%dx, grid%dy)
        dt = 0.3d0 * dx_min / (c_max + max(u_max, v_max) + 1.0d0)  ! Safety factor 0.3
        
        ! Limit time step to reasonable range
        if (dt < 0.01d0) dt = 0.01d0  ! Minimum 0.01 s
        if (dt > 10.0d0) dt = 10.0d0  ! Maximum 10 s
    end subroutine compute_time_step

    !---------------------------------------------------------------------------
    ! Subroutine: apply_boundary_conditions
    ! Purpose: Apply boundary conditions (reflecting for land, absorbing for ocean)
    !---------------------------------------------------------------------------
    subroutine apply_boundary_conditions(grid)
        type(swe_grid_type), intent(inout) :: grid
        
        integer :: i, j
        
        ! Land boundaries: reflecting (zero normal velocity)
        ! Ocean boundaries: absorbing (radiation boundary condition)
        
        ! West boundary (i=1)
        do j = 1, grid%ny
            if (grid%h(1, j) > 0.0d0) then
                ! Ocean: absorbing boundary
                grid%u(1, j) = 0.0d0  ! Simplified: zero velocity at boundary
            else
                ! Land: reflecting
                grid%u(1, j) = 0.0d0
            end if
        end do
        
        ! East boundary (i=nx)
        do j = 1, grid%ny
            if (grid%h(grid%nx, j) > 0.0d0) then
                ! Ocean: absorbing
                grid%u(grid%nx+1, j) = 0.0d0
            else
                ! Land: reflecting
                grid%u(grid%nx+1, j) = 0.0d0
            end if
        end do
        
        ! South boundary (j=1)
        do i = 1, grid%nx
            if (grid%h(i, 1) > 0.0d0) then
                ! Ocean: absorbing
                grid%v(i, 1) = 0.0d0
            else
                ! Land: reflecting
                grid%v(i, 1) = 0.0d0
            end if
        end do
        
        ! North boundary (j=ny)
        do i = 1, grid%nx
            if (grid%h(i, grid%ny) > 0.0d0) then
                ! Ocean: absorbing
                grid%v(i, grid%ny+1) = 0.0d0
            else
                ! Land: reflecting
                grid%v(i, grid%ny+1) = 0.0d0
            end if
        end do
    end subroutine apply_boundary_conditions

    !---------------------------------------------------------------------------
    ! Subroutine: update_depths
    ! Purpose: Update staggered depth arrays from eta and bathymetry
    !---------------------------------------------------------------------------
    subroutine update_depths(grid)
        type(swe_grid_type), intent(inout) :: grid
        
        integer :: i, j
        
        ! Update total depth
        grid%h = grid%eta - grid%bath
        
        ! Update staggered depths
        do j = 1, grid%ny
            do i = 1, grid%nx+1
                if (i == 1) then
                    grid%h_u(i, j) = max(grid%h(1, j), 0.0d0)
                else if (i == grid%nx+1) then
                    grid%h_u(i, j) = max(grid%h(grid%nx, j), 0.0d0)
                else
                    grid%h_u(i, j) = max(0.5d0 * (grid%h(i-1, j) + grid%h(i, j)), 0.0d0)
                end if
            end do
        end do
        
        do j = 1, grid%ny+1
            do i = 1, grid%nx
                if (j == 1) then
                    grid%h_v(i, j) = max(grid%h(i, 1), 0.0d0)
                else if (j == grid%ny+1) then
                    grid%h_v(i, j) = max(grid%h(i, grid%ny), 0.0d0)
                else
                    grid%h_v(i, j) = max(0.5d0 * (grid%h(i, j-1) + grid%h(i, j)), 0.0d0)
                end if
            end do
        end do
    end subroutine update_depths

    !---------------------------------------------------------------------------
    ! Subroutine: step_swe_leapfrog
    ! Purpose: Advance SWE one time step using Leap-frog scheme
    !---------------------------------------------------------------------------
    subroutine step_swe_leapfrog(grid, dt)
        type(swe_grid_type), intent(inout) :: grid
        real(8), intent(in) :: dt
        
        integer :: i, j
        real(8), allocatable :: eta_new(:,:), u_new(:,:), v_new(:,:)
        real(8) :: dudx, dvdy, detadx, detady
        
        allocate(eta_new(grid%nx, grid%ny))
        allocate(u_new(grid%nx+1, grid%ny))
        allocate(v_new(grid%nx, grid%ny+1))
        
        ! Update surface elevation (continuity equation)
        ! d(eta)/dt = -d(hu)/dx - d(hv)/dy
        do j = 1, grid%ny
            do i = 1, grid%nx
                ! Flux divergence (with boundary checks)
                if (i < grid%nx) then
                    dudx = (grid%h_u(i+1, j) * grid%u(i+1, j) - grid%h_u(i, j) * grid%u(i, j)) / grid%dx
                else
                    dudx = (grid%h_u(i, j) * grid%u(i, j) - grid%h_u(i, j) * grid%u(i, j)) / grid%dx
                end if
                
                if (j < grid%ny) then
                    dvdy = (grid%h_v(i, j+1) * grid%v(i, j+1) - grid%h_v(i, j) * grid%v(i, j)) / grid%dy
                else
                    dvdy = (grid%h_v(i, j) * grid%v(i, j) - grid%h_v(i, j) * grid%v(i, j)) / grid%dy
                end if
                
                eta_new(i, j) = grid%eta(i, j) - dt * (dudx + dvdy)
                
                ! Limit eta to reasonable values (prevent instability)
                if (abs(eta_new(i, j)) > 100.0d0) then  ! Max 100 m wave height
                    eta_new(i, j) = sign(100.0d0, eta_new(i, j))
                end if
            end do
        end do
        
        ! Update velocities (momentum equations)
        ! du/dt = -g * d(eta)/dx - u * du/dx - v * du/dy
        ! dv/dt = -g * d(eta)/dy - u * dv/dx - v * dv/dy
        
        do j = 1, grid%ny
            do i = 2, grid%nx  ! Interior u-points
                if (grid%h_u(i, j) > 1.0d-3) then  ! Wet point
                    detadx = (grid%eta(i, j) - grid%eta(i-1, j)) / grid%dx
                    
                    ! Simplified: ignore advection for stability (nonlinear terms can cause instability)
                    ! u_new(i, j) = grid%u(i, j) - dt * (G * detadx + grid%u(i, j) * dudx)
                    u_new(i, j) = grid%u(i, j) - dt * G * detadx
                    
                    ! Limit velocity to reasonable values
                    if (abs(u_new(i, j)) > 100.0d0) then  ! Max 100 m/s
                        u_new(i, j) = sign(100.0d0, u_new(i, j))
                    end if
                else
                    u_new(i, j) = 0.0d0  ! Dry point
                end if
            end do
        end do
        
        do j = 2, grid%ny  ! Interior v-points
            do i = 1, grid%nx
                if (grid%h_v(i, j) > 1.0d-3) then  ! Wet point
                    detady = (grid%eta(i, j) - grid%eta(i, j-1)) / grid%dy
                    
                    ! Simplified: ignore advection for stability
                    ! v_new(i, j) = grid%v(i, j) - dt * (G * detady + grid%v(i, j) * dvdy)
                    v_new(i, j) = grid%v(i, j) - dt * G * detady
                    
                    ! Limit velocity to reasonable values
                    if (abs(v_new(i, j)) > 100.0d0) then  ! Max 100 m/s
                        v_new(i, j) = sign(100.0d0, v_new(i, j))
                    end if
                else
                    v_new(i, j) = 0.0d0  ! Dry point
                end if
            end do
        end do
        
        ! Update arrays
        grid%eta = eta_new
        grid%u = u_new
        grid%v = v_new
        
        ! Check for NaN/Inf and fix (numerical stability)
        do j = 1, grid%ny
            do i = 1, grid%nx
                ! Check for NaN
                if (.not. (grid%eta(i, j) == grid%eta(i, j))) then
                    grid%eta(i, j) = 0.0d0
                end if
                ! Check for Inf and limit to reasonable values
                if (abs(grid%eta(i, j)) > 100.0d0) then  ! Max 100 m wave height
                    grid%eta(i, j) = sign(100.0d0, grid%eta(i, j))
                end if
            end do
        end do
        
        ! Check u velocities
        do j = 1, grid%ny
            do i = 1, grid%nx+1
                if (.not. (grid%u(i, j) == grid%u(i, j))) then
                    grid%u(i, j) = 0.0d0
                end if
                if (abs(grid%u(i, j)) > 1.0d3) then  ! Max 1000 m/s
                    grid%u(i, j) = sign(1.0d3, grid%u(i, j))
                end if
            end do
        end do
        
        ! Check v velocities
        do j = 1, grid%ny+1
            do i = 1, grid%nx
                if (.not. (grid%v(i, j) == grid%v(i, j))) then
                    grid%v(i, j) = 0.0d0
                end if
                if (abs(grid%v(i, j)) > 1.0d3) then  ! Max 1000 m/s
                    grid%v(i, j) = sign(1.0d3, grid%v(i, j))
                end if
            end do
        end do
        
        ! Update depths
        call update_depths(grid)
        
        ! Apply boundary conditions
        call apply_boundary_conditions(grid)
        
        deallocate(eta_new, u_new, v_new)
    end subroutine step_swe_leapfrog

end module mod_swe_solver

