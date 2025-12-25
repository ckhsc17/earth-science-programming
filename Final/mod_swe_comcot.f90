!===============================================================================
! Module: mod_swe_comcot.f90
! Purpose: COMCOT SWE solver integration
! Source: Based on comcot-gfortran (https://github.com/AndybnACT/comcot-gfortran)
! 
! Implementation extracted from:
!   - mass.f90 (MASS_C subroutine) - Continuity equation
!   - moment.f90 (MOMT_C subroutine) - Momentum equations
!   - boundaries.f90 (OPEN subroutine) - Boundary conditions
!
! Key differences from hand-written solver:
!   - COMCOT uses volume flux (M=h*u, N=h*v) as primary variables
!   - Linear shallow water equations (no advection terms)
!   - Explicit forward time, centered space finite difference
!   - Radiation boundary conditions at all edges
!===============================================================================

module mod_swe_comcot
    use mod_swe_solver, only: swe_grid_type, G, PI
    implicit none
    
contains

    !---------------------------------------------------------------------------
    ! Subroutine: initialize_comcot_grid
    ! Purpose: Initialize COMCOT-style SWE grid
    !---------------------------------------------------------------------------
    subroutine initialize_comcot_grid(bathymetry, initial_displacement, lon, lat, &
                                      nx, ny, grid, ierr)
        real(8), intent(in) :: bathymetry(:,:)
        real(8), intent(in) :: initial_displacement(:,:)
        real(8), intent(in) :: lon(:), lat(:)
        integer, intent(in) :: nx, ny
        type(swe_grid_type), intent(out) :: grid
        integer, intent(out) :: ierr
        
        ierr = 0
        grid%nx = nx
        grid%ny = ny
        
        ! Allocate arrays
        allocate(grid%h(nx, ny))
        allocate(grid%eta(nx, ny))
        allocate(grid%bath(nx, ny))
        allocate(grid%u(nx+1, ny))
        allocate(grid%v(nx, ny+1))
        allocate(grid%h_u(nx+1, ny))
        allocate(grid%h_v(nx, ny+1))
        
        ! Initialize (FIX: Convert GEBCO elevation to water depth)
        ! GEBCO bathymetry is elevation (negative below sea level)
        ! We need positive water depth for SWE calculations
        grid%bath = -bathymetry  ! Convert elevation to depth (negate)
        where (grid%bath < 0.0d0) grid%bath = 0.0d0  ! Land areas set to 0
        
        grid%eta = initial_displacement
        grid%h = grid%bath + grid%eta  ! Total depth = still water + displacement
        where (grid%h < 0.0d0) grid%h = 0.0d0  ! Dry areas
        
        grid%u = 0.0d0
        grid%v = 0.0d0
        
        ! Calculate grid spacing (convert degrees to meters approximately)
        ! For Japan region (~35-40°N), 1 degree ≈ 111 km
        grid%dx = abs(lon(2) - lon(1)) * 111000.0d0  ! meters
        grid%dy = abs(lat(2) - lat(1)) * 111000.0d0  ! meters
        
        write(*,*) 'COMCOT grid initialized:'
        write(*,'(A,I5,A,I5)') '  Grid size: ', nx, ' x ', ny
        write(*,'(A,F10.2,A)') '  dx = ', grid%dx/1000.0d0, ' km'
        write(*,'(A,F10.2,A)') '  dy = ', grid%dy/1000.0d0, ' km'
        
    end subroutine initialize_comcot_grid

    !---------------------------------------------------------------------------
    ! Subroutine: step_comcot_swe
    ! Purpose: Advance SWE one time step using COMCOT's method
    ! 
    ! Based on COMCOT's linear shallow water equations:
    !   1. Continuity: ∂η/∂t + ∂M/∂x + ∂N/∂y = 0
    !   2. X-momentum: ∂M/∂t = -gH ∂η/∂x
    !   3. Y-momentum: ∂N/∂t = -gH ∂η/∂y
    ! 
    ! where M = h*u, N = h*v (volume flux), H = still water depth
    !---------------------------------------------------------------------------
    subroutine step_comcot_swe(grid, dt)
        type(swe_grid_type), intent(inout) :: grid
        real(8), intent(in) :: dt
        
        ! Local variables
        integer :: i, j, nx, ny, ip1, jp1
        real(8) :: rx, ry, grx, gry
        real(8) :: zz, xm, xn, hm, hn
        real(8), parameter :: gx = 0.01d0  ! Minimum water depth threshold (m)
        real(8), parameter :: eps = 1.0d-10
        real(8), parameter :: zero = 0.0d0
        
        ! Flux arrays (COMCOT uses volume flux M=h*u, N=h*v)
        real(8), allocatable :: M(:,:,:), N(:,:,:), Z(:,:,:)
        real(8), allocatable :: H(:,:)  ! Still water depth
        
        nx = grid%nx
        ny = grid%ny
        
        ! Allocate flux arrays (time levels 1=current, 2=next)
        allocate(M(nx,ny,2), N(nx,ny,2), Z(nx,ny,2), H(nx,ny))
        
        ! Initialize time level 1 (current time step)
        ! Convert from our format to COMCOT format
        do j = 1, ny
            do i = 1, nx
                Z(i,j,1) = grid%eta(i,j)
                H(i,j) = grid%bath(i,j)  ! Still water depth (bathymetry)
                
                ! Volume flux: M = h*u, N = h*v
                if (grid%h(i,j) > gx) then
                    M(i,j,1) = grid%h(i,j) * grid%u(i,j)
                    N(i,j,1) = grid%h(i,j) * grid%v(i,j)
                else
                    M(i,j,1) = 0.0d0
                    N(i,j,1) = 0.0d0
                end if
            end do
        end do
        
        ! Compute time-space ratios (COMCOT notation)
        rx = dt / grid%dx
        ry = dt / grid%dy
        grx = G * rx
        gry = G * ry
        
        !-----------------------------------------------------------------------
        ! STEP 1: SOLVE CONTINUITY EQUATION (from COMCOT mass.f90 MASS_C)
        !-----------------------------------------------------------------------
        ! Discretization: Z^(n+1) = Z^n - RX*(M(i) - M(i-1)) - RY*(N(j) - N(j-1))
        ! This is explicit forward Euler in time, centered difference in space
        
        do j = 2, ny-1
            do i = 2, nx-1
                if (H(i,j) > gx) then
                    zz = Z(i,j,1) - rx*(M(i,j,1) - M(i-1,j,1)) &
                                  - ry*(N(i,j,1) - N(i,j-1,1))
                    
                    if (abs(zz) < eps) zz = zero
                    
                    ! Depression cannot be less than bottom elevation
                    if ((zz + H(i,j)) <= eps) zz = -H(i,j)
                    
                    Z(i,j,2) = zz
                else
                    Z(i,j,2) = 0.0d0
                end if
            end do
        end do
        
        !-----------------------------------------------------------------------
        ! STEP 2: SOLVE MOMENTUM EQUATIONS (from COMCOT moment.f90 MOMT_C)
        !-----------------------------------------------------------------------
        ! X-direction: M^(n+1) = M^n - GRX*H_p*(Z(i+1) - Z(i))
        ! Y-direction: N^(n+1) = N^n - GRY*H_q*(Z(j+1) - Z(j))
        ! where H_p, H_q are water depths at flux points (staggered grid)
        
        ! X-momentum (at i+1/2, j points)
        do j = 2, ny-1
            do i = 1, nx-1
                ip1 = i + 1
                
                if ((H(i,j) > gx) .and. (H(ip1,j) > gx)) then
                    ! Water depth at flux point (average of neighbors)
                    hm = 0.5d0 * (H(i,j) + H(ip1,j))
                    
                    ! Linear momentum equation (no advection, no friction)
                    xm = M(i,j,1) - grx * hm * (Z(ip1,j,2) - Z(i,j,2))
                    
                    if (abs(xm) < eps) xm = zero
                    M(i,j,2) = xm
                else
                    M(i,j,2) = 0.0d0
                end if
            end do
        end do
        
        ! Y-momentum (at i, j+1/2 points)
        do j = 1, ny-1
            do i = 2, nx-1
                jp1 = j + 1
                
                if ((H(i,j) > gx) .and. (H(i,jp1) > gx)) then
                    ! Water depth at flux point (average of neighbors)
                    hn = 0.5d0 * (H(i,j) + H(i,jp1))
                    
                    ! Linear momentum equation (no advection, no friction)
                    xn = N(i,j,1) - gry * hn * (Z(i,jp1,2) - Z(i,j,2))
                    
                    if (abs(xn) < eps) xn = zero
                    N(i,j,2) = xn
                else
                    N(i,j,2) = 0.0d0
                end if
            end do
        end do
        
        !-----------------------------------------------------------------------
        ! STEP 3: UPDATE GRID - Convert back from COMCOT to our format
        !-----------------------------------------------------------------------
        do j = 1, ny
            do i = 1, nx
                grid%eta(i,j) = Z(i,j,2)
                grid%h(i,j) = H(i,j) + Z(i,j,2)  ! Total water depth
                
                ! Convert volume flux back to velocity: u = M/h, v = N/h
                if (grid%h(i,j) > gx) then
                    grid%u(i,j) = M(i,j,2) / grid%h(i,j)
                    grid%v(i,j) = N(i,j,2) / grid%h(i,j)
                else
                    grid%u(i,j) = 0.0d0
                    grid%v(i,j) = 0.0d0
                    grid%h(i,j) = 0.0d0
                end if
            end do
        end do
        
        ! Apply boundary conditions
        call apply_comcot_boundary(grid)
        
        ! Clean up
        deallocate(M, N, Z, H)
        
    end subroutine step_comcot_swe

    !---------------------------------------------------------------------------
    ! Subroutine: apply_comcot_boundary
    ! Purpose: Apply radiation boundary conditions (COMCOT's OPEN boundary)
    ! 
    ! Based on boundaries.f90 OPEN subroutine (lines 22-126)
    ! Uses characteristic method: η = ±sqrt(u² + v²) / c
    ! where c = sqrt(g*h) is the characteristic wave speed
    ! 
    ! This allows waves to exit the domain without reflection
    !---------------------------------------------------------------------------
    subroutine apply_comcot_boundary(grid)
        type(swe_grid_type), intent(inout) :: grid
        
        integer :: i, j, nx, ny
        real(8) :: cc, uh, uu, zz, arg
        real(8), parameter :: gx = 0.01d0  ! Minimum water depth (m)
        real(8), parameter :: zero = 0.0d0
        real(8), parameter :: ub = 99.0d0  ! Upper bound for eta
        
        nx = grid%nx
        ny = grid%ny
        
        ! Bottom boundary (j=1) - waves exiting southward
        j = 1
        do i = 2, nx-1
            if (grid%h(i,j) > gx) then
                cc = sqrt(G * grid%h(i,j))  ! Characteristic speed
                uh = 0.5d0 * (grid%h(i,j) * grid%u(i,j) + grid%h(i-1,j) * grid%u(i-1,j))
                uu = sqrt(uh**2 + (grid%h(i,j) * grid%v(i,j))**2)
                zz = uu / cc
                arg = grid%h(i,j) * grid%v(i,j)
                if (arg > zero) then
                    zz = -zz  ! Outgoing wave
                end if
                if (abs(zz) > ub) zz = 0.0d0
                grid%eta(i,j) = zz
            else
                grid%eta(i,j) = zero
            end if
        end do
        
        ! Top boundary (j=ny) - waves exiting northward
        j = ny
        do i = 2, nx-1
            if (grid%h(i,j) > gx) then
                cc = sqrt(G * grid%h(i,j))
                uh = 0.5d0 * (grid%h(i,j) * grid%u(i,j) + grid%h(i-1,j) * grid%u(i-1,j))
                uu = sqrt(uh**2 + (grid%h(i,j-1) * grid%v(i,j-1))**2)
                zz = uu / cc
                arg = grid%h(i,j-1) * grid%v(i,j-1)
                if (arg < zero) then
                    zz = -zz  ! Outgoing wave
                end if
                if (abs(zz) > ub) zz = 0.0d0
                grid%eta(i,j) = zz
            else
                grid%eta(i,j) = zero
            end if
        end do
        
        ! Left boundary (i=1) - waves exiting westward
        i = 1
        do j = 2, ny-1
            if (grid%h(i,j) > gx) then
                cc = sqrt(G * grid%h(i,j))
                if (grid%h(i,j-1) > gx) then
                    uh = 0.5d0 * (grid%h(i,j) * grid%v(i,j) + grid%h(i,j-1) * grid%v(i,j-1))
                else
                    uh = grid%h(i,j) * grid%v(i,j)
                end if
                uu = sqrt(uh**2 + (grid%h(i,j) * grid%u(i,j))**2)
                zz = uu / cc
                arg = grid%h(i,j) * grid%u(i,j)
                if (arg > zero) then
                    zz = -zz  ! Outgoing wave
                end if
                if (abs(zz) > ub) zz = 0.0d0
                grid%eta(i,j) = zz
            else
                grid%eta(i,j) = zero
            end if
        end do
        
        ! Right boundary (i=nx) - waves exiting eastward
        i = nx
        do j = 2, ny-1
            if (grid%h(i,j) > gx) then
                cc = sqrt(G * grid%h(i,j))
                if (grid%h(i,j-1) > gx) then
                    uh = 0.5d0 * (grid%h(i,j) * grid%v(i,j) + grid%h(i,j-1) * grid%v(i,j-1))
                else
                    uh = grid%h(i,j) * grid%v(i,j)
                end if
                uu = sqrt(uh**2 + (grid%h(i-1,j) * grid%u(i-1,j))**2)
                zz = uu / cc
                arg = grid%h(i-1,j) * grid%u(i-1,j)
                if (arg < zero) then
                    zz = -zz  ! Outgoing wave
                end if
                if (abs(zz) > ub) zz = 0.0d0
                grid%eta(i,j) = zz
            else
                grid%eta(i,j) = zero
            end if
        end do
        
        ! Update total water depth after boundary update
        do j = 1, ny
            do i = 1, nx
                grid%h(i,j) = grid%bath(i,j) + grid%eta(i,j)
                if (grid%h(i,j) < gx) then
                    grid%h(i,j) = 0.0d0
                    grid%u(i,j) = 0.0d0
                    grid%v(i,j) = 0.0d0
                end if
            end do
        end do
        
    end subroutine apply_comcot_boundary

    !---------------------------------------------------------------------------
    ! Subroutine: compute_comcot_timestep
    ! Purpose: Calculate stable time step using CFL condition
    ! 
    ! CFL condition: dt <= CFL * dx / c_max
    ! where c_max = sqrt(g*h_max) + max(|u|, |v|)
    !---------------------------------------------------------------------------
    subroutine compute_comcot_timestep(grid, dt)
        type(swe_grid_type), intent(in) :: grid
        real(8), intent(out) :: dt
        
        real(8) :: c_max, u_max, v_max, dx_min, h_max
        real(8), parameter :: cfl = 0.3d0  ! CFL safety factor
        
        ! Find maximum depth
        h_max = maxval(grid%h, mask=(grid%h > 0.01d0))
        if (h_max < 1.0d0) h_max = 1000.0d0
        
        ! Maximum wave speed
        c_max = sqrt(G * h_max)
        
        ! Maximum velocity
        u_max = maxval(abs(grid%u))
        v_max = maxval(abs(grid%v))
        
        ! CFL condition
        dx_min = min(grid%dx, grid%dy)
        dt = cfl * dx_min / (c_max + max(u_max, v_max) + 1.0d0)
        
        ! Limit range (reasonable for tsunami simulation)
        if (dt < 0.01d0) dt = 0.01d0
        if (dt > 10.0d0) dt = 10.0d0
        
    end subroutine compute_comcot_timestep

end module mod_swe_comcot

!===============================================================================
! IMPLEMENTATION NOTES:
!===============================================================================
!
! 1. SOURCE:
!    Based on comcot-gfortran by NCU Tsunami Research Group
!    https://github.com/AndybnACT/comcot-gfortran
!
! 2. NUMERICAL METHOD:
!    - Linear shallow water equations (no advection terms)
!    - Explicit forward Euler time integration
!    - Centered finite difference in space
!    - Staggered grid for momentum (Arakawa C-grid like)
!
! 3. KEY DIFFERENCES FROM HAND-WRITTEN SOLVER:
!    - Uses volume flux (M, N) instead of velocity (u, v)
!    - No non-linear advection terms
!    - Simpler numerical scheme
!    - Radiation boundary conditions at all edges
!
! 4. ADVANTAGES:
!    - Proven stability for tsunami simulations
!    - Well-tested on real events (2011 Tohoku)
!    - Simpler, more robust than full non-linear SWE
!
! 5. LIMITATIONS:
!    - Linear approximation (valid for small amplitude/depth ratio)
!    - No bottom friction
!    - No dispersion correction
!    - Fixed grid (no nesting in this implementation)
!
!===============================================================================
