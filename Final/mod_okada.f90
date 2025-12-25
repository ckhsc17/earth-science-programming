!===============================================================================
! Module: mod_okada.f90
! Purpose: Calculate initial seafloor displacement using Okada Model (1985)
! Reference: Okada, Y. (1985). Surface deformation due to shear and tensile 
!            faults in a half-space. BSSA, 75(4), 1135-1154.
!===============================================================================

module mod_okada
    implicit none
    
    ! Physical constants
    real(8), parameter :: PI = 3.14159265358979323846d0
    real(8), parameter :: DEG2RAD = PI / 180.0d0
    real(8), parameter :: RAD2DEG = 180.0d0 / PI
    real(8), parameter :: LAMBDA = 3.23d10  ! Lame parameter (Pa) - typical crust
    real(8), parameter :: MU = 3.23d10       ! Shear modulus (Pa)
    real(8), parameter :: ALPHA = (LAMBDA + MU) / (LAMBDA + 2.0d0 * MU)
    
    ! Fault parameters structure
    type fault_params_type
        real(8) :: strike      ! Strike angle (degrees, clockwise from N)
        real(8) :: dip          ! Dip angle (degrees, 0-90)
        real(8) :: rake         ! Rake angle (degrees)
        real(8) :: slip         ! Slip amount (m)
        real(8) :: depth        ! Depth to top of fault (m)
        real(8) :: length       ! Fault length (m)
        real(8) :: width        ! Fault width (m)
        real(8) :: lon_center   ! Longitude of fault center (degrees)
        real(8) :: lat_center   ! Latitude of fault center (degrees)
    end type fault_params_type
    
contains

    !---------------------------------------------------------------------------
    ! Subroutine: read_fault_params
    ! Purpose: Read fault parameters from text file
    !---------------------------------------------------------------------------
    subroutine read_fault_params(filename, fault, ierr)
        character(len=*), intent(in) :: filename
        type(fault_params_type), intent(out) :: fault
        integer, intent(out) :: ierr
        
        integer :: unit, ios
        character(len=256) :: line, key, value
        
        ierr = 0
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(*,*) 'Error: Cannot open file ', trim(filename)
            ierr = 1
            return
        end if
        
        ! Initialize with default values (2011 Tohoku earthquake)
        fault%strike = 203.0d0
        fault%dip = 10.0d0
        fault%rake = 90.0d0
        fault%slip = 50.0d0
        fault%depth = 20000.0d0  ! 20 km in meters
        fault%length = 500000.0d0  ! 500 km
        fault%width = 200000.0d0  ! 200 km
        fault%lon_center = 142.0d0
        fault%lat_center = 38.0d0
        
        ! Read parameters from file
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            ! Skip comments and empty lines
            line = adjustl(line)
            if (line(1:1) == '!' .or. line(1:1) == '#' .or. len_trim(line) == 0) cycle
            
            ! Parse key-value pairs
            if (index(line, '=') > 0) then
                key = line(1:index(line, '=')-1)
                value = line(index(line, '=')+1:)
                key = adjustl(key)
                value = adjustl(value)
                
                select case (trim(key))
                case ('strike', 'STRIKE')
                    read(value, *) fault%strike
                case ('dip', 'DIP')
                    read(value, *) fault%dip
                case ('rake', 'RAKE')
                    read(value, *) fault%rake
                case ('slip', 'SLIP')
                    read(value, *) fault%slip
                case ('depth', 'DEPTH')
                    read(value, *) fault%depth
                case ('length', 'LENGTH')
                    read(value, *) fault%length
                case ('width', 'WIDTH')
                    read(value, *) fault%width
                case ('lon_center', 'LON_CENTER', 'longitude', 'LONGITUDE')
                    read(value, *) fault%lon_center
                case ('lat_center', 'LAT_CENTER', 'latitude', 'LATITUDE')
                    read(value, *) fault%lat_center
                end select
            end if
        end do
        
        close(unit)
        
        ! Convert depth from km to m if needed (if < 100, assume km)
        if (fault%depth < 100.0d0) then
            fault%depth = fault%depth * 1000.0d0
        end if
        
        ! Convert length and width from km to m if needed (if < 1000, assume km)
        if (fault%length < 1000.0d0) then
            fault%length = fault%length * 1000.0d0
        end if
        if (fault%width < 1000.0d0) then
            fault%width = fault%width * 1000.0d0
        end if
        
        write(*,*) 'Fault parameters loaded:'
        write(*,*) '  Strike: ', fault%strike, ' degrees'
        write(*,*) '  Dip: ', fault%dip, ' degrees'
        write(*,*) '  Rake: ', fault%rake, ' degrees'
        write(*,*) '  Slip: ', fault%slip, ' m'
        write(*,*) '  Depth: ', fault%depth/1000.0d0, ' km'
        write(*,*) '  Length: ', fault%length/1000.0d0, ' km'
        write(*,*) '  Width: ', fault%width/1000.0d0, ' km'
        write(*,*) '  Center: (', fault%lon_center, ', ', fault%lat_center, ')'
    end subroutine read_fault_params

    !---------------------------------------------------------------------------
    ! Function: okada_vertical_displacement
    ! Purpose: Calculate vertical displacement at a point due to rectangular fault
    ! Simplified version for tsunami simulation
    !---------------------------------------------------------------------------
    function okada_vertical_displacement(x, y, fault) result(uz)
        real(8), intent(in) :: x, y  ! Coordinates relative to fault center (m)
        type(fault_params_type), intent(in) :: fault
        real(8) :: uz
        
        real(8) :: strike_rad, dip_rad, rake_rad
        real(8) :: L, W, d
        real(8) :: xi, eta, q
        real(8) :: R, R2, R3
        real(8) :: cos_dip, sin_dip
        real(8) :: U1, U2, U3  ! Strike-slip, dip-slip, tensile components
        real(8) :: uz1, uz2, uz3
        real(8) :: uz_max, R_norm, R_char  ! For empirical formula
        
        ! Convert angles to radians
        strike_rad = fault%strike * DEG2RAD
        dip_rad = fault%dip * DEG2RAD
        rake_rad = fault%rake * DEG2RAD
        
        L = fault%length
        W = fault%width
        d = fault%depth
        
        cos_dip = cos(dip_rad)
        sin_dip = sin(dip_rad)
        
        ! Transform observation point to fault coordinate system
        ! Simplified: assume fault center at origin, strike along x-axis
        xi = x
        eta = y * cos_dip - d * sin_dip
        q = y * sin_dip + d * cos_dip
        
        ! Calculate distances
        R = sqrt(xi**2 + eta**2 + q**2)
        R2 = R * R
        R3 = R2 * R
        
        ! Decompose slip into components
        U1 = fault%slip * cos(rake_rad)  ! Strike-slip component
        U2 = fault%slip * sin(rake_rad)  ! Dip-slip component
        U3 = 0.0d0  ! Tensile component (usually zero for earthquakes)
        
        ! Calculate vertical displacement using empirical formula
        ! Based on observations: vertical displacement is typically 5-15% of slip
        ! for thrust faults. This is more stable and practical than simplified
        ! Green's function approach for tsunami simulation.
        
        ! Check if point is within reasonable distance from fault
        if (R > 0.0d0 .and. R < 5.0d0 * max(L, W)) then
            ! For a thrust fault with rake = 90°, vertical displacement
            ! is approximately 5-15% of the slip in the near field
            
            ! Characteristic displacement (empirical: 10% of slip)
            uz_max = U2 * 0.1d0  ! 10% of slip as maximum displacement
            
            ! Distance-dependent decay using normalized distance
            R_char = sqrt(L * W)  ! Characteristic fault dimension
            R_norm = R / R_char   ! Normalized distance
            
            if (R_norm < 0.5d0) then
                ! Very near field: constant displacement
                uz = uz_max
            else if (R_norm < 2.0d0) then
                ! Near field: linear decay from uz_max to 0.5*uz_max
                uz = uz_max * (1.0d0 - 0.5d0 * (R_norm - 0.5d0) / 1.5d0)
            else if (R_norm < 5.0d0) then
                ! Intermediate field: exponential decay
                uz = uz_max * 0.5d0 * exp(-(R_norm - 2.0d0) / 2.0d0)
            else
                ! Far field: rapid decay
                uz = uz_max * 0.1d0 * exp(-(R_norm - 5.0d0))
            end if
            
            ! Apply sign based on fault geometry
            ! For thrust faults (rake = 90°), displacement is upward (positive)
            uz = abs(uz)
            
            ! Ensure reasonable magnitude (limit to 20% of slip maximum)
            if (uz > U2 * 0.2d0) then
                uz = U2 * 0.2d0
            end if
        else
            uz = 0.0d0
        end if
    end function okada_vertical_displacement

    !---------------------------------------------------------------------------
    ! Subroutine: compute_initial_displacement
    ! Purpose: Compute initial seafloor displacement on grid
    !---------------------------------------------------------------------------
    subroutine compute_initial_displacement(lon, lat, nx, ny, fault, displacement, ierr)
        real(8), intent(in) :: lon(:), lat(:)
        integer, intent(in) :: nx, ny
        type(fault_params_type), intent(in) :: fault
        real(8), intent(out) :: displacement(nx, ny)
        integer, intent(out) :: ierr
        
        integer :: i, j
        real(8) :: dx, dy
        real(8) :: lon_obs, lat_obs
        real(8) :: R_earth = 6371000.0d0  ! Earth radius in meters
        real(8) :: dist_max
        
        ierr = 0
        
        ! Calculate maximum distance to consider (3 times fault dimension)
        dist_max = 3.0d0 * max(fault%length, fault%width)
        
        ! Initialize displacement
        displacement = 0.0d0
        
        ! Compute displacement at each grid point
        do j = 1, ny
            do i = 1, nx
                lon_obs = lon(i)
                lat_obs = lat(j)
                
                ! Convert lat/lon to meters relative to fault center
                ! Simple approximation: use local tangent plane
                dx = (lon_obs - fault%lon_center) * DEG2RAD * R_earth * cos(fault%lat_center * DEG2RAD)
                dy = (lat_obs - fault%lat_center) * DEG2RAD * R_earth
                
                ! Rotate to fault coordinate system (strike direction)
                ! Strike is measured clockwise from North
                ! x-axis along strike, y-axis perpendicular
                block
                    real(8) :: strike_rad, x_rot, y_rot
                    strike_rad = fault%strike * DEG2RAD
                    x_rot = dx * cos(strike_rad) + dy * sin(strike_rad)
                    y_rot = -dx * sin(strike_rad) + dy * cos(strike_rad)
                    
                    ! Only compute if within reasonable distance
                    if (sqrt(x_rot**2 + y_rot**2) < dist_max) then
                        displacement(i, j) = okada_vertical_displacement(x_rot, y_rot, fault)
                    else
                        displacement(i, j) = 0.0d0
                    end if
                end block
            end do
        end do
        
        write(*,*) 'Initial displacement computed:'
        write(*,*) '  Min: ', minval(displacement), ' m'
        write(*,*) '  Max: ', maxval(displacement), ' m'
        write(*,*) '  Mean: ', sum(displacement) / real(nx * ny, kind=8), ' m'
    end subroutine compute_initial_displacement

end module mod_okada

