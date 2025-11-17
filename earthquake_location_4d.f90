program earthquake_location_4d
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: max_stations = 250
    
    ! Station data arrays
    character(len=4) :: station_names(max_stations)
    real(dp) :: station_x(max_stations), station_y(max_stations), station_z(max_stations)
    real(dp) :: observed_times(max_stations)
    integer :: n_stations
    
    ! Earthquake location variables (4D: X, Y, Z, T)
    real(dp) :: eq_x, eq_y, eq_z, eq_t
    real(dp) :: initial_x, initial_y, initial_z, initial_t
    
    ! Iteration variables
    integer :: max_iter, iter
    real(dp) :: tolerance, convergence_check
    logical :: converged
    
    ! Matrix variables for Gauss-Newton
    real(dp) :: G(max_stations, 4), D(max_stations)
    real(dp) :: GT(4, max_stations), GTG(4, 4), GTG_inv(4, 4), GTD(4)
    real(dp) :: delta_params(4)
    real(dp) :: weights(max_stations)
    
    ! Constants
    real(dp), parameter :: velocity = 6.5d0  ! km/s
    
    print *, '============================================='
    print *, '    4D Earthquake Location Program'
    print *, '    (Half-space model with weighting)'
    print *, '============================================='
    
    ! Read station data and observed travel times
    call read_station_data(station_names, station_x, station_y, station_z, n_stations)
    call read_observed_times(station_names, observed_times, n_stations)
    
    print *, 'Successfully loaded', n_stations, 'stations with travel time data'
    
    ! Get initial guess from user
    print *, ''
    print *, 'Enter initial earthquake location estimate:'
    print *, 'X (km):'
    read *, initial_x
    print *, 'Y (km):'
    read *, initial_y
    print *, 'Z (km, depth >= 0):'
    read *, initial_z
    print *, 'Origin time (seconds):'
    read *, initial_t
    
    ! Initialize parameters
    eq_x = initial_x
    eq_y = initial_y
    eq_z = max(0.0d0, initial_z)  ! Ensure half-space constraint
    eq_t = initial_t
    
    max_iter = 50
    tolerance = 1.0d-6
    converged = .false.
    
    print *, ''
    print *, 'Starting iterative location process...'
    print *, ''
    
    ! Gauss-Newton iteration
    do iter = 1, max_iter
        ! Build Jacobian matrix G and residual vector D
        call build_jacobian_residual(n_stations, station_x, station_y, station_z, &
                                    observed_times, eq_x, eq_y, eq_z, eq_t, &
                                    G, D, weights, velocity)
        
        ! Apply weights to G and D
        call apply_weights(G, D, weights, n_stations, 4)
        
        ! Solve normal equations: (G^T G) * delta = G^T * D
        call mat_transpose(G, GT, n_stations, 4)
        call mat_mult(GT, G, GTG, 4, n_stations, 4)
        call mat_mult_vec(GT, D, GTD, 4, n_stations)
        
        ! Check for singularity
        if (is_singular(GTG, 4)) then
            print *, 'Warning: Matrix is singular at iteration', iter
            exit
        endif
        
        call MATRIXINV(GTG, 4)
        call mat_mult_vec(GTG, GTD, delta_params, 4, 4)
        
        ! Apply damping factor for stability
        if (iter > 1) then
            delta_params = delta_params * 0.5d0
        endif
        
        ! Update parameters with bounds checking
        eq_x = eq_x + delta_params(1)
        eq_y = eq_y + delta_params(2)
        eq_z = eq_z + delta_params(3)
        eq_t = eq_t + delta_params(4)
        
        ! Apply constraints
        if (eq_z < 0.0d0) eq_z = 0.0d0
        if (eq_z > 100.0d0) eq_z = 100.0d0  ! Reasonable depth limit
        if (abs(eq_t) > 1000.0d0) eq_t = sign(1000.0d0, eq_t)  ! Time limit
        
        ! Check convergence
        convergence_check = sqrt(sum(delta_params**2))
        
        if (mod(iter, 5) == 0 .or. iter <= 3) then
            if (abs(eq_t) < 1.0d6 .and. convergence_check < 1.0d6) then
                print '(A,I3,A,F10.6,A,4F10.3)', 'Iter ', iter, ': Conv=', convergence_check, &
                      ' Params: ', eq_x, eq_y, eq_z, eq_t
            else
                print '(A,I3,A)', 'Iter ', iter, ': Parameters out of range'
            endif
        endif
        
        if (convergence_check < tolerance) then
            converged = .true.
            exit
        endif
        
        ! Emergency exit if parameters become unreasonable
        if (abs(eq_t) > 1.0d6 .or. convergence_check > 1.0d6) then
            print *, 'Warning: Solution diverging, stopping at iteration', iter
            exit
        endif
    enddo
    
    ! Output results
    print *, ''
    print *, '============================================='
    if (converged) then
        print *, 'CONVERGED in', iter, 'iterations'
    else
        print *, 'WARNING: Did not converge after', max_iter, 'iterations'
    endif
    print *, '============================================='
    print *, 'Final earthquake location:'
    print '(A,F12.3,A)', '  X (Easting)  = ', eq_x, ' km'
    print '(A,F12.3,A)', '  Y (Northing) = ', eq_y, ' km'
    print '(A,F12.3,A)', '  Z (Depth)    = ', eq_z, ' km'
    print '(A,F12.3,A)', '  T (Origin)   = ', eq_t, ' seconds'
    print *, ''
    print *, 'Geographic coordinates:'
    call display_geographic_location(eq_x, eq_y)
    print *, '============================================='
    
    ! Calculate and display residuals
    call display_residuals(n_stations, station_names, station_x, station_y, station_z, &
                          observed_times, eq_x, eq_y, eq_z, eq_t, velocity)

contains

    !----------------------------------------------------------
    subroutine read_station_data(stn_names, sx, sy, sz, n_stn)
        character(len=4), intent(out) :: stn_names(max_stations)
        real(dp), intent(out) :: sx(max_stations), sy(max_stations), sz(max_stations)
        integer, intent(out) :: n_stn
        
        character(len=4) :: stn_code
        integer :: la, lo, iret, i, j
        real(dp) :: xla, xlo, elev
        real(dp) :: lat_deg, lon_deg
        logical :: already_exists
        
        n_stn = 0
        open(2, file='nsta.dat', status='old')
        
        do i = 1, max_stations
            read(2, '(A4, i2, f5.2, 1x, i3, f5.2, 1x, f6.1)', iostat=iret) &
                 stn_code, la, xla, lo, xlo, elev
            
            if (iret /= 0) exit
            
            ! Check if station already exists (handle duplicates)
            already_exists = .false.
            do j = 1, n_stn
                if (trim(adjustl(stn_code)) == trim(adjustl(stn_names(j)))) then
                    already_exists = .true.
                    ! Update with latest coordinates (overwrite)
                    lat_deg = real(la, dp) + xla / 60.0d0
                    lon_deg = real(lo, dp) + xlo / 60.0d0
                    call delaz(24.0d0, 121.0d0, lat_deg, lon_deg, sx(j), sy(j))
                    sz(j) = elev / 1000.0d0
                    exit
                endif
            enddo
            
            ! Add new station if not already exists
            if (.not. already_exists) then
                n_stn = n_stn + 1
                stn_names(n_stn) = stn_code
                
                ! Convert to decimal degrees
                lat_deg = real(la, dp) + xla / 60.0d0
                lon_deg = real(lo, dp) + xlo / 60.0d0
                
                ! Convert to km using delaz subroutine (relative to reference point)
                ! Using Taiwan center as reference: ~24°N, 121°E
                call delaz(24.0d0, 121.0d0, lat_deg, lon_deg, sx(n_stn), sy(n_stn))
                sz(n_stn) = elev / 1000.0d0  ! Convert elevation to km
            endif
        enddo
        
        close(2)
        print *, 'Read', n_stn, 'unique stations from nsta.dat'
    end subroutine read_station_data

    !----------------------------------------------------------
    subroutine read_observed_times(stn_names, obs_times, n_stn)
        character(len=4), intent(in) :: stn_names(max_stations)
        real(dp), intent(out) :: obs_times(max_stations)
        integer, intent(in) :: n_stn
        
        character(len=4) :: stn_code
        integer :: iy, im, id, ih, mm, ios, i, j
        real(dp) :: xsec, origin_time, arrival_time
        real(dp) :: dist_km
        logical :: found
        
        ! Initialize all times to -999 (missing data indicator)
        obs_times = -999.0d0
        
        open(1, file='ppfile.txt', status='old')
        
        ! Skip comment lines and read header line to get origin time
        do
            read(1, '(A)', iostat=ios) ! Read a line as string first
            if (ios /= 0) then
                print *, 'Error: Cannot find valid header in ppfile.txt'
                stop
            endif
            backspace(1) ! Go back to read the same line with proper format
            
            ! Try to read as header format
            read(1, '(1x,i4,4i2,f6.2)', iostat=ios) iy, im, id, ih, mm, xsec
            if (ios == 0) exit ! Successfully read header, exit loop
        enddo
        origin_time = real(mm, dp) * 60.0d0 + xsec
        
        ! Read station data using the successful format from hw6-2_plot.f90
        do
            read(1, '(1x,A4,f6.1,9x,i3,f6.2)', iostat=ios) stn_code, dist_km, mm, xsec
            if (ios /= 0) exit
            
            ! Find matching station in our list
            found = .false.
            do j = 1, n_stn
                if (trim(adjustl(stn_code)) == trim(adjustl(stn_names(j)))) then
                    arrival_time = real(mm, dp) * 60.0d0 + xsec
                    obs_times(j) = arrival_time - origin_time  ! Travel time
                    found = .true.
                    exit
                endif
            enddo
            
            if (.not. found) then
                print *, 'Warning: Station', trim(stn_code), 'not found in station list'
            endif
        enddo
        
        close(1)
        
        ! Count valid observations
        j = 0
        do i = 1, n_stn
            if (obs_times(i) > -999.0d0) j = j + 1
        enddo
        print *, 'Found travel times for', j, 'stations'
    end subroutine read_observed_times

    !----------------------------------------------------------
    subroutine build_jacobian_residual(n, sx, sy, sz, obs_t, x0, y0, z0, t0, &
                                      G, D, w, vel)
        integer, intent(in) :: n
        real(dp), intent(in) :: sx(n), sy(n), sz(n), obs_t(n)
        real(dp), intent(in) :: x0, y0, z0, t0, vel
        real(dp), intent(out) :: G(n, 4), D(n), w(n)
        
        integer :: i
        real(dp) :: dx, dy, dz, dist, theo_time, residual
        
        ! Initialize arrays
        G = 0.0d0
        D = 0.0d0
        w = 0.0d0
        
        do i = 1, n
            if (obs_t(i) <= -999.0d0) cycle  ! Skip missing data
            
            dx = x0 - sx(i)
            dy = y0 - sy(i)
            dz = z0 - sz(i)
            dist = sqrt(dx*dx + dy*dy + dz*dz)
            
            ! Avoid division by zero
            if (dist < 1.0d-6) dist = 1.0d-6
            
            ! Theoretical travel time
            theo_time = dist / vel
            
            ! Jacobian matrix (partial derivatives)
            G(i, 1) = dx / (vel * dist)  ! ∂t/∂x
            G(i, 2) = dy / (vel * dist)  ! ∂t/∂y
            G(i, 3) = dz / (vel * dist)  ! ∂t/∂z
            G(i, 4) = 1.0d0              ! ∂t/∂t0
            
            ! Residual (observed - theoretical - origin time correction)
            ! obs_t is travel time, theo_time is calculated travel time, t0 is origin time correction
            residual = obs_t(i) - theo_time - t0
            D(i) = residual
            
            ! Weight = 1/r^2 (distance weighting) with minimum weight
            w(i) = max(1.0d0 / (dist * dist), 1.0d-6)
        enddo
    end subroutine build_jacobian_residual

    !----------------------------------------------------------
    subroutine apply_weights(G, D, w, m, n)
        integer, intent(in) :: m, n
        real(dp), intent(inout) :: G(m, n), D(m)
        real(dp), intent(in) :: w(m)
        integer :: i, j
        
        do i = 1, m
            if (w(i) > 0.0d0) then
                D(i) = D(i) * sqrt(w(i))
                do j = 1, n
                    G(i, j) = G(i, j) * sqrt(w(i))
                enddo
            endif
        enddo
    end subroutine apply_weights

    !----------------------------------------------------------
    function is_singular(A, n) result(singular)
        integer, intent(in) :: n
        real(dp), intent(in) :: A(n, n)
        logical :: singular
        integer :: i
        
        singular = .false.
        do i = 1, n
            if (abs(A(i, i)) < 1.0d-15) then
                singular = .true.
                return
            endif
        enddo
    end function is_singular

    !----------------------------------------------------------
    subroutine display_residuals(n, stn_names, sx, sy, sz, obs_t, x0, y0, z0, t0, vel)
        integer, intent(in) :: n
        character(len=4), intent(in) :: stn_names(n)
        real(dp), intent(in) :: sx(n), sy(n), sz(n), obs_t(n)
        real(dp), intent(in) :: x0, y0, z0, t0, vel
        
        integer :: i, valid_count
        real(dp) :: dx, dy, dz, dist, theo_time, residual, rms
        real(dp) :: sum_sq_residuals, calc_time
        
        print *, ''
        print *, 'Station Residuals:'
        print *, 'Stn   Dist(km)  Obs(s)  Calc(s)  Residual(s)'
        print *, '----  --------  ------  -------  -----------'
        
        sum_sq_residuals = 0.0d0
        valid_count = 0
        
        do i = 1, n
            if (obs_t(i) <= -999.0d0) cycle
            
            dx = x0 - sx(i)
            dy = y0 - sy(i)
            dz = z0 - sz(i)
            dist = sqrt(dx*dx + dy*dy + dz*dz)
            theo_time = dist / vel
            calc_time = theo_time + t0
            residual = obs_t(i) - theo_time - t0
            
            ! Check for reasonable values before printing
            if (abs(calc_time) < 1.0d6 .and. abs(residual) < 1.0d6) then
                print '(A4, 2X, F8.2, 2X, F6.2, 2X, F7.2, 2X, F11.3)', &
                      stn_names(i), dist, obs_t(i), calc_time, residual
            else
                print '(A4, 2X, F8.2, 2X, F6.2, 2X, A7, 2X, A11)', &
                      stn_names(i), dist, obs_t(i), '*******', '***********'
            endif
            
            if (abs(residual) < 1.0d6) then
                sum_sq_residuals = sum_sq_residuals + residual*residual
                valid_count = valid_count + 1
            endif
        enddo
        
        if (valid_count > 4) then
            rms = sqrt(sum_sq_residuals / real(valid_count - 4, dp))
            print *, '----  --------  ------  -------  -----------'
            if (rms < 1.0d6) then
                print '(A, F8.3, A)', 'RMS residual = ', rms, ' seconds'
            else
                print *, 'RMS residual = ******** seconds'
            endif
        endif
    end subroutine display_residuals

    !----------------------------------------------------------
    subroutine delaz(elat, elon, slat, slon, dx, dy)
        real(dp), intent(in) :: elat, elon, slat, slon
        real(dp), intent(out) :: dx, dy
        real(dp) :: avlat, a, b, dlat, dlon
        
        avlat = 0.5d0 * (elat + slat)
        
        a = 1.840708d0 + avlat * (0.0015269d0 + avlat * (-0.00034d0 + avlat * 1.02337d-6))
        b = 1.843404d0 + avlat * (-6.93799d-5 + avlat * (8.79993d-6 + avlat * (-6.47527d-8)))
        
        dlat = slat - elat
        dlon = slon - elon
        
        dx = a * dlon * 60.0d0
        dy = b * dlat * 60.0d0
    end subroutine delaz

    !----------------------------------------------------------
    subroutine inverse_delaz(ref_lat, ref_lon, dx, dy, lat, lon)
        ! Inverse transformation: convert km offsets back to lat/lon
        real(dp), intent(in) :: ref_lat, ref_lon, dx, dy
        real(dp), intent(out) :: lat, lon
        real(dp) :: a, b, dlat, dlon
        
        ! Use reference point for coefficients (approximation)
        a = 1.840708d0 + ref_lat * (0.0015269d0 + ref_lat * (-0.00034d0 + ref_lat * 1.02337d-6))
        b = 1.843404d0 + ref_lat * (-6.93799d-5 + ref_lat * (8.79993d-6 + ref_lat * (-6.47527d-8)))
        
        ! Convert km back to degrees
        dlon = dx / (a * 60.0d0)
        dlat = dy / (b * 60.0d0)
        
        lat = ref_lat + dlat
        lon = ref_lon + dlon
    end subroutine inverse_delaz

    !----------------------------------------------------------
    subroutine display_geographic_location(x_km, y_km)
        real(dp), intent(in) :: x_km, y_km
        real(dp) :: latitude, longitude
        integer :: lat_deg, lat_min, lon_deg, lon_min
        real(dp) :: lat_sec, lon_sec
        
        ! Convert km coordinates back to lat/lon (reference: 24°N, 121°E)
        call inverse_delaz(24.0d0, 121.0d0, x_km, y_km, latitude, longitude)
        
        ! Convert decimal degrees to degrees, minutes, seconds
        lat_deg = int(latitude)
        lat_min = int((latitude - lat_deg) * 60.0d0)
        lat_sec = ((latitude - lat_deg) * 60.0d0 - lat_min) * 60.0d0
        
        lon_deg = int(longitude)
        lon_min = int((longitude - lon_deg) * 60.0d0)
        lon_sec = ((longitude - lon_deg) * 60.0d0 - lon_min) * 60.0d0
        
        print '(A,F10.6,A)', '  Latitude  = ', latitude, '°'
        print '(A,F10.6,A)', '  Longitude = ', longitude, '°'
        print *, ''
        print '(A,I2,A,I2,A,F6.3,A)', '  Latitude  = ', lat_deg, '° ', lat_min, "' ", lat_sec, '" N'
        print '(A,I3,A,I2,A,F6.3,A)', '  Longitude = ', lon_deg, '° ', lon_min, "' ", lon_sec, '" E'
    end subroutine display_geographic_location

    !----------------------------------------------------------
    ! Matrix operations (from hw6-3.f90)
    !----------------------------------------------------------
    subroutine mat_transpose(A, AT, m, n)
        integer, intent(in) :: m, n
        real(dp), intent(in) :: A(m,n)
        real(dp), intent(out) :: AT(n,m)
        integer :: i,j
        do i=1,m
            do j=1,n
                AT(j,i) = A(i,j)
            enddo
        enddo
    end subroutine mat_transpose

    !----------------------------------------------------------
    subroutine mat_mult(A, B, C, m, n, p)
        integer, intent(in) :: m, n, p
        real(dp), intent(in) :: A(m,n)
        real(dp), intent(in) :: B(n,p)
        real(dp), intent(out) :: C(m,p)
        integer :: i,j,k
        C = 0.0d0
        do i=1,m
            do j=1,p
                do k=1,n
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                enddo
            enddo
        enddo
    end subroutine mat_mult

    !----------------------------------------------------------
    subroutine mat_mult_vec(A, B, C, m, n)
        integer, intent(in) :: m, n
        real(dp), intent(in) :: A(m,n)
        real(dp), intent(in) :: B(n)
        real(dp), intent(out) :: C(m)
        integer :: i,k
        C = 0.0d0
        do i=1,m
            do k=1,n
                C(i) = C(i) + A(i,k)*B(k)
            enddo
        enddo
    end subroutine mat_mult_vec

    !----------------------------------------------------------
    subroutine MATRIXINV(C, n)
        integer, intent(in) :: n
        real(dp), intent(inout) :: C(n,n)
        integer :: i, j
        integer :: indx(n)
        real(dp) :: y(n,n), D

        y = 0.0d0
        do i=1,n
            y(i,i) = 1.0d0
        enddo

        call LUDCMP(C, n, indx, D)
        do j=1,n
            call LUBKSB(C, n, indx, y(1,j))
        enddo

        do i=1,n
            do j=1,n
                C(i,j) = y(i,j)
            enddo
        enddo
    end subroutine MATRIXINV

    !----------------------------------------------------------
    subroutine LUDCMP(A, N, INDX, D)
        integer, intent(in) :: N
        real(dp), intent(inout) :: A(N,N)
        integer, intent(out) :: INDX(N)
        real(dp), intent(out) :: D
        integer :: I, IMAX, J, K
        real(dp) :: VV(N), SUM, AAMAX, DUM
        real(dp), parameter :: TINY = 1.0d-12

        D = 1.0d0
        do I=1,N
            AAMAX = 0.0d0
            do J=1,N
                AAMAX = max(AAMAX, abs(A(I,J)))
            enddo
            if (AAMAX == 0.0d0) stop 'Singular matrix'
            VV(I) = 1.0d0 / AAMAX
        enddo

        do J=1,N
            do I=1,J-1
                SUM = A(I,J)
                do K=1,I-1
                    SUM = SUM - A(I,K)*A(K,J)
                enddo
                A(I,J) = SUM
            enddo
            AAMAX = 0.0d0
            do I=J,N
                SUM = A(I,J)
                do K=1,J-1
                    SUM = SUM - A(I,K)*A(K,J)
                enddo
                A(I,J) = SUM
                DUM = VV(I)*abs(SUM)
                if (DUM >= AAMAX) then
                    IMAX = I
                    AAMAX = DUM
                endif
            enddo
            if (J /= IMAX) then
                do K=1,N
                    DUM = A(IMAX,K)
                    A(IMAX,K) = A(J,K)
                    A(J,K) = DUM
                enddo
                D = -D
                VV(IMAX) = VV(J)
            endif
            INDX(J) = IMAX
            if (A(J,J) == 0.0d0) A(J,J) = TINY
            if (J /= N) then
                DUM = 1.0d0 / A(J,J)
                A(J+1:N,J) = A(J+1:N,J) * DUM
            endif
        enddo
    end subroutine LUDCMP

    !----------------------------------------------------------
    subroutine LUBKSB(A, N, INDX, B)
        integer, intent(in) :: N
        real(dp), intent(in) :: A(N,N)
        real(dp), intent(inout) :: B(N)
        integer, intent(in) :: INDX(N)
        integer :: I, II, J, LL
        real(dp) :: SUM

        II = 0
        do I=1,N
            LL = INDX(I)
            SUM = B(LL)
            B(LL) = B(I)
            if (II /= 0) then
                do J=II,I-1
                    SUM = SUM - A(I,J)*B(J)
                enddo
            else if (SUM /= 0.0d0) then
                II = I
            endif
            B(I) = SUM
        enddo
        do I=N,1,-1
            SUM = B(I)
            do J=I+1,N
                SUM = SUM - A(I,J)*B(J)
            enddo
            B(I) = SUM / A(I,I)
        enddo
    end subroutine LUBKSB

end program earthquake_location_4d
