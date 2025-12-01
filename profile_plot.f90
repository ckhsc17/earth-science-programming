program earthquake_profile
  implicit none
  
  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: max_eq = 50000
  
  ! Profile endpoints
  real(dp), parameter :: prof_lat1 = 23.0d0, prof_lon1 = 120.0d0
  real(dp), parameter :: prof_lat2 = 25.0d0, prof_lon2 = 122.0d0
  real(dp), parameter :: max_dist = 50.0d0  ! 50 km
  
  ! Earthquake data
  real(dp) :: eq_lat(max_eq), eq_lon(max_eq), eq_depth(max_eq)
  real(dp) :: eq_dist_to_profile(max_eq), eq_proj_dist(max_eq)
  integer :: n_eq, n_selected, i, ios
  character(len=200) :: line
  
  ! Parsing variables
  integer :: lat_deg, lon_deg
  real :: lat_min, lon_min, depth
  real(dp) :: latitude, longitude
  
  ! Profile line in Cartesian coordinates
  real(dp) :: prof_x1, prof_y1, prof_x2, prof_y2
  real(dp) :: prof_dx, prof_dy, prof_length
  real(dp) :: prof_unit_x, prof_unit_y
  
  ! For each earthquake
  real(dp) :: eq_x, eq_y, dx, dy, delta
  real(dp) :: vec_to_eq_x, vec_to_eq_y
  real(dp) :: proj_scalar, perp_dist
  
  ! Plotting variables
  integer :: pgopen
  real :: x_min, x_max, depth_min, depth_max
  real :: plot_x, plot_depth
  
  write(*,*) '============================================='
  write(*,*) '  Earthquake Profile Plot'
  write(*,*) '  Profile: (120., 23.) to (122., 25.0)'
  write(*,*) '  Distance threshold: 50 km'
  write(*,*) '============================================='
  write(*,*) ''
  
  ! Convert profile endpoints to Cartesian coordinates
  ! Using first endpoint as reference
  call delaz(prof_lat1, prof_lon1, prof_lat1, prof_lon1, prof_x1, prof_y1)
  prof_x1 = 0.0d0
  prof_y1 = 0.0d0
  
  call delaz(prof_lat1, prof_lon1, prof_lat2, prof_lon2, prof_x2, prof_y2)
  
  ! Calculate profile line vector and length
  prof_dx = prof_x2 - prof_x1
  prof_dy = prof_y2 - prof_y1
  prof_length = sqrt(prof_dx*prof_dx + prof_dy*prof_dy)
  
  ! Unit vector along profile
  prof_unit_x = prof_dx / prof_length
  prof_unit_y = prof_dy / prof_length
  
  write(*,*) 'Profile line:'
  write(*,'(A,2F10.4)') '  Endpoint 1 (lat, lon): ', prof_lat1, prof_lon1
  write(*,'(A,2F10.4)') '  Endpoint 2 (lat, lon): ', prof_lat2, prof_lon2
  write(*,'(A,F10.2,A)') '  Profile length: ', prof_length, ' km'
  write(*,*) ''
  
  ! Read earthquake data from 1999.lis
  write(*,*) 'Reading earthquake data from 1999.lis...'
  open(unit=10, file='1999.lis', status='old', action='read')
  n_eq = 0
  
  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    
    ! Parse latitude and longitude
    read(line,'(18X,I2,F5.2,I3,F5.2)', iostat=ios) lat_deg, lat_min, lon_deg, lon_min
    if (ios /= 0) cycle
    
    ! Convert to decimal degrees
    latitude = real(lat_deg, dp) + real(lat_min, dp) / 60.0d0
    longitude = real(lon_deg, dp) + real(lon_min, dp) / 60.0d0
    
    ! Read depth (around position 34-39)
    read(line(34:39), *, iostat=ios) depth
    if (ios /= 0) depth = 10.0
    
    ! Convert earthquake location to Cartesian coordinates
    ! Using profile endpoint 1 as reference
    call delaz(prof_lat1, prof_lon1, latitude, longitude, eq_x, eq_y)
    
    ! Calculate vector from profile start to earthquake
    vec_to_eq_x = eq_x - prof_x1
    vec_to_eq_y = eq_y - prof_y1
    
    ! Project onto profile line
    proj_scalar = vec_to_eq_x * prof_unit_x + vec_to_eq_y * prof_unit_y
    
    ! Calculate perpendicular distance from earthquake to profile line
    ! Projected point on line
    dx = prof_x1 + proj_scalar * prof_unit_x - eq_x
    dy = prof_y1 + proj_scalar * prof_unit_y - eq_y
    perp_dist = sqrt(dx*dx + dy*dy)
    
    ! Check if within 50 km of profile
    if (perp_dist <= max_dist) then
      n_eq = n_eq + 1
      eq_lat(n_eq) = latitude
      eq_lon(n_eq) = longitude
      eq_depth(n_eq) = real(depth, dp)
      eq_dist_to_profile(n_eq) = perp_dist
      eq_proj_dist(n_eq) = proj_scalar  ! Distance along profile from start
      
      if (n_eq <= 5) then
        write(*,'(A,I5,A,F8.4,A,F9.4,A,F6.2,A,F8.2,A,F8.2)') &
             '  Event ', n_eq, ': Lat=', latitude, ', Lon=', longitude, &
             ', Depth=', depth, ' km, Dist=', perp_dist, ' km, Proj=', proj_scalar, ' km'
      end if
    end if
    
    if (n_eq >= max_eq) exit
  end do
  close(10)
  
  n_selected = n_eq
  write(*,*) ''
  write(*,'(A,I0,A)') 'Found ', n_selected, ' earthquakes within 50 km of profile'
  write(*,*) ''
  
  if (n_selected == 0) then
    write(*,*) 'No earthquakes found within 50 km of profile!'
    stop
  end if
  
  ! Calculate plot ranges
  x_min = minval(eq_proj_dist(1:n_selected)) - 10.0
  x_max = maxval(eq_proj_dist(1:n_selected)) + 10.0
  depth_min = 0.0
  depth_max = maxval(eq_depth(1:n_selected)) + 5.0
  
  write(*,*) 'Plot ranges:'
  write(*,'(A,2F10.2)') '  Distance along profile: ', x_min, x_max
  write(*,'(A,2F10.2)') '  Depth: ', depth_min, depth_max
  write(*,*) ''
  
  ! Initialize PGPLOT
  if (pgopen('earthquake_profile.ps/vcps') <= 0) then
    stop 'ERROR: Unable to open PostScript file'
  end if
  
  call pgslw(2)
  call pgsch(1.2)
  
  ! Set up plot environment
  call pgenv(x_min, x_max, depth_min, depth_max, 0, 0)
  call pglabel('Distance along profile (km)', 'Depth (km)', &
               'Earthquake Profile: (120., 23.) to (122., 25.0)')
  
  ! Draw axes
  call pgsci(1)
  call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
  
  ! Plot earthquakes with color coding by depth
  ! Use different colors for different depth ranges
  do i = 1, n_selected
    plot_x = real(eq_proj_dist(i))
    plot_depth = real(eq_depth(i))
    
    ! Color coding by depth: shallow (red), medium (green), deep (blue)
    if (eq_depth(i) < 20.0d0) then
      call pgsci(2)  ! Red for shallow (< 20 km)
    else if (eq_depth(i) < 50.0d0) then
      call pgsci(3)  ! Green for medium (20-50 km)
    else
      call pgsci(4)  ! Blue for deep (> 50 km)
    end if
    
    call pgpt(1, plot_x, plot_depth, 4)  ! Small circle
  end do
  
  ! Add text annotation and legend
  call pgsci(1)
  call pgtext(x_min + (x_max - x_min) * 0.05, depth_max * 0.95, &
              'Events within 50 km of profile')
  
  ! Add legend
  call pgtext(x_min + (x_max - x_min) * 0.05, depth_max * 0.88, 'Depth:')
  call pgsci(2)
  call pgpt(1, x_min + (x_max - x_min) * 0.12, depth_max * 0.88, 4)
  call pgsci(1)
  call pgtext(x_min + (x_max - x_min) * 0.15, depth_max * 0.88, '< 20 km')
  
  call pgsci(3)
  call pgpt(1, x_min + (x_max - x_min) * 0.12, depth_max * 0.81, 4)
  call pgsci(1)
  call pgtext(x_min + (x_max - x_min) * 0.15, depth_max * 0.81, '20-50 km')
  
  call pgsci(4)
  call pgpt(1, x_min + (x_max - x_min) * 0.12, depth_max * 0.74, 4)
  call pgsci(1)
  call pgtext(x_min + (x_max - x_min) * 0.15, depth_max * 0.74, '> 50 km')
  
  ! End PGPLOT
  call pgend
  
  write(*,*) 'Plot complete. Output file: earthquake_profile.ps'
  write(*,*) ''
  
contains
  
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
  
end program earthquake_profile

