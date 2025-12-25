!===============================================================================
! Program: plot_tsunami_pgplot.f90
! Purpose: Visualize tsunami simulation results using PGPLOT
! Requires: PGPLOT library (libpgplot-dev on Linux, or compile from source)
!===============================================================================

program plot_tsunami_pgplot
    use mod_netcdf_io
    use mod_netcdf_output
    implicit none
    
    ! PGPLOT interface
    interface
        subroutine pgopen(device, ierr)
            character(len=*), intent(in) :: device
            integer, intent(out) :: ierr
        end subroutine pgopen
        
        subroutine pgclos()
        end subroutine pgclos
        
        subroutine pgpage()
        end subroutine pgpage
        
        subroutine pgsubp(nx, ny)
            integer, intent(in) :: nx, ny
        end subroutine pgsubp
        
        subroutine pgvstd()
        end subroutine pgvstd
        
        subroutine pgswin(x1, x2, y1, y2)
            real, intent(in) :: x1, x2, y1, y2
        end subroutine pgswin
        
        subroutine pgbox(xopt, xtick, nxsub, yopt, ytick, nysub)
            character(len=*), intent(in) :: xopt, yopt
            real, intent(in) :: xtick, ytick
            integer, intent(in) :: nxsub, nysub
        end subroutine pgbox
        
        subroutine pglab(xlabel, ylabel, title)
            character(len=*), intent(in) :: xlabel, ylabel, title
        end subroutine pglab
        
        subroutine pgcont(a, idim, jdim, i1, i2, j1, j2, c, nc, tr)
            integer, intent(in) :: idim, jdim, i1, i2, j1, j2, nc
            real, intent(in) :: a(idim, jdim), c(nc), tr(6)
        end subroutine pgcont
        
        subroutine pgwnad(x1, x2, y1, y2)
            real, intent(in) :: x1, x2, y1, y2
        end subroutine pgwnad
        
        subroutine pgimag(a, idim, jdim, i1, i2, j1, j2, a1, a2, tr)
            integer, intent(in) :: idim, jdim, i1, i2, j1, j2
            real, intent(in) :: a(idim, jdim), a1, a2, tr(6)
        end subroutine pgimag
        
        subroutine pgwedg(opt, side, disp, width, fg, bg, label, c1, c2)
            character(len=*), intent(in) :: opt, side
            real, intent(in) :: disp, width, fg, bg, c1, c2
            character(len=*), intent(in) :: label
        end subroutine pgwedg
        
        subroutine pgmtxt(side, disp, coord, fjust, text)
            character(len=*), intent(in) :: side, text
            real, intent(in) :: disp, coord, fjust
        end subroutine pgmtxt
        
        subroutine pgslw(lw)
            integer, intent(in) :: lw
        end subroutine pgslw
        
        subroutine pgsci(ci)
            integer, intent(in) :: ci
        end subroutine pgsci
        
        subroutine pgscf(font)
            integer, intent(in) :: font
        end subroutine pgscf
        
        subroutine pgsch(size)
            real, intent(in) :: size
        end subroutine pgsch
    end interface
    
    character(len=256) :: filename
    integer :: ncid, ierr
    integer :: nx, ny, n_times
    real(8), allocatable :: lon(:), lat(:), time(:)
    real(8), allocatable :: eta(:,:,:)
    real(8) :: lon_min, lon_max, lat_min, lat_max
    real(8) :: eta_min, eta_max
    integer :: time_varid, eta_varid
    integer :: start(3), count(3)
    integer :: i, j, t_idx
    real(4), allocatable :: eta_plot(:,:)
    real(4) :: tr(6)
    integer :: pgopen_status
    character(len=80) :: title_str
    
    ! File name
    filename = 'tsunami_output.nc'
    
    write(*,*) '============================================================'
    write(*,*) 'Tsunami Visualization using PGPLOT'
    write(*,*) '============================================================'
    write(*,*)
    
    ! Open NetCDF file
    ierr = nf90_open(filename, NF90_NOWRITE, ncid)
    if (ierr /= nf90_noerr) then
        write(*,*) 'Error opening file: ', trim(filename)
        stop 1
    end if
    
    ! Get dimensions
    call get_netcdf_info(filename, nx, ny, lon_min, lon_max, lat_min, lat_max, ierr)
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'Error getting file info'
        stop 1
    end if
    
    ! Get time dimension
    block
        integer :: time_dimid
        ierr = nf90_inq_dimid(ncid, 'time', time_dimid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: time dimension not found'
            stop 1
        end if
        ierr = nf90_inquire_dimension(ncid, time_dimid, len=n_times)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error getting time dimension'
            stop 1
        end if
    end block
    
    write(*,*) 'File information:'
    write(*,*) '  Grid size: ', nx, ' x ', ny
    write(*,*) '  Time steps: ', n_times
    write(*,*) '  Longitude: ', lon_min, ' to ', lon_max
    write(*,*) '  Latitude: ', lat_min, ' to ', lat_max
    write(*,*)
    
    ! Allocate arrays
    allocate(lon(nx), lat(ny), time(n_times))
    allocate(eta(nx, ny, n_times))
    allocate(eta_plot(nx, ny))
    
    ! Read coordinates
    block
        integer :: lon_varid, lat_varid
        ierr = nf90_inq_varid(ncid, 'lon', lon_varid)
        ierr = nf90_get_var(ncid, lon_varid, lon)
        ierr = nf90_inq_varid(ncid, 'lat', lat_varid)
        ierr = nf90_get_var(ncid, lat_varid, lat)
        ierr = nf90_inq_varid(ncid, 'time', time_varid)
        ierr = nf90_get_var(ncid, time_varid, time)
    end block
    
    ! Read eta data
    ierr = nf90_inq_varid(ncid, 'eta', eta_varid)
    if (ierr /= nf90_noerr) then
        write(*,*) 'Error: eta variable not found'
        stop 1
    end if
    
    ! Read all time steps (note: NetCDF stores as (time, lat, lon))
    do t_idx = 1, n_times
        start = (/1, 1, t_idx/)
        count = (/nx, ny, 1/)
        ! Note: NetCDF dimension order is (lon, lat, time), but stored as (time, lat, lon)
        ! Need to transpose
        block
            real(8), allocatable :: eta_temp(:,:)
            allocate(eta_temp(ny, nx))
            ierr = nf90_get_var(ncid, eta_varid, eta_temp, start=(/t_idx, 1, 1/), &
                               count=(/1, ny, nx/))
            ! Transpose to (nx, ny)
            do j = 1, ny
                do i = 1, nx
                    eta(i, j, t_idx) = eta_temp(j, i)
                end do
            end do
            deallocate(eta_temp)
        end block
    end do
    
    ! Find min/max for scaling
    eta_min = minval(eta)
    eta_max = maxval(eta)
    
    write(*,*) 'Data range:'
    write(*,*) '  Eta min: ', eta_min, ' m'
    write(*,*) '  Eta max: ', eta_max, ' m'
    write(*,*)
    
    ! Set up transformation matrix for PGPLOT
    ! tr = [x0, dx, 0, y0, 0, dy]
    tr(1) = real(lon(1), kind=4)
    tr(2) = real((lon(nx) - lon(1)) / real(nx-1, kind=8), kind=4)
    tr(3) = 0.0
    tr(4) = real(lat(1), kind=4)
    tr(5) = 0.0
    tr(6) = real((lat(ny) - lat(1)) / real(ny-1, kind=8), kind=4)
    
    ! Initialize PGPLOT
    write(*,*) 'Initializing PGPLOT...'
    call pgopen('/XWIN', pgopen_status)  ! Use '/XWIN' for X11, '/PNG' for file output
    if (pgopen_status <= 0) then
        write(*,*) 'Error opening PGPLOT device'
        stop 1
    end if
    
    ! Set up plot style
    call pgslw(2)
    call pgscf(1)  ! Normal font
    call pgsch(1.2)
    
    ! Plot 1: Initial condition
    write(*,*) 'Plotting initial condition...'
    call pgpage()
    call pgsubp(1, 1)
    call pgvstd()
    call pgswin(real(lon_min, kind=4), real(lon_max, kind=4), &
                real(lat_min, kind=4), real(lat_max, kind=4))
    call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
    call pglab('Longitude (°E)', 'Latitude (°N)', 'Initial Seafloor Displacement')
    
    ! Convert to single precision for plotting
    eta_plot = real(eta(:, :, 1), kind=4)
    
    ! Use contour plot
    call pgcont(eta_plot, nx, ny, 1, nx, 1, ny, &
                (/real(eta_min, kind=4), real(eta_max, kind=4)/), 2, tr)
    
    ! Plot 2: Maximum amplitude
    write(*,*) 'Plotting maximum amplitude...'
    call pgpage()
    call pgsubp(1, 1)
    call pgvstd()
    call pgswin(real(lon_min, kind=4), real(lon_max, kind=4), &
                real(lat_min, kind=4), real(lat_max, kind=4))
    call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
    call pglab('Longitude (°E)', 'Latitude (°N)', 'Maximum Wave Amplitude')
    
    ! Compute maximum amplitude
    do j = 1, ny
        do i = 1, nx
            eta_plot(i, j) = real(maxval(abs(eta(i, j, :))), kind=4)
        end do
    end do
    
    call pgimag(eta_plot, nx, ny, 1, nx, 1, ny, &
                real(0.0d0, kind=4), real(maxval(eta_plot), kind=4), tr)
    call pgwedg('RI', 2.0, 3.0, 1.5, 0.0, 1.0, 'Amplitude (m)', 0.0, 1.0)
    
    ! Plot 3: Time series at selected locations
    write(*,*) 'Plotting time series...'
    call pgpage()
    call pgsubp(2, 2)
    
    ! Select 4 locations
    block
        integer :: loc_i(4), loc_j(4)
        integer :: k
        real(4), allocatable :: time_plot(:), eta_ts(:)
        
        allocate(time_plot(n_times), eta_ts(n_times))
        time_plot = real(time / 60.0d0, kind=4)  ! Convert to minutes
        
        ! Location 1: Near epicenter
        loc_i(1) = nx / 2
        loc_j(1) = ny / 2
        
        ! Location 2: East
        loc_i(2) = nx * 3 / 4
        loc_j(2) = ny / 2
        
        ! Location 3: West
        loc_i(3) = nx / 4
        loc_j(3) = ny / 2
        
        ! Location 4: North
        loc_i(4) = nx / 2
        loc_j(4) = ny * 3 / 4
        
        do k = 1, 4
            call pgsubp(2, 2, k)
            call pgvstd()
            
            ! Extract time series
            do t_idx = 1, n_times
                eta_ts(t_idx) = real(eta(loc_i(k), loc_j(k), t_idx), kind=4)
            end do
            
            call pgswin(real(time_plot(1), kind=4), real(time_plot(n_times), kind=4), &
                       real(minval(eta_ts), kind=4), real(maxval(eta_ts), kind=4))
            call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
            
            write(title_str, '(A,F6.2,A,F6.2,A)') 'Location (', &
                  real(lon(loc_i(k)), kind=4), ', ', real(lat(loc_j(k)), kind=4), ')'
            call pglab('Time (min)', 'Elevation (m)', trim(title_str))
            
            ! Plot line (simplified - PGPLOT line plotting would go here)
            ! For now, just draw the box
        end do
        
        deallocate(time_plot, eta_ts)
    end block
    
    ! Plot 4: Wave propagation at selected times
    write(*,*) 'Plotting wave propagation...'
    do t_idx = 1, min(6, n_times)
        call pgpage()
        call pgsubp(1, 1)
        call pgvstd()
        call pgswin(real(lon_min, kind=4), real(lon_max, kind=4), &
                    real(lat_min, kind=4), real(lat_max, kind=4))
        call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        
        write(title_str, '(A,F6.1,A)') 'Wave Propagation at t = ', &
              real(time(t_idx)/60.0d0, kind=4), ' minutes'
        call pglab('Longitude (°E)', 'Latitude (°N)', trim(title_str))
        
        eta_plot = real(eta(:, :, t_idx), kind=4)
        call pgimag(eta_plot, nx, ny, 1, nx, 1, ny, &
                   real(eta_min, kind=4), real(eta_max, kind=4), tr)
        call pgwedg('RI', 2.0, 3.0, 1.5, 0.0, 1.0, 'Elevation (m)', 0.0, 1.0)
    end do
    
    write(*,*) 'Press Enter to close plots...'
    read(*,*)
    
    call pgclos()
    
    ! Cleanup
    deallocate(lon, lat, time, eta, eta_plot)
    ierr = nf90_close(ncid)
    
    write(*,*) 'Done!'
    
end program plot_tsunami_pgplot

