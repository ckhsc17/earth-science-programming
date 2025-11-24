program read_seismic_bin
  implicit none
  
  ! Define Header type
  type :: Header
    character*4 :: Code
    real*8 :: orgintime
    integer*2 :: ncom
    integer*4 :: ndata
    real :: dt
    character*4 :: blk
  end type Header
  
  type(Header) :: wh
  real, allocatable :: wd(:,:)
  real, allocatable :: time(:)
  integer :: i, j, k, l, ios
  integer :: pgopen
  real :: t_min, t_max, data_min, data_max, data_range
  real :: comp_mean, comp_peak, time_peak
  
  ! Open binary file with stream access
  open(8, file='seismicdata.bin', status='old', form='unformatted', access='stream')
  
  ! First read: Read header
  read(8) wh
  
  write(*,*) 'Header information:'
  write(*,*) '  Code: ', wh%Code
  write(*,*) '  Origin time: ', wh%orgintime
  write(*,*) '  Number of components: ', wh%ncom
  write(*,*) '  Number of data points: ', wh%ndata
  write(*,*) '  Sampling interval (s): ', wh%dt
  
  ! Allocate dynamic array for waveform data
  allocate(wd(wh%ncom, wh%ndata))
  
  ! Allocate time array
  allocate(time(wh%ndata))
  
  ! Second read: Read waveform data
  ! Data is stored row-major: all time points for component 1, then component 2, etc.
  ! But we want wd(component, time), so we read component by component
  do j = 1, wh%ncom
    do i = 1, wh%ndata
      read(8, iostat=ios) wd(j, i)
      if (ios /= 0) then
        ! End of file reached
        if (j < wh%ncom .or. i < wh%ndata) then
          write(*,*) 'Warning: End of file at component ', j, ', time point ', i
          ! Fill remaining with zeros
          if (i < wh%ndata) then
            do k = i+1, wh%ndata
              wd(j, k) = 0.0
            end do
          end if
          if (j < wh%ncom) then
            do k = j+1, wh%ncom
              do l = 1, wh%ndata
                wd(k, l) = 0.0
              end do
            end do
          end if
        end if
        exit
      end if
    end do
    if (ios /= 0) exit
  end do
  
  close(8)
  
  write(*,*) 'Successfully read ', wh%ncom, ' components with ', wh%ndata, ' data points each'
  write(*,*) ''
  write(*,*) '=== Data Statistics ==='
  
  ! Log data for each component
  do j = 1, wh%ncom
    write(*,*) 'Component ', j, ':'
    write(*,*) '  First 10 values:'
    do i = 1, min(10, wh%ndata)
      write(*,'(A,I5,A,F15.6)') '    [', i, '] = ', wd(j, i)
    end do
    write(*,*) '  Last 10 values:'
    do i = max(1, wh%ndata-9), wh%ndata
      write(*,'(A,I5,A,F15.6)') '    [', i, '] = ', wd(j, i)
    end do
    write(*,'(A,F15.6)') '  Min value: ', minval(wd(j, 1:wh%ndata))
    write(*,'(A,F15.6)') '  Max value: ', maxval(wd(j, 1:wh%ndata))
    write(*,'(A,F15.6)') '  Mean value: ', sum(wd(j, 1:wh%ndata)) / real(wh%ndata)
    write(*,*) ''
  end do
  
  ! Create time array
  do i = 1, wh%ndata
    time(i) = real(i-1) * wh%dt
  end do
  
  write(*,*) 'Time array:'
  write(*,'(A,F10.3)') '  t_min = ', 0.0
  write(*,'(A,F10.3)') '  t_max = ', time(wh%ndata)
  write(*,'(A,F10.6)') '  dt = ', wh%dt
  write(*,*) ''
  
  t_min = 0.0
  t_max = time(wh%ndata)
  
  ! Compute and remove means for each component
  do j = 1, wh%ncom
    comp_mean = sum(wd(j, 1:wh%ndata)) / real(wh%ndata)
    do i = 1, wh%ndata
      wd(j, i) = wd(j, i) - comp_mean
    end do
  end do
  
  ! Initialize PGPLOT
  if (pgopen('seismogram.ps/vcps') <= 0) then
    stop 'ERROR: Unable to open PostScript file'
  end if
  
  call pgsubp(1, 3)      ! Divide into 3 vertical subplots
  call pgslw(2)
  call pgsch(1.0)
  
  ! Plot each component
  if (wh%ncom > 0) then
    do j = 1, wh%ncom
      ! Calculate data range for this component
      data_min = minval(wd(j, 1:wh%ndata)) * 1.1
      data_max = maxval(wd(j, 1:wh%ndata)) * 1.1
      
      ! Set up plot environment
      if (j == 1) then
        call pgenv(t_min, t_max, data_min, data_max, 0, 0)
      else
        call pgenv(t_min, t_max, data_min, data_max, 0, 1)
      end if
      
      ! Set labels based on component
      if (j == 1) then
        call pglabel('Time (s)', 'Acceleration (mean removed)', 'Vertical Component')
      else if (j == 2) then
        call pglabel('Time (s)', 'Acceleration (mean removed)', 'North-South Component')
      else if (j == 3) then
        call pglabel('Time (s)', 'Acceleration (mean removed)', 'East-West Component')
      else
        call pglabel('Time (s)', 'Acceleration (mean removed)', 'Component')
      end if
      
      ! Draw axes (black)
      call pgsci(1)
      call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
      
      ! Find peak for this component
      comp_peak = wd(j, 1)
      time_peak = time(1)
      do i = 2, wh%ndata
        if (abs(wd(j, i)) > abs(comp_peak)) then
          comp_peak = wd(j, i)
          time_peak = time(i)
        end if
      end do
      
      ! Plot waveform (colored: red for comp1, green for comp2, blue for comp3)
      call pgsci(j+1)
      call pgline(wh%ndata, time(1:wh%ndata), wd(j, 1:wh%ndata))
      
      ! Mark peak point
      call pgpt(1, time_peak, comp_peak, 12)
      
      ! Reset to black
      call pgsci(1)
    end do
  end if
  
  ! End PGPLOT
  call pgend
  
  write(*,*) 'Plot complete. Output file: seismogram.ps'
  
  ! Deallocate arrays
  deallocate(wd)
  deallocate(time)
  
end program read_seismic_bin

