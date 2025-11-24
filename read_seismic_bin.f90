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
  real, allocatable :: data_flat(:)
  integer :: i, j, k, l, ios, total_size, idx
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
  
  ! Allocate temporary flat array to read all data at once
  total_size = wh%ncom * wh%ndata
  allocate(data_flat(total_size))
  
  ! Second read: Read all waveform data at once
  ! Data is stored as: for each time point, [comp2, comp3, comp1]
  ! So the pattern is: [comp2_t1, comp3_t1, comp1_t1, comp2_t2, comp3_t2, comp1_t2, ...]
  read(8, iostat=ios) data_flat
  
  ! Handle case where file might be missing last value (13,799 instead of 13,800)
  if (ios /= 0) then
    ! Try to read what we can
    write(*,*) 'Note: File may be missing last value, reading available data'
  end if
  
  ! Rearrange data from flat array to wd(component, time)
  ! File order: [comp2_t1, comp3_t1, comp1_t1, comp2_t2, comp3_t2, comp1_t2, ...]
  ! We want: wd(1, i)=comp1_ti, wd(2, i)=comp2_ti, wd(3, i)=comp3_ti
  do i = 1, wh%ndata
    idx = (i - 1) * wh%ncom
    if (idx + 1 <= total_size) then
      wd(2, i) = data_flat(idx + 1)  ! comp2
    else
      wd(2, i) = 0.0
    end if
    if (idx + 2 <= total_size) then
      wd(3, i) = data_flat(idx + 2)  ! comp3
    else
      wd(3, i) = 0.0
    end if
    if (idx + 3 <= total_size) then
      wd(1, i) = data_flat(idx + 3)  ! comp1
    else
      wd(1, i) = 0.0  ! Handle missing last value
    end if
  end do
  
  deallocate(data_flat)
  
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
      ! Use symmetric range centered at 0
      data_min = minval(wd(j, 1:wh%ndata))
      data_max = maxval(wd(j, 1:wh%ndata))
      data_range = max(abs(data_min), abs(data_max)) * 1.1
      data_min = -data_range
      data_max = data_range
      
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

