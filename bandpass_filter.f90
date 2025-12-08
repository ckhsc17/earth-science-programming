program bandpass_filter
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
  real, allocatable :: filtered_wd(:,:)
  real, allocatable :: time(:)
  real, allocatable :: data_flat(:)
  integer :: i, j, ios, total_size, idx
  integer :: pgopen
  real :: comp_mean
  real :: f_low, f_high  ! Filter frequencies
  real :: t_min, t_max, amp_min, amp_max  ! Plotting ranges
  integer :: filter_order
  character*8 :: filter_type, filter_proto
  
  ! Filter parameters
  filter_order = 4
  filter_type = 'BP      '  ! Bandpass
  filter_proto = 'BUTTER  '  ! Butterworth
  f_low = 0.5   ! 0.5 Hz
  f_high = 3.0  ! 3.0 Hz
  
  write(*,*) '============================================='
  write(*,*) '  Bandpass Filter (0.5-3 Hz)'
  write(*,*) '  Using IIRFILT from BPFILTER.for'
  write(*,*) '============================================='
  write(*,*) ''
  
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
  write(*,*) ''
  
  ! Allocate dynamic array for waveform data
  allocate(wd(wh%ncom, wh%ndata))
  allocate(filtered_wd(wh%ncom, wh%ndata))
  
  ! Allocate time array
  allocate(time(wh%ndata))
  
  ! Allocate temporary flat array to read all data at once
  total_size = wh%ncom * wh%ndata
  allocate(data_flat(total_size))
  
  ! Second read: Read all waveform data at once
  ! Data is stored as: for each time point, [comp2, comp3, comp1]
  read(8, iostat=ios) data_flat
  
  if (ios /= 0) then
    write(*,*) 'Note: File may be missing last value, reading available data'
  end if
  
  ! Rearrange data from flat array to wd(component, time)
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
      wd(1, i) = 0.0
    end if
  end do
  
  deallocate(data_flat)
  close(8)
  
  write(*,*) 'Successfully read ', wh%ncom, ' components with ', wh%ndata, ' data points each'
  write(*,*) ''
  
  ! Create time array
  do i = 1, wh%ndata
    time(i) = real(i-1) * wh%dt
  end do
  
  ! Compute and remove means for each component
  do j = 1, wh%ncom
    comp_mean = sum(wd(j, 1:wh%ndata)) / real(wh%ndata)
    do i = 1, wh%ndata
      wd(j, i) = wd(j, i) - comp_mean
    end do
  end do
  
  write(*,*) 'Filter parameters:'
  write(*,*) '  Type: Bandpass (BP)'
  write(*,*) '  Prototype: Butterworth'
  write(*,*) '  Order: ', filter_order
  write(*,*) '  Low corner: ', f_low, ' Hz'
  write(*,*) '  High corner: ', f_high, ' Hz'
  write(*,*) '  Passes: 2 (zero-phase)'
  write(*,*) ''
  
  ! Apply bandpass filter to each component
  write(*,*) 'Applying bandpass filter to each component...'
  do j = 1, wh%ncom
    ! Copy data to filtered array
    do i = 1, wh%ndata
      filtered_wd(j, i) = wd(j, i)
    end do
    
    ! Call IIRFILT: DATA1, NSAMPS, APROTO, IORD, TYPE, FLO, FHI, TS, PASSES
    call IIRFILT( filtered_wd(j, 1:wh%ndata), wh%ndata, filter_proto, &
                  filter_order, filter_type, f_low, f_high, wh%dt, 2 )
    
    write(*,*) '  Component ', j, ' filtered'
  end do
  write(*,*) ''
  
  ! Initialize PGPLOT
  if (pgopen('bandpass_filtered.ps/vcps') <= 0) then
    stop 'ERROR: Unable to open PostScript file'
  end if
  
  call pgsubp(1, 3)      ! Divide into 3 vertical subplots
  call pgslw(2)
  call pgsch(1.0)
  
  ! Plot filtered waveform for each component
  do j = 1, wh%ncom
    ! Calculate plot ranges for time domain
    t_min = time(1)
    t_max = time(wh%ndata)
    amp_min = minval(filtered_wd(j, 1:wh%ndata)) * 1.1
    amp_max = maxval(filtered_wd(j, 1:wh%ndata)) * 1.1
    
    ! Set up plot environment
    if (j == 1) then
      call pgenv(t_min, t_max, amp_min, amp_max, 0, 0)
    else
      call pgenv(t_min, t_max, amp_min, amp_max, 0, 1)
    end if
    
    ! Set labels based on component
    if (j == 1) then
      call pglabel('Time (s)', 'Amplitude', 'Vertical Component (Filtered 0.5-3 Hz)')
    else if (j == 2) then
      call pglabel('Time (s)', 'Amplitude', 'North-South Component (Filtered 0.5-3 Hz)')
    else if (j == 3) then
      call pglabel('Time (s)', 'Amplitude', 'East-West Component (Filtered 0.5-3 Hz)')
    else
      call pglabel('Time (s)', 'Amplitude', 'Component (Filtered 0.5-3 Hz)')
    end if
    
    ! Draw axes with y-axis major tick interval of 1
    call pgsci(1)
    call pgbox('BCNST', 0.0, 0, 'BCNLT', 1.0, 0)
    
    ! Plot filtered waveform
    call pgsci(j+1)
    call pgline(wh%ndata, time(1:wh%ndata), filtered_wd(j, 1:wh%ndata))
    
    ! Reset to black
    call pgsci(1)
  end do
  
  ! End PGPLOT
  call pgend
  
  write(*,*) 'Filtered waveform plot complete. Output file: bandpass_filtered.ps'
  write(*,*) ''
  
  ! Deallocate arrays
  deallocate(wd)
  deallocate(filtered_wd)
  deallocate(time)

end program bandpass_filter
