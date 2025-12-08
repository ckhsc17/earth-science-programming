program fft_spectrum
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
  integer :: i, j, ios, total_size, idx
  integer :: pgopen
  real :: comp_mean
  
  ! FFT variables
  integer :: n_fft, nu, k
  real, allocatable :: freq(:)
  real, allocatable :: amplitude(:,:)
  real, allocatable :: xreal(:), ximag(:)
  real, allocatable :: filtered_wd(:,:)  ! Filtered waveform data
  real :: pi, df
  real :: amp
  real :: f_low, f_high  ! Filter frequency range
  
  ! Plotting variables
  real :: f_min, f_max, amp_min, amp_max
  
  pi = 4.0 * atan(1.0)
  
  write(*,*) '============================================='
  write(*,*) '  FFT Spectrum Analysis'
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
  
  ! FFT size: find power of 2 that is >= ndata
  ! Calculate NU such that 2^NU >= ndata
  nu = 0
  n_fft = 1
  do while (n_fft < wh%ndata)
    nu = nu + 1
    n_fft = n_fft * 2
  end do
  
  ! If n_fft > ndata, we need to pad with zeros
  write(*,*) 'FFT parameters:'
  write(*,'(A,I0)') '  Original data points: ', wh%ndata
  write(*,'(A,I0)') '  FFT size (2^NU): ', n_fft
  write(*,'(A,I0)') '  NU: ', nu
  write(*,'(A,F10.6,A)') '  Sampling rate: ', 1.0/wh%dt, ' Hz'
  
  ! Frequency resolution
  df = 1.0 / (real(n_fft) * wh%dt)
  write(*,'(A,F10.6,A)') '  Frequency resolution: ', df, ' Hz'
  write(*,'(A,F10.6,A)') '  Nyquist frequency: ', 1.0/(2.0*wh%dt), ' Hz'
  write(*,*) ''
  
  ! Filter frequency range
  f_low = 0.5   ! 0.5 Hz
  f_high = 3.0  ! 3.0 Hz
  write(*,*) 'Filter parameters:'
  write(*,'(A,F5.2,A,F5.2,A)') '  Keep frequencies: ', f_low, ' - ', f_high, ' Hz'
  write(*,*) ''
  
  ! Allocate frequency and amplitude arrays
  allocate(freq(n_fft))
  allocate(amplitude(wh%ncom, n_fft))
  allocate(xreal(n_fft))
  allocate(ximag(n_fft))
  allocate(filtered_wd(wh%ncom, wh%ndata))
  
  ! Calculate frequency array (0 to Nyquist frequency)
  do k = 1, n_fft
    freq(k) = real(k-1) * df
  end do
  
  ! Compute FFT, filter, and IFFT for each component
  write(*,*) 'Computing FFT, filtering, and IFFT for each component...'
  do j = 1, wh%ncom
    ! Initialize FFT arrays
    do i = 1, wh%ndata
      xreal(i) = wd(j, i)
      ximag(i) = 0.0
    end do
    ! Pad with zeros if needed
    if (n_fft > wh%ndata) then
      do i = wh%ndata + 1, n_fft
        xreal(i) = 0.0
        ximag(i) = 0.0
      end do
    end if
    
    ! ===== Forward FFT =====
    call FFT(xreal, ximag, n_fft, nu)
    
    ! Multiply by dt after FFT (as per user's notes)
    do k = 1, n_fft
      xreal(k) = xreal(k) * wh%dt
      ximag(k) = ximag(k) * wh%dt
    end do
    
    ! Calculate amplitude spectrum for plotting
    do k = 1, n_fft
      amp = sqrt(xreal(k)**2 + ximag(k)**2)
      amplitude(j, k) = amp
    end do
    
    ! ===== Frequency Filtering: Keep 0.5-3 Hz, zero out others =====
    ! For real signal FFT with N points:
    ! k=1: DC (0 Hz)
    ! k=2 to n_fft/2+1: positive frequencies (freq = (k-1)*df)
    ! k=n_fft/2+2 to n_fft: negative frequencies
    ! Negative frequency at k: freq = -(n_fft - k + 1)*df
    ! Conjugate symmetry: X[k] = conj(X[n_fft-k+2]) for k=2 to n_fft/2
    
    do k = 1, n_fft
      if (k == 1) then
        ! DC component (0 Hz) - zero out since it's outside 0.5-3 Hz
        xreal(k) = 0.0
        ximag(k) = 0.0
      else if (k <= n_fft/2 + 1) then
        ! Positive frequencies (including Nyquist if n_fft is even)
        if (freq(k) < f_low .or. freq(k) > f_high) then
          xreal(k) = 0.0
          ximag(k) = 0.0
        end if
      else
        ! Negative frequencies: k > n_fft/2+1
        ! Corresponding positive frequency index: n_fft - k + 2
        ! Filter negative frequencies to maintain conjugate symmetry
        if (n_fft - k + 2 <= n_fft/2 + 1) then
          ! Check if corresponding positive frequency is in range
          if (freq(n_fft - k + 2) < f_low .or. freq(n_fft - k + 2) > f_high) then
            xreal(k) = 0.0
            ximag(k) = 0.0
          end if
        else
          ! Should not reach here, but zero out to be safe
          xreal(k) = 0.0
          ximag(k) = 0.0
        end if
      end if
    end do
    
    ! ===== Inverse FFT =====
    ! Prepare for IFFT: 
    ! 1. Since we multiplied by dt after forward FFT, we need to undo that (divide by dt)
    ! 2. Negate imaginary part to prepare for IFFT: FFT(conj(X[k])) gives N*conj(x[n])
    !    Since x[n] is real, conj(x[n]) = x[n], so result is N*x[n]
    do k = 1, n_fft
      xreal(k) = xreal(k) / wh%dt
      ximag(k) = -ximag(k) / wh%dt
    end do
    
    ! Call FFT again for IFFT
    call FFT(xreal, ximag, n_fft, nu)
    
    ! Extract filtered waveform (only original data points)
    do i = 1, wh%ndata
      filtered_wd(j, i) = xreal(i)
    end do
    
    write(*,*) '  Component ', j, ' FFT -> Filter -> IFFT complete'
  end do
  write(*,*) ''
  
  deallocate(xreal)
  deallocate(ximag)
  
  ! Initialize PGPLOT for filtered waveform plot
  if (pgopen('filtered_waveform.ps/vcps') <= 0) then
    stop 'ERROR: Unable to open PostScript file'
  end if
  
  call pgsubp(1, 3)      ! Divide into 3 vertical subplots
  call pgslw(2)
  call pgsch(1.0)
  
  ! Plot filtered waveform for each component
  do j = 1, wh%ncom
    ! Calculate plot ranges for time domain
    f_min = time(1)
    f_max = time(wh%ndata)
    ! Calculate y-axis range and round to integer boundaries for tick interval of 1
    amp_min = minval(filtered_wd(j, 1:wh%ndata)) * 1.1
    amp_max = maxval(filtered_wd(j, 1:wh%ndata)) * 1.1
    ! Round down min and round up max to nearest integers for clean tick marks
    amp_min = floor(amp_min)
    amp_max = ceiling(amp_max)
    
    ! Set up plot window (without drawing axes to avoid duplicate labels)
    call pgswin(f_min, f_max, amp_min, amp_max)
    
    ! Draw axes with y-axis major tick interval of 1
    call pgsci(1)
    call pgbox('BCNST', 0.0, 0, 'BCNIT', 1.0, 0)
    
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
    
    ! Plot filtered waveform
    call pgsci(j+1)
    call pgline(wh%ndata, time(1:wh%ndata), filtered_wd(j, 1:wh%ndata))
    
    ! Reset to black
    call pgsci(1)
  end do
  
  ! End PGPLOT
  call pgend
  
  write(*,*) 'Filtered waveform plot complete. Output file: filtered_waveform.ps'
  write(*,*) ''
  
  ! Deallocate arrays
  deallocate(wd)
  deallocate(time)
  deallocate(freq)
  deallocate(amplitude)
  deallocate(filtered_wd)
  
contains
  
  !----------------------------------------------------------
  ! FFT Subroutine (完全按照提供的原始代碼)
  !----------------------------------------------------------
  subroutine FFT(XREAL, XIMAG, N, NU)
    integer, intent(in) :: N, NU
    real, intent(inout) :: XREAL(N), XIMAG(N)
    
    integer :: N2, NU1, K, L, I, K1, K1N2, I1
    real :: P, ARG, C, S, TREAL, TIMAG
    
    N2 = N / 2
    NU1 = NU - 1
    K = 0
    
    do L = 1, NU
      ! 對應原始代碼的標籤 102
      do
        do I = 1, N2
          ! 對應原始代碼的 DO 101 循環
          P = real(IBITR(K/2**NU1, NU))
          ARG = 6.283185 * P / real(N)
          C = cos(ARG)
          S = sin(ARG)
          K1 = K + 1
          K1N2 = K1 + N2
          
          TREAL = XREAL(K1N2) * C + XIMAG(K1N2) * S
          TIMAG = XIMAG(K1N2) * C - XREAL(K1N2) * S
          
          XREAL(K1N2) = XREAL(K1) - TREAL
          XIMAG(K1N2) = XIMAG(K1) - TIMAG
          XREAL(K1) = XREAL(K1) + TREAL
          XIMAG(K1) = XIMAG(K1) + TIMAG
          
          ! 對應原始代碼的標籤 101: K=K+1
          K = K + 1
        end do
        
        ! 對應原始代碼: K=K+N2, IF(K.LT.N) GOTO 102
        K = K + N2
        if (K < N) cycle
        exit
      end do
      
      ! 對應原始代碼的標籤 100
      K = 0
      NU1 = NU1 - 1
      N2 = N2 / 2
    end do
    
    ! 對應原始代碼的 DO 103 循環 (Bit reversal)
    do K = 1, N
      I1 = IBITR(K-1, NU) + 1
      if (I1 <= K) cycle  ! 對應原始代碼: IF(I.LE.K) GOTO 103
      
      TREAL = XREAL(K)
      TIMAG = XIMAG(K)
      XREAL(K) = XREAL(I1)
      XIMAG(K) = XIMAG(I1)
      XREAL(I1) = TREAL
      XIMAG(I1) = TIMAG
    end do
    
  end subroutine FFT
  
  !----------------------------------------------------------
  ! IBITR Function (完全按照提供的原始代碼)
  !----------------------------------------------------------
  integer function IBITR(J, NU)
    integer, intent(in) :: J, NU
    integer :: J1, J2, I
    
    J1 = J
    IBITR = 0
    
    ! 對應原始代碼的 DO 200 循環
    do I = 1, NU
      J2 = J1 / 2
      IBITR = IBITR * 2 + (J1 - 2 * J2)
      J1 = J2
    end do
    
  end function IBITR
  
end program fft_spectrum


