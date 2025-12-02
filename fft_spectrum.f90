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
  real :: pi, df
  real :: amp
  
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
  
  ! Allocate frequency and amplitude arrays
  allocate(freq(n_fft))
  allocate(amplitude(wh%ncom, n_fft))
  allocate(xreal(n_fft))
  allocate(ximag(n_fft))
  
  ! Calculate frequency array (0 to Nyquist frequency)
  do k = 1, n_fft
    freq(k) = real(k-1) * df
  end do
  
  ! Compute FFT for each component
  write(*,*) 'Computing FFT for each component...'
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
    
    ! Call FFT subroutine
    call FFT(xreal, ximag, n_fft, nu)
    
    ! Calculate amplitude spectrum
    ! 直接使用 FFT 結果的模（不除以 N）
    ! 這樣 amplitude 會對應原始時域信號的累積效應
    do k = 1, n_fft
      amp = sqrt(xreal(k)**2 + ximag(k)**2)
      amplitude(j, k) = amp
    end do
    write(*,*) '  Component ', j, ' FFT complete'
  end do
  write(*,*) ''
  
  deallocate(xreal)
  deallocate(ximag)
  
  ! Initialize PGPLOT
  if (pgopen('fft_spectrum.ps/vcps') <= 0) then
    stop 'ERROR: Unable to open PostScript file'
  end if
  
  call pgsubp(1, 3)      ! Divide into 3 vertical subplots
  call pgslw(2)
  call pgsch(1.0)
  
  ! Plot each component's spectrum
  do j = 1, wh%ncom
    ! Calculate plot ranges
    f_min = 0.0
    f_max = 100.0  ! Plot up to 100 Hz
    amp_min = 0.0
    ! Find max amplitude up to 100 Hz
    k = min(n_fft, int(100.0 / df) + 1)
    amp_max = maxval(amplitude(j, 1:k)) * 1.1
    
    ! Set up plot environment
    if (j == 1) then
      call pgenv(f_min, f_max, amp_min, amp_max, 0, 0)
    else
      call pgenv(f_min, f_max, amp_min, amp_max, 0, 1)
    end if
    
    ! Set labels based on component
    if (j == 1) then
      call pglabel('Frequency (Hz)', 'Amplitude', 'Vertical Component Spectrum')
    else if (j == 2) then
      call pglabel('Frequency (Hz)', 'Amplitude', 'North-South Component Spectrum')
    else if (j == 3) then
      call pglabel('Frequency (Hz)', 'Amplitude', 'East-West Component Spectrum')
    else
      call pglabel('Frequency (Hz)', 'Amplitude', 'Component Spectrum')
    end if
    
    ! Draw axes
    call pgsci(1)
    call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
    
    ! Plot amplitude spectrum (up to 100 Hz)
    k = min(n_fft, int(100.0 / df) + 1)
    call pgsci(j+1)
    call pgline(k, freq(1:k), amplitude(j, 1:k))
    
    ! Reset to black
    call pgsci(1)
  end do
  
  ! End PGPLOT
  call pgend
  
  write(*,*) 'Plot complete. Output file: fft_spectrum.ps'
  write(*,*) ''
  
  ! Deallocate arrays
  deallocate(wd)
  deallocate(time)
  deallocate(freq)
  deallocate(amplitude)
  
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


