program seismic_waveform_plot
  implicit none
  
  integer, parameter :: max_points = 10000
  real :: time(max_points), vert_acc(max_points), ns_acc(max_points), ew_acc(max_points)
  real :: vert_mean, ns_mean, ew_mean
  real :: vert_peak, ns_peak, ew_peak
  real :: time_vert_peak, time_ns_peak, time_ew_peak
  integer :: n_points, i, ios
  integer :: pgopen
  real :: t_min, t_max, acc_min, acc_max

  ! ---- Read data file ----
  open(unit=10, file='seisdata.txt', status='old', action='read')
  n_points = 0
  do
    read(10, *, iostat=ios) time(n_points+1), vert_acc(n_points+1), ns_acc(n_points+1), ew_acc(n_points+1)
    if (ios /= 0) exit
    n_points = n_points + 1
    if (n_points >= max_points) exit
  end do
  close(10)
  write(*,*) 'Successfully read', n_points, 'data points'

  ! ---- Compute and remove means ----
  vert_mean = sum(vert_acc(1:n_points)) / real(n_points)
  ns_mean   = sum(ns_acc(1:n_points)) / real(n_points)
  ew_mean   = sum(ew_acc(1:n_points)) / real(n_points)

  do i = 1, n_points
    vert_acc(i) = vert_acc(i) - vert_mean
    ns_acc(i)   = ns_acc(i) - ns_mean
    ew_acc(i)   = ew_acc(i) - ew_mean
  end do

  ! ---- Find peaks ----
  vert_peak = vert_acc(1); ns_peak = ns_acc(1); ew_peak = ew_acc(1)
  time_vert_peak = time(1); time_ns_peak = time(1); time_ew_peak = time(1)

  do i = 2, n_points
    if (abs(vert_acc(i)) > abs(vert_peak)) then
      vert_peak = vert_acc(i); time_vert_peak = time(i)
    end if
    if (abs(ns_acc(i)) > abs(ns_peak)) then
      ns_peak = ns_acc(i); time_ns_peak = time(i)
    end if
    if (abs(ew_acc(i)) > abs(ew_peak)) then
      ew_peak = ew_acc(i); time_ew_peak = time(i)
    end if
  end do

  ! ---- Initialize PGPLOT ----
  if (pgopen('seismic_waveform.ps/cps') <= 0) then
    stop 'ERROR: Unable to open PostScript file'
  end if

  call pgsubp(1,3)
  call pgslw(2)
  call pgsch(1.0)

  t_min = minval(time(1:n_points))
  t_max = maxval(time(1:n_points))

  ! ---------------- Vertical Component ----------------
  acc_min = minval(vert_acc(1:n_points)) * 1.1
  acc_max = maxval(vert_acc(1:n_points)) * 1.1
  call pgenv(t_min, t_max, acc_min, acc_max, 0, 0)

  call pgsci(1)                                     ! black for axes and text
  call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
  call pglabel('Time (s)', 'Acceleration (mean removed)', 'Vertical Component')

  call pgsci(2)                                     ! red for data
  call pgline(n_points, time(1:n_points), vert_acc(1:n_points))
  call pgpt(1, time_vert_peak, vert_peak, 12)
  call pgsci(1)                                     ! reset to black

  ! ---------------- North-South Component ----------------
  acc_min = minval(ns_acc(1:n_points)) * 1.1
  acc_max = maxval(ns_acc(1:n_points)) * 1.1
  call pgenv(t_min, t_max, acc_min, acc_max, 0, 0)

  call pgsci(1)
  call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
  call pglabel('Time (s)', 'Acceleration (mean removed)', 'North-South Component')

  call pgsci(3)
  call pgline(n_points, time(1:n_points), ns_acc(1:n_points))
  call pgpt(1, time_ns_peak, ns_peak, 12)
  call pgsci(1)

  ! ---------------- East-West Component ----------------
  acc_min = minval(ew_acc(1:n_points)) * 1.1
  acc_max = maxval(ew_acc(1:n_points)) * 1.1
  call pgenv(t_min, t_max, acc_min, acc_max, 0, 0)

  call pgsci(1)
  call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
  call pglabel('Time (s)', 'Acceleration (mean removed)', 'East-West Component')

  call pgsci(4)
  call pgline(n_points, time(1:n_points), ew_acc(1:n_points))
  call pgpt(1, time_ew_peak, ew_peak, 12)
  call pgsci(1)

  ! ---- End PGPLOT ----
  call pgend

  write(*,*) 'Plot complete. Output file: seismic_waveform.ps'
  write(*,*) 'Convert to PDF with:'
  write(*,*) 'gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -r600 -sOutputFile=seismic_waveform.pdf seismic_waveform.ps'

end program seismic_waveform_plot
