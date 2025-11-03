program fit_ppfile_paxis
  implicit none
  integer, parameter :: nmax = 1000
  real :: x(nmax), y(nmax)
  real :: cept, slop, rms, r_cor, sdv
  integer :: n, i, mm, ios
  real :: xsec
  integer :: iy, im, id, ih

  ! PGPLOT variables
  integer :: pgopen
  real :: x_min, x_max, y_min, y_max
  real :: x_plot(2), y_plot(2)
  character(len=80) :: eq_text, r_text, std_text, n_text

  !--------------------------------------------
  ! 讀取 ppfile.txt
  !--------------------------------------------
  open(unit=1, file="ppfile.txt", status="old", action="read")
  read(1,'(1x,i4,4i2,f6.2)', iostat=ios) iy, im, id, ih, mm, xsec
  if (ios /= 0) stop "Error reading header"
  xsec = mm*60.0 + xsec

  n = 0
  do i = 1, nmax
     read(1,'(5x,f6.1,9x,i3,f6.2)', iostat=ios) x(i), mm, y(i)
     if (ios /= 0) exit
     n = n + 1
     y(i) = mm*60.0 + y(i) - xsec
  end do
  close(1)

  print *, "Total points read: ", n
  if (n < 2) stop "Not enough data points"

  !--------------------------------------------
  ! 主軸法擬合
  !--------------------------------------------
  call xyfit(x, y, n, cept, slop, rms, r_cor, sdv)

  print *, "Principal Axis Fit: Y = aX + b"
  print *, "a (slope)       = ", slop
  print *, "b (intercept)   = ", cept
  print *, "Std. deviation  = ", sdv
  print *, "Correlation R   = ", r_cor
  print *, "RMS residual    = ", rms

  !--------------------------------------------
  ! PGPLOT 繪圖輸出
  !--------------------------------------------
  if (pgopen('paxis_fit_plot.ps/vcps') <= 0) then
     stop 'ERROR: cannot open PostScript output'
  end if

  call pgslw(2)
  call pgsch(1.2)

  x_min = minval(x(1:n)) * 0.9
  x_max = maxval(x(1:n)) * 1.1
  y_min = minval(y(1:n)) * 0.9
  y_max = maxval(y(1:n)) * 1.1

  call pgenv(x_min, x_max, y_min, y_max, 0, 1)
  call pglabel('Epicentral Distance (km)', 'P-wave Travel Time (s)', &
               'Principal Axis Fit: P-wave Travel Time vs Epicentral Distance')

  ! 資料點
  call pgsci(4)
  call pgpt(n, x(1:n), y(1:n), 17)

  ! 擬合直線
  call pgsci(1)
  call pgslw(3)
  x_plot(1) = x_min
  x_plot(2) = x_max
  y_plot(1) = slop*x_min + cept
  y_plot(2) = slop*x_max + cept
  call pgline(2, x_plot, y_plot)

  ! ±1σ 虛線
  call pgsls(2)
  call pgslw(2)
  y_plot(1) = slop*x_min + cept + sdv
  y_plot(2) = slop*x_max + cept + sdv
  call pgline(2, x_plot, y_plot)
  y_plot(1) = slop*x_min + cept - sdv
  y_plot(2) = slop*x_max + cept - sdv
  call pgline(2, x_plot, y_plot)

  call pgsls(1)
  call pgslw(1)
  call pgsci(1)

  !--------------------------------------------
  ! 標註文字統計資訊
  !--------------------------------------------
  write(eq_text, '(A,F6.3,A,F6.2)') 'Y = ', slop, 'X + ', cept
  call pgtext(x_min+(x_max-x_min)*0.05, y_max-(y_max-y_min)*0.08, trim(eq_text))

  write(r_text, '(A,F5.3,A,F5.3)') 'R = ', r_cor, ', R² = ', r_cor**2
  call pgtext(x_min+(x_max-x_min)*0.05, y_max-(y_max-y_min)*0.13, trim(r_text))

  write(std_text, '(A,F5.2,A)') 'Std Dev = ', sdv, ' s'
  call pgtext(x_min+(x_max-x_min)*0.05, y_max-(y_max-y_min)*0.18, trim(std_text))

  write(n_text, '(A,I0,A)') 'N = ', n, ' stations'
  call pgtext(x_min+(x_max-x_min)*0.05, y_max-(y_max-y_min)*0.23, trim(n_text))

  call pgend

  print *, "Plot completed -> paxis_fit_plot.ps"
  print *, "Convert to PDF:"
  print *, "gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=paxis_fit_plot.pdf paxis_fit_plot.ps"

end program fit_ppfile_paxis


!============================================================
subroutine xyfit(x, y, ndata, cept, slop, rms, r_cor, sdv)
  implicit none
  integer, intent(in) :: ndata
  real, intent(in)  :: x(ndata), y(ndata)
  real, intent(out) :: cept, slop, rms, r_cor, sdv

  real :: sx, sy, sxx, syy, sxy
  real :: xmean, ymean

  sx = sum(x(1:ndata))
  sy = sum(y(1:ndata))
  xmean = sx / ndata
  ymean = sy / ndata

  sxx = sum((x(1:ndata) - xmean)**2) / ndata
  syy = sum((y(1:ndata) - ymean)**2) / ndata
  sxy = sum((x(1:ndata) - xmean)*(y(1:ndata) - ymean)) / ndata

  slop = (syy - sxx + sqrt((sxx - syy)**2 + 4.0*sxy**2)) / (2.0*sxy)
  cept = ymean - slop * xmean

  sdv = sqrt(sum((y(1:ndata) - (slop*x(1:ndata) + cept))**2) / (ndata - 1))
  rms = sqrt(sum((y(1:ndata) - (slop*x(1:ndata) + cept))**2) / ndata)
  r_cor = sxy / sqrt(sxx * syy)
end subroutine xyfit
