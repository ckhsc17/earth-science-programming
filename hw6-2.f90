program fit_ppfile_paxis
  implicit none
  integer, parameter :: nmax = 1000
  real :: x(nmax), y(nmax)
  real :: cept, slop, rms, r_cor, sdv
  integer :: n, i, mm, ios
  real :: xsec
  integer :: iy, im, id, ih

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

  call xyfit(x, y, n, cept, slop, rms, r_cor, sdv)

  print *, "Principal Axis Fit: Y = aX + b"
  print *, "a (slope)       = ", slop
  print *, "b (intercept)   = ", cept
  print *, "Std. deviation  = ", sdv
  print *, "Correlation R   = ", r_cor
  print *, "RMS residual    = ", rms

end program fit_ppfile_paxis


!============================================================
subroutine xyfit(x, y, ndata, cept, slop, rms, r_cor, sdv)
  implicit none
  integer, intent(in) :: ndata
  real, intent(in)  :: x(ndata), y(ndata)
  real, intent(out) :: cept, slop, rms, r_cor, sdv

  real :: sx, sy, sxx, syy, sxy
  real :: xmean, ymean
  integer :: i

  ! ---- 零均值共變異量 ----
  sx = sum(x(1:ndata))
  sy = sum(y(1:ndata))
  xmean = sx / ndata
  ymean = sy / ndata

  sxx = sum((x(1:ndata) - xmean)**2) / ndata
  syy = sum((y(1:ndata) - ymean)**2) / ndata
  sxy = sum((x(1:ndata) - xmean)*(y(1:ndata) - ymean)) / ndata

  ! ---- 主軸法斜率 ----
  slop = (syy - sxx + sqrt((sxx - syy)**2 + 4.0*sxy**2)) / (2.0*sxy)
  cept = ymean - slop * xmean

  ! ---- 擬合誤差 ----
  sdv = sqrt(sum((y(1:ndata) - (slop*x(1:ndata) + cept))**2) / (ndata - 1))
  rms = sqrt(sum((y(1:ndata) - (slop*x(1:ndata) + cept))**2) / ndata)

  ! ---- 相關係數 ----
  r_cor = sxy / sqrt(sxx * syy)

end subroutine xyfit
