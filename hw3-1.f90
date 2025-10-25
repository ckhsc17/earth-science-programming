program fit_ppfile
  implicit none
  integer, parameter :: nmax = 1000
  real :: x(nmax), y(nmax)
  real :: sx, sy, sxx, syy, sxy
  real :: a, b, sigma, r
  real :: sa, sb
  real :: delta, S
  real :: denom
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
     y(i) = mm*60.0 + y(i) - xsec   ! 轉成走時
  end do
  close(1)

  print *, "Total points read: ", n

  sx = sum(x(1:n))
  sy = sum(y(1:n))
  sxx = sum(x(1:n)**2)
  syy = sum(y(1:n)**2)
  sxy = sum(x(1:n)*y(1:n))

  denom = n*sxx - sx**2
  if (denom == 0.0) stop "Denominator zero, cannot fit"

  ! 直線係數
  a = (n*sxy - sx*sy) / denom
  b = (sy*sxx - sx*sxy) / denom

  ! 殘差標準差
  sigma = sqrt(sum((y(1:n) - (a*x(1:n) + b))**2) / (n-1))

  ! 決定係數 R
  r = (n*sxy - sx*sy) / sqrt((n*sxx - sx**2)*(n*syy - sy**2))

  ! a, b 的標準差
  ! sa = sigma * sqrt(n / denom)
  ! sb = sigma * sqrt(sxx / denom)
  delta = n*sxx - sx**2
  sa = sqrt((sxx / delta))
  sb = sqrt((n / delta)) 

  print *, "Fitted line: Y = aX + b"
  print *, "a (slope)       = ", a
  print *, "b (intercept)   = ", b
  print *, "Std. deviation  = ", sigma
  print *, "Correlation R   = ", r
  print *, "Std. dev of a   = ", sa
  print *, "Std. dev of b   = ", sb

end program fit_ppfile
