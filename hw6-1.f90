! onland_quakes.f90
! 讀 Taiwan.txt（lon,lat），讀 1999.lis（固定欄位），
! 用 locpt 判斷點是否在台灣多邊形內，輸出 onland_eq.csv

program onland_quakes
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none

  ! ---- 讀地震檔用 ----
  character(len=200) :: line
  integer :: lat_deg, lon_deg, ios, kept, npoly, l, m
  real(real64) :: lat_min, lon_min, lat_degdec, lon_degdec

  ! ---- 台灣海岸線（多邊形）----
  real(real64), allocatable :: xpoly(:), ypoly(:)  ! x=lon, y=lat

  ! 讀入海岸線（會自動在尾端補上首點，確保封閉）
  call read_coast('Taiwan.txt', xpoly, ypoly, npoly)

  ! 開啟輸入 / 輸出
  open(unit=10, file='1999.lis', status='old', action='read')
  open(unit=20, file='onland_eq.csv', status='replace', action='write')
  write(20,'(A)') 'Latitude,Longitude'   ! CSV 標頭

  kept = 0
  do
     read(10,'(A)', iostat=ios) line
     if (ios /= 0) exit   ! EOF 或讀取錯誤

     ! 依你提供的格式：跳過前 18 個字元；緯度: I2,F5.2；經度: I3,F5.2
     ! 注意：x0, y0 要給「lon, lat」順序，和多邊形座標一致
     read(line,'(18X,I2,F5.2,I3,F5.2)', iostat=ios) lat_deg, lat_min, lon_deg, lon_min
     if (ios /= 0) cycle   ! 這行不是目標資料則跳過

     ! 轉十進位度（依先前作業：min/60.0）
     lat_degdec = real(lat_deg,real64) + lat_min/60.0_real64
     lon_degdec = real(lon_deg,real64) + lon_min/60.0_real64

     ! 用多邊形測試（x0=lon, y0=lat）
     call locpt(lon_degdec, lat_degdec, xpoly, ypoly, npoly, l, m)

     if (l == 1) then   ! 1=inside
        write(20,'(F10.5,A,F11.5)') lat_degdec, ',', lon_degdec
        kept = kept + 1
     end if
  end do

  close(10); close(20)

  print *, 'Done. On-land events written = ', kept, ' -> onland_eq.csv'

contains

  !---------------------------------------------------------------
  subroutine read_coast(fname, x, y, n)
    !! 先數行數再配置，最後把首點複製到尾端，確保封閉
    character(len=*), intent(in) :: fname
    real(real64), allocatable, intent(out) :: x(:), y(:)
    integer, intent(out) :: n
    integer :: fh, cnt, ios, i
    real(real64) :: tx, ty

    fh = 11
    open(fh, file=fname, status='old', action='read')
    cnt = 0
    do
      read(fh,*,iostat=ios) tx, ty
      if (ios /= 0) exit
      cnt = cnt + 1
    end do
    if (cnt < 3) then
      print *, 'Taiwan.txt too short (need >=3 points).'
      stop
    end if
    rewind(fh)

    allocate(x(cnt+1), y(cnt+1))
    do i = 1, cnt
      read(fh,*,iostat=ios) x(i), y(i)
      if (ios /= 0) exit
    end do
    close(fh)

    ! 關閉多邊形：尾點=首點
    x(cnt+1) = x(1)
    y(cnt+1) = y(1)
    n = cnt + 1
  end subroutine read_coast

  !---------------------------------------------------------------
  subroutine locpt (x0, y0, x, y, n, l, m)
    !! 角度累加法（winding number）
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    integer, intent(in) :: n
    real(real64), intent(in) :: x0, y0, x(n), y(n)
    integer, intent(out) :: l, m

    integer :: i
    real(real64) :: eps, angle, sumang, th1, thi, u, v
    real(real64), parameter :: pi  = 3.1415926535897932384626433832795_real64
    real(real64), parameter :: pi2 = 6.2831853071795864769252867665590_real64
    real(real64), parameter :: tol = 1.0e-8_real64

    eps    = epsilon(1.0_real64)
    sumang = 0.0_real64
    l = -1; m = 0

    do i = 1, n-1
      u = x(i)   - x0;  v = y(i)   - y0;  th1 = atan2(v,u)
      u = x(i+1) - x0;  v = y(i+1) - y0;  thi = atan2(v,u)

      angle = thi - th1
      if (angle >  pi) angle = angle - pi2
      if (angle < -pi) angle = angle + pi2

      sumang = sumang + angle
    end do

    if (abs(sumang) <= tol) then
      l = -1           ! outside
      m = 0
    else
      l = 1            ! inside
      m = nint(sumang/pi2)
    end if
  end subroutine locpt

end program onland_quakes
