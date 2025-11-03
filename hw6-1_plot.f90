program plot_eq_map
  implicit none
  integer, parameter :: nmax = 50000
  real :: eq_lat(nmax), eq_lon(nmax)
  real :: coast_lat(nmax), coast_lon(nmax)
  integer :: ne, nc, ios
  integer :: pgopen
  real :: lon_min, lon_max, lat_min, lat_max

  !------------------------------------------
  ! 讀取地震資料 (onland_eq.csv)
  ! 假設欄位：Latitude,Longitude
  !------------------------------------------
  open(10, file='onland_eq.csv', status='old', action='read')
  read(10,*)  ! skip header
  ne = 0
  do
    read(10,*, iostat=ios) eq_lat(ne+1), eq_lon(ne+1)
    if (ios /= 0) exit
    ne = ne + 1
  end do
  close(10)

  !------------------------------------------
  ! 讀取海岸線資料 (Taiwan.txt)
  ! 假設欄位：lon lat（空白分隔）
  !------------------------------------------
  open(20, file='Taiwan.txt', status='old', action='read')
  nc = 0
  do
    read(20,*, iostat=ios) coast_lon(nc+1), coast_lat(nc+1)
    if (ios /= 0) exit
    nc = nc + 1
  end do
  close(20)

  print *, "Read ", ne, " earthquake points"
  print *, "Read ", nc, " coastline points"

  lon_min = minval(coast_lon(1:nc))
  lon_max = maxval(coast_lon(1:nc))
  lat_min = minval(coast_lat(1:nc))
  lat_max = maxval(coast_lat(1:nc))

  !------------------------------------------
  ! 開啟繪圖
  !------------------------------------------
  if (pgopen('eq_map.ps/vcps') <= 0) stop "Error opening PGPLOT output"

  call pgenv(lon_min, lon_max, lat_min, lat_max, 0, 1)
  call pglabel('Longitude (°E)', 'Latitude (°N)', 'On-land Earthquakes (1999)')

  ! 畫海岸線
  call pgsci(1)
  call pgslw(2)
  call pgline(nc, coast_lon, coast_lat)

  ! 畫地震點
  call pgsci(2)
  call pgpt(ne, eq_lon, eq_lat, 17)  ! symbol 17 = 實心圓
  call pgend

  print *, "Output: eq_map.ps"
  print *, "Convert to PDF:"
  print *, "gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=eq_map.pdf eq_map.ps"

end program plot_eq_map
