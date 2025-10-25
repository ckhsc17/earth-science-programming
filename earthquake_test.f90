program parse_quake
  implicit none
  character(len=200) :: line
  integer :: lat_deg, lon_deg
  real    :: lat_min, lon_min, latitude, longitude
  integer :: ios

  ! 測試資料
  line = '1999 1 1 054 41.662354.9512044.58 10.692.04 6  7.3174 .18  .8  .7 F 11C 01010054.P99'

  ! 直接跳過前 18 個字元，讀緯度與經度
  read(line,'(18X,I2,F5.2,I3,F5.2)', iostat=ios) lat_deg, lat_min, lon_deg, lon_min

  ! 轉換成十進制度數
  latitude  = lat_deg + lat_min/60.0
  longitude = lon_deg + lon_min/60.0

  ! 印出結果
  print *, 'Latitude :', latitude
  print *, 'Longitude:', longitude
end program parse_quake
