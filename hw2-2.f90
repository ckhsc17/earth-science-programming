program parse_quake_csv
  implicit none
  character(len=200) :: line
  integer :: lat_deg, lon_deg
  real    :: lat_min, lon_min, latitude, longitude
  integer :: ios, count

  ! 開啟輸入檔 (地震目錄)
  open(unit=10, file='1999.lis', status='old', action='read')

  ! 開啟輸出檔 (CSV)
  open(unit=20, file='earthquake.csv', status='replace', action='write')

  ! 寫入 CSV 標題
  write(20,'(A)') 'Latitude,Longitude'

  count = 0

  ! 逐行讀取
  do
     read(10,'(A)', iostat=ios) line
     if (ios /= 0) exit  ! EOF or error

     ! 嘗試解析經緯度
     read(line,'(18X,I2,F5.2,I3,F5.2)', iostat=ios) lat_deg, lat_min, lon_deg, lon_min
     if (ios /= 0) cycle  ! 若該行不是資料，跳過

     ! 轉換成十進制度數
     latitude  = lat_deg + lat_min/60.0
     longitude = lon_deg + lon_min/60.0

     ! 寫入 CSV
     write(20,'(F10.5,A,F11.5)') latitude, ',', longitude

     count = count + 1
  end do

  close(10)
  close(20)

  print *, '處理完成，總共寫入 ', count, ' 筆資料到 quake_latlon.csv'

end program parse_quake_csv
