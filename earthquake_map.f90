! PGPLOT 地震分布圖程式
! 繪製台灣地震分布：海岸線 + 不同規模大小 + 不同深度顏色
program earthquake_distribution_map
  implicit none
  
  ! 地震資料變數
  integer, parameter :: max_eq = 50000, max_coast = 2000
  real :: eq_lon(max_eq), eq_lat(max_eq), eq_depth(max_eq), eq_mag(max_eq)
  real :: coast_lon(max_coast), coast_lat(max_coast)
  integer :: n_eq, n_coast, i, ios
  character(len=200) :: line
  
  ! 地震資料解析變數
  integer :: lat_deg, lon_deg
  real :: lat_min, lon_min, depth, magnitude
  real :: latitude, longitude
  
  ! 繪圖變數
  integer :: pgopen
  real :: map_lon_min, map_lon_max, map_lat_min, map_lat_max
  real :: symbol_size
  integer :: symbol_type, color_int
  
  ! 圖例變數
  real :: legend_x, legend_y, legend_spacing
  
  write(*,*) 'Reading earthquake data from 1999.lis...'
  
  ! 讀取地震資料
  open(unit=10, file='1999.lis', status='old', action='read')
  n_eq = 0
  
  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    
    ! 使用與hw2-2.f90相同的方式解析經緯度
    read(line,'(18X,I2,F5.2,I3,F5.2)', iostat=ios) lat_deg, lat_min, lon_deg, lon_min
    if (ios /= 0) cycle  ! 若該行不是資料，跳過
    
    ! 轉換為十進位度
    latitude = lat_deg + lat_min/60.0
    longitude = lon_deg + lon_min/60.0
    
    ! 嘗試讀取深度和規模 (根據1999.lis的實際格式)
    ! 深度在第37-42位置，規模在第43-46位置
    read(line(37:42), '(F6.2)', iostat=ios) depth
    if (ios /= 0) then
      depth = 10.0  ! 預設值
    end if
    
    read(line(43:46), '(F4.2)', iostat=ios) magnitude
    if (ios /= 0) then
      magnitude = 2.0  ! 預設值
    end if
    
    ! 檢查合理範圍 (台灣附近)
    if (latitude >= 21.0 .and. latitude <= 26.0 .and. &
        longitude >= 119.0 .and. longitude <= 123.0) then
      
      n_eq = n_eq + 1
      eq_lat(n_eq) = latitude
      eq_lon(n_eq) = longitude
      eq_depth(n_eq) = depth
      eq_mag(n_eq) = magnitude
      
      ! 印出前5筆資料以便驗證
      if (n_eq <= 5) then
        write(*,'(A,I0,A,F8.4,A,F9.4,A,F6.2,A,F4.2)') &
             'Event ', n_eq, ': Lat=', latitude, ', Lon=', longitude, &
             ', Depth=', depth, ', Mag=', magnitude
      end if
    end if
    
    if (n_eq >= max_eq) exit
  end do
  close(10)
  
  write(*,'(A,I0,A)') 'Successfully read ', n_eq, ' earthquake events'
  
  ! 讀取台灣海岸線
  write(*,*) 'Reading Taiwan coastline from Taiwan.txt...'
  open(unit=11, file='Taiwan.txt', status='old', action='read')
  n_coast = 0
  
  do
    read(11, *, iostat=ios) coast_lon(n_coast+1), coast_lat(n_coast+1)
    if (ios /= 0) exit
    n_coast = n_coast + 1
    if (n_coast >= max_coast) exit
  end do
  close(11)
  
  write(*,'(A,I0,A)') 'Successfully read ', n_coast, ' coastline points'
  
  ! 設定座標範圍
  map_lon_min = 119.5
  map_lon_max = 122.5
  map_lat_min = 21.5
  map_lat_max = 25.5
  
  ! 初始化 PGPLOT
  if (pgopen('earthquake_map.ps/vcps') <= 0) then
    stop 'ERROR: Cannot open PostScript file'
  end if
  
  call pgslw(1)
  call pgsch(1.0)
  
  ! 設定繪圖環境
  call pgenv(map_lon_min, map_lon_max, map_lat_min, map_lat_max, 1, 0)
  call pglabel('Longitude (°E)', 'Latitude (°N)', &
               'Taiwan Earthquake Distribution Map (1999)')
  
  ! 繪製台灣海岸線 (黑色)
  call pgsci(1)  ! 黑色
  call pgslw(2)  ! 較粗線條
  call pgline(n_coast, coast_lon(1:n_coast), coast_lat(1:n_coast))
  call pgslw(1)  ! 恢復正常線寬
  
  ! 繪製地震事件
  do i = 1, n_eq
    ! 根據深度設定顏色 (改善顏色對比)
    if (eq_depth(i) <= 30.0) then
      color_int = 2      ! 紅色 (淺層)
    else if (eq_depth(i) <= 70.0) then
      color_int = 3      ! 綠色 (中層)
    else if (eq_depth(i) <= 150.0) then
      color_int = 4      ! 藍色 (中深層)
    else
      color_int = 5      ! 青色 (深層)
    end if
    call pgsci(color_int)
    
    ! 根據規模設定符號大小 (改善大小對比)
    if (eq_mag(i) <= 3.0) then
      symbol_size = 0.4
      symbol_type = 1    ! 實心點
    else if (eq_mag(i) <= 4.0) then
      symbol_size = 0.7
      symbol_type = 1
    else if (eq_mag(i) <= 5.0) then
      symbol_size = 1.1
      symbol_type = 1
    else
      symbol_size = 1.6
      symbol_type = 1    ! 大地震用更大符號
    end if
    
    call pgsch(symbol_size)
    call pgpt(1, eq_lon(i), eq_lat(i), symbol_type)
  end do
  
  ! 繪製圖例 (左下角，較小字體)
  call pgsci(1)  ! 黑色
  call pgsch(0.7)  ! 較小字體
  
  ! 深度圖例 (左下角)
  legend_x = map_lon_min + 0.05
  legend_y = map_lat_min + 1.8
  legend_spacing = 0.15
  
  call pgtext(legend_x, legend_y, 'Focal Depth (km):')
  
  call pgsci(2)
  call pgsch(0.5)
  call pgpt(1, legend_x + 0.05, legend_y - legend_spacing, 1)
  call pgsci(1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - legend_spacing, '0-30')
  
  call pgsci(3)
  call pgsch(0.5)
  call pgpt(1, legend_x + 0.05, legend_y - 2*legend_spacing, 1)
  call pgsci(1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 2*legend_spacing, '30-70')
  
  call pgsci(4)
  call pgsch(0.5)
  call pgpt(1, legend_x + 0.05, legend_y - 3*legend_spacing, 1)
  call pgsci(1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 3*legend_spacing, '70-150')
  
  call pgsci(5)
  call pgsch(0.5)
  call pgpt(1, legend_x + 0.05, legend_y - 4*legend_spacing, 1)
  call pgsci(1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 4*legend_spacing, '>150')
  
  ! 規模圖例 (左下角右側)
  legend_x = map_lon_min + 0.8
  legend_y = map_lat_min + 1.8
  
  call pgsch(0.7)
  call pgtext(legend_x, legend_y, 'Magnitude:')
  
  call pgsci(1)
  call pgsch(0.3)
  call pgpt(1, legend_x + 0.05, legend_y - legend_spacing, 1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - legend_spacing, 'M≤3.0')
  
  call pgsch(0.5)
  call pgpt(1, legend_x + 0.05, legend_y - 2*legend_spacing, 1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 2*legend_spacing, '3.0<M≤4.0')
  
  call pgsch(0.8)
  call pgpt(1, legend_x + 0.05, legend_y - 3*legend_spacing, 1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 3*legend_spacing, '4.0<M≤5.0')
  
  call pgsch(1.2)
  call pgpt(1, legend_x + 0.05, legend_y - 4*legend_spacing, 1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 4*legend_spacing, 'M>5.0')
  
  ! 添加統計資訊 (右下角)
  call pgsch(0.6)
  call pgtext(map_lon_max - 0.8, map_lat_min + 0.1, &
              'Total Events: ' // trim(adjustl(int_to_string(n_eq))))
  
  ! 結束 PGPLOT
  call pgend
  
  write(*,*) 'Map plotting completed!'
  write(*,*) 'Output file: earthquake_map.ps'
  write(*,*) 'Convert to PDF with:'
  write(*,*) 'gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite', &
             ' -sOutputFile=earthquake_map.pdf earthquake_map.ps'

contains

  ! 將整數轉換為字串
  function int_to_string(val) result(str)
    integer, intent(in) :: val
    character(len=20) :: str
    write(str, '(I0)') val
    str = adjustl(str)
  end function int_to_string

end program earthquake_distribution_map