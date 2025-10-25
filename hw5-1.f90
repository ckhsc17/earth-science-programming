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
  
  ! 統計變數
  integer :: total_lines, skipped_format, skipped_range
  integer :: line_number
  
  ! 繪圖變數
  integer :: pgopen
  real :: map_lon_min, map_lon_max, map_lat_min, map_lat_max
  real :: symbol_size
  integer :: color_int
  
  ! 圖例變數
  real :: legend_x, legend_y, legend_spacing
  
  write(*,*) 'Reading earthquake data from 1999.lis...'
  
  ! 讀取地震資料
  open(unit=10, file='1999.lis', status='old', action='read')
  n_eq = 0
  total_lines = 0
  skipped_format = 0
  skipped_range = 0
  line_number = 0
  
  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    
    line_number = line_number + 1
    total_lines = total_lines + 1
    
    ! 使用hw2-2.f90完全相同的解析方法
    read(line,'(18X,I2,F5.2,I3,F5.2)', iostat=ios) lat_deg, lat_min, lon_deg, lon_min
    if (ios /= 0) then
      skipped_format = skipped_format + 1
      if (skipped_format <= 5) then  ! 只顯示前5個格式錯誤
        write(*,'(A,I0,A,A)') 'Line ', line_number, ' format error: ', trim(line)
      end if
      cycle  ! 若該行不是資料，跳過
    end if
    
    ! 轉換成十進制度數
    latitude = lat_deg + lat_min/60.0
    longitude = lon_deg + lon_min/60.0
    
    ! 嘗試讀取深度和規模
    ! 從範例資料分析: 1999 1 1 032 26.482252.5712041.46 15.971.82 5 15.6190 .42 2.6 3.0
    read(line(34:39), *, iostat=ios) depth
    if (ios /= 0) depth = 15.0  ! 預設深度
    
    read(line(40:43), *, iostat=ios) magnitude  
    if (ios /= 0) magnitude = 2.5  ! 預設規模
    
    ! 基本範圍檢查
    if (latitude >= 18.0 .and. latitude <= 29.0 .and. &
        longitude >= 115.0 .and. longitude <= 125.0 .and. &
        magnitude >= 0.0 .and. depth >= 0.0) then
      
      n_eq = n_eq + 1
      eq_lat(n_eq) = latitude
      eq_lon(n_eq) = longitude
      eq_depth(n_eq) = depth
      eq_mag(n_eq) = magnitude
      
      ! 顯示前10筆資料用於除錯
      if (n_eq <= 10) then
        write(*,'(A,I3,A,I0,A,F7.3,A,F8.3,A,F6.2,A,F4.2)') &
               'Event ', n_eq, ' (Line ', line_number, '): Lat=', latitude, &
               ' Lon=', longitude, ' Depth=', depth, ' Mag=', magnitude
      end if
    else
      skipped_range = skipped_range + 1
      if (skipped_range <= 10) then  ! 只顯示前10個範圍錯誤
        write(*,'(A,I0,A,F7.3,A,F8.3,A,F6.2,A,F4.2,A)') &
               'Line ', line_number, ' out of range: Lat=', latitude, &
               ' Lon=', longitude, ' Depth=', depth, ' Mag=', magnitude, ' - SKIPPED'
      end if
    end if
    
    if (n_eq >= max_eq) exit
  end do
  close(10)
  
  write(*,*) '================== 讀取統計 =================='
  write(*,'(A,I0)') 'Total lines read: ', total_lines
  write(*,'(A,I0)') 'Skipped (format error): ', skipped_format  
  write(*,'(A,I0)') 'Skipped (out of range): ', skipped_range
  write(*,'(A,I0)') 'Valid earthquake events: ', n_eq
  write(*,'(A,I0)') 'Expected total (lines - format errors): ', total_lines - skipped_format
  write(*,'(A,I0)') 'Missing events: ', (total_lines - skipped_format) - n_eq - skipped_range
  write(*,*) '=============================================='
  
  if (n_eq == 0) then
    write(*,*) 'No earthquake data found. Please check the file format.'
    stop
  end if
  
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
    ! 根據深度設定顏色
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
    
    ! 根據規模設定符號大小
    if (eq_mag(i) <= 3.0) then
      symbol_size = 1.0
    else if (eq_mag(i) <= 4.0) then
      symbol_size = 1.3
    else if (eq_mag(i) <= 5.0) then
      symbol_size = 1.7
    else
      symbol_size = 2.0
    end if
    
    call pgsch(symbol_size)
    call pgpt(1, eq_lon(i), eq_lat(i), 1)  ! 實心圓點
  end do
  
  ! 繪製圖例
  call pgsci(1)  ! 黑色
  call pgsch(0.5) 
  
  ! 深度圖例 (帶顏色示意點)
  legend_x = map_lon_min + 0.1
  legend_y = map_lat_min + 0.85
  legend_spacing = 0.15
  
  call pgsch(0.7)
  call pgtext(legend_x, legend_y, 'Focal Depth (km):')
  
  ! 0-30 km (紅色)
  call pgsci(2)  ! 紅色
  call pgsch(4.0)  ! 適當大小的點
  call pgpt(1, legend_x + 0.05, legend_y - legend_spacing, 1)  ! 實心圓點
  call pgsci(1)  ! 黑色文字
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - legend_spacing, '0-30')
  
  ! 30-70 km (綠色)
  call pgsci(3)  ! 綠色
  call pgsch(4.0)
  call pgpt(1, legend_x + 0.05, legend_y - 2*legend_spacing, 1)
  call pgsci(1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 2*legend_spacing, '30-70')
  
  ! 70-150 km (藍色)
  call pgsci(4)  ! 藍色
  call pgsch(4.0)
  call pgpt(1, legend_x + 0.05, legend_y - 3*legend_spacing, 1)
  call pgsci(1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 3*legend_spacing, '70-150')
  
  ! >150 km (青色)
  call pgsci(5)  ! 青色
  call pgsch(4.0)
  call pgpt(1, legend_x + 0.05, legend_y - 4*legend_spacing, 1)
  call pgsci(1)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 4*legend_spacing, '>150')
  
  ! 規模圖例 (帶大小示意的空心圓圈)
  legend_x = map_lon_min + 0.7
  
  call pgsch(0.7)
  call pgtext(legend_x, legend_y, 'Magnitude:')
  
  call pgsci(1)  ! 黑色
  
  ! M≤3.0 (小空心圓)
  call pgsch(0.6)  ! 對應程式中的 symbol_size = 1.0
  call pgpt(1, legend_x + 0.05, legend_y - legend_spacing, 4)  ! 4 = 空心圓圈
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - legend_spacing, 'M<3.0')
  
  ! 3.0<M≤4.0 (中小空心圓)
  call pgsch(0.9)  ! 對應程式中的 symbol_size = 1.5
  call pgpt(1, legend_x + 0.05, legend_y - 2*legend_spacing, 4)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 2*legend_spacing, '3.0<M<4.0')
  
  ! 4.0<M≤5.0 (中大空心圓)
  call pgsch(1.1)  ! 對應程式中的 symbol_size = 2.0
  call pgpt(1, legend_x + 0.05, legend_y - 3*legend_spacing, 4)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 3*legend_spacing, '4.0<M<5.0')
  
  ! M>5.0 (大空心圓)
  call pgsch(1.3)  ! 對應程式中的 symbol_size = 2.5
  call pgpt(1, legend_x + 0.05, legend_y - 4*legend_spacing, 4)
  call pgsch(0.6)
  call pgtext(legend_x + 0.15, legend_y - 4*legend_spacing, 'M>5.0')
  
  ! 統計資訊
  call pgsch(0.6)
  call pgtext(map_lon_max - 1.0, map_lat_min + 0.2, &
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