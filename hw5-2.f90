! 線性回歸分析程式：震央距離 vs P波到時
! 讀取 ppfile.txt，進行 Y = aX + b 的線性擬合
program linear_regression_analysis
  implicit none
  
  integer, parameter :: max_stations = 1000
  real :: epicentral_dist(max_stations), p_time(max_stations)
  integer :: n_data, i, ios
  
  ! 時間解析變數
  integer :: iy, im, id, ih, mm
  real :: xsec
  
  ! 統計變數
  real :: sum_x, sum_y, sum_xy, sum_x2, sum_y2
  real :: mean_x, mean_y
  real :: a, b  ! 線性回歸係數 Y = aX + b
  real :: r_correlation  ! 相關係數
  real :: std_dev_fit    ! 擬合標準差
  real :: std_dev_a, std_dev_b  ! 係數標準差
  real :: y_predicted, residual
  real :: sum_residual_sq
  
  ! PGPLOT 變數
  integer :: pgopen
  real :: x_min, x_max, y_min, y_max
  real :: x_plot(2), y_plot(2)
  
  ! 字串變數 (用於圖上顯示)
  character(len=80) :: eq_text, r_text, std_text, n_text
  
  ! 初始化
  n_data = 0
  
  ! 讀取資料檔案
  open(unit=10, file='ppfile.txt', status='old', action='read')
  
  ! 讀取第一行：時間資訊 
  read(10,'(1x,i4,4i2,f6.2)', iostat=ios) iy, im, id, ih, mm, xsec
  if (ios /= 0) stop "Error reading header"
  
  write(*,'(A,I4,A,I2,A,I2,A,I2,A,I2,A,F6.2)') &
        'Original header: ', iy, '/', im, '/', id, ' ', ih, ':', mm, ':', xsec
  
  ! 轉換成總秒數
  xsec = mm*60.0 + xsec
  
  write(*,'(A,F8.2,A)') 'Event origin time: ', xsec, ' seconds from hour start'
  
  ! 讀取測站資料 (完全參考 hw3-1.f90 的格式)
  do i = 1, max_stations
    read(10,'(5x,f6.1,9x,i3,f6.2)', iostat=ios) &
         epicentral_dist(i), mm, p_time(i)
    if (ios /= 0) exit
    
    n_data = n_data + 1
    
    ! 轉換成走時 (相對於地震發生時間) - 與 hw3-1.f90 相同
    p_time(i) = mm*60.0 + p_time(i) - xsec
    
    ! 印出詳細資料以供除錯 (前15筆)
    if (n_data <= 15) then
      write(*,'(A,I3,A,F6.1,A,I3,A,F6.2,A,F8.2,A,F8.2)') &
           'Station ', n_data, ': Dist=', epicentral_dist(i), &
           ' km, mm=', mm, ', raw_sec=', p_time(i) + xsec, &
           ', origin_time=', xsec, ', travel_time=', p_time(i)
      
      ! 進一步驗證計算
      write(*,'(A,F8.2,A,F8.2,A,F8.2)') &
           '  Verification: ', mm*60.0, ' + ', p_time(i) + xsec, &
           ' - ', xsec, ' = ', mm*60.0 + p_time(i) + xsec - xsec
    end if
    
    if (n_data >= max_stations) exit
  end do
  close(10)
  
  write(*,'(A,I0,A)') 'Successfully read ', n_data, ' valid data points'
  
  if (n_data < 2) then
    write(*,*) 'Too few data points for linear regression'
    stop
  end if
  
  ! 顯示資料範圍統計
  write(*,*) '============== Data Statistics =============='
  write(*,'(A,F8.2,A,F8.2)') 'Distance range: ', minval(epicentral_dist(1:n_data)), &
           ' - ', maxval(epicentral_dist(1:n_data))
  write(*,'(A,F8.2,A,F8.2)') 'Travel time range: ', minval(p_time(1:n_data)), &
           ' - ', maxval(p_time(1:n_data))  
  write(*,*) '=============================================='
  
  ! 計算統計量
  sum_x = sum(epicentral_dist(1:n_data))
  sum_y = sum(p_time(1:n_data))
  sum_xy = sum(epicentral_dist(1:n_data) * p_time(1:n_data))
  sum_x2 = sum(epicentral_dist(1:n_data)**2)
  sum_y2 = sum(p_time(1:n_data)**2)
  
  mean_x = sum_x / real(n_data)
  mean_y = sum_y / real(n_data)
  
  ! 線性回歸係數計算 Y = aX + b
  a = (real(n_data) * sum_xy - sum_x * sum_y) / &
      (real(n_data) * sum_x2 - sum_x**2)
  b = mean_y - a * mean_x
  
  ! 相關係數計算
  r_correlation = (real(n_data) * sum_xy - sum_x * sum_y) / &
                  sqrt((real(n_data) * sum_x2 - sum_x**2) * &
                       (real(n_data) * sum_y2 - sum_y**2))
  
  ! 計算擬合標準差
  sum_residual_sq = 0.0
  do i = 1, n_data
    y_predicted = a * epicentral_dist(i) + b
    residual = p_time(i) - y_predicted
    sum_residual_sq = sum_residual_sq + residual**2
  end do
  std_dev_fit = sqrt(sum_residual_sq / real(n_data - 2))
  
  ! 計算係數標準差
  std_dev_a = std_dev_fit / sqrt(sum_x2 - sum_x**2 / real(n_data))
  std_dev_b = std_dev_fit * sqrt(1.0/real(n_data) + mean_x**2 / &
                                (sum_x2 - sum_x**2 / real(n_data)))
  
  ! 輸出結果
  write(*,*)
  write(*,*) '=============== Linear Regression Results ==============='
  write(*,'(A,F8.4,A,F8.4)') 'Regression equation: Y = ', a, ' * X + ', b
  write(*,'(A,F8.4,A)') 'Slope (a) = ', a, ' s/km'
  write(*,'(A,F8.4,A)') 'Intercept (b) = ', b, ' s'
  write(*,'(A,F8.4)') 'Correlation coefficient R = ', r_correlation
  write(*,'(A,F8.4,A)') 'Standard deviation = ', std_dev_fit, ' s'
  write(*,'(A,F8.4,A)') 'Std. dev of slope = ', std_dev_a, ' s/km'
  write(*,'(A,F8.4,A)') 'Std. dev of intercept = ', std_dev_b, ' s'
  write(*,'(A,F8.4)') 'R-squared = ', r_correlation**2
  write(*,*) '======================================================='
  write(*,*)
  
  ! 初始化 PGPLOT
  if (pgopen('regression_plot.ps/vcps') <= 0) then
    stop 'ERROR: 無法開啟 PostScript 檔案'
  end if
  
  call pgslw(2)
  call pgsch(1.2)
  
  ! 設定座標範圍
  x_min = minval(epicentral_dist(1:n_data)) * 0.9
  x_max = maxval(epicentral_dist(1:n_data)) * 1.1
  y_min = minval(p_time(1:n_data)) * 0.9
  y_max = maxval(p_time(1:n_data)) * 1.1
  
  ! 設定繪圖環境
  call pgenv(x_min, x_max, y_min, y_max, 0, 1)
  call pglabel('Epicentral Distance (km)', 'P-wave Travel Time (s)', &
               'Linear Regression: P-wave Travel Time vs Epicentral Distance')
  
  ! 繪製資料點 (使用多種方式確保可見)
  call pgsci(1)  ! 黑色
  call pgsch(1.5)  ! 較大的點
  
  ! 方法1: 使用圓形符號
  ! call pgpt(n_data, epicentral_dist(1:n_data), p_time(1:n_data), 17)  ! 實心圓點
  
  ! 方法2: 再加上圓圈符號確保可見
  call pgsci(4)  ! 藍色圓圈
  call pgpt(n_data, epicentral_dist(1:n_data), p_time(1:n_data), 4)   ! 空心圓圈
  
  ! 恢復設定
  call pgsci(1)  ! 黑色
  call pgsch(1.0)  ! 正常大小
  
  ! 繪製回歸直線
  call pgslw(3)  ! 較粗線條
  x_plot(1) = x_min
  x_plot(2) = x_max
  y_plot(1) = a * x_min + b
  y_plot(2) = a * x_max + b
  call pgline(2, x_plot, y_plot)
  
  ! 繪製標準差範圍 (虛線)
  call pgslw(2)
  call pgsls(2)  ! 虛線樣式
  
  ! 上標準差線
  y_plot(1) = a * x_min + b + std_dev_fit
  y_plot(2) = a * x_max + b + std_dev_fit
  call pgline(2, x_plot, y_plot)
  
  ! 下標準差線
  y_plot(1) = a * x_min + b - std_dev_fit
  y_plot(2) = a * x_max + b - std_dev_fit
  call pgline(2, x_plot, y_plot)
  
  call pgsls(1)  ! 恢復實線
  call pgslw(1)  ! 恢復正常線寬
  
  ! 添加圖例和統計資訊 (英文)
  call pgsci(1)  ! 黑色
  call pgsch(1.0)
  
  ! 回歸方程式 (使用計算出的實際數值)
  ! 建立方程式字串
  write(eq_text, '(A,F6.3,A,F5.2)') 'Y = ', a, 'X + ', b
  call pgtext(x_min + (x_max - x_min) * 0.05, y_max - (y_max - y_min) * 0.08, trim(eq_text))
  
  ! 相關係數資訊
  write(r_text, '(A,F5.3,A,F5.3)') 'R = ', r_correlation, ', R² = ', r_correlation**2
  call pgtext(x_min + (x_max - x_min) * 0.05, y_max - (y_max - y_min) * 0.13, trim(r_text))
  
  ! 標準差資訊
  write(std_text, '(A,F4.2,A)') 'Std Dev = ', std_dev_fit, ' s'
  call pgtext(x_min + (x_max - x_min) * 0.05, y_max - (y_max - y_min) * 0.18, trim(std_text))
  
  ! 資料點數
  write(n_text, '(A,I0,A)') 'N = ', n_data, ' stations'
  call pgtext(x_min + (x_max - x_min) * 0.05, y_max - (y_max - y_min) * 0.23, trim(n_text))
  
  ! 結束 PGPLOT
  call pgend
  
  write(*,*) 'Plotting completed! Output file: regression_plot.ps'
  write(*,*) 'Convert to PDF with command:'
  write(*,*) 'gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite', &
             ' -sOutputFile=regression_plot.pdf regression_plot.ps'

end program linear_regression_analysis