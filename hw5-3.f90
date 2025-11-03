program plot_vp_profile
  ! 讀取 Vp_prof.dat 並繪製 Vp 剖面與速度擾動圖
  implicit none
  
  integer, parameter :: max_data = 6000
  real :: dist(max_data), depth(max_data), vp(max_data), vp_pert(max_data)
  real :: lon(max_data), lat(max_data)
  
  integer :: i, n_data, ios
  integer :: pgopen, color_index
  real :: z, dfz, xr, xg, xb  ! 用於色彩計算
  real :: x, y, dx  ! 參考 subroutine 的變數名稱
  real :: color_ratio
  
  ! 色表參數
  integer, parameter :: ncolor = 10
  real :: l(ncolor), r(ncolor), g(ncolor), b(ncolor)
  
  ! 繪圖範圍
  real :: xmin, xmax, ymin, ymax
  real :: vp_min, vp_max, pert_min, pert_max
  real :: x_center
  
  ! 讀取資料
  open(unit=10, file='Vp_prof.dat', status='old', action='read')
  
  ! 跳過標題行
  read(10, *)
  
  n_data = 0
  do i = 1, max_data
    read(10, *, iostat=ios) dist(i), depth(i), vp(i), vp_pert(i), lon(i), lat(i)
    if (ios /= 0) exit
    n_data = n_data + 1
  end do
  close(10)
  
  write(*,*) '成功讀取 ', n_data, ' 筆資料'
  
  ! 計算繪圖範圍
  xmin = minval(dist(1:n_data)) - 5.0
  xmax = maxval(dist(1:n_data)) + 5.0
  ymin = -maxval(depth(1:n_data)) - 5.0  ! 深度轉為負值，因為 y 向下
  ymax = -minval(depth(1:n_data)) + 5.0
  
  vp_min = 3.5
  vp_max = 8.5
  pert_min = -10.0
  pert_max = 10.0
  
  ! 計算矩形大小 (參考 subroutine 的 dx)
  dx = (xmax - xmin) / 200.0  ! 可調整此值來控制矩形大小
  
  write(*,*) 'Vp 範圍: ', vp_min, ' - ', vp_max
  write(*,*) '擾動範圍: ', pert_min, ' - ', pert_max
  
  ! 初始化 PGPLOT
  if (pgopen('vp_profile.ps/vcps') <= 0) then
    stop '錯誤: 無法開啟 PostScript 檔案'
  end if
  
  call pgslw(2)
  call pgsch(1.2)
  call pgscf(1)  ! 設定字體為簡單字體
  
    ! 設定兩個子圖
  call pgsubp(1, 2)
  
  ! ========== 第一個圖: Vp Profile ==========
  call pgenv(xmin, xmax, ymin, ymax, 0, 0)
  
  ! 設定漸層色表 (從紅色到藍色)
  l(1) = 0.0; r(1) = 1.0; g(1) = 0.0; b(1) = 0.0  ! 紅色
  l(2) = 1.0/9.0; r(2) = 1.0; g(2) = 0.5; b(2) = 0.0
  l(3) = 2.0/9.0; r(3) = 1.0; g(3) = 1.0; b(3) = 0.0  ! 黃色
  l(4) = 3.0/9.0; r(4) = 0.8; g(4) = 1.0; b(4) = 0.0  ! 黃綠色
  l(5) = 4.0/9.0; r(5) = 0.5; g(5) = 1.0; b(5) = 0.3  ! 綠色
  l(6) = 5.0/9.0; r(6) = 0.0; g(6) = 1.0; b(6) = 0.8  ! 青綠色
  l(7) = 6.0/9.0; r(7) = 0.0; g(7) = 0.8; b(7) = 1.0  ! 青色
  l(8) = 7.0/9.0; r(8) = 0.0; g(8) = 0.5; b(8) = 1.0  ! 淺藍
  l(9) = 8.0/9.0; r(9) = 0.0; g(9) = 0.0; b(9) = 1.0  ! 藍色
  l(10) = 1.0; r(10) = 0.3; g(10) = 0.0; b(10) = 0.7  ! 深藍
  
  call pgscir(16, 255)
  call pgctab(l, r, g, b, 10, 1.0, 0.5)
  
  write(*,*) '開始繪製第一個圖 (Vp Profile)'
  
  ! 直接根據每個資料點繪製 Vp 矩形
  do i = 1, n_data
    if (vp(i) > 1.0 .and. vp(i) < 12.0) then
      ! 設定座標 (參考 subroutine)
      x = dist(i)          ! xdis
      y = -1.0 * depth(i)  ! -1.*xdep
      z = vp(i)            ! vp
      
      ! 限制 z 值在範圍內
      if (z > vp_max) z = vp_max
      if (z < vp_min) z = vp_min
      
      ! 使用色表映射而不是 RGB 計算
      color_ratio = (z - vp_min) / (vp_max - vp_min)
      color_index = 16 + int(color_ratio * 239)
      color_index = max(16, min(255, color_index))
      
      call pgsci(color_index)
      call pgsfs(1)  ! 實心填充
      call pgrect(x - dx, x + dx, y - dx, y + dx)
    end if
  end do
  
  write(*,*) '第一個圖繪製完成'
  
  ! 重置顏色為黑色，避免影響文字
  call pgscr(1, 0.0, 0.0, 0.0)  ! 設定色彩索引 1 為黑色
  call pgsci(1)
  
  ! 繪製軸
  call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
  call pglabel('Distance (km)', 'Depth (km)', 'Vp Profile (120.000,23.000) to (122.000,23.000)')
  
  x_center = (xmin + xmax) / 2.0
  call pgtext(x_center, ymax - (ymax - ymin) * 0.05, 'Vp (km/sec)')
  
  ! 繪製第一個圖的內部色條圖例 (橫條形式，左下角)
  call draw_colorbar_inside_horizontal(xmin + 10.0, xmin + 60.0, ymin + 10.0, ymin + 20.0, &
                                       vp_min, vp_max, 'km/s', 1)
  
  ! ========== 第二個圖: Vp Perturbation ==========
  call pgenv(xmin, xmax, ymin, ymax, 0, 0)
  
  ! 設定擾動圖的色表 (紅白藍)
  l(1) = 0.0; r(1) = 1.0; g(1) = 0.0; b(1) = 0.0  ! 紅色 (正擾動)
  l(2) = 0.25; r(2) = 1.0; g(2) = 0.5; b(2) = 0.5
  l(3) = 0.5; r(3) = 1.0; g(3) = 1.0; b(3) = 1.0  ! 白色 (零擾動)
  l(4) = 0.75; r(4) = 0.5; g(4) = 0.5; b(4) = 1.0
  l(5) = 1.0; r(5) = 0.0; g(5) = 0.0; b(5) = 1.0  ! 藍色 (負擾動)
  
  call pgscir(16, 255)  ! 重新設定色彩範圍
  call pgctab(l, r, g, b, 5, 1.0, 0.5)
  
  write(*,*) '開始繪製第二個圖 (Vp Perturbation)...'
  write(*,*) '擾動範圍: ', pert_min, ' - ', pert_max
  
  ! 直接根據每個資料點繪製擾動圖矩形
  do i = 1, n_data
    ! 檢查資料是否合理
    if (vp_pert(i) > -50.0 .and. vp_pert(i) < 50.0) then
      ! 設定座標 (參考 subroutine)
      x = dist(i)          ! xdis
      y = -1.0 * depth(i)  ! -1.*xdep
      z = vp_pert(i)       ! vp_pert
      
      ! 限制 z 值在範圍內
      if (z > pert_max) z = pert_max
      if (z < pert_min) z = pert_min
      
      ! 使用色表映射
      color_ratio = (z - pert_min) / (pert_max - pert_min)
      color_index = 16 + int(color_ratio * 239)
      color_index = max(16, min(255, color_index))
      
      call pgsci(color_index)
      call pgsfs(1)  ! 實心填充
      call pgrect(x - dx, x + dx, y - dx, y + dx)
    end if
  end do
  
  write(*,*) '第二個圖繪製完成'
  
  ! 重置顏色為黑色，避免影響文字
  call pgscr(1, 0.0, 0.0, 0.0)  ! 設定色彩索引 1 為黑色
  call pgsci(1)
  
  ! 繪製軸
  call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
  call pglabel('Distance (km)', 'Depth (km)', 'Vp perturbation (120.000,23.000) to (122.000,23.000)')
  
  call pgtext(x_center, ymax - (ymax - ymin) * 0.05, 'Perturbation (%)')
  
  ! 繪製第二個圖的內部色條圖例 (橫條形式，左下角)
  call draw_colorbar_inside_horizontal(xmin + 10.0, xmin + 60.0, ymin + 10.0, ymin + 20.0, &
                                       pert_min, pert_max, '%', 2)
  
  ! 結束 PGPLOT
  call pgend
  
  write(*,*) '繪圖完成！輸出檔案: vp_profile.ps'
  write(*,*) '轉換為 PDF:'
  write(*,*) 'gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=vp_profile.pdf vp_profile.ps'

end program plot_vp_profile

! 繪製內部橫向色條圖例的 subroutine
subroutine draw_colorbar_inside_horizontal(x1, x2, y1, y2, val_min, val_max, unit_label, color_type)
  implicit none
  real, intent(in) :: x1, x2, y1, y2, val_min, val_max
  character(*), intent(in) :: unit_label
  integer, intent(in) :: color_type
  
  integer :: i, n_segments, color_index
  real :: dx, x_pos, val, color_ratio
  character(len=20) :: val_str
  
  ! 色表參數 (與主程式一致)
  integer, parameter :: ncolor = 10
  real :: l(ncolor), r(ncolor), g(ncolor), b(ncolor)
  
  n_segments = 50  ! 橫向分段數
  dx = (x2 - x1) / real(n_segments)
  
  ! 設定對應的色表
  if (color_type == 1) then
    ! Vp 色表 (紅到藍)
    l(1) = 0.0; r(1) = 1.0; g(1) = 0.0; b(1) = 0.0
    l(2) = 1.0/9.0; r(2) = 1.0; g(2) = 0.5; b(2) = 0.0
    l(3) = 2.0/9.0; r(3) = 1.0; g(3) = 1.0; b(3) = 0.0
    l(4) = 3.0/9.0; r(4) = 0.8; g(4) = 1.0; b(4) = 0.0
    l(5) = 4.0/9.0; r(5) = 0.5; g(5) = 1.0; b(5) = 0.3
    l(6) = 5.0/9.0; r(6) = 0.0; g(6) = 1.0; b(6) = 0.8
    l(7) = 6.0/9.0; r(7) = 0.0; g(7) = 0.8; b(7) = 1.0
    l(8) = 7.0/9.0; r(8) = 0.0; g(8) = 0.5; b(8) = 1.0
    l(9) = 8.0/9.0; r(9) = 0.0; g(9) = 0.0; b(9) = 1.0
    l(10) = 1.0; r(10) = 0.3; g(10) = 0.0; b(10) = 0.7
    call pgctab(l, r, g, b, 10, 1.0, 0.5)
  else
    ! 擾動色表 (紅白藍)
    l(1) = 0.0; r(1) = 1.0; g(1) = 0.0; b(1) = 0.0
    l(2) = 0.25; r(2) = 1.0; g(2) = 0.5; b(2) = 0.5
    l(3) = 0.5; r(3) = 1.0; g(3) = 1.0; b(3) = 1.0
    l(4) = 0.75; r(4) = 0.5; g(4) = 0.5; b(4) = 1.0
    l(5) = 1.0; r(5) = 0.0; g(5) = 0.0; b(5) = 1.0
    call pgctab(l, r, g, b, 5, 1.0, 0.5)
  end if
  
  ! 繪製橫向色條段
  do i = 1, n_segments
    x_pos = x1 + (i - 1) * dx
    val = val_min + (i - 1) * (val_max - val_min) / real(n_segments - 1)
    
    ! 計算色彩索引
    color_ratio = (val - val_min) / (val_max - val_min)
    color_index = 16 + int(color_ratio * 239)
    color_index = max(16, min(255, color_index))
    
    call pgsci(color_index)
    call pgsfs(1)  ! 實心填充
    call pgrect(x_pos, x_pos + dx, y1, y2)
  end do
  
  ! 重置為黑色繪製框線和文字
  call pgsci(1)
  call pgsfs(2)  ! 空心
  call pgrect(x1, x2, y1, y2)  ! 繪製邊框
  
  ! 添加數值標籤 (橫向排列)
  write(val_str, '(f5.1)') val_min
  call pgtext(x1 - 3.0, y1 - 4.0, val_str)
  
  write(val_str, '(f5.1)') (val_min + val_max) / 2.0
  call pgtext((x1 + x2) / 2.0 - 3.0, y1 - 4.0, val_str)
  
  write(val_str, '(f5.1)') val_max
  call pgtext(x2 - 3.0, y1 - 4.0, val_str)
  
  ! 添加單位標籤
  call pgtext((x1 + x2) / 2.0 - 3.0, y2 + 3.0, unit_label)
  
end subroutine draw_colorbar_inside_horizontal

! 繪製內部色條圖例的 subroutine
subroutine draw_colorbar_inside(x1, x2, y1, y2, val_min, val_max, unit_label, color_type)
  implicit none
  real, intent(in) :: x1, x2, y1, y2, val_min, val_max
  character(len=*), intent(in) :: unit_label
  integer, intent(in) :: color_type  ! 1=Vp, 2=perturbation
  
  real :: dy, y_pos, val, color_ratio
  integer :: i, n_segments, color_index
  character(len=20) :: val_str
  
  n_segments = 50  ! 色條分段數
  dy = (y2 - y1) / real(n_segments)
  
  ! 繪製色條矩形
  do i = 1, n_segments
    y_pos = y1 + (i - 1) * dy
    val = val_min + (i - 1) * (val_max - val_min) / real(n_segments - 1)
    
    ! 計算色彩
    color_ratio = (val - val_min) / (val_max - val_min)
    color_index = 16 + int(color_ratio * 239)
    color_index = max(16, min(255, color_index))
    
    call pgsci(color_index)
    call pgsfs(1)  ! 實心填充
    call pgrect(x1, x2, y_pos, y_pos + dy)
  end do
  
  ! 重置為黑色繪製框線和文字
  call pgsci(1)
  call pgsfs(2)  ! 空心
  call pgrect(x1, x2, y1, y2)  ! 繪製邊框
  
  ! 添加數值標籤
  write(val_str, '(f4.1)') val_max
  call pgtext(x2 + 1.0, y2 - 2.0, val_str)
  
  write(val_str, '(f4.1)') (val_min + val_max) / 2.0
  call pgtext(x2 + 1.0, (y1 + y2) / 2.0 - 1.0, val_str)
  
  write(val_str, '(f4.1)') val_min
  call pgtext(x2 + 1.0, y1 - 2.0, val_str)
  
  ! 添加單位標籤
  call pgtext(x1 - 1.0, y2 + 5.0, unit_label)
  
end subroutine draw_colorbar_inside

