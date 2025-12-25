# NetCDF I/O 模組使用說明

## 檔案說明

- `mod_netcdf_io.f90`: NetCDF 讀取模組，提供讀取 GEBCO 地形資料的功能
- `test_netcdf_io.f90`: 測試程式，驗證模組功能
- `netcdf_spec.md`: NetCDF 檔案規格文件
- `Makefile`: 編譯設定檔

## 系統需求

### 1. 安裝 netcdf-fortran 函式庫

#### macOS (使用 Homebrew)
```bash
brew install netcdf
```

#### Linux (Ubuntu/Debian)
```bash
sudo apt-get update
sudo apt-get install libnetcdff-dev libnetcdf-dev
```

#### Linux (RHEL/CentOS/Fedora)
```bash
sudo yum install netcdf-fortran-devel netcdf-devel
# 或使用 dnf (Fedora)
sudo dnf install netcdf-fortran-devel netcdf-devel
```

### 2. 確認安裝

安裝後，可以檢查：
```bash
# 檢查 nc-config (如果可用)
nc-config --version

# 或檢查 pkg-config
pkg-config --modversion netcdf-fortran
```

## 編譯與執行

### 方法一：使用 Makefile

```bash
# 編譯
make

# 編譯並執行測試
make run

# 清理編譯產物
make clean
```

### 方法二：手動編譯

如果 Makefile 無法自動偵測 NetCDF 路徑，可以手動指定：

#### macOS (Homebrew 安裝在 /opt/homebrew)
```bash
gfortran -O2 -I/opt/homebrew/include -c mod_netcdf_io.f90
gfortran -O2 -I/opt/homebrew/include -c test_netcdf_io.f90
gfortran -O2 -o test_netcdf_io test_netcdf_io.o mod_netcdf_io.o \
    -L/opt/homebrew/lib -lnetcdff -lnetcdf
```

#### Linux (系統套件)
```bash
gfortran -O2 -c mod_netcdf_io.f90
gfortran -O2 -c test_netcdf_io.f90
gfortran -O2 -o test_netcdf_io test_netcdf_io.o mod_netcdf_io.o \
    -lnetcdff -lnetcdf
```

### 執行測試程式

```bash
./test_netcdf_io
```

預期輸出應包含：
- 檔案資訊（網格大小、座標範圍）
- 高程統計（最小值、最大值、平均值）
- 座標陣列驗證
- 樣本資料點
- 陸地/海洋分布

## 模組使用範例

在其他 Fortran 程式中使用此模組：

```fortran
program my_program
    use mod_netcdf_io
    implicit none
    
    character(len=256) :: filename
    integer :: nx, ny, ierr
    real(8), allocatable :: elevation(:,:), lon(:), lat(:)
    
    filename = 'GEBCO_21_Dec_2025_d9303d544c3e/gebco_2025_n41.5_s34.0_w138.0_e146.0.nc'
    
    ! 讀取地形資料
    call read_gebco_bathymetry(filename, elevation, lon, lat, nx, ny, ierr)
    
    if (ierr /= NC_SUCCESS) then
        write(*,*) 'Error reading file'
        stop
    end if
    
    ! 使用 elevation, lon, lat 陣列進行計算
    ! ...
    
    deallocate(elevation, lon, lat)
end program my_program
```

## 模組 API

### `get_netcdf_info`
取得 NetCDF 檔案的基本資訊。

```fortran
subroutine get_netcdf_info(filename, nx, ny, lon_min, lon_max, lat_min, lat_max, ierr)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: nx, ny
    real(8), intent(out) :: lon_min, lon_max, lat_min, lat_max
    integer, intent(out) :: ierr
```

### `read_gebco_bathymetry`
讀取 GEBCO 地形資料。

```fortran
subroutine read_gebco_bathymetry(filename, elevation, lon, lat, nx, ny, ierr)
    character(len=*), intent(in) :: filename
    real(8), intent(out), allocatable :: elevation(:,:)
    real(8), intent(out), allocatable :: lon(:), lat(:)
    integer, intent(out) :: nx, ny
    integer, intent(out) :: ierr
```

**注意事項：**
- `elevation` 陣列順序為 `(nx, ny)` = `(lon, lat)`
- 高程單位為公尺，海平面下為負值
- 座標單位為度（經度、緯度）

## 錯誤處理

模組使用錯誤碼：
- `NC_SUCCESS` (0): 成功
- `NC_ERR_FILE` (-1): 檔案開啟錯誤
- `NC_ERR_VAR` (-2): 變數讀取錯誤
- `NC_ERR_DIM` (-3): 維度讀取錯誤

## 疑難排解

### 編譯錯誤：找不到 netcdf.mod
- 確認 netcdf-fortran 已正確安裝
- 檢查編譯器是否能找到 NetCDF 標頭檔路徑
- 在 Makefile 中手動指定 `-I` 和 `-L` 路徑

### 連結錯誤：undefined reference
- 確認連結時包含 `-lnetcdff -lnetcdf`
- 注意順序：先 `-lnetcdff` 再 `-lnetcdf`

### 執行錯誤：檔案不存在
- 確認 NetCDF 檔案路徑正確
- 檢查檔案權限

## 下一步

完成 NetCDF 讀取模組後，可以進行：
1. Okada Model 實作（計算初始位移）
2. 2D Shallow Water Equations 求解器
3. 時間步進與邊界條件處理
4. 結果輸出（NetCDF 格式）


