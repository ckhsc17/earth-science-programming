# Tsunami Simulation Project: 2011 Tohoku Earthquake

完整的海嘯數值模擬系統，使用 Fortran 實作，包含 Okada Model 初始位移計算與 2D 淺水波方程求解器。

## 專案結構

```
Final/
├── mod_netcdf_io.f90          # NetCDF 讀取模組
├── mod_okada.f90               # Okada Model 初始位移計算
├── mod_swe_solver.f90          # 2D Shallow Water Equations 求解器
├── mod_netcdf_output.f90       # NetCDF 輸出模組
├── tsunami_sim.f90             # 主程式
├── test_netcdf_io.f90          # NetCDF I/O 測試程式
├── fault_params.txt            # 地震參數設定檔
├── Makefile                    # 編譯設定檔
├── guide.md                    # 專案指南
├── README.md                   # 本文件
├── README_netcdf.md            # NetCDF 模組說明
└── GEBCO_21_Dec_2025_d9303d544c3e/
    └── gebco_2025_n41.5_s34.0_w138.0_e146.0.nc  # 地形資料
```

## 系統需求

### 必要套件

1. **Fortran 編譯器** (gfortran 推薦)
2. **netcdf-fortran** 函式庫

#### 安裝 netcdf-fortran

**macOS (Homebrew):**
```bash
brew install netcdf
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install libnetcdff-dev libnetcdf-dev gfortran
```

**Linux (RHEL/CentOS/Fedora):**
```bash
sudo yum install netcdf-fortran-devel netcdf-devel gcc-gfortran
# 或使用 dnf (Fedora)
sudo dnf install netcdf-fortran-devel netcdf-devel gcc-gfortran
```

## 編譯與執行

### 編譯完整模擬程式

```bash
make tsunami_sim
# 或直接
make
```

### 執行模擬

```bash
./tsunami_sim
```

模擬將：
1. 讀取 GEBCO 地形資料
2. 讀取 `fault_params.txt` 地震參數
3. 計算初始海底位移（Okada Model）
4. 執行 1 小時的海嘯傳播模擬
5. 每 5 分鐘輸出一次結果到 `tsunami_output.nc`

### 測試 NetCDF I/O

```bash
make test_netcdf_io
./test_netcdf_io
```

## 設定檔說明

### fault_params.txt

地震參數設定檔，格式為 `key = value`：

```
strike = 203.0      # 走向角（度，從正北順時針）
dip = 10.0          # 傾角（度，0-90）
rake = 90.0         # 滑移角（度）
slip = 50.0         # 滑移量（公尺）
depth = 20.0        # 深度（公里，會自動轉換為公尺）
length = 500.0      # 斷層長度（公里）
width = 200.0       # 斷層寬度（公里）
lon_center = 142.0  # 斷層中心經度（度）
lat_center = 38.0   # 斷層中心緯度（度）
```

**2011 東北地震預設參數：**
- Strike: 203°
- Dip: 10°
- Rake: 90°
- Slip: 50 m
- Depth: 20 km
- Length: 500 km
- Width: 200 km
- Center: (142°E, 38°N)

## 輸出檔案

### tsunami_output.nc

NetCDF 格式的模擬結果，包含：

- **維度：**
  - `lon`: 經度 (1920 點)
  - `lat`: 緯度 (1800 點)
  - `time`: 時間（無限制）

- **變數：**
  - `lon`: 經度座標陣列
  - `lat`: 緯度座標陣列
  - `time`: 時間陣列（秒）
  - `eta`: 表面高程（公尺），維度為 `(lon, lat, time)`

### 使用 Python 讀取結果

```python
import netCDF4
import numpy as np
import matplotlib.pyplot as plt

# 讀取結果
nc = netCDF4.Dataset('tsunami_output.nc', 'r')
lon = nc.variables['lon'][:]
lat = nc.variables['lat'][:]
time = nc.variables['time'][:]
eta = nc.variables['eta'][:]

# 繪製某個時間點的水位
t_idx = 0  # 初始條件
plt.contourf(lon, lat, eta[t_idx, :, :], levels=20)
plt.colorbar(label='Surface elevation (m)')
plt.xlabel('Longitude')
plt.ylabel('Latitude')
plt.title(f'Tsunami at t = {time[t_idx]:.0f} s')
plt.show()

nc.close()
```

## 模擬參數調整

在 `tsunami_sim.f90` 中可以調整：

- `t_end`: 模擬總時間（秒），預設 3600 秒（1 小時）
- `output_interval`: 輸出間隔（秒），預設 300 秒（5 分鐘）
- `dt`: 時間步長（自動計算，基於 CFL 條件）

## 物理模型

### 1. Okada Model (1985)

計算地震斷層引起的初始海底位移：
- 輸入：斷層幾何參數（strike, dip, rake, slip, depth, length, width）
- 輸出：網格化初始水位位移

### 2. 2D Shallow Water Equations

非線性淺水波方程：
- **質量守恆：** ∂η/∂t + ∂(hu)/∂x + ∂(hv)/∂y = 0
- **動量守恆：** ∂u/∂t + u·∂u/∂x + v·∂u/∂y = -g·∂η/∂x
- **動量守恆：** ∂v/∂t + u·∂v/∂x + v·∂v/∂y = -g·∂η/∂y

其中：
- η: 表面高程（水位）
- h: 總水深 = η - bathymetry
- u, v: x, y 方向流速
- g: 重力加速度 (9.81 m/s²)

### 數值方法

- **網格系統：** Arakawa C-grid（交錯網格）
- **時間步進：** Leap-frog 方法
- **邊界條件：**
  - 陸地：反射邊界（零法向流速）
  - 海洋：吸收邊界（簡化處理）

## 效能優化

程式已使用以下優化：
- 陣列運算向量化
- 避免深層巢狀迴圈
- 動態記憶體分配

## 疑難排解

### 編譯錯誤

**找不到 netcdf.mod：**
- 確認 netcdf-fortran 已安裝
- 在 Makefile 中手動指定 `-I` 路徑

**連結錯誤：**
- 確認包含 `-lnetcdff -lnetcdf`
- 檢查庫檔案路徑

### 執行錯誤

**檔案不存在：**
- 確認 NetCDF 地形檔案路徑正確
- 確認 `fault_params.txt` 存在

**數值不穩定：**
- 檢查時間步長是否滿足 CFL 條件
- 確認地形資料沒有異常值

## 參考文獻

1. Okada, Y. (1985). Surface deformation due to shear and tensile faults in a half-space. *Bulletin of the Seismological Society of America*, 75(4), 1135-1154.

2. LeVeque, R. J. (2002). *Finite Volume Methods for Hyperbolic Problems*. Cambridge University Press.

3. GEBCO 2025 Grid: https://www.gebco.net/

## 授權

本專案為教育用途，遵循 GEBCO 資料使用條款。

## 作者

Generated for Earth Science Programming Final Project

