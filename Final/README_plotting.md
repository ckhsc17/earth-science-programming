# Tsunami Visualization and Analysis

本目錄包含用於分析和視覺化海嘯模擬結果的程式。

## Python 視覺化程式

### 檔案：`plot_tsunami.py`

完整的 Python 視覺化腳本，使用 matplotlib 和 netCDF4。

#### 功能

1. **初始位移圖** (`initial_displacement.png`)
   - 顯示初始海底位移場
   - 使用對稱色階（紅-白-藍）

2. **時間序列圖** (`time_series.png`)
   - 在選定位置繪製水位隨時間變化
   - 包含多個觀測點

3. **波傳播動畫幀** (`wave_propagation_t*.png`)
   - 生成多個時間點的波傳播快照
   - 可用於製作動畫

4. **最大振幅圖** (`maximum_amplitude.png`)
   - 顯示整個模擬期間的最大波振幅
   - 識別高風險區域

5. **波能量演化** (`wave_energy.png`)
   - 總波能量隨時間變化
   - 分析能量耗散

6. **統計分析** (`statistics.txt`)
   - 詳細的統計資訊
   - 最大振幅位置和時間

#### 使用方法

```bash
# 安裝依賴套件
pip3 install numpy matplotlib netCDF4

# 執行視覺化
python3 plot_tsunami.py
```

所有圖表將保存在 `plots/` 目錄中。

#### 輸出檔案

- `plots/initial_displacement.png` - 初始位移
- `plots/time_series.png` - 時間序列
- `plots/wave_propagation_t*.png` - 波傳播快照
- `plots/maximum_amplitude.png` - 最大振幅
- `plots/wave_energy.png` - 能量演化
- `plots/statistics.txt` - 統計資訊

## Fortran PGPLOT 視覺化程式

### 檔案：`plot_tsunami_pgplot.f90`

使用 PGPLOT 函式庫的 Fortran 視覺化程式。

#### 功能

1. 初始條件圖
2. 最大振幅圖
3. 時間序列圖（多個位置）
4. 波傳播動畫（多個時間點）

#### 編譯要求

需要安裝 PGPLOT 函式庫：

**macOS:**
```bash
brew install pgplot
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get install libpgplot-dev
```

**Linux (RHEL/CentOS):**
```bash
sudo yum install pgplot-devel
```

#### 編譯

在 `Makefile` 中添加：

```makefile
plot_tsunami_pgplot: plot_tsunami_pgplot.o mod_netcdf_io.o mod_netcdf_output.o
	$(FC) $(FFLAGS) -o plot_tsunami_pgplot plot_tsunami_pgplot.o \
	    mod_netcdf_io.o mod_netcdf_output.o $(NETCDF_LIB) -lpgplot -lX11
```

或手動編譯：

```bash
gfortran -O2 -o plot_tsunami_pgplot plot_tsunami_pgplot.f90 \
    mod_netcdf_io.f90 mod_netcdf_output.f90 \
    -lnetcdff -lnetcdf -lpgplot -lX11
```

#### 執行

```bash
./plot_tsunami_pgplot
```

**注意：** PGPLOT 需要 X11 顯示器。如果使用 SSH，需要 X11 forwarding：
```bash
ssh -X user@host
```

#### 輸出模式

修改程式中的 `pgopen` 呼叫以改變輸出：

- `/XWIN` - X11 視窗（互動式）
- `/PNG` - PNG 檔案輸出
- `/PDF` - PDF 檔案輸出
- `/PS` - PostScript 檔案輸出

例如，輸出 PNG：
```fortran
call pgopen('/PNG:tsunami_plot.png', pgopen_status)
```

## 快速開始

### Python（推薦）

```bash
# 1. 安裝依賴
pip3 install numpy matplotlib netCDF4

# 2. 執行
python3 plot_tsunami.py

# 3. 查看結果
ls plots/
```

### Fortran PGPLOT

```bash
# 1. 安裝 PGPLOT
brew install pgplot  # macOS
# 或
sudo apt-get install libpgplot-dev  # Linux

# 2. 編譯（需要更新 Makefile）
make plot_tsunami_pgplot

# 3. 執行
./plot_tsunami_pgplot
```

## 自訂化

### Python 腳本

可以修改 `plot_tsunami.py` 中的參數：

- `n_frames`: 波傳播圖的幀數
- `target_lons`, `target_lat`: 時間序列的位置
- 色階和圖表樣式

### PGPLOT 程式

可以修改：

- 輸出設備（XWIN, PNG, PDF, PS）
- 圖表大小和樣式
- 選定的時間點和位置

## 疑難排解

### Python

**錯誤：找不到 netCDF4**
```bash
pip3 install netCDF4
```

**錯誤：找不到 matplotlib**
```bash
pip3 install matplotlib
```

### PGPLOT

**錯誤：找不到 PGPLOT**
- 確認已安裝 PGPLOT 函式庫
- 檢查連結器路徑（`-L` 選項）

**錯誤：無法開啟顯示器**
- 使用 X11 forwarding：`ssh -X`
- 或改用檔案輸出：`/PNG` 或 `/PDF`

## 進階用法

### 製作動畫（Python）

可以使用 ImageMagick 或 ffmpeg 將 PNG 幀組合成動畫：

```bash
# 使用 ImageMagick
convert plots/wave_propagation_t*.png -delay 10 -loop 0 animation.gif

# 使用 ffmpeg
ffmpeg -framerate 2 -i plots/wave_propagation_t%04ds.png -c:v libx264 animation.mp4
```

### 批次處理多個模擬

修改 Python 腳本以處理多個輸出檔案：

```python
for filename in ['tsunami_output1.nc', 'tsunami_output2.nc']:
    lon, lat, time, eta = load_tsunami_data(filename)
    # ... 處理 ...
```

