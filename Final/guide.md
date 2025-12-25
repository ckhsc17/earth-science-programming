這是一份為你的 IDE Agent（如 GitHub Copilot, Cursor 或 Windsurf）量身打造的 `guide.md`。這份文件整合了我們討論的所有物理原理、資料格式以及 Fortran 的實作邏輯。

你可以直接將此檔案放入你的專案目錄，並告訴 Agent：「**請閱讀 guide.md，並根據其中的物理公式與資料格式，幫我生成高品質的 Fortran 程式碼。**」

---

# Tsunami Simulation Project Guide: 2011 Tohoku Earthquake

## 1. 專案目標 (Project Objective)

使用 Fortran 語言實現一個基於物理原理的海嘯數值模擬器。包含從地震參數計算初始位移（Okada Model），並透過二維淺水波方程（2D SWE）模擬波浪在 GEBCO 地形上的傳播。

---

## 2. 物理模型與控制方程 (Physical Models)

### A. 初始條件：Okada Model (1985)

利用彈性半空間理論，將斷層錯動轉化為海底垂直位移 。

* **輸入參數：** Strike, Dip, Rake, Slip, Depth, Length, Width。
* **輸出：** 網格化初始水位 。

### B. 傳播模型：2D Non-linear Shallow Water Equations (SWE)

用於計算  時的水位  與流速 ：

1. **質量守恆 (Continuity)：** 
2. **動量守恆 (Momentum)：**
*  方向：
*  方向：



* **變數說明：**  (總水深)， 為 GEBCO 靜水深。

---

## 3. 資料格式與輸入 (Data Inputs)

### A. 地形資料 (Bathymetry)

* **來源：** GEBCO 2025 NetCDF (Grid)。
* **變數：** `elevation` (單位：公尺，海平面下為負值)。
* **Fortran 要求：** 使用 `netcdf-fortran` 函式庫讀取 2D 陣列。

### B. 地震參數 (Seismic Source - 311 Tohoku Mainshock)

為確保模組化，請 Agent 生成一個讀取 `fault_params.txt` 的副程式。
**311 關鍵參數 (NP1 優先)：**

* **Moment:**  N-m
* **Magnitude:**  
* **Depth:**  km (注意：Okada 公式需使用斷層頂部或中心深度)
* **Strike:** 
* **Dip:** 
* **Rake:** 
* **Slip (估計值):**  m

---

## 4. 數值方案與實作細節 (Implementation Details)

### A. 離散化方法：有限差分法 (FDM)

* **網格系統：** 建議使用 Arakawa C-grid（交錯網格）以提高穩定性。
* **時間步進：** Leap-frog 或 Runge-Kutta 4th Order。
* **邊界條件：**
* **陸地：** 反射邊界或乾濕處理（Wet/Dry condition）。
* **海洋：** 吸收邊界（Radiation boundary），防止人造反射。



### B. 數值穩定性 (CFL Condition)

* **限制：** 。
* **模擬建議：** 日本海溝深約 8000m，若網格 m，則  應小於  秒。

---

## 5. 給 Agent 的指令 (Instructions for AI)

1. **模組化設計：** 請建立 `mod_netcdf_io.f90` 負責讀取 GEBCO 數據，`mod_okada.f90` 計算初始位移，`mod_swe_solver.f90` 執行 FDM 計算。
2. **動態讀檔：** 地震參數請從外部文字檔讀取，格式需包含 Strike, Dip, Rake 等。
3. **效能優化：** Fortran 的陣列運算應利用 `forall` 或向量化處理，避免深層巢狀迴圈。
4. **結果輸出：** 每隔固定時間步長，將水位  輸出為新的 NetCDF 檔案，維度為 `(time, lat, lon)`，以便後續用 Python 繪圖。