# Okada Model 修正說明

## 問題診斷

### 發現的問題

1. **單位轉換錯誤**
   - `length` 和 `width` 從檔案讀取後沒有從 km 轉換為 m
   - 只有 `depth` 有轉換邏輯
   - 導致斷層尺寸錯誤（500 km 被當作 500 m）

2. **Okada 公式縮放問題**
   - 原始公式中的縮放因子 `(L*W)/(4*PI*R^2)` 會讓位移變得極小
   - 距離衰減 `exp(-(R-W)/(2*W))` 過於激進
   - 導致計算出的位移幾乎為 0

3. **eta 的定義**
   - `eta` = 表面高程（surface elevation），即水位相對於平均海平面的高度
   - 初始條件：`eta(t=0) = Okada 計算的海底垂直位移`
   - 在 SWE 中：`h = eta - bathymetry`（總水深）

## 已修正的內容

### 1. 單位轉換修正

```fortran
! 現在 length 和 width 也會從 km 轉換為 m
if (fault%length < 1000.0d0) then
    fault%length = fault%length * 1000.0d0
end if
if (fault%width < 1000.0d0) then
    fault%width = fault%width * 1000.0d0
end if
```

### 2. Okada 公式改進

改進了位移計算的縮放邏輯，使用距離相關的縮放：
- 近場（R < fault size）：使用常數縮放
- 遠場（R > fault size）：使用 1/R^2 縮放
- 移除了過於激進的指數衰減

## 測試建議

重新編譯並執行模擬：

```bash
make clean
make
./tsunami_sim
```

檢查輸出中的初始位移是否合理：
- 應該看到非零的初始位移值
- 最大值應該在幾米到十幾米的範圍內（對於 50 m 滑移）

## 如果問題仍然存在

如果修正後位移仍然太小，可以考慮：

1. **使用更簡單的經驗公式**：
   ```fortran
   ! 簡單的經驗公式：位移與距離的關係
   uz = U2 * exp(-R / (2*W)) * (1.0 - R / (5*W))
   ```

2. **直接設定初始位移**：
   ```fortran
   ! 在斷層附近直接設定位移值
   if (R < W) then
       uz = U2 * 0.1  ! 滑移的 10% 轉化為垂直位移
   end if
   ```

3. **使用已發布的 Okada 實作**：
   - 參考其他開源專案（如 COMCOT, GeoClaw）
   - 使用更完整的 Okada 積分實現

## 參考資料

- Okada, Y. (1985). Surface deformation due to shear and tensile faults in a half-space. BSSA, 75(4), 1135-1154.
- 完整的 Okada 實現需要對斷層平面的四個角進行積分，這是一個複雜的計算

