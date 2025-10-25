#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
簡化版地震散布圖
專門處理 Latitude,Longitude 格式的 CSV 檔案
"""

import pandas as pd
import matplotlib.pyplot as plt

def simple_earthquake_plot():
    """讀取並繪製簡單的地震散布圖"""
    
    try:
        # 讀取 CSV 檔案
        print("正在讀取 earthquake.csv...")
        df = pd.read_csv('earthquake.csv')
        
        # 移除可能的空白字符
        df.columns = df.columns.str.strip()
        
        print(f"成功讀取 {len(df)} 筆資料")
        print("前 5 筆資料:")
        print(df.head())
        
        # 檢查欄位名稱
        print(f"欄位名稱: {list(df.columns)}")
        
        # 創建散布圖
        plt.figure(figsize=(10, 8))
        
        # 繪製散布圖
        plt.scatter(df['Longitude'], df['Latitude'], 
                   alpha=0.7, s=30, c='red', edgecolors='darkred', linewidth=0.5)
        
        # 設定標題和軸標籤
        plt.title('地震震央分布圖', fontsize=16, fontweight='bold')
        plt.xlabel('經度 (°E)', fontsize=12)
        plt.ylabel('緯度 (°N)', fontsize=12)
        
        # 設定網格
        plt.grid(True, alpha=0.3)
        
        # 設定軸刻度
        plt.xticks(fontsize=10)
        plt.yticks(fontsize=10)
        
        # 顯示統計資訊
        lat_range = df['Latitude'].max() - df['Latitude'].min()
        lon_range = df['Longitude'].max() - df['Longitude'].min()
        
        print(f"\n資料範圍:")
        print(f"緯度: {df['Latitude'].min():.4f}° ~ {df['Latitude'].max():.4f}° (範圍: {lat_range:.4f}°)")
        print(f"經度: {df['Longitude'].min():.4f}° ~ {df['Longitude'].max():.4f}° (範圍: {lon_range:.4f}°)")
        
        # 調整圖表範圍
        plt.xlim(df['Longitude'].min() - 0.1, df['Longitude'].max() + 0.1)
        plt.ylim(df['Latitude'].min() - 0.1, df['Latitude'].max() + 0.1)
        
        # 確保長寬比例正確
        plt.gca().set_aspect('equal', adjustable='box')
        
        # 調整布局
        plt.tight_layout()
        
        # 儲存圖片
        plt.savefig('earthquake_scatter.png', dpi=300, bbox_inches='tight')
        print("\n圖片已儲存為 earthquake_scatter.png")
        
        # 顯示圖表
        plt.show()
        
    except FileNotFoundError:
        print("錯誤: 找不到 earthquake.csv 檔案")
    except KeyError as e:
        print(f"錯誤: 找不到欄位 {e}")
        print("請確認 CSV 檔案包含 'Latitude' 和 'Longitude' 欄位")
    except Exception as e:
        print(f"發生錯誤: {e}")

if __name__ == "__main__":
    simple_earthquake_plot()
