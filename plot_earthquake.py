#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
地震震央分布圖
讀取 earthquake.csv 檔案並繪製經緯度散布圖
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# 設定中文字體
plt.rcParams['font.sans-serif'] = ['Arial Unicode MS', 'SimHei', 'DejaVu Sans']
plt.rcParams['axes.unicode_minus'] = False

def plot_earthquake_distribution():
    """讀取地震資料並繪製震央分布圖"""
    
    try:
        # 讀取 CSV 檔案
        df = pd.read_csv('earthquake.csv')
        
        # 顯示資料基本資訊
        print(f"總共讀取 {len(df)} 筆地震資料")
        print("資料範圍:")
        print(f"緯度: {df['Latitude'].min():.4f} ~ {df['Latitude'].max():.4f}")
        print(f"經度: {df['Longitude'].min():.4f} ~ {df['Longitude'].max():.4f}")
        
        # 創建圖表
        plt.figure(figsize=(12, 10))
        
        # 繪製散布圖
        plt.scatter(df['Longitude'], df['Latitude'], 
                   alpha=0.6, s=20, c='red', edgecolors='black', linewidth=0.3)
        
        # 設定標題和軸標籤
        plt.title('1999年地震震央分布圖', fontsize=16, fontweight='bold')
        plt.xlabel('經度 (Longitude)', fontsize=12)
        plt.ylabel('緯度 (Latitude)', fontsize=12)
        
        # 設定網格
        plt.grid(True, alpha=0.3)
        
        # 設定軸的範圍，讓圖表更美觀
        lat_margin = (df['Latitude'].max() - df['Latitude'].min()) * 0.1
        lon_margin = (df['Longitude'].max() - df['Longitude'].min()) * 0.1
        
        plt.xlim(df['Longitude'].min() - lon_margin, df['Longitude'].max() + lon_margin)
        plt.ylim(df['Latitude'].min() - lat_margin, df['Latitude'].max() + lat_margin)
        
        # 添加台灣主要城市標記（可選）
        cities = {
            '台北': (121.5654, 25.0330),
            '台中': (120.6736, 24.1477),
            '高雄': (120.3014, 22.6273),
            '花蓮': (121.6015, 23.9917)
        }
        
        for city, (lon, lat) in cities.items():
            plt.plot(lon, lat, 'bs', markersize=8, label=city if city == '台北' else "")
            plt.text(lon + 0.1, lat + 0.1, city, fontsize=10, fontweight='bold')
        
        # 添加圖例
        plt.legend(['地震震央', '主要城市'], loc='upper right')
        
        # 調整布局
        plt.tight_layout()
        
        # 儲存圖片
        plt.savefig('earthquake_distribution.png', dpi=300, bbox_inches='tight')
        print("圖表已儲存為 earthquake_distribution.png")
        
        # 顯示圖表
        plt.show()
        
        # 統計資訊
        print("\n統計資訊:")
        print(f"地震數量: {len(df)}")
        if 'Magnitude' in df.columns:
            print(f"平均規模: {df['Magnitude'].mean():.2f}")
            print(f"最大規模: {df['Magnitude'].max():.2f}")
        if 'Depth' in df.columns:
            print(f"平均深度: {df['Depth'].mean():.2f} km")
            print(f"最大深度: {df['Depth'].max():.2f} km")
        
    except FileNotFoundError:
        print("錯誤: 找不到 earthquake.csv 檔案")
        print("請確認檔案在當前目錄中")
    except Exception as e:
        print(f"錯誤: {e}")

def plot_enhanced_earthquake_map():
    """進階版地震分布圖，包含規模和深度資訊"""
    
    try:
        df = pd.read_csv('earthquake.csv')
        
        # 如果有規模和深度資料，創建更詳細的圖表
        if 'Magnitude' in df.columns and 'Depth' in df.columns:
            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(20, 8))
            
            # 左圖：以規模大小表示點的大小
            scatter1 = ax1.scatter(df['Longitude'], df['Latitude'], 
                                 s=df['Magnitude']**2 * 10, 
                                 c=df['Magnitude'], 
                                 cmap='Reds', alpha=0.6, 
                                 edgecolors='black', linewidth=0.3)
            ax1.set_title('地震震央分布圖 (點大小代表規模)', fontsize=14)
            ax1.set_xlabel('經度 (Longitude)')
            ax1.set_ylabel('緯度 (Latitude)')
            ax1.grid(True, alpha=0.3)
            plt.colorbar(scatter1, ax=ax1, label='規模 (Magnitude)')
            
            # 右圖：以深度表示顏色
            scatter2 = ax2.scatter(df['Longitude'], df['Latitude'], 
                                 s=30, c=df['Depth'], 
                                 cmap='viridis_r', alpha=0.7,
                                 edgecolors='black', linewidth=0.3)
            ax2.set_title('地震震央分布圖 (顏色代表深度)', fontsize=14)
            ax2.set_xlabel('經度 (Longitude)')
            ax2.set_ylabel('緯度 (Latitude)')
            ax2.grid(True, alpha=0.3)
            plt.colorbar(scatter2, ax=ax2, label='深度 (km)')
            
            plt.tight_layout()
            plt.savefig('earthquake_enhanced.png', dpi=300, bbox_inches='tight')
            plt.show()
        
    except Exception as e:
        print(f"進階圖表錯誤: {e}")

if __name__ == "__main__":
    print("開始繪製地震震央分布圖...")
    plot_earthquake_distribution()
    
    print("\n繪製進階版分布圖...")
    plot_enhanced_earthquake_map()
