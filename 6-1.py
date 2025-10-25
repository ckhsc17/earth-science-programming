import pandas as pd
import matplotlib.pyplot as plt

# 讀島內事件
eq = pd.read_csv('onland_eq.csv')   # columns: Latitude,Longitude

# 讀海岸線
coast = pd.read_csv('Taiwan.txt', sep=r"\s+", names=["lon","lat"], engine="python")

plt.figure(figsize=(6,8))
plt.plot(coast['lon'], coast['lat'], linewidth=1)       # 台灣海岸線
plt.scatter(eq['Longitude'], eq['Latitude'], s=10, alpha=0.7)  # 震央
plt.gca().set_aspect('equal', adjustable='box')
plt.xlabel('Longitude (°E)'); plt.ylabel('Latitude (°N)')
plt.title('On-land Earthquakes (1999)')
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()
