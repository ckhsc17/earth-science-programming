#!/usr/bin/env python3
"""
Tsunami Simulation Visualization and Analysis Script
Reads tsunami_output.nc and creates various plots and analyses
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from matplotlib.colors import TwoSlopeNorm
import netCDF4
from mpl_toolkits.axes_grid1 import make_axes_locatable
import os
import sys

def load_tsunami_data(filename='tsunami_output.nc'):
    """Load tsunami simulation data from NetCDF file"""
    print(f"Loading data from {filename}...")
    nc = netCDF4.Dataset(filename, 'r')
    
    # Read coordinates
    lon = nc.variables['lon'][:]
    lat = nc.variables['lat'][:]
    time = nc.variables['time'][:]
    eta = nc.variables['eta'][:]  # Shape: (time, lat, lon) based on NetCDF output
    
    # Convert to numpy arrays
    lon = np.array(lon)
    lat = np.array(lat)
    time = np.array(time)
    eta = np.array(eta)
    
    # Transpose eta to (lon, lat, time) for easier indexing
    # Original: (time, lat, lon) -> Transpose to: (lon, lat, time)
    if eta.shape[0] == len(time):
        eta = np.transpose(eta, (2, 1, 0))  # (time, lat, lon) -> (lon, lat, time)
    
    # Get metadata
    n_times = len(time)
    n_lon = len(lon)
    n_lat = len(lat)
    
    print(f"  Grid size: {n_lon} x {n_lat}")
    print(f"  Time steps: {n_times}")
    print(f"  Time range: {time[0]:.1f} to {time[-1]:.1f} seconds ({time[-1]/60:.1f} minutes)")
    print(f"  Eta range: {eta.min():.4f} to {eta.max():.4f} m")
    
    nc.close()
    
    return lon, lat, time, eta

def plot_initial_condition(lon, lat, eta, output_dir='plots'):
    """Plot initial seafloor displacement"""
    os.makedirs(output_dir, exist_ok=True)
    
    fig, ax = plt.subplots(figsize=(12, 10))
    
    # Initial condition (first time step)
    eta_init = eta[:, :, 0]
    
    # Create colormap centered at zero
    vmax = np.max(np.abs(eta_init))
    vmin = -vmax
    norm = TwoSlopeNorm(vmin=vmin, vcenter=0, vmax=vmax)
    
    # Create contour plot
    LON, LAT = np.meshgrid(lon, lat, indexing='ij')
    im = ax.contourf(LON, LAT, eta_init, levels=50, cmap='RdBu_r', norm=norm, extend='both')
    
    # Add contour lines
    contours = ax.contour(LON, LAT, eta_init, levels=20, colors='black', alpha=0.3, linewidths=0.5)
    ax.clabel(contours, inline=True, fontsize=8, fmt='%1.1f')
    
    # Colorbar
    cbar = plt.colorbar(im, ax=ax, label='Surface Elevation (m)', shrink=0.8)
    
    ax.set_xlabel('Longitude (°E)', fontsize=12)
    ax.set_ylabel('Latitude (°N)', fontsize=12)
    ax.set_title('Initial Seafloor Displacement (t = 0 s)', fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.set_aspect('equal', adjustable='box')
    
    plt.tight_layout()
    filename = os.path.join(output_dir, 'initial_displacement.png')
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    print(f"  Saved: {filename}")
    plt.close()

def plot_time_series(lon, lat, time, eta, output_dir='plots'):
    """Plot time series at selected locations"""
    os.makedirs(output_dir, exist_ok=True)
    
    # Select locations (near coast and in deep water)
    # Find indices for specific locations
    target_lons = [140.0, 141.0, 142.0, 143.0]  # Different longitudes
    target_lat = 38.0  # Fixed latitude
    
    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    axes = axes.flatten()
    
    for idx, target_lon in enumerate(target_lons):
        # Find nearest grid point
        lon_idx = np.argmin(np.abs(lon - target_lon))
        lat_idx = np.argmin(np.abs(lat - target_lat))
        
        # Extract time series
        eta_ts = eta[lon_idx, lat_idx, :]
        
        ax = axes[idx]
        ax.plot(time / 60.0, eta_ts, 'b-', linewidth=2)
        ax.axhline(y=0, color='k', linestyle='--', linewidth=0.5)
        ax.set_xlabel('Time (minutes)', fontsize=11)
        ax.set_ylabel('Surface Elevation (m)', fontsize=11)
        ax.set_title(f'Time Series at ({target_lon:.1f}°E, {target_lat:.1f}°N)', fontsize=11)
        ax.grid(True, alpha=0.3)
        
        # Add statistics
        max_val = np.max(np.abs(eta_ts))
        ax.text(0.05, 0.95, f'Max: {max_val:.3f} m', 
                transform=ax.transAxes, verticalalignment='top',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    plt.tight_layout()
    filename = os.path.join(output_dir, 'time_series.png')
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    print(f"  Saved: {filename}")
    plt.close()

def plot_wave_propagation(lon, lat, time, eta, output_dir='plots', n_frames=6):
    """Create animation frames showing wave propagation"""
    os.makedirs(output_dir, exist_ok=True)
    
    # Select time indices
    time_indices = np.linspace(0, len(time)-1, n_frames, dtype=int)
    
    LON, LAT = np.meshgrid(lon, lat, indexing='ij')
    
    # Find common color scale
    vmax = np.max(np.abs(eta))
    vmin = -vmax
    norm = TwoSlopeNorm(vmin=vmin, vcenter=0, vmax=vmax)
    
    for i, t_idx in enumerate(time_indices):
        fig, ax = plt.subplots(figsize=(12, 10))
        
        eta_t = eta[:, :, t_idx]
        
        im = ax.contourf(LON, LAT, eta_t, levels=50, cmap='RdBu_r', norm=norm, extend='both')
        contours = ax.contour(LON, LAT, eta_t, levels=20, colors='black', alpha=0.3, linewidths=0.5)
        
        cbar = plt.colorbar(im, ax=ax, label='Surface Elevation (m)', shrink=0.8)
        
        ax.set_xlabel('Longitude (°E)', fontsize=12)
        ax.set_ylabel('Latitude (°N)', fontsize=12)
        ax.set_title(f'Tsunami Wave Propagation (t = {time[t_idx]/60:.1f} minutes)', 
                    fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal', adjustable='box')
        
        plt.tight_layout()
        filename = os.path.join(output_dir, f'wave_propagation_t{int(time[t_idx]):04d}s.png')
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        print(f"  Saved: {filename}")
        plt.close()

def plot_maximum_amplitude(lon, lat, eta, output_dir='plots'):
    """Plot maximum wave amplitude over entire simulation"""
    os.makedirs(output_dir, exist_ok=True)
    
    # Compute maximum amplitude at each grid point
    eta_max = np.max(np.abs(eta), axis=2)
    
    fig, ax = plt.subplots(figsize=(12, 10))
    
    LON, LAT = np.meshgrid(lon, lat, indexing='ij')
    
    # Use log scale for better visualization
    im = ax.contourf(LON, LAT, eta_max, levels=50, cmap='viridis', extend='max')
    
    # Add contour lines
    contours = ax.contour(LON, LAT, eta_max, levels=20, colors='white', alpha=0.5, linewidths=0.8)
    ax.clabel(contours, inline=True, fontsize=9, fmt='%1.2f', colors='white')
    
    cbar = plt.colorbar(im, ax=ax, label='Maximum Wave Amplitude (m)', shrink=0.8)
    
    ax.set_xlabel('Longitude (°E)', fontsize=12)
    ax.set_ylabel('Latitude (°N)', fontsize=12)
    ax.set_title('Maximum Wave Amplitude During Simulation', fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3, color='white')
    ax.set_aspect('equal', adjustable='box')
    
    plt.tight_layout()
    filename = os.path.join(output_dir, 'maximum_amplitude.png')
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    print(f"  Saved: {filename}")
    plt.close()

def plot_wave_energy(lon, lat, time, eta, output_dir='plots'):
    """Plot total wave energy as function of time"""
    os.makedirs(output_dir, exist_ok=True)
    
    # Compute total energy (proportional to eta^2 integrated over domain)
    # E = 0.5 * rho * g * integral(eta^2) dA
    # For simplicity, we'll use eta^2 as proxy (assuming constant density and g)
    energy = np.sum(eta**2, axis=(0, 1))
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    ax.plot(time / 60.0, energy / energy.max(), 'b-', linewidth=2, label='Normalized Energy')
    ax.set_xlabel('Time (minutes)', fontsize=12)
    ax.set_ylabel('Normalized Wave Energy', fontsize=12)
    ax.set_title('Total Wave Energy Evolution', fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend()
    
    plt.tight_layout()
    filename = os.path.join(output_dir, 'wave_energy.png')
    plt.savefig(filename, dpi=300, bbox_inches='tight')
    print(f"  Saved: {filename}")
    plt.close()

def analyze_statistics(lon, lat, time, eta, output_dir='plots'):
    """Generate statistical analysis"""
    os.makedirs(output_dir, exist_ok=True)
    
    stats = {
        'time_min': time.min(),
        'time_max': time.max(),
        'time_mean': time.mean(),
        'eta_min': eta.min(),
        'eta_max': eta.max(),
        'eta_mean': eta.mean(),
        'eta_std': eta.std(),
        'max_amplitude': np.max(np.abs(eta)),
        'n_times': len(time),
        'n_lon': len(lon),
        'n_lat': len(lat),
    }
    
    # Find location of maximum amplitude
    eta_abs = np.abs(eta)
    max_idx = np.unravel_index(np.argmax(eta_abs), eta_abs.shape)
    stats['max_lon'] = lon[max_idx[0]]
    stats['max_lat'] = lat[max_idx[1]]
    stats['max_time'] = time[max_idx[2]]
    
    # Write statistics to file
    stats_file = os.path.join(output_dir, 'statistics.txt')
    with open(stats_file, 'w') as f:
        f.write("Tsunami Simulation Statistics\n")
        f.write("=" * 50 + "\n\n")
        f.write(f"Grid Information:\n")
        f.write(f"  Grid size: {stats['n_lon']} x {stats['n_lat']}\n")
        f.write(f"  Longitude range: {lon.min():.4f} to {lon.max():.4f}°E\n")
        f.write(f"  Latitude range: {lat.min():.4f} to {lat.max():.4f}°N\n\n")
        f.write(f"Time Information:\n")
        f.write(f"  Time steps: {stats['n_times']}\n")
        f.write(f"  Time range: {stats['time_min']:.1f} to {stats['time_max']:.1f} seconds\n")
        f.write(f"  Duration: {stats['time_max']/60:.1f} minutes\n\n")
        f.write(f"Surface Elevation Statistics:\n")
        f.write(f"  Minimum: {stats['eta_min']:.4f} m\n")
        f.write(f"  Maximum: {stats['eta_max']:.4f} m\n")
        f.write(f"  Mean: {stats['eta_mean']:.4f} m\n")
        f.write(f"  Standard deviation: {stats['eta_std']:.4f} m\n")
        f.write(f"  Maximum amplitude: {stats['max_amplitude']:.4f} m\n\n")
        f.write(f"Maximum Amplitude Location:\n")
        f.write(f"  Longitude: {stats['max_lon']:.4f}°E\n")
        f.write(f"  Latitude: {stats['max_lat']:.4f}°N\n")
        f.write(f"  Time: {stats['max_time']:.1f} seconds ({stats['max_time']/60:.1f} minutes)\n")
    
    print(f"  Statistics saved to: {stats_file}")
    
    return stats

def main():
    """Main function"""
    input_file = 'tsunami_output.nc'
    
    if not os.path.exists(input_file):
        print(f"Error: File {input_file} not found!")
        sys.exit(1)
    
    print("=" * 60)
    print("Tsunami Simulation Visualization and Analysis")
    print("=" * 60)
    print()
    
    # Load data
    lon, lat, time, eta = load_tsunami_data(input_file)
    
    print()
    print("Generating plots...")
    print("-" * 60)
    
    # Create output directory
    output_dir = 'plots'
    os.makedirs(output_dir, exist_ok=True)
    
    # Generate all plots
    plot_initial_condition(lon, lat, eta, output_dir)
    plot_time_series(lon, lat, time, eta, output_dir)
    plot_wave_propagation(lon, lat, time, eta, output_dir, n_frames=8)
    plot_maximum_amplitude(lon, lat, eta, output_dir)
    plot_wave_energy(lon, lat, time, eta, output_dir)
    
    # Generate statistics
    print()
    print("Computing statistics...")
    stats = analyze_statistics(lon, lat, time, eta, output_dir)
    
    print()
    print("=" * 60)
    print("Analysis complete!")
    print(f"All outputs saved to: {output_dir}/")
    print("=" * 60)

if __name__ == '__main__':
    main()

