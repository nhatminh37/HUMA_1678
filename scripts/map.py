import matplotlib.pyplot as plt

# 1. Define the coordinates
locations = {
    "Xiakou": (114.1, 30.6),
    "Chaisang": (115.5, 29.8),
    "Poyang Lake": (116.0, 29.4),
    "Red Cliff": (113.6, 29.9),
    "Nanping Mountain": (113.5, 29.8),
    "Fankou": (114.5, 30.5)
}

# 2. Update sequences to include the Chapter numbers
paths = {
    "Lu Su": [("Xiakou", 43), ("Chaisang", 44), ("Poyang Lake", 44), ("Red Cliff", 45), ("Red Cliff", 50)],
    "Zhuge Liang": [("Xiakou", 43), ("Chaisang", 45), ("Red Cliff", 49), ("Nanping Mountain", 49), ("Fankou", 50)],
    "Zhou Yu": [("Poyang Lake", 44), ("Chaisang", 44), ("Red Cliff", 45)]
}

# 3. Setup the plot styling
colors = {"Lu Su": "#D9883E", "Zhuge Liang": "#459C45", "Zhou Yu": "#3E7DB3"}
offsets = {"Lu Su": 0, "Zhuge Liang": 0.02, "Zhou Yu": -0.02}

fig, ax = plt.subplots(figsize=(10, 8))

# 4. Plot the base locations
for place, (lon, lat) in locations.items():
    ax.plot(lon, lat, 'ko', markersize=6, alpha=0.6)
    ax.text(lon, lat - 0.03, place, fontsize=9, ha='center', weight='bold')

# 5. Draw paths and add Chapter annotations
for person, route in paths.items():
    color = colors[person]
    offset = offsets[person]
    
    for i in range(len(route) - 1):
        start_loc, start_chap = route[i]
        end_loc, end_chap = route[i+1]
        
        # Handle cases where they stay in the same place (e.g., Lu Su at Red Cliff)
        if start_loc == end_loc:
            lon, lat = locations[start_loc]
            ax.text(lon + offset, lat + 0.04, f"Ch {end_chap}", color=color, fontsize=8, ha='center')
            continue
            
        start_lon, start_lat = locations[start_loc]
        end_lon, end_lat = locations[end_loc]
        
        # Draw line with arrow
        ax.annotate('', 
                    xy=(end_lon + offset, end_lat + offset), 
                    xytext=(start_lon + offset, start_lat + offset),
                    arrowprops=dict(arrowstyle="->", color=color, lw=2, ls='--'))
        
        # Calculate midpoint to place the Chapter text
        mid_lon = (start_lon + end_lon) / 2
        mid_lat = (start_lat + end_lat) / 2
        
        # Place Chapter label at the midpoint
        ax.text(mid_lon + offset, mid_lat + offset + 0.05, f"Ch {end_chap}", 
                color=color, fontsize=8, ha='center', backgroundcolor='white')

# 6. Format axes and legend
ax.set_xlabel("Longitude (°E)")
ax.set_ylabel("Latitude (°N)")
ax.set_title("Red Cliffs: Character Paths & Timelines")

from matplotlib.lines import Line2D
legend_elements = [Line2D([0], [0], color=c, lw=2, ls='--', label=p) for p, c in colors.items()]
ax.legend(handles=legend_elements, loc="upper left")

plt.grid(True, linestyle=':', alpha=0.5)
plt.show()