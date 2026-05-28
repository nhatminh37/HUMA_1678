# HUMA 1678 — Red Cliffs Digital Humanities Project

Re-evaluating the Sun–Liu alliance in the Battle of Red Cliffs (Chapters 43–50) using network analysis and spatial GIS methods on *Romance of the Three Kingdoms* (Moss Roberts translation).

**Repository:** [https://github.com/nhatminh37/HUMA_1678](https://github.com/nhatminh37/HUMA_1678)

---

## Repository layout

| Path | Description |
|------|-------------|
| `network_graph_red_cliffs.png` | Person–person network (ggraph / stress layout) |
| `map1_interaction_weighted_points.png` | Map 1: interaction activity by location |
| `map2_qgis_routes.png` | Map 2: route reconstruction (QGIS export) |
| `map3_timeline_routes.png` | Map 3: chapter-ordered timeline routes |
| `data/` | Interaction CSV, centrality export, GeoJSON anchors |
| `data/shapefiles/` | Route and area layers for QGIS / R (`sf`) |
| `scripts/` | R analysis pipelines |

---

## Data

- **`data/red_cliffs_with_locations.csv`** — Dyadic interactions with location, weight, sentiment, chapter (Map 1 and summaries).
- **`data/RedCliffs_Centrality_Findings.csv`** — Node-level degree and eigenvector scores (from network script).
- **`data/Nodes_table_Red_Cliffs.csv`**, **`data/Edges_table_Red_Cliffs.csv`** — Node/edge tables for the tidygraph network (required for `network_analysis_red_cliffs.R`).
- **`data/Location.geojson`** — Canonical place anchors (Chaisang, RedCliffs, Xiakou, etc.).
- **`data/Location_updated.geojson`** — Anchors for proxy coordinate offsets (Map 1).
- **`data/LuSu_move_location.geojson`** — Lu Su waypoint sequence.
- **`data/shapefiles/`** — `LuSu_path`, `LuSu_area`, `ZhouYu_path`, `ZhugeLiang_path`, `Marked_point` (`.shp` + sidecars).

---

## Scripts

Run from the **repository root** (`cd HUMA_1678`).

| Script | Purpose | Main output |
|--------|---------|-------------|
| `scripts/network_analysis_red_cliffs.R` | Person–person network, centrality table, network figure | `network_graph_red_cliffs.png`, `data/RedCliffs_Centrality_Findings.csv` |
| `scripts/map1_interaction_weighted_points.R` | Map 1: weighted points by location | `map1_interaction_weighted_points.png` |
| `scripts/map2_route_reconstruction.R` | Route reconstruction preview (base R `sf`) | optional `marked_points_map.png` |
| `scripts/map3_timeline_routes.R` | Map 3: timeline routes (`ggplot2`) | `map3_timeline_routes.png` |
| `scripts/codex_example.R` | Supplementary centrality / sentiment charts | `centrality_comparison.png`, etc. |

### R packages (typical)

`tidyverse`, `tidygraph`, `igraph`, `ggraph`, `ggrepel`, `sf`, `ggplot2`, `ggspatial` (Map 3)

### Quick start

```bash
git clone https://github.com/nhatminh37/HUMA_1678.git
cd HUMA_1678

Rscript scripts/network_analysis_red_cliffs.R
Rscript scripts/map1_interaction_weighted_points.R
Rscript scripts/map3_timeline_routes.R
```

Copy regenerated PNGs to the repo root if you change output paths. Map 2 (`map2_qgis_routes.png`) is exported from QGIS using layers under `data/`.

---

## QGIS (Map 2)

Open `data/Location.geojson` and character path shapefiles; export the layout as `map2_qgis_routes.png` at the repo root.

---

## Citation

If you use this repository, cite the HUMA 1678 report and the sources listed in your bibliography (Moretti, Painter et al., Zhang et al., Murrieta-Flores et al., Roberts translation of Luo Guanzhong, etc.).

---

## License

Academic project materials for HUMA 1678. Check course requirements before redistribution of the novel excerpt data.
