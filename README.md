# HUMA 1678 — Red Cliffs Digital Humanities Project

Re-evaluating the Sun–Liu alliance in the Battle of Red Cliffs (Chapters 43–50) using network analysis and spatial GIS methods on *Romance of the Three Kingdoms* (Moss Roberts translation).

**Paper source:** `main.tex` + `references.bib` → compile with `pdflatex`/`biber` or `latexmk -pdf -bibtex main.tex`.

**Repository:** [https://github.com/nhatminh37/HUMA_1678](https://github.com/nhatminh37/HUMA_1678)

---

## Repository layout

| Path | Description |
|------|-------------|
| `main.tex`, `references.bib` | LaTeX report and bibliography |
| `*.png` (root) | Figures embedded in the paper |
| `data/` | Interaction CSV, centrality export, GeoJSON anchors |
| `data/shapefiles/` | Route and area layers for QGIS / R (`sf`) |
| `scripts/` | R (and optional Python) analysis pipelines |

---

## Figures (paper)

| File | Map / output |
|------|----------------|
| `RedCliffs_Network_graph.png` | Person–person network (ggraph) |
| `red_cliffs_spatial_proxy_map.png` | Map 1: interaction / activity points |
| `Layout 3.png` | Map 2: route reconstruction (QGIS export) |
| `timeline_red_cliffs_professional.png` | Map 3: chapter-ordered timeline routes |

---

## Data

- **`data/red_cliffs_with_locations.csv`** — Dyadic interactions with location, weight, sentiment, chapter (primary analysis table).
- **`data/RedCliffs_Centrality_Findings.csv`** — Node-level degree and eigenvector scores.
- **`data/Location.geojson`** — Canonical place anchors (Chaisang, RedCliffs, Xiakou, etc.).
- **`data/Location_updated.geojson`** — Anchors for proxy coordinate offsets (Map 1).
- **`data/LuSu_move_location.geojson`** — Lu Su waypoint sequence.
- **`data/shapefiles/`** — `LuSu_path`, `LuSu_area`, `ZhouYu_path`, `ZhugeLiang_path`, `Marked_point` (`.shp` + sidecars).

The LaTeX pseudocode for the network section references `Nodes_table_Red_Cliffs.csv` and `Edges_table_Red_Cliffs.csv`; those are the tidygraph node/edge tables used when building the network figure. Export them from your graph object if you regenerate the network from `red_cliffs_with_locations.csv`.

---

## Scripts

Run from the **repository root** (paths are relative to root).

| Script | Purpose | Main output |
|--------|---------|-------------|
| `scripts/red_cliffs_centrality_analysis.R` | Centrality, sentiment, **Map 1** interaction map | `red_cliffs_spatial_proxy_map.png` |
| `scripts/loading_example.R` | Route reconstruction (base R `sf`) | `marked_points_map.png` (optional) |
| `scripts/new_example.R` | **Map 3** timeline with `ggplot2` + `geom_curve` | `timeline_red_cliffs_professional.png` |
| `scripts/codex_example.R` | Centrality / sentiment summaries (no maps) | `centrality_comparison.png`, etc. |
| `scripts/map.py` | Alternate timeline plot (matplotlib) | — |

### R packages (typical)

`tidyverse`, `tidygraph`, `igraph`, `ggraph`, `ggrepel`, `sf`, `ggplot2`, `ggspatial` (timeline map)

### Quick start

```bash
# Clone
git clone https://github.com/nhatminh37/HUMA_1678.git
cd HUMA_1678

# Regenerate spatial / network outputs (requires R + packages)
Rscript scripts/red_cliffs_centrality_analysis.R
Rscript scripts/new_example.R

# Build PDF
latexmk -pdf -bibtex main.tex
```

Copy generated PNGs to the repo root if paths in `main.tex` should match (figure filenames are already set at root).

---

## QGIS

Map 2 (`Layout 3.png`) was exported from a QGIS project using the same GeoJSON/shapefile layers under `data/`. Open `data/Location.geojson` and character path shapefiles as layers; see Methods §3.2 in `main.tex`.

---

## Citation

If you use this repository, cite the report and the underlying sources listed in `references.bib` (Moretti, Painter et al., Zhang et al., Murrieta-Flores et al., Roberts translation of Luo Guanzhong, etc.).

---

## License

Academic project materials for HUMA 1678. Check course requirements before redistribution of the novel excerpt data.
