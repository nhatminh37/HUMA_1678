# Visualize Location.geojson + Lu Su waypoints + optional shapefiles (sf)
# Run from this folder:  setwd(".../Huma 1678")  or source with full paths
#
# Lu Su routes (orange):
#   • Route 1: LuSu_path.shp line 1 if present; else polyline through LuSu_places / LuSu_move_location.
#   • Route 2: LuSu_path.shp line 2 if shapefile has ≥2 lines; else Xiakou→Chaisang→Poyang_lake→RedCliffs
#              from Location.geojson (vector order_lusu_r2).
# Line type constants: LTY_LUSU, LTY_ZHOU, LTY_ZHUGE (sparse hex lty strings).
#
# If Zhou/Zhuge *_path.shp are empty, routes use Location.geojson (orders below).

library(sf)

if (!file.exists("data/Location.geojson")) {
  stop("Need Location.geojson in the working directory.")
}

loc <- st_read("data/Location.geojson", quiet = TRUE)
names(loc)[names(loc) == "point"] <- "name"
loc$track <- "Place"
loc <- loc[, c("name", "track", "geometry")]

read_lusu_waypoints <- function() {
  candidates <- c("data/shapefiles/LuSu_places.shp", "data/LuSu_places.geojson", "data/LuSu_move_location.geojson")
  for (path in candidates) {
    if (!file.exists(path)) next
    x <- st_read(path, quiet = TRUE)
    if (!nrow(x)) {
      message("Note: ", path, " has 0 features — skipped.")
      next
    }
    g <- st_geometry(x)
    if (!all(st_is(g, "POINT"))) {
      message("Note: ", path, " is not all POINT — skipped for Lu Su waypoints.")
      next
    }
    nm <- NULL
    for (col in c("name", "place", "Name", "label", "site")) {
      if (col %in% names(x)) {
        nm <- as.character(x[[col]])
        break
      }
    }
    if (is.null(nm) && "LuSu_area" %in% names(x)) {
      nm <- paste0("LuSu_", x$LuSu_area, "_", seq_len(nrow(x)))
    } else if (is.null(nm)) {
      nm <- paste0("LuSu_", seq_len(nrow(x)))
    }
    if (anyDuplicated(nm)) {
      nm <- paste(nm, seq_len(nrow(x)), sep = "_")
    }
    out <- st_sf(name = nm, track = "Lu Su", geometry = g, crs = st_crs(x))
    if ("seq" %in% names(x)) {
      out$seq <- suppressWarnings(as.numeric(x$seq))
    } else if ("order" %in% names(x)) {
      out$seq <- suppressWarnings(as.numeric(x$order))
    }
    message("Lu Su waypoints: ", path, " (", nrow(out), " points).")
    return(out)
  }
  message("Note: no Lu Su waypoints file (LuSu_places.* / LuSu_move_location.geojson).")
  NULL
}

lusu_pts <- read_lusu_waypoints()

pts <- if (!is.null(lusu_pts)) rbind(loc, lusu_pts[, c("name", "track", "geometry")]) else loc

pal <- c(
  "Zhou Yu" = "#1f77b4",
  "Zhuge Liang" = "#2ca02c",
  "Lu Su" = "#e67e22",
  "Place" = "#495057",
  "Other" = "#6c757d"
)

# Legend (and console) names for Lu Su lines 1–2; extra rows fall back to "Lu Su route i".
LUSU_ROUTE_LEGEND_LABELS <- c("Lu Su RedCliffs Travel Area", "Lu Su path")

# Sparse line types (hex lty: alternating stroke/gap length, 1/96" per digit 0–F).
# Different rhythm per character; all use long gaps (8–C) so patterns stay open, not dense.
LTY_LUSU   <- "F8C8F8C8" # Lu Su:       sparse dash-dot
LTY_ZHUGE  <- "FCECFC8E" # Zhuge Liang: very disconnected long-gap
LTY_ZHOU   <- "E8E4E8C4" # Zhou Yu:     sparse long-short alternation

# Prefer non-empty LINESTRING shapefiles; otherwise NULL
read_route_lines <- function(paths) {
  for (path in paths) {
    if (!file.exists(path)) next
    x <- st_read(path, quiet = TRUE)
    if (!nrow(x)) {
      message("Note: ", path, " exists but has 0 features — skipped.")
      next
    }
    g <- st_geometry(x)
    if (!all(st_is(g, "LINESTRING"))) {
      message("Note: ", path, " is not all LINESTRING — skipped as route.")
      next
    }
    return(x)
  }
  NULL
}

linestring_from_places <- function(place_sf, ordered_names) {
  if (!nrow(place_sf)) return(NULL)
  ix <- match(ordered_names, place_sf$name)
  if (anyNA(ix)) {
    message(
      "Note: cannot build line — missing place(s): ",
      paste(ordered_names[is.na(ix)], collapse = ", ")
    )
    return(NULL)
  }
  m <- st_coordinates(place_sf[ix, , drop = FALSE])
  if (nrow(m) < 2L) return(NULL)
  ls <- st_linestring(m[, 1L:2L, drop = FALSE])
  st_sf(geometry = st_sfc(ls, crs = st_crs(place_sf)))
}

build_lusu_route_from_waypoints <- function(lusu_sf) {
  if (is.null(lusu_sf) || nrow(lusu_sf) < 2L) {
    return(list(line = NULL, start_name = NA_character_))
  }
  s <- lusu_sf
  if ("seq" %in% names(s) && !all(is.na(s$seq))) {
    s <- s[order(s$seq, na.last = TRUE), , drop = FALSE]
  }
  m <- st_coordinates(s)
  list(
    line = st_sf(geometry = st_sfc(st_linestring(m[, 1L:2L]), crs = st_crs(lusu_sf))),
    start_name = s$name[1L]
  )
}

nearest_waypoint_name_to_line_start <- function(line_sf, pt_sf) {
  if (is.null(line_sf) || !nrow(line_sf) || is.null(pt_sf) || !nrow(pt_sf)) {
    return(NA_character_)
  }
  g <- st_geometry(line_sf)[[1L]]
  co <- st_coordinates(g)
  p0 <- st_sfc(st_point(co[1L, 1L:2L]), crs = st_crs(line_sf))
  d <- as.numeric(st_distance(st_geometry(pt_sf), p0))
  if (length(d) && min(d, na.rm = TRUE) < 8000) {
    return(pt_sf$name[which.min(d)])
  }
  NA_character_
}

# Vertex order for paths when shapefiles are empty (edit to match your story)
order_zhuge <- c("Xiakou", "Chaisang", "RedCliffs", "Fankou")
order_zhou  <- c("Poyang_lake", "Chaisang", "RedCliffs")
# Lu Su route 2 (always from Location.geojson unless LuSu_path.shp already has ≥2 lines)
order_lusu_r2 <- c("Xiakou", "Chaisang", "Poyang_lake", "RedCliffs")

read_lusu_line_shapefile <- function() {
  for (path in c("data/shapefiles/LuSu_path.shp", "data/shapefiles/movement_path.shp")) {
    if (!file.exists(path)) next
    x <- st_read(path, quiet = TRUE)
    if (!nrow(x)) {
      message("Note: ", path, " has 0 features.")
      next
    }
    if (!all(st_is(st_geometry(x), "LINESTRING"))) {
      message("Note: ", path, " is not all LINESTRING — skipped.")
      next
    }
    return(x)
  }
  NULL
}

# Lu Su: (1) If LuSu_path.shp has 2+ LINESTRINGs → use them as route 1 & 2.
# (2) If it has exactly 1 line → route 1 = that line, route 2 = order_lusu_r2.
#         (3) If empty → route 1 = waypoint polyline, route 2 = order_lusu_r2.
assemble_lusu_routes <- function() {
  shp <- read_lusu_line_shapefile()
  r2 <- linestring_from_places(loc, order_lusu_r2)
  if (is.null(r2)) {
    message("Note: Lu Su route 2 not built — check place names in Location.geojson.")
  }

  if (!is.null(shp) && nrow(shp) >= 2L) {
    message(
      "Lu Su: ",
      nrow(shp),
      " separate route(s) from shapefile (no extra Location-based route 2 added)."
    )
    return(st_sf(route_idx = seq_len(nrow(shp)), geometry = st_geometry(shp), crs = st_crs(shp)))
  }

  geoms <- list()
  if (!is.null(shp) && nrow(shp) == 1L) {
    geoms[[1L]] <- st_geometry(shp)[[1L]]
    message("Lu Su route 1 from shapefile (single line).")
  } else if (is.null(shp) && !is.null(lusu_pts) && nrow(lusu_pts) >= 2L) {
    bl <- build_lusu_route_from_waypoints(lusu_pts)
    if (!is.null(bl$line)) {
      geoms[[1L]] <- st_geometry(bl$line)[[1L]]
      message("Lu Su route 1 from Lu Su waypoint file (connected points).")
    }
  }

  if (!is.null(r2)) {
    geoms[[length(geoms) + 1L]] <- st_geometry(r2)[[1L]]
    message(
      "Lu Su route 2 from Location.geojson: ",
      paste(order_lusu_r2, collapse = " → "),
      "."
    )
  }

  if (!length(geoms)) {
    return(NULL)
  }
  sfc <- st_sfc(geoms, crs = st_crs(loc))
  st_sf(route_idx = seq_along(sfc), geometry = sfc, crs = st_crs(sfc))
}

path_zhuge <- read_route_lines(c("data/shapefiles/ZhugeLiang_path.shp", "data/shapefiles/ZhugeLiang_movements.shp"))
if (is.null(path_zhuge)) {
  path_zhuge <- linestring_from_places(loc, order_zhuge)
  if (!is.null(path_zhuge)) message("Zhuge Liang route built from Location.geojson (order: ", paste(order_zhuge, collapse = " → "), ").")
}

path_zhou <- read_route_lines(c("data/shapefiles/ZhouYu_path.shp", "data/shapefiles/ZhouYu_movement.shp"))
if (is.null(path_zhou)) {
  path_zhou <- linestring_from_places(loc, order_zhou)
  if (!is.null(path_zhou)) message("Zhou Yu route built from Location.geojson (order: ", paste(order_zhou, collapse = " → "), ").")
}

path_lusu <- assemble_lusu_routes()

read_area_polygon <- function(paths) {
  for (path in paths) {
    if (!file.exists(path)) next
    x <- st_read(path, quiet = TRUE)
    if (!nrow(x)) {
      message("Note: ", path, " exists but has 0 features — skipped.")
      next
    }
    g <- st_geometry(x)
    if (all(st_is(g, c("POLYGON", "MULTIPOLYGON")))) {
      return(x)
    }
    message("Note: ", path, " is not polygon geometry — skipped as area.")
  }
  NULL
}

area_lusu_rc <- read_area_polygon(c("data/shapefiles/LuSu_Red_Cliffs_Travel_area.shp", "data/shapefiles/LuSu_area.shp"))

# Geodesic length of each route (meters → km; WGS 84 / S2 ellipsoid)
path_length_km <- function(obj) {
  if (is.null(obj) || !nrow(obj)) return(NA_real_)
  sum(as.numeric(st_length(obj))) / 1000
}

fmt_km <- function(km) {
  if (is.na(km)) return("—")
  formatC(km, format = "f", digits = 1, width = 1)
}

# Arrowheads along each LINESTRING (vertex order = travel direction)
draw_path_arrows <- function(obj, col, head_in = 0.14, angle_deg = 22, lwd_arr = 2.2) {
  if (is.null(obj) || !nrow(obj)) return()
  for (k in seq_len(nrow(obj))) {
    g <- st_geometry(obj)[k]
    if (!st_is(g, "LINESTRING")) next
    co <- st_coordinates(g)
    if (nrow(co) < 2L) next
    for (i in seq_len(nrow(co) - 1L)) {
      x0 <- co[i, 1L]
      y0 <- co[i, 2L]
      x1 <- co[i + 1L, 1L]
      y1 <- co[i + 1L, 2L]
      t0 <- 0.4
      t1 <- 0.62
      ax0 <- x0 + t0 * (x1 - x0)
      ay0 <- y0 + t0 * (y1 - y0)
      ax1 <- x0 + t1 * (x1 - x0)
      ay1 <- y0 + t1 * (y1 - y0)
      arrows(
        ax0,
        ay0,
        ax1,
        ay1,
        length = head_in,
        angle = angle_deg,
        col = col,
        lwd = lwd_arr,
        code = 2L
      )
    }
  }
}

# Shift route geometry slightly to reduce overlap where routes share segments/starts.
shift_lines_sf <- function(obj, dx = 0, dy = 0) {
  if (is.null(obj) || !nrow(obj)) return(obj)
  geoms <- lapply(seq_len(nrow(obj)), function(i) {
    g <- st_geometry(obj)[[i]]
    if (!st_is(g, "LINESTRING")) return(g)
    m <- st_coordinates(g)
    st_linestring(cbind(m[, 1L] + dx, m[, 2L] + dy))
  })
  out <- obj
  st_geometry(out) <- st_sfc(geoms, crs = st_crs(obj))
  out
}

# First vertex of each LINESTRING = start (×)
mark_path_start <- function(obj, col) {
  if (is.null(obj) || !nrow(obj)) return()
  for (k in seq_len(nrow(obj))) {
    g <- st_geometry(obj)[k]
    if (!st_is(g, "LINESTRING")) next
    co <- st_coordinates(g)
    points(co[1L, 1L], co[1L, 2L], pch = 4L, col = col, lwd = 2.8, cex = 1.2)
  }
}

# Spread labels for points whose symbols fall within ~thresh_deg (lon/lat degrees)
spread_label_coords <- function(x, y, thresh_deg = 0.024, dlat = 0.065, dlon = 0.02) {
  n <- length(x)
  parent <- seq_len(n)
  find <- function(i) {
    if (parent[i] != i) parent[i] <<- find(parent[i])
    parent[i]
  }
  join <- function(i, j) {
    ri <- find(i)
    rj <- find(j)
    if (ri != rj) parent[rj] <<- ri
  }
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d <- sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2)
      if (i != j && d <= thresh_deg) {
        join(i, j)
      }
    }
  }
  root <- vapply(seq_len(n), find, integer(1))
  ox <- x
  oy <- y
  for (r in unique(root)) {
    idx <- which(root == r)
    m <- length(idx)
    if (m < 2L) next
    off <- seq_len(m) - (m + 1L) / 2
    ox[idx] <- x[idx] + off * dlon
    oy[idx] <- y[idx] + off * dlat
  }
  list(x = ox, y = oy)
}

km_lusu_vec <- if (!is.null(path_lusu) && nrow(path_lusu)) {
  sapply(seq_len(nrow(path_lusu)), function(i) {
    path_length_km(path_lusu[i, , drop = FALSE])
  })
} else {
  numeric(0)
}
km_zhou  <- path_length_km(path_zhou)
km_zhuge <- path_length_km(path_zhuge)

# Plot-only offsets (degrees) so overlapping starts/routes are both visible.
path_lusu_plot  <- shift_lines_sf(path_lusu,  dx = 0.000, dy = 0.000)
path_zhou_plot  <- shift_lines_sf(path_zhou,  dx = -0.012, dy = 0.010)
path_zhuge_plot <- shift_lines_sf(path_zhuge, dx = 0.012, dy = -0.010)

# No ● at route starts (× only): match by waypoint name
start_no_dot <- character(0)
if (!is.null(path_zhuge)) start_no_dot <- c(start_no_dot, order_zhuge[1L])
if (!is.null(path_zhou)) start_no_dot <- c(start_no_dot, order_zhou[1L])
if (!is.null(path_lusu) && nrow(path_lusu)) {
  for (i in seq_len(nrow(path_lusu))) {
    li <- path_lusu[i, , drop = FALSE]
    nm <- nearest_waypoint_name_to_line_start(li, pts)
    if (!is.na(nm)) start_no_dot <- c(start_no_dot, nm)
  }
}
start_no_dot <- unique(start_no_dot)
show_dot <- !(pts$name %in% start_no_dot)

# Bounding box: points + routes + Red Cliffs area polygon
sfc_for_bbox <- st_geometry(pts)
if (!is.null(path_zhuge_plot)) sfc_for_bbox <- c(sfc_for_bbox, st_geometry(path_zhuge_plot))
if (!is.null(path_zhou_plot))  sfc_for_bbox <- c(sfc_for_bbox, st_geometry(path_zhou_plot))
if (!is.null(path_lusu_plot))  sfc_for_bbox <- c(sfc_for_bbox, st_geometry(path_lusu_plot))
if (!is.null(area_lusu_rc)) {
  sfc_for_bbox <- c(sfc_for_bbox, st_geometry(area_lusu_rc))
}
bb <- st_bbox(sfc_for_bbox)

# --- Base R plot (no extra packages) ---
png("marked_points_map.png", width = 2200, height = 1800, res = 220)
par(mar = c(4, 4, 3.6, 1))

plot(
  st_as_sfc(bb),
  border = NA,
  axes = TRUE,
  xlab = "Longitude (°E)",
  ylab = "Latitude (°N)",
  main = "Red Cliffs (chs. 43–50): points + Lu Su / Zhou Yu / Zhuge Liang paths"
)

if (!is.null(area_lusu_rc)) {
  plot(
    st_geometry(area_lusu_rc),
    add = TRUE,
    border = pal["Lu Su"],
    col = grDevices::rgb(230 / 255, 126 / 255, 34 / 255, alpha = 0.22),
    lwd = 1.6
  )
}

# Draw routes under points (Lu Su, Zhou Yu, Zhuge Liang — same colors as point tracks)
if (!is.null(path_lusu_plot) && nrow(path_lusu_plot)) {
  for (i in seq_len(nrow(path_lusu_plot))) {
    plot(
      st_geometry(path_lusu_plot)[i],
      add = TRUE,
      col = pal["Lu Su"],
      lwd = 2.8,
      lty = LTY_LUSU
    )
  }
}
if (!is.null(path_zhou_plot)) {
  plot(
    st_geometry(path_zhou_plot),
    add = TRUE,
    col = pal["Zhou Yu"],
    lwd = 2.8,
    lty = LTY_ZHOU
  )
}
if (!is.null(path_zhuge_plot)) {
  plot(
    st_geometry(path_zhuge_plot),
    add = TRUE,
    col = pal["Zhuge Liang"],
    lwd = 2.8,
    lty = LTY_ZHUGE
  )
}

if (!is.null(path_lusu_plot) && nrow(path_lusu_plot)) {
  draw_path_arrows(path_lusu_plot, pal["Lu Su"], head_in = 0.19, angle_deg = 24, lwd_arr = 2.4)
}
if (!is.null(path_zhou_plot)) {
  draw_path_arrows(path_zhou_plot, pal["Zhou Yu"], head_in = 0.19, angle_deg = 24, lwd_arr = 2.4)
}
if (!is.null(path_zhuge_plot)) {
  draw_path_arrows(path_zhuge_plot, pal["Zhuge Liang"], head_in = 0.19, angle_deg = 24, lwd_arr = 2.4)
}

# Filled waypoint ● for all GeoJSON stops except each track’s polyline start (× only there)
pts_mark <- pts[show_dot, , drop = FALSE]
if (nrow(pts_mark)) {
  plot(
    st_geometry(pts_mark),
    add = TRUE,
    pch = 21L,
    bg = unname(pal[pts_mark$track]),
    col = "gray20",
    cex = 1.35,
    lwd = 0.6
  )
}

if (!is.null(path_lusu_plot) && nrow(path_lusu_plot)) {
  mark_path_start(path_lusu_plot, pal["Lu Su"])
}
if (!is.null(path_zhou_plot)) {
  mark_path_start(path_zhou_plot, pal["Zhou Yu"])
}
if (!is.null(path_zhuge_plot)) {
  mark_path_start(path_zhuge_plot, pal["Zhuge Liang"])
}

route_leg <- character(0)
route_col <- character(0)
route_lty <- character(0)
if (!is.null(path_lusu) && nrow(path_lusu)) {
  for (i in seq_len(nrow(path_lusu))) {
    lab <- if (i <= length(LUSU_ROUTE_LEGEND_LABELS)) {
      LUSU_ROUTE_LEGEND_LABELS[i]
    } else {
      paste0("Lu Su route ", i)
    }
    route_leg <- c(route_leg, paste0(lab, " (", fmt_km(km_lusu_vec[i]), " km)"))
    route_col <- c(route_col, pal["Lu Su"])
    route_lty <- c(route_lty, LTY_LUSU)
  }
}
if (!is.null(path_zhou)) {
  route_leg <- c(route_leg, paste0("Zhou Yu path (", fmt_km(km_zhou), " km)"))
  route_col <- c(route_col, pal["Zhou Yu"])
  route_lty <- c(route_lty, LTY_ZHOU)
}
if (!is.null(path_zhuge)) {
  route_leg <- c(route_leg, paste0("Zhuge Liang path (", fmt_km(km_zhuge), " km)"))
  route_col <- c(route_col, pal["Zhuge Liang"])
  route_lty <- c(route_lty, LTY_ZHUGE)
}
if (length(route_leg)) {
  legend(
    "topleft",
    legend = route_leg,
    col = unname(route_col),
    lty = route_lty,
    lwd = 2.5,
    seg.len = 4.8,
    bty = "n",
    title = "Routes"
  )
}

if (!is.null(area_lusu_rc)) {
  legend(
    "topright",
    legend = "Lu Su Red Cliffs area (polygon)",
    fill = grDevices::rgb(230 / 255, 126 / 255, 34 / 255, alpha = 0.35),
    border = pal["Lu Su"],
    bty = "n",
    cex = 0.85
  )
}

tracks_in_pts <- unique(pts$track)
leg_tracks <- names(pal)[names(pal) %in% tracks_in_pts]

crds <- st_coordinates(pts)
lab <- spread_label_coords(crds[, 1L], crds[, 2L], thresh_deg = 0.030, dlat = 0.090, dlon = 0.030)
dx_track <- ifelse(pts$track == "Zhou Yu", -0.012, ifelse(pts$track == "Zhuge Liang", 0.012, 0))
dy_track <- ifelse(pts$track == "Lu Su", 0.010, ifelse(pts$track == "Place", -0.006, 0))
lab$x <- lab$x + dx_track
lab$y <- lab$y + dy_track
label_col <- unname(pal[pts$track])
segments(crds[, 1L], crds[, 2L], lab$x, lab$y, col = grDevices::rgb(0, 0, 0, alpha = 0.35), lwd = 0.6)
text(
  lab$x,
  lab$y,
  labels = pts$name,
  cex = 0.56,
  col = label_col,
  font = 2
)
dev.off()
message("Saved: ", normalizePath("marked_points_map.png", mustWork = FALSE))
if (length(km_lusu_vec)) {
  for (i in seq_along(km_lusu_vec)) {
    lab <- if (i <= length(LUSU_ROUTE_LEGEND_LABELS)) {
      LUSU_ROUTE_LEGEND_LABELS[i]
    } else {
      paste0("Lu Su route ", i)
    }
    message(lab, ": ", fmt_km(km_lusu_vec[i]), " km")
  }
}
if (!is.na(km_zhou))  message("Zhou Yu route:   ", fmt_km(km_zhou),  " km")
if (!is.na(km_zhuge)) message("Zhuge Liang route: ", fmt_km(km_zhuge), " km")

# --- Optional: ggplot2 (uncomment if installed) ---
# library(ggplot2)
# ggplot(pts) +
#   geom_sf(aes(color = track), size = 3, show.legend = TRUE) +
#   geom_sf_text(aes(label = name), size = 2.3, nudge_y = 0.04, check_overlap = FALSE) +
#   scale_color_manual(values = pal) +
#   coord_sf(crs = st_crs(pts), default_crs = st_crs(pts)) +
#   theme_minimal() +
#   labs(title = "Marked points", x = NULL, y = NULL, color = "Track")
# ggsave("marked_points_map_ggplot.png", width = 8, height = 6, dpi = 200)
