# Map 3: chapter-ordered timeline routes (ggplot2 + sf)
# Run from repository root:  Rscript scripts/map3_timeline_routes.R
# Output: map3_timeline_routes.png (repo root)

required_pkgs <- c("sf", "ggplot2", "ggrepel", "ggspatial")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Missing package(s): ", paste(missing_pkgs, collapse = ", "),
       ". Install them with install.packages().")
}

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(ggrepel)
  library(ggspatial)
  library(grid)
})

if (!file.exists("data/Location.geojson")) {
  stop("Need Location.geojson in the working directory.")
}

safe_read_sf <- function(path) {
  tryCatch(st_read(path, quiet = TRUE), error = function(e) NULL)
}

read_first_nonempty <- function(paths, geom_type = NULL) {
  for (path in paths) {
    if (!file.exists(path)) next
    x <- safe_read_sf(path)
    if (is.null(x) || !nrow(x)) next
    if (!is.null(geom_type) && !all(st_is(st_geometry(x), geom_type))) next
    return(x)
  }
  NULL
}

norm_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))
canon_key <- function(x) {
  k <- norm_key(x)
  k[k == "redcliff"] <- "redcliffs"
  k
}
pretty_name <- function(x) {
  out <- gsub("_", " ", x, fixed = TRUE)
  out <- gsub("RedCliffs", "Red Cliffs", out, fixed = TRUE)
  out <- gsub("Nanping mountain", "Nanping Mountain", out, fixed = TRUE)
  out <- gsub("Poyang lake", "Poyang Lake", out, fixed = TRUE)
  trimws(out)
}
line_length_km <- function(x, y, xend, yend, crs = 4326) {
  if (length(x) == 0) return(0)
  vals <- mapply(function(x0, y0, x1, y1) {
    if (identical(round(c(x0, y0), 8), round(c(x1, y1), 8))) return(0)
    ls <- st_sfc(st_linestring(matrix(c(x0, y0, x1, y1), ncol = 2, byrow = TRUE)), crs = crs)
    as.numeric(st_length(ls)) / 1000
  }, x, y, xend, yend)
  sum(vals)
}

loc <- st_read("data/Location.geojson", quiet = TRUE)
names(loc)[names(loc) == "point"] <- "name"
loc$key <- canon_key(loc$name)
xy <- st_coordinates(loc)
loc$lon <- xy[, 1]
loc$lat <- xy[, 2]
loc$label <- pretty_name(loc$name)

lusu_area     <- read_first_nonempty(c("data/shapefiles/LuSu_area.shp", "data/shapefiles/LuSu_Red_Cliffs_Travel_area.shp"),
                                     c("POLYGON", "MULTIPOLYGON"))
lusu_move_pts <- read_first_nonempty("data/LuSu_move_location.geojson", "POINT")
if (!is.null(lusu_move_pts)) {
  mxy <- st_coordinates(lusu_move_pts)
  lusu_move_pts$lon <- mxy[, 1]
  lusu_move_pts$lat <- mxy[, 2]
}

timeline <- data.frame(
  track = c(rep("Lu Su", 5), rep("Zhuge Liang", 5), rep("Zhou Yu", 3)),
  chapter = c(43, 44, 44, 45, 50,
               43, 45, 49, 49, 50,
               44, 44, 45),
  place = c(
    "Xiakou", "Chaisang", "Poyang Lake", "Red Cliff", "Red Cliff",
    "Xiakou", "Chaisang", "Red Cliff", "Nanping mountain", "Fankou",
    "Poyang Lake", "Chaisang", "Red Cliff"
  ),
  stop_idx = c(1:5, 1:5, 1:3),
  stringsAsFactors = FALSE
)
timeline$key <- canon_key(timeline$place)

idx <- match(timeline$key, loc$key)
if (anyNA(idx)) {
  stop("Timeline places missing in Location.geojson: ",
       paste(unique(timeline$place[is.na(idx)]), collapse = ", "))
}
timeline$lon <- loc$lon[idx]
timeline$lat <- loc$lat[idx]

pal        <- c("Lu Su" = "#EE9B00", "Zhuge Liang" = "#0A9396", "Zhou Yu" = "#3A86FF")
line_types <- c("Lu Su" = "solid",   "Zhuge Liang" = "22",      "Zhou Yu" = "42")
track_curvature <- c("Lu Su" = 0.18, "Zhuge Liang" = -0.12, "Zhou Yu" = 0.09)

make_segments <- function(df_track, curvature, loop_nudge = 0.07) {
  n <- nrow(df_track)
  if (n < 2L) return(data.frame())
  out <- vector("list", n - 1L)
  for (i in seq_len(n - 1L)) {
    x    <- df_track$lon[i];     y    <- df_track$lat[i]
    xend <- df_track$lon[i+1L];  yend <- df_track$lat[i+1L]
    same_place <- identical(round(c(x, y), 7), round(c(xend, yend), 7))
    c_use <- curvature
    if (same_place) {
      xend  <- xend + loop_nudge
      yend  <- yend + loop_nudge * 0.42
      c_use <- ifelse(curvature >= 0, 0.42, -0.42)
    }
    mx <- (x + xend) / 2;  my <- (y + yend) / 2
    nx <- -(yend - y);     ny <- (xend - x)
    nlen <- sqrt(nx^2 + ny^2)
    if (nlen < 1e-9) { nx <- 0; ny <- 1; nlen <- 1 }
    nx <- nx / nlen;  ny <- ny / nlen
    out[[i]] <- data.frame(
      track         = df_track$track[i],
      x = x, y = y, xend = xend, yend = yend,
      chapter_from  = df_track$chapter[i],
      chapter_to    = df_track$chapter[i+1L],
      chapter_label = paste0("Ch.", df_track$chapter[i], " -> Ch.", df_track$chapter[i+1L]),
      curvature     = c_use,
      label_x       = mx + nx * 0.035 * c_use,
      label_y       = my + ny * 0.035 * c_use,
      seg_idx       = i,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

# ── FIX: sort first, then split using the *sorted* track column ──────────────
timeline_sorted <- timeline[order(timeline$track, timeline$stop_idx), ]
timeline_split  <- split(timeline_sorted, timeline_sorted$track)
# ─────────────────────────────────────────────────────────────────────────────

segments_df <- do.call(rbind, lapply(names(timeline_split), function(track_nm) {
  make_segments(timeline_split[[track_nm]], track_curvature[track_nm])
}))
rownames(segments_df) <- NULL

chapter_labels <- segments_df

lusu_move_seg   <- data.frame()
lusu_move_label <- data.frame()
if (!is.null(lusu_move_pts) && nrow(lusu_move_pts) >= 2) {
  lxy <- st_coordinates(lusu_move_pts)
  lusu_move_seg <- data.frame(
    track = "Lu Su",
    x    = lxy[-nrow(lxy), 1],  y    = lxy[-nrow(lxy), 2],
    xend = lxy[-1,          1],  yend = lxy[-1,          2],
    stringsAsFactors = FALSE
  )
  lusu_move_label <- data.frame(
    x = 113.642299595569699, y = 29.741710958088525,
    label = "LuSu Ch.50 Route", track = "Lu Su",
    stringsAsFactors = FALSE
  )
}

seg_lusu  <- segments_df[segments_df$track == "Lu Su",       , drop = FALSE]
seg_zhuge <- segments_df[segments_df$track == "Zhuge Liang", , drop = FALSE]
seg_zhou  <- segments_df[segments_df$track == "Zhou Yu",     , drop = FALSE]

starts    <- do.call(rbind, lapply(timeline_split, function(x) x[1, , drop = FALSE]))
dup_group <- paste(round(starts$lon, 5), round(starts$lat, 5), sep = "_")
starts$dup_idx <- ave(seq_len(nrow(starts)), dup_group, FUN = seq_along)
starts$dup_n   <- ave(seq_len(nrow(starts)), dup_group, FUN = length)
theta          <- (starts$dup_idx - 1) * (2 * pi / pmax(1, starts$dup_n))
rad            <- ifelse(starts$dup_n > 1, 0.024, 0)
starts$lon_off <- starts$lon + rad * cos(theta)
starts$lat_off <- starts$lat + rad * sin(theta)

place_labels_df  <- data.frame(lon = loc$lon, lat = loc$lat,
                                label = loc$label, key = loc$key,
                                stringsAsFactors = FALSE)
redcliffs_label  <- place_labels_df[place_labels_df$key == "redcliffs",  , drop = FALSE]
other_place_labels <- place_labels_df[place_labels_df$key != "redcliffs", , drop = FALSE]
if (nrow(redcliffs_label)) {
  redcliffs_label$x <- redcliffs_label$lon - 0.14
  redcliffs_label$y <- redcliffs_label$lat + 0.015
}

dist_km <- sapply(names(timeline_split), function(track_nm) {
  d <- timeline_split[[track_nm]]
  if (nrow(d) < 2) return(0)
  line_length_km(d$lon[-nrow(d)], d$lat[-nrow(d)], d$lon[-1], d$lat[-1])
})
if (nrow(lusu_move_seg)) {
  dist_km["Lu Su"] <- dist_km["Lu Su"] +
    line_length_km(lusu_move_seg$x, lusu_move_seg$y,
                   lusu_move_seg$xend, lusu_move_seg$yend)
}
dist_text <- paste0(
  "Total distance moved: Lu Su ", sprintf("%.1f", dist_km["Lu Su"]), " km | ",
  "Zhuge Liang ", sprintf("%.1f", dist_km["Zhuge Liang"]), " km | ",
  "Zhou Yu ", sprintf("%.1f", dist_km["Zhou Yu"]), " km"
)
dist_legend_label <- paste(
  "Distance (km)",
  paste0("Lu Su: ",       sprintf("%.1f", dist_km["Lu Su"])),
  paste0("Zhuge Liang: ", sprintf("%.1f", dist_km["Zhuge Liang"])),
  paste0("Zhou Yu: ",     sprintf("%.1f", dist_km["Zhou Yu"])),
  sep = "\n"
)

bbox  <- st_bbox(loc)
if (!is.null(lusu_area))
  bbox <- st_bbox(st_union(st_geometry(loc), st_geometry(lusu_area)))
x_pad <- (bbox$xmax - bbox$xmin) * 0.08
y_pad <- (bbox$ymax - bbox$ymin) * 0.10
dist_box <- data.frame(
  x     = bbox$xmax - x_pad * 0.38,
  y     = bbox$ymax + y_pad * 0.82,
  label = dist_legend_label,
  stringsAsFactors = FALSE
)

p <- ggplot() +
  {if (!is.null(lusu_area)) geom_sf(
    data = lusu_area,
    fill  = grDevices::adjustcolor(pal["Lu Su"], alpha.f = 0.10),
    color = grDevices::adjustcolor(pal["Lu Su"], alpha.f = 0.55),
    linewidth = 0.65
  )} +
  geom_sf(data = loc, shape = 21, size = 3.0, stroke = 0.5,
          color = "#33415C", fill = "#F8F9FA") +
  geom_curve(data = seg_lusu,
    aes(x=x, y=y, xend=xend, yend=yend, color=track, linetype=track),
    linewidth=1.40, alpha=0.95, lineend="round",
    arrow=arrow(type="closed", length=unit(0.10,"inches")),
    curvature=unname(track_curvature["Lu Su"])) +
  geom_curve(data = seg_zhuge,
    aes(x=x, y=y, xend=xend, yend=yend, color=track, linetype=track),
    linewidth=1.40, alpha=0.95, lineend="round",
    arrow=arrow(type="closed", length=unit(0.10,"inches")),
    curvature=unname(track_curvature["Zhuge Liang"])) +
  geom_curve(data = seg_zhou,
    aes(x=x, y=y, xend=xend, yend=yend, color=track, linetype=track),
    linewidth=1.40, alpha=0.95, lineend="round",
    arrow=arrow(type="closed", length=unit(0.10,"inches")),
    curvature=unname(track_curvature["Zhou Yu"])) +
  {if (nrow(lusu_move_seg)) geom_curve(
    data = lusu_move_seg,
    aes(x=x, y=y, xend=xend, yend=yend),
    color=unname(pal["Lu Su"]), linewidth=1.30, linetype="13",
    alpha=0.95, lineend="round", curvature=0.25,
    arrow=arrow(type="closed", length=unit(0.09,"inches"))
  )} +
  geom_label_repel(
    data = chapter_labels,
    aes(x=label_x, y=label_y, label=chapter_label, color=track),
    seed=1678,
    fill=grDevices::adjustcolor("white", alpha.f=0.92),
    size=3.0, fontface="bold", label.size=0.18,
    min.segment.length=0, box.padding=0.20, point.padding=0.10,
    segment.size=0.30, show.legend=FALSE
  ) +
  {if (nrow(lusu_move_label)) geom_label(
    data = lusu_move_label, aes(x=x, y=y, label=label),
    size=2.95, fill=grDevices::adjustcolor("white", alpha.f=0.92),
    color=unname(pal["Lu Su"]), linewidth=0.18,
    label.r=unit(0.08,"lines"), fontface="bold"
  )} +
  geom_point(data = starts,
    aes(x=lon_off, y=lat_off, color=track),
    shape=4, stroke=1.45, size=4.4, show.legend=FALSE) +
  {if (nrow(redcliffs_label)) geom_segment(
    data=redcliffs_label,
    aes(x=lon, y=lat, xend=x, yend=y), inherit.aes=FALSE,
    color=grDevices::adjustcolor("#1B263B", alpha.f=0.35), linewidth=0.30
  )} +
  {if (nrow(redcliffs_label)) geom_label(
    data=redcliffs_label, aes(x=x, y=y, label=label), inherit.aes=FALSE,
    fill=grDevices::adjustcolor("white", alpha.f=0.92),
    color="#1B263B", size=3.15, linewidth=0.16
  )} +
  geom_label_repel(
    data=other_place_labels, aes(x=lon, y=lat, label=label),
    fill=grDevices::adjustcolor("white", alpha.f=0.92),
    color="#1B263B", label.size=0.16, size=3.15,
    box.padding=0.28, point.padding=0.14,
    min.segment.length=0, seed=1678,
    segment.color=grDevices::adjustcolor("#1B263B", alpha.f=0.30),
    segment.size=0.32, force=1.7
  ) +
  scale_color_manual(values=pal, name="Commander") +
  scale_linetype_manual(values=line_types, name="Commander") +
  annotation_north_arrow(
    location="tr", which_north="true",
    style=north_arrow_fancy_orienteering(
      line_col="#2B2D42", text_col="#2B2D42", fill=c("#FFFFFF","#2B2D42")),
    pad_x=unit(0.20,"in"), pad_y=unit(0.24,"in"),
    height=unit(0.85,"cm"), width=unit(0.85,"cm")
  ) +
  annotation_scale(
    location="bl", width_hint=0.20,
    pad_x=unit(0.20,"in"), pad_y=unit(0.18,"in"),
    text_cex=0.72, line_width=0.35
  ) +
  geom_label(
    data=dist_box, aes(x=x, y=y, label=label), inherit.aes=FALSE,
    hjust=1, vjust=1, size=3.0, linewidth=0.16,
    fill=grDevices::adjustcolor("white", alpha.f=0.90),
    color="#36454F", lineheight=1.05
  ) +
  coord_sf(
    xlim=c(bbox$xmin - x_pad, bbox$xmax + x_pad),
    ylim=c(bbox$ymin - y_pad, bbox$ymax + y_pad),
    expand=FALSE
  ) +
  labs(
    title    = "Red Cliffs Campaign Timeline (Chapters 43-50)",
    subtitle = "Curved routes with chapter transitions; Lu Su Ch 50 travel area included",
    x = "Longitude (deg E)", y = "Latitude (deg N)",
    caption  = paste0("Data: Location.geojson + LuSu_move_location.geojson",
                      if (!is.null(lusu_area)) " + LuSu_area.shp" else "")
  ) +
  theme_minimal(base_size=11) +
  theme(
    panel.grid.major  = element_line(color="#DEE2E6", linewidth=0.32),
    panel.grid.minor  = element_blank(),
    panel.background  = element_rect(fill="#F6F8FA", color=NA),
    plot.background   = element_rect(fill="white",   color=NA),
    axis.title        = element_text(color="#33415C", face="bold"),
    axis.text         = element_text(color="#495057"),
    plot.title        = element_text(size=18, face="bold", color="#1D3557"),
    plot.subtitle     = element_text(size=10.6, color="#495057"),
    plot.caption      = element_text(size=8.4,  color="#6C757D"),
    legend.position   = c(0.14, 0.84),
    legend.background = element_rect(
      fill=grDevices::adjustcolor("white", alpha.f=0.85), color="#CED4DA"),
    legend.title      = element_text(face="bold")
  )

out_file <- "map3_timeline_routes.png"
ggsave(out_file, p, width=16, height=9, dpi=300, bg="white")
message("Saved: ", normalizePath(out_file, mustWork=FALSE))
message(dist_text)