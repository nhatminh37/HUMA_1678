###############################################################################
# Red Cliffs DH / HNA / Spatial Humanities analysis
# Refreshed analysis + visualization workflow
###############################################################################

required_pkgs <- c(
  "readr", "dplyr", "tidyr", "stringr", "purrr",
  "igraph", "ggplot2", "forcats", "sf", "ggrepel", "scales"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install required packages first: ", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(igraph)
  library(ggplot2)
  library(forcats)
  library(sf)
  library(ggrepel)
  library(scales)
})

focal_chars <- c("Lu Su", "Zhuge Liang", "Zhou Yu")

# ---------------------------
# 1. Load data
# ---------------------------
df <- read_csv("data/red_cliffs_with_locations.csv", show_col_types = FALSE)
loc_sf <- st_read("data/Location_updated.geojson", quiet = TRUE)
names(loc_sf)[names(loc_sf) == "point"] <- "place"

# ---------------------------
# 2. Helper functions
# ---------------------------
shannon_index <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_real_)
  p <- prop.table(table(x))
  -sum(p * log(p))
}

sort_pair <- function(a, b) {
  tibble(from = pmin(a, b), to = pmax(a, b))
}

most_common <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return("Other")
  names(sort(table(x), decreasing = TRUE))[1]
}

pretty_place <- function(x) {
  x |>
    str_replace_all("_", " ") |>
    str_replace_all("RedCliffs", "Red Cliffs") |>
    str_replace_all("Nanping mountain", "Nanping Mountain") |>
    str_replace_all("Poyang lake", "Poyang Lake")
}

anchor_xy <- st_coordinates(loc_sf)
anchors <- data.frame(
  place = loc_sf$place,
  lon = anchor_xy[, 1],
  lat = anchor_xy[, 2],
  stringsAsFactors = FALSE
)

anchor_point <- function(place) {
  anchors[match(place, anchors$place), c("lon", "lat"), drop = FALSE]
}

proxy_from_anchor <- function(place, dx = 0, dy = 0) {
  base <- anchor_point(place)
  c(lon = base$lon + dx, lat = base$lat + dy)
}

resolve_proxy_coord <- function(location_name) {
  x <- location_name

  if (str_detect(x, fixed("Chaisang (assembly hall)")))         return(proxy_from_anchor("Chaisang",  0.000,  0.035))
  if (str_detect(x, fixed("Chaisang (guest-house)")))           return(proxy_from_anchor("Chaisang",  0.040,  0.018))
  if (str_detect(x, fixed("Chaisang (audience chamber)")))      return(proxy_from_anchor("Chaisang", -0.035,  0.030))
  if (str_detect(x, fixed("Chaisang (palace council chamber)")))return(proxy_from_anchor("Chaisang", -0.010,  0.050))
  if (str_detect(x, fixed("Chaisang (council chamber / private walk)"))) return(proxy_from_anchor("Chaisang", -0.055, -0.005))
  if (str_detect(x, fixed("Chaisang (council chamber)")))       return(proxy_from_anchor("Chaisang", -0.045,  0.010))
  if (str_detect(x, fixed("Chaisang (private apartments)")))    return(proxy_from_anchor("Chaisang", -0.020, -0.040))
  if (str_detect(x, fixed("Chaisang (Zhou Yu's quarters → audience chamber)"))) return(proxy_from_anchor("Chaisang", 0.035, 0.022))
  if (str_detect(x, fixed("Chaisang (Zhou Yu's quarters → palace)"))) return(proxy_from_anchor("Chaisang", 0.040, 0.000))
  if (str_detect(x, fixed("Chaisang (Zhou Yu's quarters)")))    return(proxy_from_anchor("Chaisang",  0.050, -0.010))
  if (str_detect(x, fixed("Chaisang (boat, assembly hall, guest-house)"))) return(proxy_from_anchor("Chaisang", 0.015, 0.020))

  if (str_detect(x, fixed("Fankou (Liu Bei's camp)")))          return(proxy_from_anchor("Fankou",  0.020, -0.020))
  if (str_detect(x, fixed("Fankou / Wu naval camp riverbank"))) return(c(lon = 114.78, lat = 30.26))

  if (str_detect(x, fixed("Wu naval camp, Three Gorges")))      return(c(lon = 114.82, lat = 29.98))
  if (str_detect(x, fixed("Wu naval camp (Gan Ning's quarters)"))) return(c(lon = 114.90, lat = 29.95))
  if (str_detect(x, fixed("Wu naval camp (Huang Gai's tent)"))) return(c(lon = 114.74, lat = 29.90))
  if (str_detect(x, fixed("Wu naval camp (Zhou Yu's tent)")))   return(c(lon = 114.86, lat = 29.89))

  if (str_detect(x, fixed("Yangtze River (Red Cliffs battle)"))) return(proxy_from_anchor("RedCliffs",  0.025, -0.010))
  if (str_detect(x, fixed("Yangtze River (Red Cliffs, Han Dang's ship)"))) return(proxy_from_anchor("RedCliffs", 0.055, 0.020))
  if (str_detect(x, fixed("Yangtze River (fog arrow operation)"))) return(proxy_from_anchor("RedCliffs", 0.090, 0.070))
  if (str_detect(x, fixed("Yangtze River (pursuit from altar)"))) return(proxy_from_anchor("RedCliffs", 0.110, -0.050))
  if (str_detect(x, fixed("Yangtze River (Cao Cao's flagship)"))) return(proxy_from_anchor("RedCliffs", -0.050, 0.210))

  if (str_detect(x, fixed("Cao Cao's camp, north bank (waterfront)"))) return(proxy_from_anchor("RedCliffs", 0.060, 0.185))
  if (str_detect(x, fixed("Cao Cao's camp, north bank")))       return(proxy_from_anchor("RedCliffs",  0.000,  0.185))
  if (str_detect(x, fixed("Black Forest, north bank")))         return(proxy_from_anchor("RedCliffs",  0.145,  0.155))

  if (str_detect(x, fixed("Cross-faction (letter: Jiangling to Chaisang)"))) return(c(lon = 114.65, lat = 30.13))
  if (str_detect(x, fixed("Cross-faction (letter: Wu camp to north bank)"))) return(proxy_from_anchor("RedCliffs", 0.095, 0.110))
  if (str_detect(x, fixed("Cross-faction (letter: north bank to Three Gorges)"))) return(c(lon = 114.35, lat = 30.02))

  if (str_detect(x, fixed("Jiangling (city fortress)")))        return(c(lon = 113.24, lat = 30.35))
  if (str_detect(x, fixed("Huarong Road (mountain pass)")))     return(c(lon = 113.14, lat = 30.08))
  if (str_detect(x, fixed("Western Hills (mountain hut)")))     return(c(lon = 115.72, lat = 30.10))

  if (str_detect(x, fixed("Xiakou")))                           return(proxy_from_anchor("Xiakou", 0, 0))
  if (str_detect(x, fixed("Chaisang")))                         return(proxy_from_anchor("Chaisang", 0, 0))
  if (str_detect(x, fixed("Fankou")))                           return(proxy_from_anchor("Fankou", 0, 0))
  if (str_detect(x, "Red Cliffs|RedCliffs"))                    return(proxy_from_anchor("RedCliffs", 0, 0))
  if (str_detect(x, "Nanping"))                                 return(proxy_from_anchor("Nanping_mountain", 0, 0))
  if (str_detect(x, "Poyang"))                                  return(proxy_from_anchor("Poyang_lake", 0, 0))

  return(proxy_from_anchor("RedCliffs", 0.000, 0.000))
}

classify_domain <- function(description) {
  if (str_detect(description, "Diplomatic|Persuasion|Social|Strategic")) {
    return("Diplomatic / Strategic")
  }
  "Conflict / Command"
}

# ---------------------------
# 3. Network centrality analysis
# ---------------------------
edge_pairs <- sort_pair(df$Person_A, df$Person_B)

edges_undirected <- df %>%
  bind_cols(edge_pairs) %>%
  mutate(
    Domain = vapply(Description, classify_domain, character(1))
  ) %>%
  group_by(from, to) %>%
  summarise(
    weight = sum(Weight, na.rm = TRUE),
    mean_sentiment = mean(Sentiment, na.rm = TRUE),
    domain = most_common(Domain),
    .groups = "drop"
  )

g <- graph_from_data_frame(edges_undirected, directed = FALSE)
E(g)$distance <- 1 / E(g)$weight
E(g)$domain <- edges_undirected$domain
E(g)$mean_sentiment <- edges_undirected$mean_sentiment

person_factions <- bind_rows(
  df %>% transmute(Person = Person_A, Alliance = Alliance_A),
  df %>% transmute(Person = Person_B, Alliance = Alliance_B)
) %>%
  group_by(Person) %>%
  summarise(Faction = most_common(Alliance), .groups = "drop")

V(g)$faction <- person_factions$Faction[match(V(g)$name, person_factions$Person)]
V(g)$faction[is.na(V(g)$faction)] <- "Other"

centrality_tbl <- tibble(
  Person = V(g)$name,
  Betweenness = betweenness(g, directed = FALSE, weights = E(g)$distance, normalized = TRUE),
  Eigenvector = eigen_centrality(g, directed = FALSE, weights = E(g)$weight)$vector,
  Degree = degree(g, normalized = TRUE),
  Weighted_Degree = strength(g, weights = E(g)$weight)
) %>%
  arrange(desc(Betweenness))

focal_centrality <- centrality_tbl %>%
  filter(Person %in% focal_chars) %>%
  arrange(desc(Betweenness))

print(focal_centrality)

centrality_long <- focal_centrality %>%
  select(Person, Betweenness, Eigenvector, Degree) %>%
  pivot_longer(-Person, names_to = "Metric", values_to = "Score")

p_centrality <- ggplot(centrality_long, aes(x = fct_reorder(Person, Score), y = Score, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Betweenness" = "#1b9e77",
    "Eigenvector" = "#7570b3",
    "Degree" = "#d95f02"
  )) +
  labs(
    title = "Centrality Comparison: Lu Su vs Zhuge Liang vs Zhou Yu",
    x = NULL,
    y = "Centrality Score"
  ) +
  theme_minimal(base_size = 12)

ggsave("centrality_comparison.png", p_centrality, width = 9, height = 5, dpi = 300)

# ---------------------------
# 4. Person-event long table for spatial and sentiment analysis
# ---------------------------
person_events <- bind_rows(
  df %>%
    transmute(
      Person = Person_A,
      Person_Alliance = Alliance_A,
      Other = Person_B,
      Other_Alliance = Alliance_B,
      Weight,
      Sentiment,
      Chapter,
      Description,
      Location_Name,
      Location_Type,
      Territory,
      Setting
    ),
  df %>%
    transmute(
      Person = Person_B,
      Person_Alliance = Alliance_B,
      Other = Person_A,
      Other_Alliance = Alliance_A,
      Weight,
      Sentiment,
      Chapter,
      Description,
      Location_Name,
      Location_Type,
      Territory,
      Setting
    )
)

spatial_summary <- person_events %>%
  group_by(Person) %>%
  summarise(
    n_events = n(),
    n_territories = n_distinct(Territory),
    n_location_types = n_distinct(Location_Type),
    n_locations = n_distinct(Location_Name),
    n_settings = n_distinct(Setting),
    n_partner_alliances = n_distinct(Other_Alliance),
    H_territory = shannon_index(Territory),
    H_location_type = shannon_index(Location_Type),
    Spatial_Diversity = H_territory + H_location_type,
    .groups = "drop"
  ) %>%
  arrange(desc(Spatial_Diversity))

focal_spatial <- spatial_summary %>%
  filter(Person %in% focal_chars)

print(focal_spatial)

p_spatial <- focal_spatial %>%
  select(Person, H_territory, H_location_type, Spatial_Diversity) %>%
  pivot_longer(-Person, names_to = "Metric", values_to = "Score") %>%
  ggplot(aes(x = fct_reorder(Person, Score), y = Score, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = c(
    "H_territory" = "#66c2a5",
    "H_location_type" = "#fc8d62",
    "Spatial_Diversity" = "#8da0cb"
  )) +
  labs(
    title = "Spatial Diversity by Character",
    x = NULL,
    y = "Entropy / Composite Score"
  ) +
  theme_minimal(base_size = 12)

ggsave("spatial_diversity_comparison.png", p_spatial, width = 9, height = 5, dpi = 300)

focal_spatial_detail <- person_events %>%
  filter(Person %in% focal_chars) %>%
  group_by(Person) %>%
  summarise(
    Territories = paste(sort(unique(Territory)), collapse = "; "),
    Location_Types = paste(sort(unique(Location_Type)), collapse = "; "),
    Locations = paste(sort(unique(Location_Name)), collapse = "; "),
    .groups = "drop"
  )

print(focal_spatial_detail)

# ---------------------------
# 5. Sentiment and functional role analysis
# ---------------------------
sentiment_summary <- person_events %>%
  filter(Person %in% focal_chars) %>%
  mutate(
    Edge_Scope = if_else(Person_Alliance == Other_Alliance, "Within-alliance", "Cross-alliance")
  ) %>%
  group_by(Person, Edge_Scope) %>%
  summarise(
    n_edges = n(),
    mean_sentiment = mean(Sentiment, na.rm = TRUE),
    positive_share = mean(Sentiment > 0, na.rm = TRUE),
    negative_share = mean(Sentiment < 0, na.rm = TRUE),
    .groups = "drop"
  )

print(sentiment_summary)

role_events <- bind_rows(
  person_events %>%
    filter(Person %in% focal_chars, str_detect(Description, "Social|Diplomatic|Persuasion")) %>%
    mutate(Role_Family = "Social/Diplomatic"),
  person_events %>%
    filter(Person %in% focal_chars, str_detect(Description, "Conflict|Command")) %>%
    mutate(Role_Family = "Conflict/Command"),
  person_events %>%
    filter(Person %in% focal_chars, str_detect(Description, "Strategic|Deception")) %>%
    mutate(Role_Family = "Strategic/Deceptive")
)

role_summary <- role_events %>%
  group_by(Person, Role_Family) %>%
  summarise(
    n_edges = n(),
    mean_sentiment = mean(Sentiment, na.rm = TRUE),
    positive_share = mean(Sentiment > 0, na.rm = TRUE),
    negative_share = mean(Sentiment < 0, na.rm = TRUE),
    .groups = "drop"
  )

print(role_summary)

p_sentiment <- sentiment_summary %>%
  ggplot(aes(x = Person, y = mean_sentiment, fill = Edge_Scope)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_fill_manual(values = c(
    "Cross-alliance" = "#e76f51",
    "Within-alliance" = "#2a9d8f"
  )) +
  labs(
    title = "Sentiment by Character and Alliance Scope",
    x = NULL,
    y = "Mean Sentiment"
  ) +
  theme_minimal(base_size = 12)

ggsave("sentiment_scope_comparison.png", p_sentiment, width = 9, height = 5, dpi = 300)

# ---------------------------
# 6. Refreshed network visualization
# ---------------------------
faction_pal <- c(
  "Wu" = "#3a86ff",
  "Shu" = "#2a9d8f",
  "Wei" = "#d62828",
  "Neutral" = "#f4a261",
  "Other" = "#6c757d"
)

domain_pal <- c(
  "Diplomatic / Strategic" = "#f4a261",
  "Conflict / Command" = "#6c757d"
)

set.seed(1678)
lay <- layout_with_fr(g, weights = E(g)$weight, niter = 3000)
lay_df <- data.frame(
  Person = V(g)$name,
  x = lay[, 1],
  y = lay[, 2],
  Faction = V(g)$faction,
  Betweenness = centrality_tbl$Betweenness[match(V(g)$name, centrality_tbl$Person)],
  Weighted_Degree = centrality_tbl$Weighted_Degree[match(V(g)$name, centrality_tbl$Person)],
  stringsAsFactors = FALSE
)
lay_df$node_size <- rescale(lay_df$Betweenness, to = c(4, 15))
top_labels <- centrality_tbl$Person[1:10]
lay_df$label_flag <- lay_df$Person %in% unique(c(focal_chars, top_labels, "Sun Quan", "Cao Cao", "Liu Bei", "Pang Tong"))

edge_df <- igraph::as_data_frame(g, what = "edges")
edge_df$x <- lay_df$x[match(edge_df$from, lay_df$Person)]
edge_df$y <- lay_df$y[match(edge_df$from, lay_df$Person)]
edge_df$xend <- lay_df$x[match(edge_df$to, lay_df$Person)]
edge_df$yend <- lay_df$y[match(edge_df$to, lay_df$Person)]
edge_df$line_wt <- rescale(edge_df$weight, to = c(0.25, 2.7))

p_network <- ggplot() +
  geom_segment(
    data = edge_df,
    aes(x = x, y = y, xend = xend, yend = yend, color = domain, linewidth = line_wt),
    alpha = 0.35,
    lineend = "round",
    show.legend = TRUE
  ) +
  geom_point(
    data = lay_df,
    aes(x = x, y = y, fill = Faction, size = node_size),
    shape = 21,
    color = "white",
    stroke = 0.55,
    alpha = 0.97
  ) +
  geom_label_repel(
    data = lay_df[lay_df$label_flag, ],
    aes(x = x, y = y, label = Person, fill = Faction),
    color = "#1d3557",
    size = 3.2,
    seed = 1678,
    box.padding = 0.35,
    point.padding = 0.18,
    segment.color = alpha("#495057", 0.35),
    min.segment.length = 0,
    label.size = 0.10,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = faction_pal) +
  scale_color_manual(values = domain_pal) +
  scale_size_identity() +
  scale_linewidth_identity() +
  labs(
    title = "Red Cliffs Character Network",
    subtitle = "Node size = betweenness centrality; edge color = dominant interaction domain",
    fill = "Faction",
    color = "Domain"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(size = 17, face = "bold", color = "#1d3557"),
    plot.subtitle = element_text(size = 10.5, color = "#495057"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(10, 16, 10, 10)
  )

ggsave("red_cliffs_network_plot.png", p_network, width = 12, height = 8, dpi = 320, bg = "white")

# ---------------------------
# 7. More specific proxy-based spatial visualization
# ---------------------------
location_proxy <- tibble(Location_Name = sort(unique(df$Location_Name))) %>%
  rowwise() %>%
  mutate(
    coord = list(resolve_proxy_coord(Location_Name)),
    lon = coord[["lon"]],
    lat = coord[["lat"]]
  ) %>%
  ungroup() %>%
  select(-coord) %>%
  mutate(
    Label = Location_Name |>
      str_replace_all("\\(", "\n(") |>
      pretty_place()
  )

focal_code_lookup <- c("Lu Su" = "A", "Zhou Yu" = "B", "Zhuge Liang" = "C")

encode_person_code <- function(person) {
  code <- unname(focal_code_lookup[person])
  if (is.na(code)) "D" else code
}

encode_actor_marker <- function(person_a, person_b) {
  code_a <- encode_person_code(person_a)
  code_b <- encode_person_code(person_b)

  codes <- sort(unique(c(code_a, code_b)))

  if (length(codes) == 1) {
    return(codes)
  }

  paste(codes, collapse = "->")
}

pair_summary_df <- df %>%
  mutate(
    Actor_Code = purrr::map2_chr(Person_A, Person_B, encode_actor_marker)
  ) %>%
  count(Location_Name, Actor_Code, name = "pair_n") %>%
  mutate(
    Pair_Label = if_else(
      pair_n > 1,
      paste0(Actor_Code, " (x", pair_n, ")"),
      Actor_Code
    )
  ) %>%
  group_by(Location_Name) %>%
  summarise(
    pair_summary = paste(Pair_Label, collapse = "; "),
    .groups = "drop"
  )

spatial_plot_df <- df %>%
  mutate(
    Actor_Code = purrr::map2_chr(Person_A, Person_B, encode_actor_marker)
  ) %>%
  left_join(location_proxy, by = "Location_Name") %>%
  left_join(pair_summary_df, by = "Location_Name") %>%
  group_by(Location_Name, Label, lon, lat, Territory, Location_Type) %>%
  summarise(
    interactions = n(),
    total_weight = sum(Weight, na.rm = TRUE),
    mean_sentiment = mean(Sentiment, na.rm = TRUE),
    chapter_min = min(Chapter, na.rm = TRUE),
    chapter_max = max(Chapter, na.rm = TRUE),
    pair_summary = dplyr::first(pair_summary),
    .groups = "drop"
  ) %>%
  mutate(
    pair_summary = na_if(pair_summary, "")
  )

spatial_plot_df$Label <- paste0(
  spatial_plot_df$Label,
  ifelse(
    !is.na(spatial_plot_df$pair_summary),
    paste0("\nPairs: ", spatial_plot_df$pair_summary),
    ""
  )
)

pts_sf <- st_as_sf(spatial_plot_df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
bb <- st_bbox(pts_sf)
x_pad <- (bb$xmax - bb$xmin) * 0.08
y_pad <- (bb$ymax - bb$ymin) * 0.08

p_map <- ggplot() +
  geom_sf(
    data = pts_sf,
    aes(size = total_weight, color = mean_sentiment),
    alpha = 0.90
  ) +
  geom_label_repel(
    data = spatial_plot_df,
    aes(x = lon, y = lat, label = Label),
    seed = 1678,
    fill = alpha("white", 0.92),
    size = 2.35,
    label.size = 0.10,
    box.padding = 0.42,
    point.padding = 0.22,
    segment.color = alpha("#495057", 0.30),
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  scale_color_gradient2(
    low = "#d62828", mid = "#adb5bd", high = "#2a9d8f",
    midpoint = 0,
    name = "Mean sentiment"
  ) +
  scale_size_continuous(name = "Total interaction weight", range = c(2.2, 8.5)) +
  coord_sf(
    xlim = c(bb$xmin - x_pad, bb$xmax + x_pad),
    ylim = c(bb$ymin - y_pad, bb$ymax + y_pad),
    expand = FALSE
  ) +
  labs(
    title = "Red Cliffs Spatial Interaction Map",
    subtitle = "One summarized point per location; labels list the interaction pairs recorded at that site",
    caption = "Codes: A = Lu Su, B = Zhou Yu, C = Zhuge Liang, D = non-focal character. Repeated pairs at the same site are shown as counts, e.g. B->D (x4).",
    x = "Longitude (deg E)",
    y = "Latitude (deg N)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "#dee2e6", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 17, face = "bold", color = "#1d3557"),
    plot.subtitle = element_text(size = 10.5, color = "#495057"),
    plot.caption = element_text(size = 9, color = "#495057"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

ggsave("red_cliffs_spatial_proxy_map.png", p_map, width = 16, height = 10.5, dpi = 320, bg = "white")

# ---------------------------
# 8. Console summary
# ---------------------------
cat("\n--- FOCAL CENTRALITY ---\n")
print(focal_centrality)

cat("\n--- FOCAL SPATIAL DIVERSITY ---\n")
print(focal_spatial)

cat("\n--- FOCAL SPATIAL DETAIL ---\n")
print(focal_spatial_detail)

cat("\n--- SENTIMENT SUMMARY ---\n")
print(sentiment_summary)

cat("\n--- ROLE SUMMARY ---\n")
print(role_summary)

cat("\n--- OUTPUT FILES ---\n")
cat("centrality_comparison.png\n")
cat("spatial_diversity_comparison.png\n")
cat("sentiment_scope_comparison.png\n")
cat("red_cliffs_network_plot.png\n")
cat("red_cliffs_spatial_proxy_map.png\n")
