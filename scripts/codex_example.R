# Red Cliffs DH / HNA / Spatial Humanities analysis
# File: red_cliffs_with_locations.csv

required_pkgs <- c("readr", "dplyr", "tidyr", "stringr", "purrr", "igraph", "ggplot2", "forcats")
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
})

# ---------------------------
# 1. Load data
# ---------------------------
df <- read_csv("data/red_cliffs_with_locations.csv", show_col_types = FALSE)

focal_chars <- c("Lu Su", "Zhuge Liang", "Zhou Yu")

# ---------------------------
# Helper functions
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

# ---------------------------
# 2. Network centrality analysis
# ---------------------------
# Build undirected weighted edge list
edge_pairs <- sort_pair(df$Person_A, df$Person_B)

edges_undirected <- df %>%
  bind_cols(edge_pairs) %>%
  group_by(from, to) %>%
  summarise(weight = sum(Weight, na.rm = TRUE), .groups = "drop")

g <- graph_from_data_frame(edges_undirected, directed = FALSE)

# For weighted betweenness, stronger tie = shorter path
E(g)$distance <- 1 / E(g)$weight

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

# Long format for plotting
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
# 3. Person-event long table for spatial and sentiment analysis
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

# ---------------------------
# 4. Spatial-network synthesis
# ---------------------------
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

# Also keep a readable table of the actual spatial proxies
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

# Functional-role families based on description tags.
# Note: rows can appear in more than one functional family because the source descriptions are composite.
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
# 6. Rank focal characters directly
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