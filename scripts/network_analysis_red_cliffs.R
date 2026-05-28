# Network analysis: person-to-person graph, centrality, and network figure
# Run from repository root:  Rscript scripts/network_analysis_red_cliffs.R
#
# Requires data/Nodes_table_Red_Cliffs.csv and data/Edges_table_Red_Cliffs.csv
# (tidygraph node/edge tables used for the paper network figure).

library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggrepel)

script_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
repo_root <- if (length(script_file)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_file[1])), ".."))
} else if (dir.exists("data")) {
  normalizePath(".")
} else {
  normalizePath("..")
}
data_dir <- file.path(repo_root, "data")
figures_dir <- repo_root

nodes_path <- file.path(data_dir, "Nodes_table_Red_Cliffs.csv")
edges_path <- file.path(data_dir, "Edges_table_Red_Cliffs.csv")
if (!file.exists(nodes_path) || !file.exists(edges_path)) {
  stop(
    "Missing node/edge tables. Place Nodes_table_Red_Cliffs.csv and ",
    "Edges_table_Red_Cliffs.csv in data/ (export from your graph build)."
  )
}

nodes <- read_csv(nodes_path, show_col_types = FALSE)
edges <- read_csv(edges_path, show_col_types = FALSE)

person_edges <- edges %>%
  filter(Type == "Human interaction") %>%
  rename(from = Nodes_A, to = Nodes_B)

rc_graph <- tbl_graph(nodes = nodes, edges = person_edges, directed = FALSE)

rc_graph <- rc_graph %>%
  activate(nodes) %>%
  mutate(
    degree_score = centrality_degree(weights = Weight),
    eigen_score  = centrality_eigen(weights = Weight)
  )

top_degree <- rc_graph %>%
  as_tibble() %>%
  arrange(desc(degree_score))

top_eigen <- rc_graph %>%
  as_tibble() %>%
  arrange(desc(eigen_score))

rc_graph_clean <- rc_graph %>%
  activate(nodes) %>%
  filter(centrality_degree() > 0)

set.seed(123)
faction_colors <- c(
  "Shu" = "#4ca750",
  "Wu" = "#a83432",
  "Wei" = "#42A5F5",
  "Neutral" = "#a4a4a4",
  "Neutral / River" = "#a4a4a4"
)

p <- ggraph(rc_graph_clean, layout = "stress") +
  geom_edge_link(aes(edge_width = Weight, color = Sentiment), alpha = 0.6) +
  scale_edge_color_gradient2(low = "red", mid = "gray85", high = "forestgreen", midpoint = 0) +
  scale_edge_width(range = c(1, 6)) +
  geom_node_point(aes(size = eigen_score, color = `Faction/Territory`), alpha = 0.95) +
  scale_size(range = c(6, 24)) +
  scale_color_manual(values = faction_colors, na.value = "grey60") +
  geom_node_text(
    aes(label = Id),
    repel = FALSE,
    vjust = 0.5,
    hjust = 0.5,
    size = 6,
    fontface = "bold",
    color = "black",
    bg.color = "white",
    bg.r = 0.15
  ) +
  theme_graph(base_family = "sans") +
  labs(
    title = "The Battle of Red Cliffs: Diplomatic & Strategic Network",
    subtitle = "Node size: Eigenvector Centrality | Edge color: Sentiment (-1 to +1)",
    edge_width = "Interaction Weight",
    size = "Eigen centrality (Influence)",
    color = "Faction"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 26, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 16, color = "grey30", margin = margin(b = 20)),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  )

print(p)

network_png <- file.path(figures_dir, "network_graph_red_cliffs.png")
ggsave(network_png, p, width = 14, height = 10, dpi = 300, bg = "white")
message("Saved: ", normalizePath(network_png, mustWork = FALSE))

centrality_export <- rc_graph_clean %>%
  as_tibble() %>%
  select(Id, `Faction/Territory`, eigen_score, degree_score) %>%
  arrange(desc(eigen_score)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

centrality_csv <- file.path(data_dir, "RedCliffs_Centrality_Findings.csv")
write_csv(centrality_export, centrality_csv)
message("Saved: ", normalizePath(centrality_csv, mustWork = FALSE))
