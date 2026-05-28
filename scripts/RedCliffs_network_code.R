library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggrepel)

#  Load our Nodes and Edges table for the network
#  Nodes table contain character and places; 
#  Edges table represent interaction or connection between them.
nodes <- read_csv("Nodes_table_Red_Cliffs.csv")
edges <- read_csv("Edges_table_Red_Cliffs.csv")

#  Keep only person-to-person interaction edges for the social network analysis
#  Exclude person-to-place links because they belong to the spatial dataset
person_edges <- edges %>%
  filter(Type == "Human interaction") %>%   
  rename(from = Nodes_A, to = Nodes_B)    

#  Convert the cleaned node and edge tables into an undirected graph object
#  Undirected because interactions are treated as mutual relationships
rc_graph <- tbl_graph(nodes = nodes, edges = person_edges, directed = FALSE)

#  Calculate degree and eigenvector centrality measures for each character node
#  Degree centrality captures direct connectivity
#  Eigenvector centrality captures how strongly a character is tied to other influential nodes
rc_graph <- rc_graph %>%
  activate(nodes) %>%
  mutate(
    degree_score = centrality_degree(weights = Weight),              
    eigen_score = centrality_eigen(weights = Weight)           
  )
#  Rank characters by weighted degree centrality
top_degree <- rc_graph %>%
  as_tibble() %>%
  arrange(desc(degree_score))
#  Rank characters by eigenvector centrality
top_eigen <- rc_graph %>%
  as_tibble() %>%
  arrange(desc(eigen_score)) 

#  Remove isolated nodes 
rc_graph_clean <- rc_graph %>%
  activate(nodes) %>%
  filter(centrality_degree() > 0)

#  Fix the random seed so the stress-layout network is reproducible
set.seed(123)
#  Assign faction colours for the node categories used in the network graph
faction_colors <- c(
  "Shu" = "#4ca750",        
  "Wu" = "#a83432",         
  "Wei" = "#42A5F5",      
  "Neutral" = "#a4a4a4",     
  "Neutral / River" = "#a4a4a4"
)
#  Plot the person-to-person interaction network.
#  Node size shows eigenvector centrality, node colour shows faction.
#  Edge width shows interaction weight, and edge colour shows sentiment.
p <- ggraph(rc_graph_clean, layout = 'stress') +
  geom_edge_link(aes(edge_width = Weight, color = Sentiment), alpha = 0.6) +
  scale_edge_color_gradient2(low = "red", mid = "gray85", high = "forestgreen", midpoint = 0) +
  scale_edge_width(range = c(1, 6)) +
  
  geom_node_point(aes(size = eigen_score, color = `Faction/Territory`), alpha = 0.95) +
  scale_size(range = c(6, 24)) +
  scale_color_manual(values = faction_colors, na.value = "grey60") +

   #  Label each character node directly at its plotted position
   geom_node_text(aes(label = Id), 
                 repel = FALSE,           
                 vjust = 0.5,              
                 hjust = 0.5,              
                 size = 6, 
                 fontface = "bold", 
                 color = "black",
                 bg.color = "white",
                 bg.r = 0.15) +
  
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
    legend.text = element_text(size = 14),
  )

print(p)

library(dplyr)
library(readr)

# Create a clean export table with the two centrality measures used in the report and poster
centrality_export <- rc_graph_clean %>%
  as_tibble() %>%
  select(Id, `Faction/Territory`, eigen_score, degree_score) %>%
  arrange(desc(eigen_score)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Save the ranking table to the Desktop as a CSV file
save_path <- file.path(getwd(), "RedCliffs_Centrality_Findings.csv")
write_csv(centrality_export, save_path)