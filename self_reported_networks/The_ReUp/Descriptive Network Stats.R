library(here)
library(tidyverse)
library(kableExtra)
source(here("self_reported_networks/The_ReUp/1. Animal Interaction Bipartite Full Network.R"))
source(here("self_reported_networks/The_ReUp/3. Animal Interaction Bipartite High Risk.R"))


# Function to calculate degree statistics and network properties for each village
calculate_network_stats <- function(graph, village_name) {
  human_degrees <- V(graph)$degree[V(graph)$type]  # Degrees for humans
  animal_degrees <- V(graph)$degree[!V(graph)$type]  # Degrees for animals
  
  
  # Collect statistics
  stats <- data.frame(
    Village = village_name,
    Number_Human_Nodes = sum(V(graph)$type == TRUE),
    Mean_Human_Degree = mean(human_degrees),
    SD_Human_Degree = sd(human_degrees),
    Min_Human_Degree = min(human_degrees),
    Max_Human_Degree = max(human_degrees),
    Number_Animal_Nodes = sum(V(graph)$type == FALSE),
    Mean_Animal_Degree = mean(animal_degrees),
    SD_Animal_Degree = sd(animal_degrees),
    Min_Animal_Degree = min(animal_degrees),
    Max_Animal_Degree = max(animal_degrees),
    Num_Edges = ecount(graph),
    Density = igraph::edge_density(graph)
  )
  
  return(stats)
}

# List of graph objects and corresponding village names
graphs <- list(
  "Ampandrana Andatsakala" = bipartite_graph_full_network_ampandrana_andatsakala,
  "Mandena" = bipartite_graph_full_network_mandena,
  "Sarahandrano" = bipartite_graph_full_network_sarahandrano
)

# Iterate over graphs and village names to calculate stats
village_stats_list <- lapply(names(graphs), function(village_name) {
  calculate_network_stats(graphs[[village_name]], village_name)
})

# Combine the results into a single data frame
village_stats <- bind_rows(village_stats_list)
village_stats$Village <- factor(village_stats$Village, levels = c("Mandena","Sarahandrano", "Ampandrana Andatsakala"))
village_stats <- village_stats[order(village_stats$Village), ]


# Print the results as a table using knitr::kable for clean display in R Markdown
kable(village_stats, 
      caption = "Full Network Statistics Comparison Across Villages",
      col.names = c("Village","Number Human Nodes", "Mean Human Degree", "SD Human Degree", "Min Human Degree", 
                    "Max Human Degree", "Number Animal Nodes", "Mean Animal Degree", "SD Animal Degree", "Min Animal Degree",
                    "Max Animal Degree",
                    "Number of Edges", "Density"),
      digits = 4,
      row.names = FALSE) 

graphs_high_risk <- list(
  "Ampandrana Andatsakala" = bipartite_graph_high_risk_ampandrana_andatsakala,
  "Mandena" = bipartite_graph_high_risk_mandena,
  "Sarahandrano" = bipartite_graph_high_risk_sarahandrano
)

# Iterate over graphs_high_risk and village names to calculate stats
village_stats_list_high_risk <- lapply(names(graphs_high_risk), function(village_name) {
  calculate_network_stats(graphs_high_risk[[village_name]], village_name)
})

# Combine the results into a single data frame
village_stats_high_risk <- bind_rows(village_stats_list_high_risk)
village_stats_high_risk$Village <- factor(village_stats_high_risk$Village, levels = c("Mandena","Sarahandrano", "Ampandrana Andatsakala"))
village_stats_high_risk <- village_stats_high_risk[order(village_stats_high_risk$Village), ]

# Print the results as a table using knitr::kable for clean display in R Markdown
kable(village_stats_high_risk, 
      caption = "Full Network Statistics Comparison Across Villages",
      col.names = c("Village", "Number Human Nodes", "Mean Human Degree", "SD Human Degree", "Min Human Degree", 
                    "Max Human Degree", "Number Animal Nodes", "Mean Animal Degree", "SD Animal Degree", "Min Animal Degree",
                    "Max Animal Degree",
                    "Number of Edges", "Density"),
      digits = 4,
      row.names = FALSE)
