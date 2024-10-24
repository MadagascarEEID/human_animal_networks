library(here)
source(here("./self_reported_networks/1. Animal Interaction Bipartite Full Network.R"))
source(here("./self_reported_networks/3. Animal Interaction Bipartite High Risk.R"))
library(kableExtra)
# List of bipartite graphs and corresponding titles
bipartite_graphs <- list(
  bipartite_graph_full_network_ampandrana_andatsakala,
  bipartite_graph_full_network_mandena,
  bipartite_graph_full_network_sarahandrano
)

graph_titles <- c("Ampandrana", "Mandena", "Sarahandrano")
# 

# Loop through each bipartite graph for community detection
for (i in seq_along(bipartite_graphs)) {
  
  if (i == 2){ # deleting lemur vertex from Mandena bipartite graph bc it's an isolate!
    bipartite_graphs[[i]] <- induced_subgraph(bipartite_graphs[[i]], 
                                              vids = V(bipartite_graphs[[i]])[V(bipartite_graphs[[i]])$animal_name != "lemurs"])
  }
  
  else{
    bipartite_graphs[[i]] <- bipartite_graphs[[i]] 
  }
  # Project the bipartite graph to get the animal mode
  animal_projection <- bipartite_projection(bipartite_graphs[[i]],
                                            multiplicity = TRUE) # multipliicty = TRUE makes edge weight attribute
  
  # Extract the projected animal graph with edge weights
  animal_graph <- animal_projection[[1]]

  # Community detection with weights
  louvain_communities <- cluster_louvain(animal_graph, weights = E(animal_graph)$weight)
  walktrap_communities <- cluster_walktrap(animal_graph, weights = E(animal_graph)$weight)
  eigenspectrum_communities <- cluster_leading_eigen(animal_graph, weights = E(animal_graph)$weight)
  
  # Calculate modularity for each method
  modularity_results <- data.frame(
    Method = c("Louvain", "Walktrap", "Eigenspectrum"),
    Modularity = c(modularity(louvain_communities),
                   modularity(walktrap_communities),
                   modularity(eigenspectrum_communities))
  )
  
  # Identify the method with the highest modularity
  best_method <- modularity_results[which.max(modularity_results$Modularity), ]
  
  cat(paste("Best community detection method for", graph_titles[i], ":", best_method$Method, 
            "with modularity =", round(best_method$Modularity, 4), "\n"))
  
  # Assign the best community membership to vertex attributes
  if (best_method$Method == "Louvain") {
    V(animal_graph)$community <- membership(louvain_communities)
  } else if (best_method$Method == "Walktrap") {
    V(animal_graph)$community <- membership(walktrap_communities)
  } else {
    V(animal_graph)$community <- membership(eigenspectrum_communities)
  }
  
  # Create a data frame for clear output
  community_df <- data.frame(
    Animal = V(animal_graph)$name,
    Community = V(animal_graph)$community
  ) %>% arrange(Community, Animal)  # Sort for better readability
  
  # Print the community table
  print(kable(community_df, col.names = c("Animal", "Community"), 
              caption = paste("Animals and their Communities in", graph_titles[i])))
  
  # Visualize the community structure for the best method
  plot(animal_graph,
       vertex.color = V(animal_graph)$community + 1,  # Color by community
       vertex.label.color = "black",
       vertex.size = 5,
       main = paste("Community Detection:", graph_titles[i], "-", best_method$Method),
       edge.width = E(animal_graph)$weight/10,  # Edge width by weight
       edge.color = "grey")
}


# # high risk ----

bipartite_graphs_high_risk <- list(
  bipartite_graph_high_risk_ampandrana_andatsakala, # no animal isolates
  bipartite_graph_high_risk_mandena, # lemur isolate
  bipartite_graph_high_risk_sarahandrano # no animal isolate
)

graph_titles <- c("Ampandrana and Andatsakala", "Mandena", "Sarahandrano")

# Loop through each bipartite graph for community detection
for (i in seq_along(bipartite_graphs_high_risk)) {
  if (i == 2){ # deleting lemur vertex from Mandena bipartite graph
    bipartite_graphs_high_risk[[i]] <- induced_subgraph(bipartite_graphs_high_risk[[i]], 
                                                        vids = V(bipartite_graphs_high_risk[[i]])[V(bipartite_graphs_high_risk[[i]])$animal_name != "lemurs"])
  }
  
  # Project the bipartite graph to get the animal mode
  animal_projection_high_risk <- bipartite_projection(bipartite_graphs_high_risk[[i]],
                                                      multiplicity = TRUE) # multiplicity = TRUE makes edge weight attribute
  
  # Extract the projected animal graph with edge weights
  animal_graph_high_risk <- animal_projection_high_risk[[1]]
  
  # Community detection with weights
  louvain_communities_high_risk <- cluster_louvain(animal_graph_high_risk, weights = E(animal_graph_high_risk)$weight)
  walktrap_communities_high_risk <- cluster_walktrap(animal_graph_high_risk, weights = E(animal_graph_high_risk)$weight)
  eigenspectrum_communities_high_risk <- cluster_leading_eigen(animal_graph_high_risk, weights = E(animal_graph_high_risk)$weight)
  
  # Calculate modularity for each method
  modularity_results_high_risk <- data.frame(
    Method = c("Louvain", "Walktrap", "Eigenspectrum"),
    Modularity = c(modularity(louvain_communities_high_risk),
                   modularity(walktrap_communities_high_risk),
                   modularity(eigenspectrum_communities_high_risk))
  )
  
  # Identify the method with the highest modularity
  best_method_high_risk <- modularity_results_high_risk[which.max(modularity_results_high_risk$Modularity), ]
  
  cat(paste("Best community detection method for", graph_titles[i], ":", best_method_high_risk$Method, 
            "with modularity =", round(best_method_high_risk$Modularity, 4), "\n"))
  
  # Assign the best community membership to vertex attributes
  if (best_method_high_risk$Method == "Louvain") {
    V(animal_graph_high_risk)$community_high_risk <- membership(louvain_communities_high_risk)
  } else if (best_method_high_risk$Method == "Walktrap") {
    V(animal_graph_high_risk)$community_high_risk <- membership(walktrap_communities_high_risk)
  } else {
    V(animal_graph_high_risk)$community_high_risk <- membership(eigenspectrum_communities_high_risk)
  }
  
  # Create a data frame for clear output
  community_df_high_risk <- data.frame(
    Animal = V(animal_graph_high_risk)$name,
    Community = V(animal_graph_high_risk)$community_high_risk
  ) %>% arrange(Community, Animal)  # Sort for better readability
  
  # Print the community table
  print(kable(community_df_high_risk, col.names = c("Animal", "Community"), 
              caption = paste("Animals and their Communities in", graph_titles[i])))
  
  # Visualize the community structure for the best method
  plot(animal_graph_high_risk,
       vertex.color = V(animal_graph_high_risk)$community_high_risk + 1,  # Color by community
       vertex.label.color = "black",
       vertex.size = 5,
       main = paste("Community Detection:", graph_titles[i], "-", best_method_high_risk$Method),
       edge.width = E(animal_graph_high_risk)$weight / 10,  # Edge width by weight
       edge.color = "grey")
}



