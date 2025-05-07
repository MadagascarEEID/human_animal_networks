library(ggpubr);library(igraph); library(ggnetwork); library(GGally); library(intergraph)

ggnet2(
  bipartite_graph_full_network_sarahandrano,
  edge.size = 0.5,        # Use the 'weight' attribute for edge thickness
  edge.color = "grey",
  node.size = 3,
  node.color = "color",        # Color based on type
  legend.size = 10,            # Legend size
  # palette = c("Animal" = "forestgreen", "Human" = "skyblue3"), 
  legend.position = "right"    # Position of the legend
)

