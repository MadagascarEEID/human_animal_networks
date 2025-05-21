library(statnet)

colorblind_palette <- c(
  "#E69F00",  # Orange
  "#56B4E9",  # Sky Blue
  "#009E73",  # Green
  "#F0E442",  # Yellow
  "#0072B2",  # Blue
  "#D55E00",  # Vermillion
  "#CC79A7",  # Reddish Purple
  "#00CCCC",  # Bright Cyan 
  "#882255",  # Dark Red
  "#44AA99",  # Teal
  "#117733",  # Dark Green
  "#332288"   # Dark Blue
)

## FULL NETWORKS -----
#### run ergm script (2.) up until the actual ergms to generate network objects

### starting ny making all humans grey
bipartite_graph_full_network_sna_list[[1]]%v%"color" <- "grey80" #ampandrana
bipartite_graph_full_network_sna_list[[2]]%v%"color" <- "grey80" # mandena
bipartite_graph_full_network_sna_list[[3]]%v%"color" <- "grey80" # sarahandrano


animal_names <- c("cows", "domestic_pigs", "cats", "dogs", "poultry", 
                  "bush_pigs", "tenrecs", "lemurs", "goats_sheep", 
                  "carnivores", "wild_birds", "rodents")

for (g in seq_along(bipartite_graph_full_network_sna_list)) {
  for (i in seq_along(animal_names)) {
    animal <- animal_names[i]
    v_idx <- which(get.vertex.attribute(bipartite_graph_full_network_sna_list[[g]], "vertex.names") == animal)
    if (length(v_idx) > 0) {
      bipartite_graph_full_network_sna_list[[g]] <- set.vertex.attribute(
        bipartite_graph_full_network_sna_list[[g]], 
        "color", 
        value = colorblind_palette[i], 
        v = v_idx
      )
    }
  }
}
## degrees
bipartite_graph_full_network_sna_list[[1]]%v%"degree" <- degree(bipartite_graph_full_network_sna_list[[1]])
bipartite_graph_full_network_sna_list[[2]]%v%"degree" <- degree(bipartite_graph_full_network_sna_list[[2]])
bipartite_graph_full_network_sna_list[[3]]%v%"degree" <- degree(bipartite_graph_full_network_sna_list[[3]])

par(mfrow = c(2, 3), mar = c(2, 0, 2, 0)) 


##### ampandrana full network -----
node_colors_ampandrana_full <- bipartite_graph_full_network_sna_list[[1]] %v% "color"
node_sizes_ampandrana_full <- bipartite_graph_full_network_sna_list[[1]] %v% "degree"
scaled_sizes_ampandrana_full <- ((node_sizes_ampandrana_full / max(node_sizes_ampandrana_full, na.rm = TRUE)) * 3) + 0.5

plot(bipartite_graph_full_network_sna_list[[1]],
     vertex.col = node_colors_ampandrana_full,
     vertex.cex = scaled_sizes_ampandrana_full,
     edge.col = adjustcolor("grey60", alpha.f = 0.5),
     displayisolates = TRUE)
mtext("A", side = 3, line = 0.5, cex = 1, font = 2)  # Add label above

##### mandena full network -----
node_colors_mandena_full <- bipartite_graph_full_network_sna_list[[2]] %v% "color"
node_sizes_mandena_full <- bipartite_graph_full_network_sna_list[[2]] %v% "degree"
scaled_sizes_mandena_full <- ((node_sizes_mandena_full / max(node_sizes_mandena_full, na.rm = TRUE)) * 3) + 0.5

plot(bipartite_graph_full_network_sna_list[[2]],
     vertex.col = node_colors_mandena_full,
     vertex.cex = scaled_sizes_mandena_full,
     edge.col = adjustcolor("grey60", alpha.f = 0.5),
     displayisolates = TRUE)
mtext("M", side = 3, line = 0.5, cex = 1, font = 2)  # Add label above

##### sarahandrano full network -----
node_colors_sarahandrano_full <- bipartite_graph_full_network_sna_list[[3]] %v% "color"
node_sizes_sarahandrano_full <- bipartite_graph_full_network_sna_list[[3]] %v% "degree"
scaled_sizes_sarahandrano_full <- ((node_sizes_sarahandrano_full / max(node_sizes_sarahandrano_full, na.rm = TRUE)) * 3) + 0.5

plot(bipartite_graph_full_network_sna_list[[3]],
     vertex.col = node_colors_sarahandrano_full,
     vertex.cex = scaled_sizes_sarahandrano_full,
     edge.col = adjustcolor("grey60", alpha.f = 0.5),
     displayisolates = TRUE)
mtext("S", side = 3, line = 0.5, cex = 1, font = 2)  # Add label above


## HIGH RISK -----
#### run ergm script (4.) up until the actual ergms to generate network objects
### starting by making all humans grey
bipartite_graph_sna_list_high_risk[[1]]%v%"color" <- "grey80" #ampandrana
bipartite_graph_sna_list_high_risk[[2]]%v%"color" <- "grey80" # mandena
bipartite_graph_sna_list_high_risk[[3]]%v%"color" <- "grey80" # sarahandrano


animal_names <- c("cows", "domestic_pigs", "cats", "dogs", "poultry", 
                  "bush_pigs", "tenrecs", "lemurs", "goats_sheep", 
                  "carnivores", "wild_birds", "rodents")

for (g in seq_along(bipartite_graph_sna_list_high_risk)) {
  for (i in seq_along(animal_names)) {
    animal <- animal_names[i]
    v_idx <- which(get.vertex.attribute(bipartite_graph_sna_list_high_risk[[g]], "vertex.names") == animal)
    if (length(v_idx) > 0) {
      bipartite_graph_sna_list_high_risk[[g]] <- set.vertex.attribute(
        bipartite_graph_sna_list_high_risk[[g]], 
        "color", 
        value = colorblind_palette[i], 
        v = v_idx
      )
    }
  }
}
## degrees
bipartite_graph_sna_list_high_risk[[1]]%v%"degree" <- degree(bipartite_graph_sna_list_high_risk[[1]])
bipartite_graph_sna_list_high_risk[[2]]%v%"degree" <- degree(bipartite_graph_sna_list_high_risk[[2]])
bipartite_graph_sna_list_high_risk[[3]]%v%"degree" <- degree(bipartite_graph_sna_list_high_risk[[3]])

##### ampandrana high risk -----
node_colors_ampandrana_high_risk <- bipartite_graph_sna_list_high_risk[[1]] %v% "color"
node_sizes_ampandrana_high_risk <- bipartite_graph_sna_list_high_risk[[1]] %v% "degree"
scaled_sizes_ampandrana_high_risk <- ((node_sizes_ampandrana_high_risk / max(node_sizes_ampandrana_high_risk, na.rm = TRUE)) * 3) + 0.5

plot(bipartite_graph_sna_list_high_risk[[1]],
     vertex.col = node_colors_ampandrana_high_risk,
     vertex.cex = scaled_sizes_ampandrana_high_risk,
     edge.col = adjustcolor("grey60", alpha.f = 0.5),
     displayisolates = TRUE)
mtext("A", side = 3, line = 0.5, cex = 1, font = 2)  # Add label above

##### mandena high risk -----
node_colors_mandena_high_risk <- bipartite_graph_sna_list_high_risk[[2]] %v% "color"
node_sizes_mandena_high_risk <- bipartite_graph_sna_list_high_risk[[2]] %v% "degree"
scaled_sizes_mandena_high_risk <- ((node_sizes_mandena_high_risk / max(node_sizes_mandena_high_risk, na.rm = TRUE)) * 3) + 0.5

plot(bipartite_graph_sna_list_high_risk[[2]],
     vertex.col = node_colors_mandena_high_risk,
     vertex.cex = scaled_sizes_mandena_high_risk,
     edge.col = adjustcolor("grey60", alpha.f = 0.5),
     displayisolates = TRUE)
mtext("M", side = 3, line = 0.5, cex = 1, font = 2)  # Add label above

##### sarahandrano high risk -----
node_colors_sarahandrano_high_risk <- bipartite_graph_sna_list_high_risk[[3]] %v% "color"
node_sizes_sarahandrano_high_risk <- bipartite_graph_sna_list_high_risk[[3]] %v% "degree"
scaled_sizes_sarahandrano_high_risk <- ((node_sizes_sarahandrano_high_risk / max(node_sizes_sarahandrano_high_risk, na.rm = TRUE)) * 3) + 0.5

plot(bipartite_graph_sna_list_high_risk[[3]],
     vertex.col = node_colors_sarahandrano_high_risk,
     vertex.cex = scaled_sizes_sarahandrano_high_risk,
     edge.col = adjustcolor("grey60", alpha.f = 0.5),
     displayisolates = TRUE)
mtext("S", side = 3, line = 0.5, cex = 1, font = 2)  # Add label above


