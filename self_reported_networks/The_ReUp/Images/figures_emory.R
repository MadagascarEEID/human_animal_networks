## loading data and packages----

library(here)
library(viridis)
source(here("./self_reported_networks/The_ReUp/3. Animal Interaction Bipartite High Risk.R"))
source(here("./self_reported_networks/The_ReUp/1. Animal Interaction Bipartite Full Network.R"))

## loading colors ----
colors <- viridis(12, option = "C", begin = 0.2)

## HIGH RISK -----
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "bush_pigs"]$color <- colors[1]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "tenrecs"]$color <- colors[2]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "lemurs"]$color <- colors[3]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "goats_sheep"]$color <- colors[4]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "carnivores"]$color <- colors[5]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "wild_birds"]$color <- colors[6]

V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "dogs"]$color <- colors[7]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "domestic_pigs"]$color <- colors[8]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "rodents"]$color <- colors[9]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "cats"]$color <- colors[10]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "cows"]$color <- colors[11]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "poultry"]$color <- colors[12]


V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$color <- "gray50"


V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]$label <- V(bipartite_graph_high_risk_ampandrana_andatsakala)$name[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$label <- NA

V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]$size <- V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]$degree/40+5 
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$size <- V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$degree 


E(bipartite_graph_high_risk_ampandrana_andatsakala)$width <- 0.5
E(bipartite_graph_high_risk_ampandrana_andatsakala)$color <- "gray80"




animal_names<-c("Bush Pigs", "Tenrecs", "Lemurs", "Goats/Sheep", "Carnivores","Wild Birds", "Dogs", "Domestic Pigs", "Rodents", "Cats", "Cows",
                 "Poultry")



# Create a plot with no axes and an empty frame
plot(1, type='n', xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)


# Calculate the positions for each color
color_positions <- seq(0, 1, length.out = length(colors) + 1)


# Draw rectangles with gradients
for (i in 1:length(colors)) {
  rect(color_positions[i], 0, color_positions[i + 1], 1, col = colors[i], border = NA)
  
  # Add labels over each vertical bar
  text((color_positions[i] + color_positions[i + 1]) / 2, 0.5, animal_names[i], pos = 3,
       srt =90, cex = 2)
}


# FULL  -----
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "bush_pigs"]$color <- colors[1]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "tenrecs"]$color <- colors[2]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "lemurs"]$color <- colors[3]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "goats_sheep"]$color <- colors[4]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "carnivores"]$color <- colors[5]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "wild_birds"]$color <- colors[6]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "dogs"]$color <- colors[7]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "domestic_pigs"]$color <- colors[8]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "rodents"]$color <- colors[9]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "cats"]$color <- colors[10]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "cows"]$color <- colors[11]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "poultry"]$color <- colors[12]


V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$color <- "gray50"


V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]$label <- V(bipartite_graph_full_network_ampandrana_andatsakala)$name[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$label <- NA

V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]$size <- V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]$degree/40+5 
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$size <- V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$degree 


E(bipartite_graph_full_network_ampandrana_andatsakala)$width <- 0.5
E(bipartite_graph_full_network_ampandrana_andatsakala)$color <- "gray80"


# PLOTTING -----
par(mfrow=c(1,2))

plot(bipartite_graph_full_network_ampandrana_andatsakala, 
     layout = layout.random, 
     vertex.label.cex = 0.7,   
     vertex.label.color = "black")


plot(bipartite_graph_high_risk_ampandrana_andatsakala, 
     layout = layout.random, 
     vertex.label.cex = 0.7,
     vertex.label.color = "black")




