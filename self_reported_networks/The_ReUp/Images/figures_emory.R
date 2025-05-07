## loading data and packages----

library(here)
library(viridis)
source(here("./self_reported_networks/The_ReUp/3. Animal Interaction Bipartite High Risk.R"))
source(here("./self_reported_networks/The_ReUp/1. Animal Interaction Bipartite Full Network.R"))

## loading colors ----
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

## HIGH RISK -----

### ampandrana andatsakala -----
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "cows"]$color <- colorblind_palette[1]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "domestic_pigs"]$color <- colorblind_palette[2]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "cats"]$color <- colorblind_palette[3]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "dogs"]$color <- colorblind_palette[4]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "poultry"]$color <- colorblind_palette[5]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "bush_pigs"]$color <- colorblind_palette[6]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "tenrecs"]$color <- colorblind_palette[7]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "lemurs"]$color <- colorblind_palette[8]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "goats_sheep"]$color <- colorblind_palette[9]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "carnivores"]$color <- colorblind_palette[10]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "wild_birds"]$color <- colorblind_palette[11]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$name == "rodents"]$color <- colorblind_palette[12]


V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$color <- "gray50"


V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]$label <- V(bipartite_graph_high_risk_ampandrana_andatsakala)$name[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$label <- NA

V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]$size <- V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[!type]]$degree/40+5 
V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$size <- V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)[type]]$degree 


E(bipartite_graph_high_risk_ampandrana_andatsakala)$width <- 0.5
E(bipartite_graph_high_risk_ampandrana_andatsakala)$color <- "gray60"

layout_high_risk_ampandrana_andatsakala <- layout_with_fr(bipartite_graph_high_risk_ampandrana_andatsakala)




### Mandena ------
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "cows"]$color <- colorblind_palette[1]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "domestic_pigs"]$color <- colorblind_palette[2]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "cats"]$color <- colorblind_palette[3]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "dogs"]$color <- colorblind_palette[4]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "poultry"]$color <- colorblind_palette[5]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "bush_pigs"]$color <- colorblind_palette[6]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "tenrecs"]$color <- colorblind_palette[7]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "lemurs"]$color <- colorblind_palette[8]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "goats_sheep"]$color <- colorblind_palette[9]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "carnivores"]$color <- colorblind_palette[10]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "wild_birds"]$color <- colorblind_palette[11]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)$name == "rodents"]$color <- colorblind_palette[12]


V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)[type]]$color <- "gray50"


V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)[!type]]$label <- V(bipartite_graph_high_risk_mandena)$name[V(bipartite_graph_high_risk_mandena)[!type]]
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)[type]]$label <- NA

V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)[!type]]$size <- V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)[!type]]$degree/40+5 
V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)[type]]$size <- V(bipartite_graph_high_risk_mandena)[V(bipartite_graph_high_risk_mandena)[type]]$degree 


E(bipartite_graph_high_risk_mandena)$width <- 0.5
E(bipartite_graph_high_risk_mandena)$color <- "gray60"

layout_high_risk_mandena <- layout_with_fr(bipartite_graph_high_risk_mandena)


### Sarahandrano -----
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "cows"]$color <- colorblind_palette[1]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "domestic_pigs"]$color <- colorblind_palette[2]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "cats"]$color <- colorblind_palette[3]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "dogs"]$color <- colorblind_palette[4]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "poultry"]$color <- colorblind_palette[5]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "bush_pigs"]$color <- colorblind_palette[6]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "tenrecs"]$color <- colorblind_palette[7]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "lemurs"]$color <- colorblind_palette[8]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "goats_sheep"]$color <- colorblind_palette[9]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "carnivores"]$color <- colorblind_palette[10]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "wild_birds"]$color <- colorblind_palette[11]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)$name == "rodents"]$color <- colorblind_palette[12]


V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)[type]]$color <- "gray50"


V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)[!type]]$label <- V(bipartite_graph_high_risk_sarahandrano)$name[V(bipartite_graph_high_risk_sarahandrano)[!type]]
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)[type]]$label <- NA

V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)[!type]]$size <- V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)[!type]]$degree/40+5 
V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)[type]]$size <- V(bipartite_graph_high_risk_sarahandrano)[V(bipartite_graph_high_risk_sarahandrano)[type]]$degree 


E(bipartite_graph_high_risk_sarahandrano)$width <- 0.5
E(bipartite_graph_high_risk_sarahandrano)$color <- "gray60"

layout_high_risk_sarahandrano <- layout_with_fr(bipartite_graph_high_risk_sarahandrano)


# FULL  -----

### ampandrana andatsakala -----
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "cows"]$color <- colorblind_palette[1]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "domestic_pigs"]$color <- colorblind_palette[2]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "cats"]$color <- colorblind_palette[3]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "dogs"]$color <- colorblind_palette[4]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "poultry"]$color <- colorblind_palette[5]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "bush_pigs"]$color <- colorblind_palette[6]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "tenrecs"]$color <- colorblind_palette[7]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "lemurs"]$color <- colorblind_palette[8]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "goats_sheep"]$color <- colorblind_palette[9]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "carnivores"]$color <- colorblind_palette[10]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "wild_birds"]$color <- colorblind_palette[11]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)$name == "rodents"]$color <- colorblind_palette[12]


V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$color <- "gray50"


V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]$label <- V(bipartite_graph_full_network_ampandrana_andatsakala)$name[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$label <- NA

V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]$size <- V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[!type]]$degree/40+5 
V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$size <- V(bipartite_graph_full_network_ampandrana_andatsakala)[V(bipartite_graph_full_network_ampandrana_andatsakala)[type]]$degree 


E(bipartite_graph_full_network_ampandrana_andatsakala)$width <- 0.5
E(bipartite_graph_full_network_ampandrana_andatsakala)$color <- "gray60"

layout_full_network_ampandrana_andatsakala <- layout_with_fr(bipartite_graph_full_network_ampandrana_andatsakala)

### mandena -----
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "cows"]$color <- colorblind_palette[1]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "domestic_pigs"]$color <- colorblind_palette[2]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "cats"]$color <- colorblind_palette[3]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "dogs"]$color <- colorblind_palette[4]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "poultry"]$color <- colorblind_palette[5]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "bush_pigs"]$color <- colorblind_palette[6]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "tenrecs"]$color <- colorblind_palette[7]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "lemurs"]$color <- colorblind_palette[8]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "goats_sheep"]$color <- colorblind_palette[9]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "carnivores"]$color <- colorblind_palette[10]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "wild_birds"]$color <- colorblind_palette[11]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)$name == "rodents"]$color <- colorblind_palette[12]


V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)[type]]$color <- "gray50"


V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)[!type]]$label <- V(bipartite_graph_full_network_mandena)$name[V(bipartite_graph_full_network_mandena)[!type]]
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)[type]]$label <- NA

V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)[!type]]$size <- V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)[!type]]$degree/40+5 
V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)[type]]$size <- V(bipartite_graph_full_network_mandena)[V(bipartite_graph_full_network_mandena)[type]]$degree 


E(bipartite_graph_full_network_mandena)$width <- 0.5
E(bipartite_graph_full_network_mandena)$color <- "gray60"

layout_full_network_mandena <- layout_with_fr(bipartite_graph_full_network_mandena)


### sarahandrano----
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "cows"]$color <- colorblind_palette[1]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "domestic_pigs"]$color <- colorblind_palette[2]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "cats"]$color <- colorblind_palette[3]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "dogs"]$color <- colorblind_palette[4]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "poultry"]$color <- colorblind_palette[5]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "bush_pigs"]$color <- colorblind_palette[6]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "tenrecs"]$color <- colorblind_palette[7]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "lemurs"]$color <- colorblind_palette[8]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "goats_sheep"]$color <- colorblind_palette[9]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "carnivores"]$color <- colorblind_palette[10]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "wild_birds"]$color <- colorblind_palette[11]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)$name == "rodents"]$color <- colorblind_palette[12]


V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)[type]]$color <- "gray50"


V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)[!type]]$label <- V(bipartite_graph_full_network_sarahandrano)$name[V(bipartite_graph_full_network_sarahandrano)[!type]]
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)[type]]$label <- NA

V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)[!type]]$size <- V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)[!type]]$degree/40+5 
V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)[type]]$size <- V(bipartite_graph_full_network_sarahandrano)[V(bipartite_graph_full_network_sarahandrano)[type]]$degree 


E(bipartite_graph_full_network_sarahandrano)$width <- 0.5
E(bipartite_graph_full_network_sarahandrano)$color <- "gray60"
E(bipartite_graph_full_network_sarahandrano)$alpha <- 0.5

layout_full_network_sarahandrano <- layout_with_fr(bipartite_graph_full_network_sarahandrano)

# PLOTTING -----
## Making color legend Box -----

animal_names<-c("Cows", "Pigs", "Cats", "Dogs","Poultry", "Bush Pigs", "Tenrecs", "Lemurs", "Goats/Sheep", "Carnivores","Wild Birds", "Rodents")



# Create a plot with no axes and an empty frame
plot(1, type='n', xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)


# Calculate the positions for each color
color_positions <- seq(0, 1, length.out = length(colorblind_palette) + 1)


# Draw rectangles with gradients
for (i in 1:length(colorblind_palette)) {
  rect(color_positions[i], 0, color_positions[i + 1], 1, col = colorblind_palette[i], border = NA)
  
  # Add labels over each vertical bar
  text((color_positions[i] + color_positions[i + 1]) / 2, 0.5, animal_names[i], pos = 3,
       srt =90, cex = 2)
}


### removing all labels
V(bipartite_graph_high_risk_ampandrana_andatsakala)$label <- NA
V(bipartite_graph_high_risk_sarahandrano)$label <- NA
V(bipartite_graph_high_risk_mandena)$label <- NA

V(bipartite_graph_full_network_ampandrana_andatsakala)$label <- NA
V(bipartite_graph_full_network_sarahandrano)$label <- NA
V(bipartite_graph_full_network_mandena)$label <- NA

### full plots----
library(ggpubr)
library(ggraph)

# full_mandena_plot <- plot(bipartite_graph_full_network_mandena, 
#                                         layout = layout_full_network_mandena, 
#                                         edge.color= adjustcolor(col = "gray60",alpha.f = 0.5),
#                                         vertex.label.cex = 0.7,
#                                         vertex.label.color = "black")

bipartite_graph_full_mandena_no_isolates <- 
  delete.vertices(bipartite_graph_full_network_mandena, V(bipartite_graph_full_network_mandena)[degree(bipartite_graph_full_network_mandena) == 0])


full_mandena_plot <- ggraph(bipartite_graph_full_mandena_no_isolates) +
  geom_edge_link(color = "grey60", alpha=0.5) +
  geom_node_point(aes(fill = color, size = degree), shape = 21, color = "black", stroke = 0.7) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")

full_sarahandrano_plot <- ggraph(bipartite_graph_full_network_sarahandrano) +
  geom_edge_link(color = "grey60", alpha=0.5) +
  geom_node_point(aes(fill = color, size = degree), shape = 21, color = "black", stroke = 0.7) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")


full_ampandrana_adatsakala_plot <- ggraph(bipartite_graph_full_network_ampandrana_andatsakala) +
  geom_edge_link(color = "grey60", alpha=0.5) +
  geom_node_point(aes(fill = color, size = degree), shape = 21, color = "black", stroke = 0.7) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")

ggarrange(full_mandena_plot,
          full_sarahandrano_plot,
          full_ampandrana_adatsakala_plot,
          ncol = 3,
          labels = c("a", "b", "c"), common.legend = FALSE)



### high risk plots -----
par(mfrow = c(1,3))
high_risk_mandena_plot <- ggraph(bipartite_graph_high_risk_mandena) +
  geom_edge_link(color = "grey60", alpha=0.5) +
  geom_node_point(aes(fill = color, size = degree), shape = 21, color = "black", stroke = 0.7) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")


bipartite_graph_high_risk_mandena_no_isolates <- 
  delete.vertices(bipartite_graph_high_risk_mandena, V(bipartite_graph_high_risk_mandena)[degree(bipartite_graph_high_risk_mandena) == 0])

# Plot without isolates
high_risk_mandena_plot <- ggraph(bipartite_graph_high_risk_mandena_no_isolates) +
  geom_edge_link(color = "grey60", alpha = 0.5) +
  geom_node_point(aes(fill = color, size = degree), shape = 21, color = "black", stroke = 0.7) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")

high_risk_mandena_plot


high_risk_sarahandrano_plot <- ggraph(bipartite_graph_high_risk_sarahandrano) +
  geom_edge_link(color = "grey60", alpha=0.5) +
  geom_node_point(aes(fill = color, size = degree), shape = 21, color = "black", stroke = 0.7) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")


high_risk_ampandrana_adatsakala_plot <- ggraph(bipartite_graph_high_risk_ampandrana_andatsakala) +
  geom_edge_link(color = "grey60", alpha=0.5) +
  geom_node_point(aes(fill = color, size = degree), shape = 21, color = "black", stroke = 0.7) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none")

class(mand_plot)


ggarrange(full_mandena_plot,
          full_sarahandrano_plot,
          full_ampandrana_adatsakala_plot,
          high_risk_mandena_plot,
          high_risk_sarahandrano_plot,
          high_risk_ampandrana_adatsakala_plot,
          labels = c("a", "b", "c","a", "b", "c"),
        common.legend = FALSE)

anda_plot + 
  geom_edge_link(width = .5)



