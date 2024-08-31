library(igraph)

# descriptive stats ----
# get number animal-human connections in bipartite graph full network
mean(V(bipartite_graph)$degree[V(bipartite_graph)$type]) ##degrees for humans
sd(V(bipartite_graph)$degree[V(bipartite_graph)$type]) ##sd degrees for humans

range(V(bipartite_graph)$degree[V(bipartite_graph)$type]) ##range for humans

mean(V(bipartite_graph)$degree[!V(bipartite_graph)$type]) ##degrees for animals
sd(V(bipartite_graph)$degree[!V(bipartite_graph)$type]) ##degrees for animals
range(V(bipartite_graph)$degree[!V(bipartite_graph)$type]) ##range for animals


# get number animal-human connections in bipartite graph high risk network
mean(V(bipartite_graph_high_risk)$degree[V(bipartite_graph_high_risk)$type]) ##degrees for humans
sd(V(bipartite_graph_high_risk)$degree[V(bipartite_graph_high_risk)$type]) ##sd degrees for humans

range(V(bipartite_graph_high_risk)$degree[V(bipartite_graph_high_risk)$type]) ##range for humans

mean(V(bipartite_graph_high_risk)$degree[!V(bipartite_graph_high_risk)$type]) ##degrees for animals
sd(V(bipartite_graph_high_risk)$degree[!V(bipartite_graph_high_risk)$type]) ##degrees for animals

range(V(bipartite_graph_high_risk)$degree[!V(bipartite_graph_high_risk)$type]) ##range for animals


table(V(bipartite_graph_high_risk)$name[!V(bipartite_graph_high_risk)$type], V(bipartite_graph_high_risk)$degree[!V(bipartite_graph_high_risk)$type])


# get number of edges
ecount(bipartite_graph)
ecount(bipartite_graph_high_risk)

# get density
igraph::edge_density(bipartite_graph)
igraph::edge_density(bipartite_graph_high_risk)

# distance
igraph::mean_distance(bipartite_graph)
igraph::mean_distance(bipartite_graph_high_risk)


## Full bipartite graph ----
V(bipartite_graph)$label<-""
# V(bipartite_graph)[animal_vertices]$label<-V(bipartite_graph)$name[animal_vertices]
V(bipartite_graph)[!V(bipartite_graph)$type]$size <- V(bipartite_graph)$degree[!V(bipartite_graph)$type]/40+5 ##degrees for animals
V(bipartite_graph)[V(bipartite_graph)$type]$size <-V(bipartite_graph)$degree[V(bipartite_graph)$type]

V(bipartite_graph)$color[V(bipartite_graph)$village == "Village 1"] <- adjustcolor("red", alpha=0.6)
V(bipartite_graph)$color[V(bipartite_graph)$village == "Village 2"] <- adjustcolor("orange", alpha=0.6)
V(bipartite_graph)$color[V(bipartite_graph)$village == "Village 3"] <- adjustcolor("purple", alpha=0.2)



# Define the new colors for each animal
colors <- c("#F0F8FF", "#B0E0E6", "#ADD8E6", "#87CEEB", "#87CEFA", 
            "#00BFFF", "#1E90FF", "#6495ED", "#5F9EA0", "#4682B4", "#4169E1")

# Apply the new colors to the vertices
V(bipartite_graph)[V(bipartite_graph)$name == "bush_pigs"]$color <- colors[1]
V(bipartite_graph)[V(bipartite_graph)$name == "lemurs"]$color <- colors[2]
V(bipartite_graph)[V(bipartite_graph)$name == "goats_sheep"]$color <- colors[3]
V(bipartite_graph)[V(bipartite_graph)$name == "carnivores"]$color <- colors[4]
V(bipartite_graph)[V(bipartite_graph)$name == "wild_birds"]$color <- colors[5]
V(bipartite_graph)[V(bipartite_graph)$name == "dogs"]$color <- colors[6]
V(bipartite_graph)[V(bipartite_graph)$name == "domestic_pigs"]$color <- colors[7]
V(bipartite_graph)[V(bipartite_graph)$name == "rodents"]$color <- colors[8]
V(bipartite_graph)[V(bipartite_graph)$name == "cats"]$color <- colors[9]
V(bipartite_graph)[V(bipartite_graph)$name == "cows"]$color <- colors[10]
V(bipartite_graph)[V(bipartite_graph)$name == "poultry"]$color <- colors[11]





# # Define the colors for each animal
# colors <- c("#F0F8FF", "#B0E0E6", "#ADD8E6", "#87CEEB", "#87CEFA", 
#             "#00BFFF", "#1E90FF", "#6495ED", "#4169E1", "#0000FF", "#0000CD")

legend_labels <- c("Bush Pigs", "Lemurs", "Goats/Sheep", "Carnivores", "Wild Birds",
                   "Dogs", "Domestic Pigs", "Rodents", "Cats", "Cows", "Poultry")


# Create a plot with no axes and an empty frame
plot(1, type='n', xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)

# Calculate the positions for each color
color_positions <- seq(0, 1, length.out = length(colors) + 1)

# Draw rectangles with gradients
for (i in 1:length(colors)) {
  rect(color_positions[i], 0, color_positions[i + 1], 1, col = colors[i], border = NA)
  
  # Add labels over each vertical bar
  text((color_positions[i] + color_positions[i + 1]) / 2, 0.5, legend_labels[i], pos = 3,
       srt =90, cex = 2)
}


# village legend
# Define the colors for each animal
colors_village <- c("red", "orange", "purple")
legend_labels_village <- c("Village 1", "Village 2", "Village 3")

# Create a plot with no axes and an empty frame
plot(1, type='n', xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)

# Calculate the positions for each color
color_positions_village <- seq(0, 1, length.out = length(colors_village) + 1)

# Draw rectangles with gradients
for (i in 1:length(colors)) {
  rect(color_positions_village[i], 0, color_positions_village[i + 1], 1, col = colors_village[i], border = NA)
  
  # Add labels over each vertical bar
  text((color_positions_village[i] + color_positions_village[i + 1]) / 2, 0.5, legend_labels_village[i], pos = 3,
       srt =90, cex = 2)
}


plot(bipartite_graph,
     layout = layout.bipartite)


## High risk bipartite graph ----
V(bipartite_graph_high_risk)$label<-""
# V(bipartite_graph_high_risk)[animal_vertices]$label<-V(bipartite_graph_high_risk)$name[animal_vertices]
V(bipartite_graph_high_risk)[animal_vertices]$size <- V(bipartite_graph_high_risk)$degree[!V(bipartite_graph_high_risk)$type]/30+5 ##degrees for animals
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$type]$size <-V(bipartite_graph_high_risk)$degree[V(bipartite_graph_high_risk)$type]*5

V(bipartite_graph_high_risk)$color[V(bipartite_graph_high_risk)$village == "Village 1"] <- adjustcolor("red", alpha=.6)
V(bipartite_graph_high_risk)$color[V(bipartite_graph_high_risk)$village == "Village 2"] <- adjustcolor("orange", alpha=.6)
V(bipartite_graph_high_risk)$color[V(bipartite_graph_high_risk)$village == "Village 3"] <- adjustcolor("purple", alpha=.6)
V(bipartite_graph_high_risk)$alpha <-0.5


V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "bush_pigs"]$color <- colors[1]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "goats_sheep"]$color <- colors[2]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "lemurs"]$color <-colors[3]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "carnivores"]$color <- colors[4]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "wild_birds"]$color <- colors[5]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "cows"]$color <- colors[6]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "cats"]$color <-colors[7]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "dogs"]$color <- colors[8]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "domestic_pigs"]$color <- colors[9]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "rodents"]$color <- colors[10]
V(bipartite_graph_high_risk)[V(bipartite_graph_high_risk)$name == "poultry"]$color <- colors[11]



# Define the colors for each animal
# colors <- c("#F0F8FF", "#B0E0E6", "#ADD8E6", "#87CEEB", "#87CEFA", 
#             "#00BFFF", "#1E90FF", "#6495ED", "#4169E1", "#0000FF", "#0000CD")

legend_labels <- c("Bush Pigs", "Goats", "Lemurs", "Carnivores", "Wild Birds", 
                   "Cows", "Cats", "Dogs", "Domestic Pigs", "Rodents", "Poultry")


# Create a plot with no axes and an empty frame
plot(1, type='n', xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)

# Calculate the positions for each color
color_positions <- seq(0, 1, length.out = length(colors) + 1)

# Draw rectangles with gradients
for (i in 1:length(colors)) {
  rect(color_positions[i], 0, color_positions[i + 1], 1, col = colors[i], border = NA)
  
  # Add labels over each vertical bar
  text((color_positions[i] + color_positions[i + 1]) / 2, 0.5, legend_labels[i], pos = 3,
       srt =90, cex = 2)
}



# village legend
# Define the colors for each animal
colors_village <- c("red", "orange", "purple")
legend_labels_village <- c("Village 1", "Village 2", "Village 3")

# Create a plot with no axes and an empty frame
plot(1, type='n', xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", axes=FALSE, frame.plot=TRUE)

# Calculate the positions for each color
color_positions_village <- seq(0, 1, length.out = length(colors_village) + 1)

# Draw rectangles with gradients
for (i in 1:length(colors)) {
  rect(color_positions_village[i], 0, color_positions_village[i + 1], 1, col = colors_village[i], border = NA)
  
  # Add labels over each vertical bar
  text((color_positions_village[i] + color_positions_village[i + 1]) / 2, 0.5, legend_labels_village[i], pos = 3,
       srt =90, cex = 2)
}
par(mfrow=c(1,1))
plot(
  bipartite_graph_high_risk,
  layout = layout.bipartite,
  col=(alpha = .5)
  # vertex.label.color = "black",
  # vertex.label.dist = 0.5, # Adjust label distance from vertices
  # vertex.label.cex = 0.8,  # Adjust label size
  # vertex.label.angle = ifelse(V(bipartite_graph_high_risk)$name %in% animal_types, 45, 0)  # Set label angle
)


library(coefplot)
## FULL NETWORK coef plot----
# Extract coefficient names
coef_names <- names(coef(mcmc_ergm3))

# Extract coefficient values
coef_values <- as.data.frame(coef(mcmc_ergm3))$`coef(mcmc_ergm3)`
ORs <- lapply(coef_values, function(x) exp(x))

ci_2.5 <- as.data.frame(confint(mcmc_ergm3))[, "2.5 %"]
ci_2.5 <- lapply(ci_2.5, function(x) exp(x))

ci_97.5 <- as.data.frame(confint(mcmc_ergm3))[, "97.5 %"]
ci_97.5 <- lapply(ci_97.5, function(x) exp(x))


# Create dataframe for plotting
full_network_ergm_df <- data.frame(
  predictor = coef_names,
  estimate = unlist(ORs),
  lower = unlist(ci_2.5),
  upper = unlist(ci_97.5)
) |> 
  filter(predictor %in% c("b1factor.animal_category.rodents", "b2degree5", "b2factor.gender.Male",
                          "b2degree6", "b2factor.village.Village 3", "b2cov.school_level", 
                          "b2cov.age", "b1factor.animal_category.wild:b2cov.commercial_goods:b2cov.house_sol", "b1factor.animal_category.wild"))

full_network_ergm_df<- full_network_ergm_df |> # removing edges
  filter(predictor != "edges") |> 
  arrange(desc(estimate))

full_network_ergm_df$predictor <- factor(full_network_ergm_df$predictor, levels = full_network_ergm_df$predictor)


# Plotting
ggplot(full_network_ergm_df, aes(reorder(x = predictor,estimate), y = estimate)) +
  geom_pointrange(
    aes(ymin = 0, ymax = 0),
    color = "blue", fill = "white"
  ) +
  geom_point(
    aes(color = ifelse(estimate > 1, "greater_than_1", ifelse(estimate <1, "less_than_1", "zero"))), 
    size = 3
  ) +
  scale_color_manual(
    values = c(greater_than_1 = "cornflowerblue", less_than_1 = "pink2"),
    guide = FALSE
  ) +
  scale_x_discrete(labels = c("b1factor.animal_category.rodents" = "Rodents", 
                              "b2factor.gender.Male" = "Male",
                              "b2factor.village.Village 3" = "Village 3",
                              "b2cov.house_sol" = "HSOL",
                              "b2cov.commercial_goods" = "CGSOL",
                              "b2cov.school_level" = "School Level",
                              "b1factor.animal_category.rodents:b2cov.commercial_goods:b2cov.house_sol"=" Rodents * CGSOL * HSOL",
                              "b2cov.age"="Age",
                              "b1factor.animal_category.wild:b2cov.commercial_goods:b2cov.house_sol" = "Wild * CGSOL * HSOL",
                              "b2factor.village.Village 2" = "Village 2",
                              "b2degree2" = "Human Degree(2)","b2degree5" = "Human Degree(5)",
                              "b2degree6" = "Human Degree(6)",
                              "b1factor.animal_category.wild" = "Wild Animals"))+
  geom_hline(yintercept = 1, linetype = 2) +
  coord_flip() +
  xlab(NULL) +
  labs(x = "Predictor", y = "Odds Ratio", title = "Full Network ERGM")+
  theme( axis.text.y = element_text(size = 18),
         axis.text.x = element_text(size = 18) )


## High risk NETWORK coef plot----
# Extract coefficient names
coef_names_high_risk <- names(coef(mcmc_ergm2_high_risk))

# Extract coefficient values
coef_values_high_risk <- as.data.frame(coef(mcmc_ergm2_high_risk))$`coef(mcmc_ergm2_high_risk)`
ORs_high_risk <- lapply(coef_values_high_risk, function(x) exp(x))

ci_2.5_high_risk <- as.data.frame(confint(mcmc_ergm2_high_risk))[, "2.5 %"]
ci_2.5_high_risk <- lapply(ci_2.5_high_risk, function(x) exp(x))

ci_97.5_high_risk <- as.data.frame(confint(mcmc_ergm2_high_risk))[, "97.5 %"]
ci_97.5_high_risk <- lapply(ci_97.5_high_risk, function(x) exp(x))


# Create dataframe for plotting
high_risk_ergm_df <- data.frame(
  predictor_high_risk = coef_names_high_risk,
  estimate_high_risk = unlist(ORs_high_risk),
  lower_high_risk = unlist(ci_2.5_high_risk),
  upper_high_risk = unlist(ci_97.5_high_risk)
) |> 
  filter(predictor_high_risk %in% c("b1factor.animal_category.rodents", "b2degree4", "b2factor.village.Village 3",
                          "b1factor.animal_category.wild")) |> 
  arrange(desc(estimate_high_risk))



high_risk_ergm_df$predictor_high_risk <- factor(high_risk_ergm_df$predictor_high_risk, levels = high_risk_ergm_df$predictor_high_risk)


# Plotting
ggplot(high_risk_ergm_df, aes(reorder(x = predictor_high_risk,estimate_high_risk), y = estimate_high_risk))+                                   
  geom_pointrange(
    aes(ymin = 0, ymax = 0),
    color = "blue", fill = "white"
  ) +
  geom_point(
    aes(color = ifelse(estimate_high_risk > 1, "greater_than_1", ifelse(estimate_high_risk <1, "less_than_1", "zero"))), 
    size = 3
  ) +
  scale_color_manual(
    values = c(greater_than_1 = "cornflowerblue", less_than_1 = "pink2"),
    guide = FALSE
  ) +
  scale_x_discrete(labels = c("b1factor.animal_category.rodents" = "Rodents", "b2degree4" = "Human Degree(4)",
                              "b2cov.school_level" = "School Level", "b2cov.house_sol" = "HSOL",
                              "b2cov.commercial_goods" = "CGSOL",
                              "b1factor.animal_category.wild:b2cov.commercial_goods:b2cov.house_sol" = "Wild * CGSOL * HSOL",
                              "b2cov.age"="Age", 
                              "b1factor.animal_category.rodents:b2cov.commercial_goods:b2cov.house_sol"=" Rodents * CGSOL * HSOL",
                              "b2factor.village.Village 2" = "Village 2", "b2factor.gender.Male" = "Male",
                              "b2factor.village.Village 3" = "Village 3","b1factor.animal_category.wild" = "Wild Animals" ))+
  geom_hline(yintercept = 1, linetype = 2) +
  coord_flip() +
    # theme_minimal() +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
    #       axis.text.y = element_text(size = 15),
    #       axis.title = element_text(size = 20),
    #       plot.title = element_text(size = 20, hjust = 0.5)) +
    xlab(NULL) +
  labs(x = "Predictor", y = "Odds Ratio", title = "High-Risk Network ERGM")+
  theme( axis.text.y = element_text(size = 18),
         axis.text.x = element_text(size = 18) )



## Heat map ----
library(hrbrthemes)
source(here("Scripts/Loading_Data_LK 2024.02.26.R"))

animals <- c("dogs", "cats", "pigs", "cows", "poultry", "carnivores", 
             "goats/sheep", "rodents",
             "bush pigs", "wild birds","lemurs")

exposure_route <- c("cooked", "eaten undercooked",
                    "found dead collected", "handled live",
                    "hunted or trapped", "in house", "pet",
                    "raised", "eaten sick", "slaughtered", 
                    "shared water", "scratched or bitten", "feces")


data<-expand.grid(Animals=animals, Exposure_Route=exposure_route)

desired_order_cooked_animals <- c("cooked_handled_dogs", "cooked_handled_cats", "cooked_handled_domestic_pigs",
                                  "cooked_handled_cows", "cooked_handled_poultry",
                                  "cooked_handled_bush_pigs", "cooked_handled_wild_birds", "cooked_handled_lemurs")
#cooked
animals_cooked_df <- merged_df |>
  select(contains("cooked_handled")) |>
  select(all_of(desired_order_cooked_animals)) |> 
  mutate(cooked_handled_carnivores = NA) |> 
  relocate(cooked_handled_carnivores, .after = "cooked_handled_poultry") |> 
  mutate(cooked_handled_goats_sheep = NA) |> 
  relocate(cooked_handled_goats_sheep, .after = "cooked_handled_carnivores") |> 
  mutate(cooked_handled_rodents = NA) |> 
  relocate(cooked_handled_rodents, .after = "cooked_handled_goats_sheep")


sums_animals_cooked <- animals_cooked_df %>%
  colSums()

sum_animals_cooked_vector <- as.vector(sums_animals_cooked)

#raw/undercooked
desired_order_raw_undercooked_animals <- c("raw_undercooked_dogs", "raw_undercooked_domestic_pigs",
                                           "raw_undercooked_cows", "raw_undercooked_poultry","raw_undercooked_goats_sheep",
                                           "raw_undercooked_bush_pigs", "raw_undercooked_wild_birds")
animals_raw_undercooked_df <- merged_df |>
  select(all_of(desired_order_raw_undercooked_animals)) |> 
  mutate(raw_undercooked_cats = NA) |> 
  relocate(raw_undercooked_cats, .after = "raw_undercooked_dogs") |> 
  mutate(raw_undercooked_carnivores = NA) |> 
  relocate(raw_undercooked_carnivores, .after = "raw_undercooked_poultry") |> 
  mutate(raw_undercooked_rodents = NA) |> 
  relocate(raw_undercooked_rodents, .after = "raw_undercooked_goats_sheep") |> 
  mutate(raw_undercooked_lemurs = NA) |> 
  relocate(raw_undercooked_lemurs, .after = "raw_undercooked_wild_birds")

sums_animals_raw_undercooked <- animals_raw_undercooked_df %>%
  colSums()

sum_animals_raw_undercooked_vector <- as.vector(sums_animals_raw_undercooked)

#dead
desired_order_dead_animals <- c("dead_dogs", "dead_cats", "dead_domestic_pigs", "dead_cows", "dead_poultry",
                                "dead_carnivores", "dead_goats_sheep", "dead_rodents", 
                                "dead_wild_birds")

animals_dead_df <- merged_df |>
  select(all_of(desired_order_dead_animals)) |> 
  mutate(dead_bush_pigs = NA) |> 
  relocate(dead_bush_pigs, .after = "dead_rodents") |> 
  mutate(dead_lemurs = NA) |> 
  relocate(dead_lemurs, .after = "dead_wild_birds")


sums_animals_dead <- animals_dead_df |> 
  colSums()

sums_animals_dead_vector <- as.vector(sums_animals_dead)


# handled
desired_order_handled_animals <- c("handle_dogs", "handle_cats","handle_domestic_pigs", "handle_cows", "handle_poultry",
                                   "handle_carnivores", "handle_goats_sheep", "handle_rodents",
                                   "handle_bush_pigs", "handle_wild_birds", "handle_lemurs")

animals_handled_df <- merged_df |>
  select(all_of(desired_order_handled_animals))

sums_animals_handle <- animals_handled_df |> 
  colSums()

sums_animals_handled_vector <- as.vector(sums_animals_handle)

#hunted
desired_order_hunted_animals <- c("hunted_trapped_dogs", "hunted_trapped_cats", "hunted_trapped_poultry",
                                  "hunted_trapped_carnivores", "hunted_trapped_rodents",
                                  "hunted_trapped_bush_pigs", "hunted_trapped_wild_birds", "hunted_trapped_lemurs")

animals_hunted_trapped_df <- merged_df |>
  select(all_of(desired_order_hunted_animals)) |> 
  mutate(hunted_trapped_domestic_pigs = NA) |> 
  relocate(hunted_trapped_domestic_pigs, .after = "hunted_trapped_cats") |>
  mutate(hunted_trapped_cows = NA) |> 
  relocate(hunted_trapped_cows, .after = "hunted_trapped_domestic_pigs") |>
  mutate(hunted_trapped_goats_sheep = NA) |> 
  relocate(hunted_trapped_goats_sheep, .after = "hunted_trapped_carnivores")


sums_animals_hunted_trapped <- animals_hunted_trapped_df |> 
  colSums()

sums_animals_hunted_trapped_vector <- as.vector(sums_animals_hunted_trapped)

# in house
desired_order_inside_home_animals <- c("inside_dogs", "inside_cats", "inside_domestic_pigs", "inside_cows", "inside_poultry",
                                       "inside_carnivores", "inside_goats_sheep",
                                       "inside_rodents", "inside_wild_birds", "inside_lemurs")

animals_inside_df <- merged_df |>
  select(all_of(desired_order_inside_home_animals)) |> 
  mutate(inside_bush_pigs = NA) |> 
  relocate(inside_bush_pigs, .after = "inside_rodents")

sums_animals_inside <- animals_inside_df |> 
  colSums()

sums_animals_inside_vector <- as.vector(sums_animals_inside)

# pets

desired_order_pet_animals <- c("pet_dogs", "pet_cats", "pet_domestic_pigs", "pet_cows", "pet_poultry",
                               "pet_carnivores", "pet_goats_sheep", "pet_rodents", "pet_bush_pigs")

animals_pets_df <- merged_df |>
  select(all_of(desired_order_pet_animals)) |> 
  mutate(pet_wild_birds = NA) |> 
  relocate(pet_wild_birds, .after = "pet_bush_pigs") |>
  mutate(pet_lemurs = NA) |> 
  relocate(pet_lemurs, .after = "pet_wild_birds")

sums_animals_pets <- animals_pets_df |> 
  colSums()

sums_animals_pets_vector <- as.vector(sums_animals_pets)

# raised
desired_order_raised_animals <- c("raised_dogs", "raised_cats", "raised_domestic_pigs", "raised_cows", "raised_poultry",
                                  "raised_carnivores", "raised_goats_sheep", "raised_bush_pigs", "raised_lemurs")

animals_raised_df <- merged_df |>
  select(all_of(desired_order_raised_animals)) |> 
  mutate(raised_rodents = NA) |> 
  relocate(raised_rodents, .after = "raised_goats_sheep") |>
  mutate(raised_wild_birds = NA) |> 
  relocate(raised_wild_birds, .after = "raised_bush_pigs")

sums_animals_raised <- animals_raised_df |> 
  colSums()

sums_animals_raised_vector <- as.vector(sums_animals_raised)


# scratched or bitten
merged_df |> 
  select(contains("scratched_bitten_")) |> 
  colnames()

desired_scratched_bitten_animals <- c("scratched_bitten_dogs", "scratched_bitten_cats","scratched_bitten_domestic_pigs",
                                      "scratched_bitten_cows", "scratched_bitten_poultry", "scratched_bitten_rodents")

animals_scratched_bitten_df <- merged_df |>
  select(all_of(desired_scratched_bitten_animals)) |> 
  mutate(scratched_bitten_carnivores = NA) |> 
  relocate(scratched_bitten_carnivores, .after = "scratched_bitten_poultry") |>
  mutate(scratched_bitten_goats_sheep = NA) |> 
  relocate(scratched_bitten_goats_sheep, .after = "scratched_bitten_carnivores") |>
  mutate(scratched_bitten_bush_pigs = NA) |> 
  relocate(scratched_bitten_bush_pigs, .after = "scratched_bitten_rodents") |> 
  mutate(scratched_bitten_wild_birds = NA) |> 
  relocate(scratched_bitten_wild_birds, .after = "scratched_bitten_bush_pigs") |>
  mutate(scratched_bitten_lemurs = NA) |> 
  relocate(scratched_bitten_lemurs, .after = "scratched_bitten_wild_birds")

sums_animals_scratched_bitten <- animals_scratched_bitten_df |> 
  colSums()

sums_animals_scratched_bitten_vector <- as.vector(sums_animals_scratched_bitten)

# slaughtered
desired_slaughtered_animals <- c("slaughtered_dogs","slaughtered_cats", "slaughtered_domestic_pigs", "slaughtered_cows",
                                 "slaughtered_poultry", "slaughtered_carnivores", "slaughtered_goats_sheep",
                                 "slaughtered_rodents", "slaughtered_bush_pigs", "slaughtered_wild_birds", "slaughtered_lemurs")

animals_slaughtered_df <- merged_df |>
  select(all_of(desired_slaughtered_animals)) 
  

sums_animals_slaughtered <- animals_slaughtered_df |> 
  colSums()

sums_animals_slaughtered_vector <- as.vector(sums_animals_slaughtered)


# shared_water

desired_shared_water_animals <- c("shared_water_dogs", "shared_water_cats", "shared_water_domestic_pigs", "shared_water_cows",
                                  "shared_water_poultry")

animals_shared_water_df <- merged_df |>
  select(all_of(desired_shared_water_animals)) |> 
  mutate(shared_water_carnivores = NA) |> 
  relocate(shared_water_carnivores, .after = "shared_water_poultry") |> 
  mutate(shared_water_goats_sheep = NA) |> 
  relocate(shared_water_goats_sheep, .after = "shared_water_carnivores") |> 
  mutate(shared_water_rodents = NA) |> 
  relocate(shared_water_rodents, .after = "shared_water_goats_sheep") |> 
  mutate(shared_water_bush_pigs = NA) |> 
  relocate(shared_water_bush_pigs, .after = "shared_water_rodents") |> 
  mutate(shared_water_wild_birds = NA) |> 
  relocate(shared_water_wild_birds, .after = "shared_water_bush_pigs") |> 
  mutate(shared_water_lemurs = NA) |> 
  relocate(shared_water_lemurs, .after = "shared_water_wild_birds") 

sums_animals_shared_water <- animals_shared_water_df |> 
  colSums()

sums_animals_shared_water_vector <- as.vector(sums_animals_shared_water)

# eaten sick
desired_eaten_sick_animals <- c("eaten_sick_dogs", "eaten_sick_cats", "eaten_sick_domestic_pigs", "eaten_sick_cows",
                                "eaten_sick_poultry", "eaten_sick_goats_sheep", "eaten_sick_bush_pigs")

animals_eaten_sick_df <- merged_df |>
  select(all_of(desired_eaten_sick_animals)) |> 
  mutate(eaten_sick_carnivores = NA) |> 
  relocate(eaten_sick_carnivores, .after = "eaten_sick_poultry") |> 
  mutate(eaten_sick_rodents = NA) |> 
  relocate(eaten_sick_rodents, .after = "eaten_sick_goats_sheep") |> 
  mutate(eaten_sick_wild_birds = NA) |> 
  relocate(eaten_sick_wild_birds, .after = "eaten_sick_bush_pigs") |> 
  mutate(eaten_sick_lemurs = NA) |> 
  relocate(eaten_sick_lemurs, .after = "eaten_sick_wild_birds") 

sums_animals_eaten_sick <- animals_eaten_sick_df |> 
  colSums()

sums_animals_eaten_sick_vector <- as.vector(sums_animals_eaten_sick)

# eaten feces
merged_df |> 
  select(contains("feces")) |> 
  colnames()

desired_feces_animals <- c("feces_dogs", "feces_cats", "feces_domestic_pigs", "feces_cows",
                           "feces_poultry", "feces_carnivores", "feces_goats_sheep", 
                           "feces_rodents", "feces_wild_birds", "feces_lemurs")

animals_feces_df <- merged_df |>
  select(all_of(desired_feces_animals)) |> 
  mutate(feces_bush_pigs = NA) |> 
  relocate(feces_bush_pigs, .after = "feces_rodents")


sums_animals_feces <- animals_feces_df |> 
  colSums()

sums_animals_feces_vector <- as.vector(sums_animals_feces)


# putting it together
animal_contact_sums <- c(sum_animals_cooked_vector,sum_animals_raw_undercooked_vector, sums_animals_dead_vector, sums_animals_handled_vector,
                         sums_animals_hunted_trapped_vector,sums_animals_inside_vector,sums_animals_pets_vector,sums_animals_raised_vector,
                         sums_animals_scratched_bitten_vector,sums_animals_slaughtered_vector,sums_animals_shared_water_vector, sums_animals_eaten_sick_vector,
                         sums_animals_feces_vector)

data$counts<-animal_contact_sums

animal_exposure_heat_map<- ggplot(data, aes(Animals, Exposure_Route, fill= counts)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low="skyblue", high="blue") +
  geom_text(aes(label = counts), color = "white", size = 4) +
  geom_text(aes(label = ifelse(is.na(counts), "NA", "")), color = "white", size = 3) +
  theme_ipsum()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90))

animal_exposure_heat_map
ggsave("animal_exposure_heat_map.png", plot = animal_exposure_heat_map, width = 9, height = 6)

#### high-risk contacts only
high_risk_animal_contact_sums <- c(sum_animals_raw_undercooked_vector, sums_animals_dead_vector,
                                   sums_animals_scratched_bitten_vector,sums_animals_shared_water_vector,
                                   sums_animals_eaten_sick_vector, sums_animals_feces_vector)

data_high_risk$counts<-high_risk_animal_contact_sums

high_risk_animal_exposure_heat_map<- ggplot(data_high_risk, aes(x = Animals, y = Exposure_Route, fill= counts)) + 
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  scale_fill_gradient(low="skyblue", high="blue") +
  geom_text(aes(label = counts), color = "white", size = 4) +
  geom_text(aes(label = ifelse(is.na(counts), "0", "")), color = "white", size = 3) +
  theme_ipsum()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size=24),
        legend.position="none")

high_risk_animal_exposure_heat_map
ggsave("high_risk_animal_exposure_heat_map.jpg", plot = high_risk_animal_exposure_heat_map, width = 9, height = 6)


