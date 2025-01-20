library(here)
library(igraph)
source(here("./self_reported_networks/The_ReUp/Loading Interaction Data.R"))

merged_df<- merged_df |> 
  select(-grep("frequency", names(merged_df)))

# setting animals of interest ----
animals <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs", "goats_sheep",
                  "carnivores", "cows", "wild_birds", "bush_pigs", "lemurs", "tenrecs")
# creating wide df ----
for (animal in animals) {
  # Find columns containing the current animal type
  feces_cols <- grep(paste0("feces_", animal), names(merged_df), value = TRUE)
  shared_water_cols <- grep(paste0("shared_water_", animal), names(merged_df), value = TRUE)
  scratched_bitten_cols <- grep(paste0("scratched_bitten_", animal), names(merged_df), value = TRUE)
  cooked_handled_cols <- grep(paste0("cooked_handled_", animal), names(merged_df), value = TRUE)
  raw_undercooked_cols <- grep(paste0("raw_undercooked_", animal), names(merged_df), value = TRUE)
  eaten_sick_cols <- grep(paste0("eaten_sick_", animal), names(merged_df), value = TRUE)
  slaughtered_cols <- grep(paste0("slaughtered_", animal), names(merged_df), value = TRUE)

  # Create edges for animals reported at least once
  merged_df[[paste0(animal, "_edge_feces")]] <- as.integer(rowSums(merged_df[feces_cols]) > 0)
  merged_df[[paste0(animal, "_edge_shared_water")]] <- as.integer(rowSums(merged_df[shared_water_cols]) > 0)
  merged_df[[paste0(animal, "_edge_scratched_bitten")]] <- as.integer(rowSums(merged_df[scratched_bitten_cols]) > 0)
  merged_df[[paste0(animal, "_edge_cooked_handled")]] <- as.integer(rowSums(merged_df[cooked_handled_cols]) > 0)
  merged_df[[paste0(animal, "_edge_raw_undercooked")]] <- as.integer(rowSums(merged_df[raw_undercooked_cols]) > 0)
  merged_df[[paste0(animal, "_edge_eaten_sick")]] <- as.integer(rowSums(merged_df[eaten_sick_cols]) > 0)
  merged_df[[paste0(animal, "_edge_slaughered")]] <- as.integer(rowSums(merged_df[slaughtered_cols]) > 0)
  
  
  # create high risk edge
  merged_df[[paste0(animal, "_edge_high_risk")]] <- as.integer(rowSums(merged_df[, c(paste0(animal, "_edge_feces"),
                                                                                     paste0(animal, "_edge_shared_water"),
                                                                                     paste0(animal, "_edge_scratched_bitten"),
                                                                                     paste0(animal, "_edge_cooked_handled"),
                                                                                     paste0(animal, "_edge_raw_undercooked"),
                                                                                     paste0(animal, "_edge_eaten_sick"),
                                                                                     paste0(animal, "_edge_slaughered"))]) > 0)
}


animal_bipartite_high_risk_df_wide <- merged_df |> 
  select(matches("social_netid|_edge", 
                 ignore.case = FALSE))

animal_edges_dfs_high_risk <- list()

for (animal in animals) {
  # Get social_netid values for the current animal type
  animal_edges_high_risk <- animal_bipartite_high_risk_df_wide |> 
    filter(!!sym(paste0(animal, "_edge_high_risk")) == 1) |> 
    select(social_netid) |> 
    pull()
  
  # Check if animal_edges_inside is not empty
  if (length(animal_edges_high_risk) > 0) {
    # Create a dataframe for the current animal type
    animal_edges_df_high_risk <- data.frame(social_netid = animal_edges_high_risk, animal = animal)
    
    # Store the dataframe in the list
    animal_edges_dfs_high_risk[[animal]] <- animal_edges_df_high_risk
  }
}
all_animal_edges_df_high_risk <- bind_rows(animal_edges_dfs_high_risk)
all_animal_edges_df_high_risk <- arrange(all_animal_edges_df_high_risk, social_netid)

# making bipartite graph----
bipartite_graph_high_risk <- graph_from_data_frame(all_animal_edges_df_high_risk, directed = FALSE)

# adding back in social netid isolates
isolates <- setdiff(merged_df$social_netid, all_animal_edges_df_high_risk$social_netid)

# adding back in animal isolates
isolate_animals <- setdiff(animals, all_animal_edges_df_high_risk$animal)

bipartite_graph_high_risk <- add_vertices(bipartite_graph_high_risk, 
                                      nv = length(isolates), name = isolates)

bipartite_graph_high_risk <- add_vertices(bipartite_graph_high_risk, 
                                      nv = length(isolate_animals), name = isolate_animals)

# Setting vertex types----
animal_vertices <- which(V(bipartite_graph_high_risk)$name %in% animals)
V(bipartite_graph_high_risk)$type <- TRUE
V(bipartite_graph_high_risk)$type[animal_vertices] <- FALSE # animals = FALSE

social_netid_vertices <- which(V(bipartite_graph_high_risk)$type)

## adding in human vector attributes for MI and demographics ----

village_values <- merged_df$village[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]

age_values <- merged_df$age[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]

gender_values <- merged_df$gender[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]
commercial_goods_values <- merged_df$commercial_goods[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]
house_sol_values <- merged_df$house_sol[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]
grew_vanilla_values <- merged_df$grew_vanilla[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]
land_size_values <- merged_df$landsize_in_daba[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]
household_size_values <- merged_df$household_size[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]
school_level_values <-merged_df$school_level_numbered[match(V(bipartite_graph_high_risk)$name[social_netid_vertices], merged_df$social_netid)]

# Assign the attributes to the social_netid vertices

V(bipartite_graph_high_risk)$village <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$village[social_netid_vertices] <- village_values

V(bipartite_graph_high_risk)$age <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$age[social_netid_vertices] <- age_values

V(bipartite_graph_high_risk)$gender <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$gender[social_netid_vertices] <- gender_values

V(bipartite_graph_high_risk)$commercial_goods <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$commercial_goods[social_netid_vertices] <- commercial_goods_values

V(bipartite_graph_high_risk)$house_sol <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$house_sol[social_netid_vertices] <- house_sol_values

V(bipartite_graph_high_risk)$grew_vanilla <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$grew_vanilla[social_netid_vertices] <- grew_vanilla_values

V(bipartite_graph_high_risk)$land_size <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$land_size[social_netid_vertices] <- land_size_values

V(bipartite_graph_high_risk)$household_size <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$household_size[social_netid_vertices] <- household_size_values

V(bipartite_graph_high_risk)$school_level <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$school_level[social_netid_vertices] <- school_level_values



# adding in animal vector attributes for animal type
V(bipartite_graph_high_risk)$animal_name <- "Human"  # Initialize the attribute
V(bipartite_graph_high_risk)$animal_name[animal_vertices] <- V(bipartite_graph_high_risk)$name[animal_vertices]


# doing different bipartite networks for each village ----
# ampandrana adatsakala
bipartite_graph_high_risk_ampandrana_andatsakala <- subgraph(graph = bipartite_graph_high_risk, 
                                                                vids = which(V(bipartite_graph_high_risk)$village == "Ampandrana" | 
                                                                               V(bipartite_graph_high_risk)$village == "Andatsakala" | 
                                                                               is.na(V(bipartite_graph_high_risk)$village))) # the na vertices are animals!

V(bipartite_graph_high_risk_ampandrana_andatsakala)$degree<-igraph::degree(bipartite_graph_high_risk_ampandrana_andatsakala)
mean(V(bipartite_graph_high_risk_ampandrana_andatsakala)$degree[V(bipartite_graph_high_risk_ampandrana_andatsakala)$type]) ##degrees for humans
range(V(bipartite_graph_high_risk_ampandrana_andatsakala)$degree[V(bipartite_graph_high_risk_ampandrana_andatsakala)$type]) ##range for humans
mean(V(bipartite_graph_high_risk_ampandrana_andatsakala)$degree[!V(bipartite_graph_high_risk_ampandrana_andatsakala)$type]) ##degrees for animals
range(V(bipartite_graph_high_risk_ampandrana_andatsakala)$degree[!V(bipartite_graph_high_risk_ampandrana_andatsakala)$type]) ##range for animals


# mandena
bipartite_graph_high_risk_mandena <- subgraph(graph=bipartite_graph_high_risk,
                                                 vids=which(V(bipartite_graph_high_risk)$village=="Mandena" |
                                                              is.na(V(bipartite_graph_high_risk)$village)))


V(bipartite_graph_high_risk_mandena)$degree<-igraph::degree(bipartite_graph_high_risk_mandena)
mean(V(bipartite_graph_high_risk_mandena)$degree[V(bipartite_graph_high_risk_mandena)$type]) ##degrees for humans
range(V(bipartite_graph_high_risk_mandena)$degree[V(bipartite_graph_high_risk_mandena)$type]) ##range for humans
mean(V(bipartite_graph_high_risk_mandena)$degree[!V(bipartite_graph_high_risk_mandena)$type]) ##degrees for animals
range(V(bipartite_graph_high_risk_mandena)$degree[!V(bipartite_graph_high_risk_mandena)$type]) ##range for animals

# sarahandrano
bipartite_graph_high_risk_sarahandrano <- subgraph(graph=bipartite_graph_high_risk, 
                                                      vids=which(V(bipartite_graph_high_risk)$village=="Sarahandrano" | 
                                                                   is.na(V(bipartite_graph_high_risk)$village)))

V(bipartite_graph_high_risk_sarahandrano)$degree<-igraph::degree(bipartite_graph_high_risk_sarahandrano)
mean(V(bipartite_graph_high_risk_sarahandrano)$degree[V(bipartite_graph_high_risk_sarahandrano)$type]) ##degrees for humans
range(V(bipartite_graph_high_risk_sarahandrano)$degree[V(bipartite_graph_high_risk_sarahandrano)$type]) ##range for humans
mean(V(bipartite_graph_high_risk_sarahandrano)$degree[!V(bipartite_graph_high_risk_sarahandrano)$type]) ##degrees for animals
range(V(bipartite_graph_high_risk_sarahandrano)$degree[!V(bipartite_graph_high_risk_sarahandrano)$type]) ##range for animals


# animal categories
domesticated_animals_1 <- c("poultry", "domestic_pigs", "cows")
domesticated_animals_2 <- c( "dogs","goats_sheep", "cats")
rodent_animals <- "rodents"
wild_animals <- c("bush_pigs", "carnivores", "tenrecs", "wild_birds", "lemurs")

# domesticated_animals <- c("poultry", "domestic_pigs", "cows", "dogs","goats_sheep", "cats")

# Create the new attribute animal_category -----
#ampandrana
V(bipartite_graph_high_risk_ampandrana_andatsakala)$animal_category <- ifelse(
  V(bipartite_graph_high_risk_ampandrana_andatsakala)$animal_name %in% domesticated_animals_1, "domesticated_1",
  ifelse( V(bipartite_graph_high_risk_ampandrana_andatsakala)$animal_name %in% domesticated_animals_2, "domesticated_2",
  ifelse(V(bipartite_graph_high_risk_ampandrana_andatsakala)$animal_name %in% wild_animals, "wild", 
         ifelse(V(bipartite_graph_high_risk_ampandrana_andatsakala)$animal_name %in% rodent_animals, "rodent", 
                NA ))))

#mandena
V(bipartite_graph_high_risk_mandena)$animal_category <- ifelse(
  V(bipartite_graph_high_risk_mandena)$animal_name %in% domesticated_animals_1, "domesticated_1",
  ifelse( V(bipartite_graph_high_risk_mandena)$animal_name %in% domesticated_animals_2, "domesticated_2",
          ifelse(V(bipartite_graph_high_risk_mandena)$animal_name %in% wild_animals, "wild", 
                 ifelse(V(bipartite_graph_high_risk_mandena)$animal_name %in% rodent_animals, "rodent", 
                        NA ))))

#sarahandrano
V(bipartite_graph_high_risk_sarahandrano)$animal_category <- ifelse(
  V(bipartite_graph_high_risk_sarahandrano)$animal_name %in% domesticated_animals_1, "domesticated_1",
  ifelse( V(bipartite_graph_high_risk_sarahandrano)$animal_name %in% domesticated_animals_2, "domesticated_2",
          ifelse(V(bipartite_graph_high_risk_sarahandrano)$animal_name %in% wild_animals, "wild", 
                 ifelse(V(bipartite_graph_high_risk_sarahandrano)$animal_name %in% rodent_animals, "rodent", 
                        NA ))))
## mixing matrix? -----

# Get the list of edges (human-animal interactions)
edges <- as.data.frame(as_edgelist(bipartite_graph_high_risk_ampandrana_andatsakala))

# Separate the human and animal nodes
human_nodes <- V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$type == TRUE]
animal_nodes <- V(bipartite_graph_high_risk_ampandrana_andatsakala)[V(bipartite_graph_high_risk_ampandrana_andatsakala)$type == FALSE]

# Create a matrix to count interactions
interaction_matrix <- matrix(0, nrow = length(human_nodes), ncol = length(animal_nodes))
rownames(interaction_matrix) <- human_nodes$name
colnames(interaction_matrix) <- animal_nodes$name

# # Fill the matrix with interaction counts
# for (i in 1:length(edges)) {
#   human <- edges$V1[i]
#   animal <- edges$V2[i]
#   
#   human_index <- which(rownames(interaction_matrix) == human)
#   animal_index <- which(colnames(interaction_matrix) == animal)
#   
#   interaction_matrix[human_index, animal_index] <- interaction_matrix[human_index, animal_index] + 1
# }
# 
# # Convert counts to probabilities
# total_interactions <- sum(interaction_matrix)
# probability_matrix <- interaction_matrix / total_interactions
# 
# # View the matrix
# mixing_matrix <- table(edges$V1, edges$V2)
# 
# 
# 
# # Create a new table with V1 as "Human" and V2 values
# mixing_matrix <- table(edges$V1, edges$V2)
# 
# # Calculate percentage for each V2 term
# 
# # Print the percentage table
# print(mixing_matrix_percent)
# # Sum interactions by social network identifier (rows)
# 
# melt_matrix <- reshape2::melt(mixing_matrix)
# ggplot(melt_matrix, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile() + 
#   theme_minimal() +
#   scale_fill_gradient(low = "white", high = "blue") +
#   labs(x = "Social Network ID", y = "Animal Type", fill = "Interaction Count")
# 
# 
# mixing_matrix <- matrix(mixing_matrix)

