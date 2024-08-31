# loading libraries/data
library(dplyr)
library(here)
library(igraph)
source(here("./self_reported_networks/Loading Interaction Data.R"))

merged_df<- merged_df |> 
  select(-grep("frequency", names(merged_df)))
length(merged_df$survey_date)
# setting animals of interest ----
animal_types <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs", "goats_sheep",
                  "carnivores", "cows", "wild_birds", "bush_pigs", "lemurs")
animal_categories<-c("peridomestic", rep("domestic", 5), rep("wild", 3))

# creating wide df ----
for (animal in animal_types) {
  # Find columns containing the current animal type
  animal_cols <- grep(paste0("_", animal), names(merged_df), value = TRUE)
  
  # Create edges for animals reported at least once
  merged_df[[paste0(animal, "_edge")]] <- as.integer(rowSums(merged_df[animal_cols]) > 0)
  
  # Calculate the sum of 1s in animal columns to get edge weights
  merged_df[[paste0(animal, "_weight")]] <- rowSums(merged_df[animal_cols])
}

animal_bipartite_df_wide <- merged_df |> 
  select(matches("social_netid|_weight|_edge", 
                 ignore.case = FALSE)) |> 
  select(-symptoms_weight_loss)

#head(animal_bipartite_df_wide)

# rodent_edges <- animal_bipartite_df_wide %>%
#   filter(rodent_edge == 1) %>%
#   select(social_netid) %>%
#   as.vector()
# 
# dog_edges <- animal_bipartite_df_wide %>%
#   filter(dogs_edge == 1) %>%
#   select(social_netid) %>%
#   as.vector()

# make wide df into 2-col df----
# Initialize an empty list to store data frames for each animal type
animal_edges_dfs <- list()

for (animal in animal_types) {
  # Get social_netid values for the current animal type
  animal_edges <- animal_bipartite_df_wide %>%
    filter(!!sym(paste0(animal, "_edge")) == 1) %>%
    select(social_netid) %>%
    as.vector()
  
  # Create a dataframe for the current animal type
  animal_edges_df <- data.frame(social_netid = animal_edges, animal = animal)
  
  # Store the dataframe in the list
  animal_edges_dfs[[animal]] <- animal_edges_df
}

all_animal_edges_df <- bind_rows(animal_edges_dfs)
all_animal_edges_df <- arrange(all_animal_edges_df, social_netid)

# making bipartite graph----
bipartite_graph <- graph_from_data_frame(all_animal_edges_df, directed = FALSE)

# Setting vertex types----
animal_vertices <- which(V(bipartite_graph)$name %in% animal_types)
V(bipartite_graph)$type[animal_vertices] <- FALSE # animals = FALSE
V(bipartite_graph)$type<-ifelse(is.na(V(bipartite_graph)$type), TRUE, FALSE) # social IDs = true

social_netid_vertices <- which(V(bipartite_graph)$type)

## adding in vector attributes for MI and demographics ----
MI_df<-merged_df |>  select(social_netid, house_sol, commercial_goods)
commercial_goods_values <- MI_df$commercial_goods[MI_df$social_netid %in% V(bipartite_graph)$name[social_netid_vertices]]
house_sol_values <- MI_df$house_sol[MI_df$social_netid %in% V(bipartite_graph)$name[social_netid_vertices]]

demo_df <-merged_df |> select(social_netid, age, gender, village_number, school_level_numbered)
age_values <- demo_df$age[demo_df$social_netid %in% V(bipartite_graph)$name[social_netid_vertices]]
gender_values <- demo_df$gender[demo_df$social_netid %in% V(bipartite_graph)$name[social_netid_vertices]]
village_values <- demo_df$village_number[demo_df$social_netid %in% V(bipartite_graph)$name[social_netid_vertices]]
school_level_values <- demo_df$school_level_numbered[demo_df$social_netid %in% V(bipartite_graph)$name[social_netid_vertices]]


# Assign the attributes to the social_netid vertices
V(bipartite_graph)$house_sol <- NA  # Initialize the attribute
V(bipartite_graph)$house_sol[social_netid_vertices] <- house_sol_values

V(bipartite_graph)$commercial_goods <- NA  # Initialize the attribute
V(bipartite_graph)$commercial_goods[social_netid_vertices] <- commercial_goods_values

V(bipartite_graph)$age <- NA  # Initialize the attribute
V(bipartite_graph)$age[social_netid_vertices] <- age_values

V(bipartite_graph)$gender <- NA  # Initialize the attribute
V(bipartite_graph)$gender[social_netid_vertices] <- gender_values

V(bipartite_graph)$village <- NA  # Initialize the attribute
V(bipartite_graph)$village[social_netid_vertices] <- village_values

V(bipartite_graph)$school_level <- NA  # Initialize the attribute
V(bipartite_graph)$school_level[social_netid_vertices] <- school_level_values

# adding in animal vector attributes for animal type
V(bipartite_graph)$animal_name <- "Human"  # Initialize the attribute
V(bipartite_graph)$animal_name[animal_vertices] <- V(bipartite_graph)$name[animal_vertices]


# V(bipartite_graph)$animal_category <- "Human"  # Initialize the attribute
# V(bipartite_graph)$animal_category[animal_vertices] <- animal_categories

# getting network characteristics----
V(bipartite_graph)$degree<-igraph::degree(bipartite_graph)
mean(V(bipartite_graph)$degree[V(bipartite_graph)$type]) ##degrees for humans
range(V(bipartite_graph)$degree[V(bipartite_graph)$type]) ##range for humans
mean(V(bipartite_graph)$degree[!V(bipartite_graph)$type]) ##degrees for animals
range(V(bipartite_graph)$degree[!V(bipartite_graph)$type]) ##range for animals

# # degrees for animals
animal_degrees <- numeric(length = length(animal_types))

# Iterate over animal types
for (i in seq_along(animal_types)) {
  # Get the name of the current animal
  animal <- animal_types[i]

  # Get the index of the animal vertex in the graph
  animal_vertex <- which(V(bipartite_graph)$name == animal)

  # Calculate the degree of the animal vertex
  animal_degree <- igraph::degree(bipartite_graph, v = animal_vertex)

  # Store the degree in the vector
  animal_degrees[i] <- animal_degree
}

# Print the degrees of each animal
names(animal_degrees) <- animal_types
print(animal_degrees)

# adding in edge weights----
# 
# # Assigning edge weight attributes
# # rodent with edge weight
# rodent_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "rodent_weight")) |> 
#   filter(rodent_weight > 0)|> 
#   rename(weight = rodent_weight)
# 
# rodent_edges_only <-all_animal_edges_df |> 
#   filter(animal == "rodent")
# 
# rodent_edges_with_weights <- inner_join(rodent_edges_only, rodent_edge_weights, by = "social_netid")
# 
# # dogs with edge weight
# dogs_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "dogs_weight")) |> 
#   filter(dogs_weight > 0)|> 
#   rename(weight = dogs_weight)
# head(dogs_edge_weights)
# 
# 
# dogs_edges_only <-all_animal_edges_df |> 
#   filter(animal == "dogs") 
# 
# dogs_edges_with_weights <- inner_join(dogs_edges_only, dogs_edge_weights, by = "social_netid")
# 
# 
# # cats with edge weight
# cats_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "cats_weight")) |> 
#   filter(cats_weight > 0)|> 
#   rename(weight = cats_weight)
# cats_edges_only <-all_animal_edges_df |> 
#   filter(animal == "cats") 
# 
# cats_edges_with_weights <- inner_join(cats_edges_only, cats_edge_weights, by = "social_netid")
# 
# # domestic_pigs with edge weight
# domestic_pigs_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "domestic_pigs_weight")) |> 
#   filter(domestic_pigs_weight > 0)|> 
#   rename(weight = domestic_pigs_weight)
# domestic_pigs_edges_only <-all_animal_edges_df |> 
#   filter(animal == "domestic_pigs")
# 
# domestic_pigs_edges_with_weights <- inner_join(domestic_pigs_edges_only, domestic_pigs_edge_weights, by = "social_netid")
# 
# # goats_sheep with edge weight
# goats_sheep_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "goats_sheep_weight")) |> 
#   filter(goats_sheep_weight > 0) |> 
#   rename(weight = goats_sheep_weight)
# 
# goats_sheep_edges_only <-all_animal_edges_df |> 
#   filter(animal == "goats_sheep") 
# 
# goats_sheep_edges_with_weights <- inner_join(goats_sheep_edges_only, goats_sheep_edge_weights, by = "social_netid")
# 
# # cows with edge weight
# cows_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "cows_weight")) |> 
#   filter(cows_weight > 0)|> 
#   rename(weight = cows_weight)
# cows_edges_only <-all_animal_edges_df |> 
#   filter(animal == "cows") 
# 
# cows_edges_with_weights <- inner_join(cows_edges_only, cows_edge_weights, by = "social_netid")
# 
# # wild_birds with edge weights
# wild_birds_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "wild_birds_weight")) |> 
#   filter(wild_birds_weight > 0) |> 
#   rename(weight = wild_birds_weight)
# 
# wild_birds_edges_only <-all_animal_edges_df |> 
#   filter(animal == "wild_birds")
# 
# wild_birds_edges_with_weights <- inner_join(wild_birds_edges_only, wild_birds_edge_weights, by = "social_netid")
# 
# # bush_pigs with edge weights
# bush_pigs_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "bush_pigs_weight")) |> 
#   filter(bush_pigs_weight > 0)|> 
#   rename(weight = bush_pigs_weight)
# bush_pigs_edges_only <-all_animal_edges_df |> 
#   filter(animal == "bush_pigs")
# 
# bush_pigs_edges_with_weights <- inner_join(bush_pigs_edges_only, bush_pigs_edge_weights, by = "social_netid")
# 
# 
# # lemurs with edge weights
# lemurs_edge_weights <- animal_bipartite_df_wide |> 
#   select(c("social_netid", "lemurs_weight")) |> 
#   filter(lemurs_weight > 0) |> 
#   rename(weight = lemurs_weight)
# lemurs_edges_only <-all_animal_edges_df |> 
#   filter(animal == "lemurs")
# 
# lemurs_edges_with_weights <- inner_join(lemurs_edges_only, lemurs_edge_weights, by = "social_netid")
# 
# df<-bind_rows(rodent_edges_with_weights, dogs_edges_with_weights, cats_edges_with_weights,
#               domestic_pigs_edges_with_weights, goats_sheep_edges_with_weights,
#               cows_edges_with_weights,wild_birds_edges_with_weights, bush_pigs_edges_with_weights,
#               lemurs_edges_with_weights) |> 
#   arrange(social_netid)
# 
# all_animal_edge_weights <- df$weight

# adding edge weights based on #s of exposure
# E(bipartite_graph)$weight <- all_animal_edge_weights

V(bipartite_graph)$label<-""
V(bipartite_graph)[animal_vertices]$label<-animal_types
V(bipartite_graph)$color <- "red"
V(bipartite_graph)[animal_vertices]$color <- "blue"

plot(bipartite_graph, 
     layout = layout.bipartite,
     vertex.label.color = "black",
     hjust = 0.5)

## seeing if degree is predicted by MI ----
# Extract the degrees for social_netid vertices
degrees <- igraph::degree(bipartite_graph, v = social_netid_vertices)

# doing different bipartite networks for each village ----

bipartite_graph_village1 <- subgraph(graph=bipartite_graph, vids=which(V(bipartite_graph)$village=="Village 1"| is.na(V(bipartite_graph)$village)))

bipartite_graph_village2 <- subgraph(graph=bipartite_graph, vids=which(V(bipartite_graph)$village=="Village 2"| is.na(V(bipartite_graph)$village)))

bipartite_graph_village3 <- subgraph(graph=bipartite_graph, vids=which(V(bipartite_graph)$village=="Village 3"| is.na(V(bipartite_graph)$village)))


