# loading libraries/data
library(here)
library(igraph)
source(here("./self_reported_networks/Loading Interaction Data.R"))


# setting animals of interest ----
animals <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs", "goats_sheep",
                  "cows", "wild_birds", "bush_pigs", "lemurs", "tenrecs", "carnivores")

# creating wide df ----
for (animal in animals) {
  # Find columns containing the current animal type
  animal_cols <- grep(paste0("_", animal), names(merged_df), value = TRUE)
  
  # Create edges for animals reported at least once
  merged_df[[paste0(animal, "_edge")]] <- as.integer(rowSums(merged_df[animal_cols]) > 0)
}

animal_bipartite_df_wide <- merged_df |> 
  select(matches("social_netid|_edge", 
                 ignore.case = FALSE)) 

# make wide df into 2-col df----
# Initialize an empty list to store data frames for each animal type
animal_edges_dfs <- list()

for (animal in animals) {
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
bipartite_graph_full_network <- graph_from_data_frame(all_animal_edges_df, directed = FALSE)

# Setting vertex types----
animal_vertices <- which(V(bipartite_graph_full_network)$name %in% animals)
V(bipartite_graph_full_network)$type[animal_vertices] <- FALSE # animals = FALSE
V(bipartite_graph_full_network)$type<-ifelse(is.na(V(bipartite_graph_full_network)$type), TRUE, FALSE) # social IDs = true

social_netid_vertices <- which(V(bipartite_graph_full_network)$type) # setting the human mode

## adding in human vector attributes for MI and demographics ----

village_values <- merged_df$village[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]

age_values <- merged_df$age[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]
gender_values <- merged_df$gender[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]
commercial_goods_values <- merged_df$commercial_goods[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]
house_sol_values <- merged_df$house_sol[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]
grew_vanilla_values <- merged_df$grew_vanilla[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]
land_size_values <- merged_df$landsize_in_daba[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]
household_size_values <- merged_df$household_size[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]
school_level_values <- merged_df$school_level_numbered[merged_df$social_netid %in% V(bipartite_graph_full_network)$name[social_netid_vertices]]

# Assign the attributes to the social_netid vertices

V(bipartite_graph_full_network)$village <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$village[social_netid_vertices] <- village_values

V(bipartite_graph_full_network)$age <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$age[social_netid_vertices] <- age_values

V(bipartite_graph_full_network)$gender <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$gender[social_netid_vertices] <- gender_values

V(bipartite_graph_full_network)$commercial_goods <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$commercial_goods[social_netid_vertices] <- commercial_goods_values

V(bipartite_graph_full_network)$house_sol <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$house_sol[social_netid_vertices] <- house_sol_values

V(bipartite_graph_full_network)$grew_vanilla <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$grew_vanilla[social_netid_vertices] <- grew_vanilla_values

V(bipartite_graph_full_network)$land_size <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$land_size[social_netid_vertices] <- land_size_values

V(bipartite_graph_full_network)$household_size <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$household_size[social_netid_vertices] <- household_size_values

V(bipartite_graph_full_network)$school_level <- NA  # Initialize the attribute
V(bipartite_graph_full_network)$school_level[social_netid_vertices] <- school_level_values



# adding in animal vector attributes for animal type
V(bipartite_graph_full_network)$animal_name <- "Human"  # Initialize the attribute
V(bipartite_graph_full_network)$animal_name[animal_vertices] <- V(bipartite_graph_full_network)$name[animal_vertices]

# getting network characteristics----
V(bipartite_graph_full_network)$degree<-igraph::degree(bipartite_graph_full_network)
mean(V(bipartite_graph_full_network)$degree[V(bipartite_graph_full_network)$type]) ##degrees for humans
range(V(bipartite_graph_full_network)$degree[V(bipartite_graph_full_network)$type]) ##range for humans
mean(V(bipartite_graph_full_network)$degree[!V(bipartite_graph_full_network)$type]) ##degrees for animals
range(V(bipartite_graph_full_network)$degree[!V(bipartite_graph_full_network)$type]) ##range for animals

# # degrees for animals
animal_degrees <- numeric(length = length(animals))

# Iterate over animal types
for (i in seq_along(animals)) {
  # Get the name of the current animal
  animal <- animals[i]

  # Get the index of the animal vertex in the graph
  animal_vertex <- which(V(bipartite_graph_full_network)$name == animal)

  # Calculate the degree of the animal vertex
  animal_degree <- igraph::degree(bipartite_graph_full_network, v = animal_vertex)

  # Store the degree in the vector
  animal_degrees[i] <- animal_degree
}

# Print the degrees of each animal
names(animal_degrees) <- animals
print(animal_degrees)

V(bipartite_graph_full_network)$label<-""
V(bipartite_graph_full_network)[animal_vertices]$label<-animals
V(bipartite_graph_full_network)$color <- "red"
V(bipartite_graph_full_network)[animal_vertices]$color <- "blue"

plot(bipartite_graph_full_network, 
     layout = layout.bipartite,
     vertex.label.color = "black",
     hjust = 0.5)
table(merged_df$village)

# doing different bipartite networks for each village ----
bipartite_graph_full_network_ampandrana_andatsakala <- subgraph(graph = bipartite_graph_full_network, 
                                                                vids = which(V(bipartite_graph_full_network)$village == "Ampandrana" | 
                                                                               V(bipartite_graph_full_network)$village == "Andatsakala" | 
                                                                               is.na(V(bipartite_graph_full_network)$village))) # the na vertices are animals!


bipartite_graph_full_network_mandena <- subgraph(graph=bipartite_graph_full_network,
                                                 vids=which(V(bipartite_graph_full_network)$village=="Mandena" |
                                                                                                  is.na(V(bipartite_graph_full_network)$village)))

bipartite_graph_full_network_sarahandrano <- subgraph(graph=bipartite_graph_full_network, 
                                                      vids=which(V(bipartite_graph_full_network)$village=="Sarahandrano" | 
                                                                                                       is.na(V(bipartite_graph_full_network)$village)))
