# loading libraries/data
library(here)
library(igraph)
source(here("./self_reported_networks/The_ReUp/Loading Interaction Data.R"))


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


# adding back in social netid isolates
isolates <- setdiff(merged_df$social_netid, all_animal_edges_df$social_netid)
isolate_animals <- setdiff(animals, all_animal_edges_df$animal)

bipartite_graph_full_network <- add_vertices(bipartite_graph_full_network, 
                                          nv = length(isolates), name = isolates)

# Setting vertex types----
animal_vertices <- which(V(bipartite_graph_full_network)$name %in% animals)

V(bipartite_graph_full_network)$type <- TRUE
V(bipartite_graph_full_network)$type[animal_vertices] <- FALSE # animals = FALSE

social_netid_vertices <- which(V(bipartite_graph_full_network)$type) # setting the human mode

## adding in human vector attributes for MI and demographics ----

village_values <- merged_df$village[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
age_values <- merged_df$age[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
gender_values <- merged_df$gender[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
commercial_goods_values <- merged_df$commercial_goods[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
house_sol_values <- merged_df$house_sol[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
grew_vanilla_values <- merged_df$grew_vanilla[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
land_size_values <- merged_df$landsize_in_daba[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
household_size_values <- merged_df$household_size[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]
school_level_values <- merged_df$school_level_numbered[match(V(bipartite_graph_full_network)$name[social_netid_vertices], merged_df$social_netid)]

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



# doing different bipartite networks for each village ----
bipartite_graph_full_network_ampandrana_andatsakala <- subgraph(graph = bipartite_graph_full_network, 
                                                                vids = which(V(bipartite_graph_full_network)$village == "Ampandrana" | 
                                                                               V(bipartite_graph_full_network)$village == "Andatsakala" | 
                                                                               is.na(V(bipartite_graph_full_network)$village))) # the na vertices are animals!

V(bipartite_graph_full_network_ampandrana_andatsakala)$degree<-igraph::degree(bipartite_graph_full_network_ampandrana_andatsakala)
mean(V(bipartite_graph_full_network_ampandrana_andatsakala)$degree[V(bipartite_graph_full_network_ampandrana_andatsakala)$type]) ##degrees for humans
range(V(bipartite_graph_full_network_ampandrana_andatsakala)$degree[V(bipartite_graph_full_network_ampandrana_andatsakala)$type]) ##range for humans
mean(V(bipartite_graph_full_network_ampandrana_andatsakala)$degree[!V(bipartite_graph_full_network_ampandrana_andatsakala)$type]) ##degrees for animals
range(V(bipartite_graph_full_network_ampandrana_andatsakala)$degree[!V(bipartite_graph_full_network_ampandrana_andatsakala)$type]) ##range for animals




bipartite_graph_full_network_mandena <- subgraph(graph=bipartite_graph_full_network,
                                                 vids=which(V(bipartite_graph_full_network)$village=="Mandena" |
                                                                                                  is.na(V(bipartite_graph_full_network)$village)))

V(bipartite_graph_full_network_mandena)$degree<-igraph::degree(bipartite_graph_full_network_mandena)
mean(V(bipartite_graph_full_network_mandena)$degree[V(bipartite_graph_full_network_mandena)$type]) ##degrees for humans
range(V(bipartite_graph_full_network_mandena)$degree[V(bipartite_graph_full_network_mandena)$type]) ##range for humans
mean(V(bipartite_graph_full_network_mandena)$degree[!V(bipartite_graph_full_network_mandena)$type]) ##degrees for animals
range(V(bipartite_graph_full_network_mandena)$degree[!V(bipartite_graph_full_network_mandena)$type]) ##range for animals



bipartite_graph_full_network_sarahandrano <- subgraph(graph=bipartite_graph_full_network, 
                                                      vids=which(V(bipartite_graph_full_network)$village=="Sarahandrano" | 
                                                                                                       is.na(V(bipartite_graph_full_network)$village)))
V(bipartite_graph_full_network_sarahandrano)$degree<-igraph::degree(bipartite_graph_full_network_sarahandrano)
mean(V(bipartite_graph_full_network_sarahandrano)$degree[V(bipartite_graph_full_network_sarahandrano)$type]) ##degrees for humans
range(V(bipartite_graph_full_network_sarahandrano)$degree[V(bipartite_graph_full_network_sarahandrano)$type]) ##range for humans
mean(V(bipartite_graph_full_network_sarahandrano)$degree[!V(bipartite_graph_full_network_sarahandrano)$type]) ##degrees for animals
range(V(bipartite_graph_full_network_sarahandrano)$degree[!V(bipartite_graph_full_network_sarahandrano)$type]) ##range for animals


## going back to assigning animals based on domesticated, rodent, or wild

# animal categories
domesticated_animals <- c("poultry", "dogs", "domestic_pigs", "cows", "goats_sheep", "cats")
rodent_animals <- "rodents"
wild_animals <- c("bush_pigs", "carnivores", "tenrecs", "wild_birds", "lemurs")


# Create the new attribute animal_category -----
#ampandrana

V(bipartite_graph_full_network_ampandrana_andatsakala)$animal_category <- ifelse(
  V(bipartite_graph_full_network_ampandrana_andatsakala)$animal_name %in% domesticated_animals, "domesticated",
  ifelse(V(bipartite_graph_full_network_ampandrana_andatsakala)$animal_name %in% wild_animals, "wild", 
         ifelse(V(bipartite_graph_full_network_ampandrana_andatsakala)$animal_name %in% rodent_animals, "rodent", 
        NA )))

#mandena
V(bipartite_graph_full_network_mandena)$animal_category <- ifelse(
  V(bipartite_graph_full_network_mandena)$animal_name %in% domesticated_animals, "domesticated",
  ifelse(V(bipartite_graph_full_network_mandena)$animal_name %in% wild_animals, "wild", 
         ifelse(V(bipartite_graph_full_network_mandena)$animal_name %in% rodent_animals, "rodent", 
                NA )))

#sarahandrano
V(bipartite_graph_full_network_sarahandrano)$animal_category <- ifelse(
  V(bipartite_graph_full_network_sarahandrano)$animal_name %in% domesticated_animals, "domesticated",
  ifelse(V(bipartite_graph_full_network_sarahandrano)$animal_name %in% wild_animals, "wild", 
         ifelse(V(bipartite_graph_full_network_sarahandrano)$animal_name %in% rodent_animals, "rodent", 
                NA )))
### CREATING NODE DEGREE TABLES TO DO CORR TESTS WITH SPATIAL NETOWRKS
# 
# bipartite_graph_full_network_ampandrana_andatsakala_for_compare <- subgraph(graph=bipartite_graph_full_network_ampandrana_andatsakala,
#                                                                             vids=which(grepl("E.|cats|cows|dogs|domestic_pigs", 
#                                                                                              V(bipartite_graph_full_network_ampandrana_andatsakala)$name)))
# 
# V(bipartite_graph_full_network_ampandrana_andatsakala_for_compare)$degree<-igraph::degree(bipartite_graph_full_network_ampandrana_andatsakala_for_compare)
# 
# node_degree_table_ampandrana_adatsakala <- data.frame(
#   name = V(bipartite_graph_full_network_ampandrana_andatsakala_for_compare)$name,
#   degree = V(bipartite_graph_full_network_ampandrana_andatsakala_for_compare)$degree
# ) |> 
#   mutate(village = "ampandrana_andatsakala") |> 
#   filter(grepl("E.", name))
# 
# bipartite_graph_full_network_mandena_for_compare <- subgraph(graph=bipartite_graph_full_network_mandena,
#                                                                             vids=which(grepl("A.|cats|cows|dogs|domestic_pigs", 
#                                                                                              V(bipartite_graph_full_network_mandena)$name)))
# 
# V(bipartite_graph_full_network_mandena_for_compare)$degree<-igraph::degree(bipartite_graph_full_network_mandena_for_compare)
# 
# 
# node_degree_table_mandena <- data.frame(
#   name = V(bipartite_graph_full_network_mandena_for_compare)$name,
#   degree = V(bipartite_graph_full_network_mandena_for_compare)$degree
# ) |> 
#   mutate(village = "mandena") |> 
#   filter(grepl("A.", name))
# 
# 
# # 
# # bipartite_graph_full_network_sarahandrano_for_compare <- subgraph(graph=bipartite_graph_full_network_sarahandrano,
# #                                                              vids=which(grepl("D.|cats|cows|dogs|domestic_pigs", 
# #                                                                               V(bipartite_graph_full_network_sarahandrano)$name)))
# # 
# # V(bipartite_graph_full_network_sarahandrano_for_compare)$degree<-igraph::degree(bipartite_graph_full_network_sarahandrano_for_compare)
# # 
# # 
# # node_degree_table_sarahandrano <- data.frame(
# #   name = V(bipartite_graph_full_network_sarahandrano_for_compare)$name,
# #   degree = V(bipartite_graph_full_network_sarahandrano_for_compare)$degree
# # ) |> 
# #   mutate(village = "sarahandrano") |> 
# #   filter(grepl("D.", name))
# # 
# # node_degree_table <- bind_rows(
# #   node_degree_table_ampandrana_adatsakala,
# #   node_degree_table_mandena,
# #   node_degree_table_sarahandrano
# # )
# # 
# # # save(node_degree_table, file = "node_degree_table_restricted_animals.RData") 
# # # 
# # # View(node_degree_table)
# 
# 
# 
