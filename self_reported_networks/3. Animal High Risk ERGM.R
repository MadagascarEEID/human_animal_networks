library(dplyr)
library(here)
library(btergm)
library(igraph)
source(here("Scripts/Loading_Data_LK 2024.02.26.R"))

merged_df<- merged_df |> 
  select(-grep("frequency", names(merged_df)))

# setting animals of interest ----
animal_types <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs", "goats_sheep",
                  "carnivores", "cows", "wild_birds", "bush_pigs", "lemurs")
# creating wide df ----
for (animal in animal_types) {
  # Find columns containing the current animal type
  feces_cols <- grep(paste0("feces_", animal), names(merged_df), value = TRUE)
  shared_water_cols <- grep(paste0("shared_water_", animal), names(merged_df), value = TRUE)
  # eaten_sick_cols <- grep(paste0("eaten_sick_", animal), names(merged_df), value = TRUE)
  # dead_cols <- grep(paste0("dead_", animal), names(merged_df), value = TRUE)
  scratched_bitten_cols <- grep(paste0("scratched_bitten_", animal), names(merged_df), value = TRUE)
  
  # Create edges for animals reported at least once
  merged_df[[paste0(animal, "_edge_feces")]] <- as.integer(rowSums(merged_df[feces_cols]) > 0)
  merged_df[[paste0(animal, "_edge_shared_water")]] <- as.integer(rowSums(merged_df[shared_water_cols]) > 0)
  # merged_df[[paste0(animal, "_edge_eaten_sick")]] <- as.integer(rowSums(merged_df[eaten_sick_cols]) > 300)
  # merged_df[[paste0(animal, "_edge_dead")]] <- as.integer(rowSums(merged_df[dead_cols]) > 300)
  merged_df[[paste0(animal, "_edge_scratched_bitten")]] <- as.integer(rowSums(merged_df[scratched_bitten_cols]) > 0)
  
  # create high risk edge
  merged_df[[paste0(animal, "_edge_high_risk")]] <- as.integer(rowSums(merged_df[, c(paste0(animal, "_edge_feces"),
                                                                                     paste0(animal, "_edge_shared_water"),
                                                                                     paste0(animal, "_edge_scratched_bitten"))]) > 0)
}

sum(merged_df$cats_edge_high_risk)


animal_bipartite_df_wide <- merged_df |> 
  select(matches("social_netid|_edge", 
                 ignore.case = FALSE))

animal_edges_dfs_high_risk <- list()

for (animal in animal_types) {
  # Get social_netid values for the current animal type
  animal_edges_high_risk <- animal_bipartite_df_wide |> 
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
isolate_animals <- setdiff(animal_types, all_animal_edges_df_high_risk$animal)


bipartite_graph_high_risk <- add_vertices(bipartite_graph_high_risk, 
                                      nv = length(isolates), name = isolates)

bipartite_graph_high_risk <- add_vertices(bipartite_graph_high_risk, 
                                      nv = length(isolate_animals), name = isolate_animals)


# Setting vertex types----
animal_vertices <- which(V(bipartite_graph_high_risk)$name %in% animal_types)
V(bipartite_graph_high_risk)$type <- TRUE
V(bipartite_graph_high_risk)$type[animal_vertices] <- FALSE # animals = FALSE

social_netid_vertices <- which(V(bipartite_graph_high_risk)$type)


MI_df<-merged_df |>  select(social_netid, house_sol, commercial_goods)
commercial_goods_values <- MI_df$commercial_goods[MI_df$social_netid %in% V(bipartite_graph_high_risk)$name[social_netid_vertices]]
house_sol_values <- MI_df$house_sol[MI_df$social_netid %in% V(bipartite_graph_high_risk)$name[social_netid_vertices]]

demo_df <-merged_df |> select(social_netid, age, gender, village_number, school_level_numbered)
age_values <- demo_df$age[demo_df$social_netid %in% V(bipartite_graph_high_risk)$name[social_netid_vertices]]
gender_values <- demo_df$gender[demo_df$social_netid %in% V(bipartite_graph_high_risk)$name[social_netid_vertices]]
village_values <- demo_df$village_number[demo_df$social_netid %in% V(bipartite_graph_high_risk)$name[social_netid_vertices]]
school_level_values <- demo_df$school_level_numbered[demo_df$social_netid %in% V(bipartite_graph_high_risk)$name[social_netid_vertices]]


# Assign the attributes to the social_netid vertices
V(bipartite_graph_high_risk)$house_sol <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$house_sol[social_netid_vertices] <- house_sol_values

V(bipartite_graph_high_risk)$commercial_goods <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$commercial_goods[social_netid_vertices] <- commercial_goods_values

V(bipartite_graph_high_risk)$age <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$age[social_netid_vertices] <- age_values

V(bipartite_graph_high_risk)$gender <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$gender[social_netid_vertices] <- gender_values

V(bipartite_graph_high_risk)$village <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$village[social_netid_vertices] <- village_values

V(bipartite_graph_high_risk)$school_level <- NA  # Initialize the attribute
V(bipartite_graph_high_risk)$school_level[social_netid_vertices] <- school_level_values

# adding in animal vector attributes for animal type
V(bipartite_graph_high_risk)$animal_name <- "Human"  # Initialize the attribute
V(bipartite_graph_high_risk)$animal_name[animal_vertices] <- V(bipartite_graph_high_risk)$name[animal_vertices]


# V(bipartite_graph_high_risk)$animal_category <- "Human"  # Initialize the attribute
# V(bipartite_graph_high_risk)$animal_category[animal_vertices] <- animal_categories

# getting network characteristics----
V(bipartite_graph_high_risk)$degree<-igraph::degree(bipartite_graph_high_risk)
mean(V(bipartite_graph_high_risk)$degree[V(bipartite_graph_high_risk)$type]) ##degrees for humans
range(V(bipartite_graph_high_risk)$degree[V(bipartite_graph_high_risk)$type]) ##range for humans
mean(V(bipartite_graph_high_risk)$degree[!V(bipartite_graph_high_risk)$type]) ##degrees for animals
range(V(bipartite_graph_high_risk)$degree[!V(bipartite_graph_high_risk)$type]) ##range for animals

# # degrees for animals
animal_degrees_high_risk <- numeric(length = length(animal_types))

# Iterate over animal types
for (i in seq_along(animal_types)) {
  # Get the name of the current animal
  animal <- animal_types[i]
  
  # Get the index of the animal vertex in the graph
  animal_vertex <- which(V(bipartite_graph_high_risk)$name == animal)
  
  # Check if animal_vertex is not empty
  if (length(animal_vertex) > 0) {
    # Calculate the degree of the animal vertex
    animal_degree <- igraph::degree(bipartite_graph_high_risk, v = animal_vertex)
    
    # Store the degree in the vector
    if (animal_degree == 0) {
      animal_degrees_high_risk[i] <- 0
    } else {
      animal_degrees_high_risk[i] <- animal_degree
    }
  } else {
    # If no vertex found, store 0 as the degree
    animal_degrees_high_risk[i] <- 0
  }
}

# # Print the degrees of each animal
# names(animal_degrees_high_risk) <- animal_types
# print(animal_degrees_high_risk)
# 
# V(bipartite_graph_high_risk)$label<-""
# # V(bipartite_graph_high_risk)[animal_vertices]$label<-V(bipartite_graph_high_risk)$name[animal_vertices]
# V(bipartite_graph_high_risk)$color <- "red"
# V(bipartite_graph_high_risk)[animal_vertices]$color <- "blue"
# 
# plot(bipartite_graph_high_risk, 
#      layout = layout.bipartite,
#      vertex.label.color = "black",
#      hjust = 0.5)
# 
# plot(
#   bipartite_graph_high_risk,
#   layout = layout.bipartite,
#   # vertex.label.color = "black",
#   # vertex.label.dist = 0.5, # Adjust label distance from vertices
#   # vertex.label.cex = 0.8,  # Adjust label size
#   vertex.size = 10,        # Adjust vertex size
#   edge.arrow.size = 0.5,   # Adjust arrow size
#   edge.curved = 0.2,       # Add some curve to the edges
#   margin = 0.1,           # Add some margin around the plot
#   vertex.label.angle = ifelse(V(bipartite_graph_high_risk)$name %in% animal_types, 45, 0)  # Set label angle
# )


bipartite_graph_high_risk_village1 <- subgraph(graph=bipartite_graph_high_risk, vids=which(V(bipartite_graph_high_risk)$village=="Village 1"| is.na(V(bipartite_graph_high_risk)$village)))

bipartite_graph_high_risk_village2 <- subgraph(graph=bipartite_graph_high_risk, vids=which(V(bipartite_graph_high_risk)$village=="Village 2"| is.na(V(bipartite_graph_high_risk)$village)))

bipartite_graph_high_risk_village3 <- subgraph(graph=bipartite_graph_high_risk, vids=which(V(bipartite_graph_high_risk)$village=="Village 3"| is.na(V(bipartite_graph_high_risk)$village)))



library(statnet)
set.seed(1234)
# Convert igraph objects to network objects
graph_names_high_risk_high_risk <- c("bipartite_graph_high_risk", "bipartite_graph_high_risk_village1", "bipartite_graph_high_risk_village2", "bipartite_graph_high_risk_village3")
bipartite_graph_sna_list_high_risk <- lapply(graph_names_high_risk_high_risk, function(graph_names_high_risk) {
  network(as_biadjacency_matrix(get(graph_names_high_risk)))
})

# Export node level variables from igraph into standalone data frames for each village
node_df_list <- lapply(graph_names_high_risk_high_risk, function(village_name) {
  node_df <- data.frame(
    id = V(get(village_name))$name,
    house_sol = V(get(village_name))$house_sol,
    commercial_goods = V(get(village_name))$commercial_goods,
    age = V(get(village_name))$age,
    village = V(get(village_name))$village,
    gender = V(get(village_name))$gender,
    degree = V(get(village_name))$degree,
    school_level = V(get(village_name))$school_level,
    animal_name = V(get(village_name))$animal_name
  )
  node_df$animal_category <- ifelse(grepl("cats|cows|dogs|domestic_pigs|goats_sheep|poultry", node_df$animal_name), "domestic",
                                    ifelse(grepl("rodents", node_df$animal_name), "rodents", "wild"))
  node_df$animal_category <- ifelse(grepl("\\.", node_df$id), "human", node_df$animal_category)
  node_df
})

# Reorder node_df to match order of IDs in statnet for each village
reorder_node_df <- function(node_df, statnet_order) {
  node_df[match(statnet_order, node_df$id), ]
}
node_df_list <- Map(reorder_node_df, node_df_list, lapply(bipartite_graph_sna_list_high_risk, function(graph) as.character(graph %v% "vertex.names")))

# Update network objects with node attributes
update_network_attributes <- function(graph, node_df) {
  graph %v% "house_sol" <- node_df$house_sol
  graph %v% "commercial_goods" <- node_df$commercial_goods
  graph %v% "age" <- node_df$age
  graph %v% "village" <- node_df$village
  graph %v% "gender" <- node_df$gender
  graph %v% "degree" <- node_df$degree
  graph %v% "school_level" <- node_df$school_level
  graph %v% "animal_category" <- node_df$animal_category
  graph %v% "animal_name" <- node_df$animal_name
  graph
}
bipartite_graph_sna_list_high_risk <- Map(update_network_attributes, bipartite_graph_sna_list_high_risk, node_df_list)
detach("package:btergm", unload = TRUE)
detach("package:igraph", unload = TRUE)
# Remove missing values from each network object
bipartite_graph_sna_list_high_risk <- lapply(bipartite_graph_sna_list_high_risk, function(graph) {
  vertex_attrs <- c("house_sol", "commercial_goods", "age", "village", "gender", "degree", "school_level", "animal_category", "animal_name")
  vertices_with_na <- integer(0)
  for (attr in vertex_attrs) {
    vertices_with_na <- union(vertices_with_na, which(is.na(graph %v% attr)))
  }
  vertices_with_na <- unique(vertices_with_na)
  vertices_with_na <- setdiff(vertices_with_na, 1:11)
  delete.vertices(graph, vertices_with_na)
})

# Run ERGMs for each village
p1_ergm_list_high_risk <- lapply(bipartite_graph_sna_list_high_risk, function(graph) {
  ergm(graph ~ edges +
         b2factor("gender") +
         b2factor("village") +
         b2cov("age") +
         b2cov("commercial_goods") +
         b2cov("house_sol") +
         b2cov("school_level") +
         b1factor("animal_category") +
         b1factor("animal_category"):b2cov("commercial_goods"):b2cov("house_sol"))
}
)

# mcmc_ergm1_high_risk<-  ergm(bipartite_graph_sna_list_high_risk[[1]] ~ edges +
#          b2factor("gender") +
#          b2factor("village") +
#          b2cov("age") +
#          b2cov("commercial_goods") +
#          b2cov("house_sol") +
#          b2cov("school_level") +
#          b1factor("animal_category") +
#          b1factor("animal_category"):b2cov("commercial_goods"):b2cov("house_sol") +
#          b2degree(2),
#        control = control.ergm(parallel = 4,seed=1234),
#        verbose = TRUE)

par(mfrow = c(3,2))
summary(mcmc_ergm1_high_risk)

mcmc.diagnostics(mcmc_ergm1_high_risk)
gof_mcmc1_high_risk<-gof(mcmc_ergm1_high_risk,
                               verbose = TRUE)
plot(gof_mcmc1_high_risk)


mcmc_ergm2_high_risk<-  ergm(bipartite_graph_sna_list_high_risk[[1]] ~ edges +
                                b2factor("gender") +
                                b2factor("village") +
                                b2cov("age") +
                                b2cov("commercial_goods") +
                                b2cov("house_sol") +
                                b2cov("school_level") +
                                b1factor("animal_category") +
                                b1factor("animal_category"):b2cov("commercial_goods"):b2cov("house_sol") +
                                b2degree(2)+
                                b2degree(3)+
                                b2degree(4),
                              control = control.ergm(parallel = 4,seed=1234),
                              verbose = TRUE)
library(broom)
library(clipr)
mcmc_ergm2_high_risk |> 
  tidy() |> 
  write_clip()
AIC(p1_ergm_list_high_risk[[1]], mcmc_ergm1_high_risk, mcmc_ergm2_high_risk)


gof_mcmc2_high_risk<-gof(mcmc_ergm2_high_risk)
plot(gof_mcmc2_high_risk)

summary(mcmc_ergm2_high_risk)
exp(confint((mcmc_ergm2_high_risk)))
exp(-2.506)

# # Summarize ERGM results
# for (i in seq_along(p1_ergm_list_high_risk)) {
#   print(summary(p1_ergm_list_high_risk[[i]]))
#   print(i)
# }
# 
# for (i in seq_along(mcmc_ergm_list_high_risk)) {
#   print(summary(mcmc_ergm_list_high_risk[[i]]))
#   print(i)
# }

# summary(mcmc_ergm_list_high_risk[[1]])
# AIC(mcmc_ergm_list_high_risk[[1]],p1_ergm_list_high_risk[[1]])
# 
# AIC(mcmc_ergm_list_high_risk[[1]],p1_ergm_list[[1]])
# 
