library(here)
source(here("self_reported_networks/3. Animal Interaction Bipartite High Risk.R"))
library(statnet)
library(broom)
set.seed(1234)

# Convert igraph objects to network objects
graph_names_high_risk_high_risk <- c("bipartite_graph_high_risk", "bipartite_graph_high_risk_ampandrana_andatsakala",
                                     "bipartite_graph_high_risk_mandena", "bipartite_graph_high_risk_sarahandrano")

bipartite_graph_sna_list_high_risk <- lapply(graph_names_high_risk_high_risk, function(graph_names_high_risk) {
  network(as_biadjacency_matrix(get(graph_names_high_risk)))
})


# Export node level variables from igraph into standalone data frames for each village
node_df_list_high_risk <- lapply(graph_names_high_risk_high_risk, function(village_name) {
  node_df <- data.frame(
    id = V(get(village_name))$name,
    village = V(get(village_name))$village,
    age = V(get(village_name))$age,
    gender = V(get(village_name))$gender,
    commercial_goods = V(get(village_name))$commercial_goods,
    house_sol = V(get(village_name))$house_sol,
    grew_vanilla = V(get(village_name))$grew_vanilla,
    land_size = V(get(village_name))$land_size,
    household_size = V(get(village_name))$household_size,
    school_level = V(get(village_name))$school_level,
    degree = V(get(village_name))$degree,
    animal_name = V(get(village_name))$animal_name
  )
  node_df$animal_category <- ifelse(grepl("cats|cows|dogs|domestic_pigs|goats_sheep|poultry", node_df$animal_name), "domestic", 
                                    ifelse(grepl("rodents", node_df$animal_name), "rodents", "wild"))
  node_df$animal_category <- ifelse(grepl("\\.", node_df$id), "human", node_df$animal_category)
  node_df
})

# unload igraph package or there will be issues with statnet
detach("package:igraph", unload = TRUE)

# Reorder node_df to match order of IDs in statnet for each village
reorder_node_df <- function(node_df, statnet_order) {
  node_df[match(statnet_order, node_df$id), ]
}

node_df_list_high_risk <- Map(reorder_node_df, node_df_list_high_risk, lapply(bipartite_graph_sna_list_high_risk,
                                                          function(graph) as.character(graph %v% "vertex.names")))

# Update network objects with node attributes
update_network_attributes <- function(graph, node_df) {
  graph %v% "village" <- node_df$village
  graph %v% "age" <- node_df$age
  graph %v% "gender" <- node_df$gender
  graph %v% "commercial_goods" <- node_df$commercial_goods
  graph %v% "house_sol" <- node_df$house_sol
  graph %v% "grew_vanilla" <- node_df$grew_vanilla
  graph %v% "land_size" <- node_df$land_size
  graph %v% "household_size" <- node_df$household_size
  graph %v% "school_level" <- node_df$school_level
  
  graph %v% "degree" <- node_df$degree
  graph %v% "animal_category" <- node_df$animal_category
  graph %v% "animal_name" <- node_df$animal_name
  
  graph
}
bipartite_graph_sna_list_high_risk <- Map(update_network_attributes, bipartite_graph_sna_list_high_risk, node_df_list_high_risk)

# Remove missing values from each network object
bipartite_graph_sna_list_high_risk <- lapply(bipartite_graph_sna_list_high_risk, function(graph) {
  vertex_attrs <- c("village", "age", "gender", "commercial_goods", "house_sol",
                    "grew_vanilla", "land_size", "household_size", "school_level",
                    "degree", "animal_category", "animal_name")
  vertices_with_na <- integer(0)
  for (attr in vertex_attrs) {
    vertices_with_na <- union(vertices_with_na, which(is.na(graph %v% attr)))
  }
  vertices_with_na <- unique(vertices_with_na)
  vertices_with_na <- setdiff(vertices_with_na, 1:12) # excluding animal vertices
  print(vertices_with_na)
  delete.vertices(graph, vertices_with_na)
})

bipartite_graph_sna_list_high_risk <- lapply(bipartite_graph_sna_list_high_risk, function(graph) {
  # Manually set the bipartite attribute to 12 for all networks...for some reason it is only setting 11 nodes as animal
  graph %n% "bipartite" <- 12
  
  # Return the updated network object
  return(graph)
})

# Run ERGMs for each village
dyad_independent_ergm_list_high_risk <- lapply(bipartite_graph_sna_list_high_risk, function(graph) {
  ergm(graph ~ edges +
         b2cov("age") +
         b2factor("gender") +
         b2cov("commercial_goods") +
         b2cov("house_sol") +
         b2factor("grew_vanilla") +
         b2cov("land_size") +
         b2cov("household_size") +
         b2cov("school_level") +
         b1factor("animal_category") +
         b1factor("animal_category"):b2cov("commercial_goods")+
         b1factor("animal_category"):b2cov("house_sol")+
         b1factor("animal_category"):b2factor("grew_vanilla"))
}
)


# Summarize ERGM results
for (i in seq_along(dyad_independent_ergm_list_high_risk)) {
  graph <- bipartite_graph_sna_list_high_risk[[i]]
  
  village_name <- unique(graph%v%"village") 
  print(village_name[!is.na(village_name)])

  print(summary(dyad_independent_ergm_list_high_risk[[i]]))
  
  print("################################################################################")
}
