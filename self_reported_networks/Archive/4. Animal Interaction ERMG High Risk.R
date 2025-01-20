library(here)
source(here("self_reported_networks/3. Animal Interaction Bipartite High Risk.R"))
library(broom)
library(ergMargins)
set.seed(1234)

# Convert igraph objects to network objects
graph_names_high_risk_high_risk <- c("bipartite_graph_high_risk_ampandrana_andatsakala",
                                     "bipartite_graph_high_risk_mandena", "bipartite_graph_high_risk_sarahandrano")

bipartite_graphs_high_risk <- list(
  bipartite_graph_high_risk_ampandrana_andatsakala,
  bipartite_graph_high_risk_mandena, # lemur isolates
  bipartite_graph_high_risk_sarahandrano 
)

library(igraph)

for (i in seq_along(bipartite_graphs_high_risk)) {
  if (i == 2){ # deleting lemur vertex from Mandena bipartite graph bc it's an isolate!
    bipartite_graphs_high_risk[[i]] <- induced_subgraph(bipartite_graphs_high_risk[[i]], 
                                                        vids = V(bipartite_graphs_high_risk[[i]])[V(bipartite_graphs_high_risk[[i]])$animal_name != "lemurs"])
  }
  
  else 
  {
    bipartite_graphs_high_risk[[i]] <- bipartite_graphs_high_risk[[i]]
  }
}

# detach("package:igraph", unload = TRUE)
library(statnet)

bipartite_graph_sna_list_high_risk <- lapply(bipartite_graphs_high_risk, function(graph) {
  network(as_biadjacency_matrix(graph))
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
    animal_name = V(get(village_name))$animal_name,
    animal_community = V(get(village_name))$animal_community
    
  )
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
  graph %v% "animal_community" <- node_df$animal_community
  graph %v% "animal_name" <- node_df$animal_name
  
  graph
}
bipartite_graph_sna_list_high_risk <- Map(update_network_attributes, bipartite_graph_sna_list_high_risk, node_df_list_high_risk)

# Remove missing values from each network object
vertex_attrs <- c("village", "age", "gender", "commercial_goods", "house_sol",
                  "grew_vanilla", "land_size", "household_size", "school_level",
                  "degree")

for (i in seq_along(bipartite_graph_sna_list_high_risk)) {
  vertices_with_na <- integer(0)
  
  # Identify vertices with missing values for any of the specified attributes
  for (attr in vertex_attrs) {
    vertices_with_na <- union(vertices_with_na, which(is.na(bipartite_graph_sna_list_high_risk[[i]] %v% attr)))
  }
  
  # Exclude the first 12 animal vertices
  vertices_with_na <- setdiff(vertices_with_na, 1:12)
  
  # Print the vertices with missing values (for debugging purposes)
  print(paste("Vertices with missing values in graph", i, ":", vertices_with_na))
  
  # Remove the vertices with missing values
  bipartite_graph_sna_list_high_risk[[i]] <- delete.vertices(bipartite_graph_sna_list_high_risk[[i]], vertices_with_na)
}


library(ergm)
# Run ERGMs for each village
ergm_ampandrana_andatsakala_high_risk<- ergm(bipartite_graph_sna_list_high_risk[[1]] ~ edges +
         b2cov("age") +
         b2factor("gender") +
         b2cov("commercial_goods") +
         b2cov("house_sol") +
         b2factor("grew_vanilla") +
         b2cov("land_size") +
         b2cov("household_size") +
         b2cov("school_level") +
         b1factor("animal_community") +
         b1factor("animal_community"):b2cov("commercial_goods")+
         b1factor("animal_community"):b2cov("house_sol")+
         b1factor("animal_community"):b2factor("grew_vanilla") +         
         b2degree(1)+
         b2degree(2) +
           b2degree(3) +
           
         b2degree(4) +
         b2degree(5) +
         b2degree(7)+
         b2degree(8),
         control = control.ergm(seed = 1234))

# mcmc.diagnostics(ergm_ampandrana_andatsakala_high_risk)
# ergm:::plot.gof(gof(ergm_ampandrana_andatsakala_high_risk))
# gof(ergm_ampandrana_andatsakala_high_risk)
vif.ergm(ergm_ampandrana_andatsakala_high_risk)
summary(ergm_ampandrana_andatsakala_high_risk)


ergm_mandena_high_risk<- ergm(bipartite_graph_sna_list_high_risk[[2]] ~ edges +
                                               b2cov("age") +
                                               b2factor("gender") +
                                               b2cov("commercial_goods") +
                                               b2cov("house_sol") +
                                               b2factor("grew_vanilla") +
                                               b2cov("land_size") +
                                               b2cov("household_size") +
                                               b2cov("school_level") +
                                               b1factor("animal_community") +
                                               b1factor("animal_community"):b2cov("commercial_goods")+
                                               b1factor("animal_community"):b2cov("house_sol")+
                                               b1factor("animal_community"):b2factor("grew_vanilla") +         
                                 b2degree(2)+
                                                b2degree(3)+
                                              b2degree(4),
                              control = control.ergm(seed=123))
# mcmc.diagnostics(ergm_mandena_high_risk)
# ergm:::plot.gof(gof(ergm_mandena_high_risk))
# gof(ergm_mandena_high_risk)
# vif.ergm(ergm_mandena_high_risk)
 summary(ergm_mandena_high_risk)


ergm_sarahandrano_high_risk<- ergm(bipartite_graph_sna_list_high_risk[[3]] ~ edges +
                                               b2cov("age") +
                                               b2factor("gender") +
                                               b2cov("commercial_goods") +
                                               b2cov("house_sol") +
                                               b2factor("grew_vanilla") +
                                               b2cov("land_size") +
                                               b2cov("household_size") +
                                               b2cov("school_level") +
                                               b1factor("animal_community") +
                                               b1factor("animal_community"):b2cov("commercial_goods")+
                                               b1factor("animal_community"):b2cov("house_sol")+
                                               b1factor("animal_community"):b2factor("grew_vanilla") +         
                                               b2degree(2) +
                                               b2degree(3) +
                                               b2degree(4)+
                                                b2degree(5)+
                                     b2degree(8),
                                             control = control.ergm(seed = 1234))

# mcmc.diagnostics(ergm_sarahandrano_high_risk)
#ergm:::plot.gof(gof(ergm_sarahandrano_high_risk))
# gof(ergm_sarahandrano_high_risk)
vif.ergm(ergm_sarahandrano_high_risk)
summary(ergm_sarahandrano_high_risk)

