library(here)
source(here("self_reported_networks/1. Animal Interaction Bipartite Full Network.R"))
library(statnet)
library(broom)
set.seed(1234)

# Convert igraph objects to network objects
graph_names <- c("bipartite_graph_full_network_ampandrana_andatsakala", 
                 "bipartite_graph_full_network_mandena", "bipartite_graph_full_network_sarahandrano")


bipartite_graph_full_network_sna_list <- lapply(graph_names, function(graph_names) {
  network(as_biadjacency_matrix(get(graph_names)))
})

# Export node level variables from igraph into standalone data frames for each village
node_df_list <- lapply(graph_names, function(village_name) {
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

node_df_list <- Map(reorder_node_df, node_df_list, lapply(bipartite_graph_full_network_sna_list,
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
  graph %v% "animal_name" <- node_df$animal_name
  graph %v% "animal_community" <- node_df$animal_community
  
  graph
}


bipartite_graph_full_network_sna_list <- Map(update_network_attributes, bipartite_graph_full_network_sna_list, node_df_list)

# Remove missing values from each network object
bipartite_graph_full_network_sna_list <- lapply(bipartite_graph_full_network_sna_list, function(graph) {
  vertex_attrs <- c("village", "age", "gender", "commercial_goods", "house_sol",
                    "grew_vanilla", "land_size", "household_size", "school_level",
                    "degree", "animal_name")
  vertices_with_na <- integer(0)
  for (attr in vertex_attrs) {
    vertices_with_na <- union(vertices_with_na, which(is.na(graph %v% attr)))
  }
  vertices_with_na <- unique(vertices_with_na)
  vertices_with_na <- setdiff(vertices_with_na, 1:12) # excluding animal vertices
  print(vertices_with_na)
  # length(vertices_with_na)/igraph::with_vertex_() # FIND OUT % VERTICES BEING EXCLUDED
  delete.vertices(graph, vertices_with_na)
})

bipartite_graph_full_network_sna_list <- lapply(bipartite_graph_full_network_sna_list, function(graph) {
  # Manually set the bipartite attribute to 12 for all networks...for some reason it is only setting 11 nodes as animal
  graph %n% "bipartite" <- 12
  
  # Return the updated network object
  return(graph)
})

ergm_ampandrana_andatsakala <- ergm(bipartite_graph_full_network_sna_list[[1]] ~
                       edges + 
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
                       b2degree(4) +
                       b2degree(5) +
                       b2degree(6),
                       control = control.ergm(seed=1234))

mcmc.diagnostics(ergm_ampandrana_andatsakala)
ergm:::plot.gof(gof(ergm_ampandrana_andatsakala))
gof(ergm_ampandrana_andatsakala)
summary(ergm_ampandrana_andatsakala)



ergm_mandena <- ergm(bipartite_graph_full_network_sna_list[[2]] ~
                       edges + 
                       b2cov("age") +
                       b2factor("gender") +
                       b2cov("commercial_goods") +
                       b2cov("house_sol") +
                       b2factor("grew_vanilla") + 
                       b2cov("land_size") +
                       b2cov("household_size") +
                       b2cov("school_level") +
                       # no different communities in mandena!
                       b2degree(2)+
                       b2degree(3)+
                       b2degree(5)+
                       b2degree(6)+
                       b2degree(7)+
                       b2degree(9)+
                       b2degree(10),
                     control = control.ergm(seed=1234))

mcmc.diagnostics(ergm_mandena)
ergm:::plot.gof(gof(ergm_mandena))
gof(ergm_mandena)
summary(ergm_mandena)

ergm_sarahandrano <- ergm(bipartite_graph_full_network_sna_list[[3]] ~
                                                  edges + 
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
                                                  b1factor("animal_community"):b2factor("grew_vanilla")+
                                                  b2degree(3)+
                                                  b2degree(4)+
                                                  b2degree(5)+
                                                  b2degree(6)+
                                                  b2degree(7)+
                                                  b2degree(8),
                          control = control.ergm(seed=1234))

mcmc.diagnostics(ergm_sarahandrano)
ergm:::plot.gof(gof(ergm_sarahandrano))

summary(ergm_sarahandrano) 


## making coef plot----
summary_df <- rbind(as.data.frame(summary(ergm_ampandrana_andatsakala)$coefficients ),
                    as.data.frame(summary(ergm_mandena)$coefficients), 
                    as.data.frame(summary(ergm_sarahandrano)$coefficients)) 

summary_df <- summary_df |> 
  mutate(term = rownames(summary_df), .before = "Estimate") |> 
  mutate(village = c(rep("Ampandrana", 18), rep("Mandena", 16), rep("Sarahandrano", 23))) |> 
  data.frame(row.names = NULL)

colnames(summary_df) <- c("term", "estimate", "standard_error", "MCMC", "z_value",
                          "p_value", "village")

summary_df<-summary_df |> 
  select(-c(MCMC, z_value)) |> 
  filter(!grepl("degree", term) & !grepl("edges", term))

summary_df$term <- c("Age", "Male Gender", "CGSOL", "HSOL", "Vanilla", "Land Size",
                     "Household Size", "School Level", "Animal Cluster 2", "Animal Cluster 2 * CGSOL",
                     "Animal Cluster 2 * HSOL", "Animal Cluster 2 * Vanilla",
                     
                     "Age", "Male Gender", "CGSOL", "HSOL", "Vanilla", "Land Size",
                     "Household Size", "School Level",
                     
                     "Age", "Male Gender", "CGSOL", "HSOL", "Vanilla", "Land Size",
                     "Household Size", "School Level", "Animal Cluster 2", "Animal Cluster 3",
                     "Animal Cluster 2 * CGSOL","Animal Cluster 3 * CGSOL",
                     "Animal Cluster 2 * HSOL", "Animal Cluster 3 * HSOL",
                     "Animal Cluster 2 * Vanilla", "Animal Cluster 3 * Vanilla")
summary_df<-summary_df |> 
  mutate(lower_ci = estimate - 1.96 * standard_error,
         upper_ci = estimate + 1.96 * standard_error) 
ggplot(summary_df, aes(x = estimate, y = term, color = village)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci),
                 height = 0.2, position = position_dodge(width = 0.5)) + 
  theme_classic() +
  labs(x = "Estimated Effect", y = "Variable", 
       title = NULL) + 
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) + 
  theme(legend.position = "right", 
            axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12))+ geom_vline(xintercept=0, 
                                                            linetype= "dashed")+
  xlim(-2.5,1)

# ggplot(summary_df, aes(x = Estimate, y = Variable, color = Village))+
#   geom_point(position = position_dodge(width = 0.5), size = 3) +
#   geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
#                  height = 0.2, position = position_dodge(width = 0.5)) + 
#   theme_classic() + 
#   labs(x = "Estimated Effect", y = "Variable", 
#                            itle = NULL) + 
#   scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) + 
#   theme(legend.position = "right",
#             axis.title.y = element_text(size = 12), 
#         axis.title.x = element_text(size = 12))+ 
#   geom_vline(xintercept=0, linetype= "dashed")
# 



