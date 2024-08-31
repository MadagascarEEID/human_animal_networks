library(here)
source(here("Scripts/1. Animal Interaction Bipartite.R"))
library(statnet)
library(broom)
library(kableExtra)
set.seed(1234)
# Convert igraph objects to network objects
graph_names <- c("bipartite_graph", "bipartite_graph_village1", "bipartite_graph_village2", "bipartite_graph_village3")

bipartite_graph_sna <- network(as_biadjacency_matrix(bipartite_graph))

bipartite_graph_sna_list <- lapply(graph_names, function(graph_names) {
  network(as_biadjacency_matrix(get(graph_names)))
})

# Export node level variables from igraph into standalone data frames for each village
node_df_list <- lapply(graph_names, function(village_name) {
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
detach()
# Reorder node_df to match order of IDs in statnet for each village
reorder_node_df <- function(node_df, statnet_order) {
  node_df[match(statnet_order, node_df$id), ]
}
node_df_list <- Map(reorder_node_df, node_df_list, lapply(bipartite_graph_sna_list, function(graph) as.character(graph %v% "vertex.names")))
detach("package:igraph", unload = TRUE)

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
bipartite_graph_sna_list <- Map(update_network_attributes, bipartite_graph_sna_list, node_df_list)

# Remove missing values from each network object
bipartite_graph_sna_list <- lapply(bipartite_graph_sna_list, function(graph) {
  vertex_attrs <- c("house_sol", "commercial_goods", "age", "village", "gender", "degree", "school_level", "animal_category", "animal_name")
  vertices_with_na <- integer(0)
  for (attr in vertex_attrs) {
    vertices_with_na <- union(vertices_with_na, which(is.na(graph %v% attr)))
  }
  vertices_with_na <- unique(vertices_with_na)
  vertices_with_na <- setdiff(vertices_with_na, 1:11)
  print(vertices_with_na)
  delete.vertices(graph, vertices_with_na)
})

# Run ERGMs for each village
p1_ergm_list <- lapply(bipartite_graph_sna_list, function(graph) {
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

# mcmc_ergm_list <- lapply(bipartite_graph_sna_list[[1]], function(graph) {
  
  
# mcmc_ergm1<-ergm(bipartite_graph_sna_list[[1]] ~ edges +
#          b2factor("gender") +
#          b2factor("village") +
#          b2cov("age") +
#          b2cov("commercial_goods") +
#          b2cov("house_sol") +
#          b2cov("school_level") +
#          b1factor("animal_category") +
#          b1factor("animal_category"):b2cov("commercial_goods"):b2cov("house_sol") +
#          b2degree(2)+
#          b2degree(3)+
#          b2degree(4)+
#          b2degree(5)+
#          b2degree(6)+
#          b2degree(7)+
#          b2degree(8)+
#          b2degree(9)+
#          b2degree(10),
#        control = control.ergm(parallel = 4,seed=1234),
#        verbose = TRUE)
# 
# mcmc_ergm2<-ergm(bipartite_graph_sna_list[[1]] ~ edges +
#                    b2factor("gender") +
#                    b2factor("village") +
#                    b2cov("age") +
#                    b2cov("commercial_goods") +
#                    b2cov("house_sol") +
#                    b2cov("school_level") +
#                    b1factor("animal_category") +
#                    b1factor("animal_category"):b2cov("commercial_goods"):b2cov("house_sol") +
#                    gwb2degree,
#                    # b2degree(2)+
#                    # b2degree(3)+
#                    # b2degree(4)+
#                    # b2degree(5)+
#                    # b2degree(6)+
#                    # b2degree(7)+
#                    # b2degree(8)+
#                    # b2degree(9)+
#                    # b2degree(10)+
#                    # gwb2dsp(decay=0.7, fixed = TRUE),
#                  control = control.ergm(parallel = 4,seed=1234),
#                  verbose = TRUE)

mcmc_ergm3<-ergm(bipartite_graph_sna_list[[1]] ~ edges + #winner
                   b2factor("gender") +
                   b2factor("village") +
                   b2cov("age") +
                   b2cov("commercial_goods") +
                   b2cov("house_sol") +
                   b2cov("school_level") +
                   b1factor("animal_category") +
                   b1factor("animal_category"):b2cov("commercial_goods"):b2cov("house_sol") +
                   gwb2degree+
                   # gwb1degree(cutoff = 1500),
                   # gwb2dsp+
                 # b2degree(2)+
                 # b2degree(3)+
                 # b2degree(4)+
                 b2degree(5)+
                 b2degree(6),
                 # b2degree(7)+
                 # b2degree(8)+
                 # b2degree(9)+
                 # b2degree(10)+
                 # ,
                 control = control.ergm(parallel = 4,seed=1234),
                 verbose = TRUE)

# mcmc_ergm4<-ergm(bipartite_graph_sna_list[[1]] ~ edges +
#                    b2factor("gender") +
#                    b2factor("village") +
#                    b2cov("age") +
#                    b2cov("commercial_goods") +
#                    b2cov("house_sol") +
#                    b2cov("school_level") +
#                    b1factor("animal_category") +
#                    b1factor("animal_category"):b2cov("commercial_goods"):b2cov("house_sol") +
#                    gwb2degree+
#                    b2degree(2)+
#                    b2degree(3)+
#                    b2degree(5)+
#                    b2degree(6)+
#                    b1degree(45),
#                  control = control.ergm(parallel = 4, seed=1234),
#                  verbose = TRUE)

exp(.303)
exp(-0.04)
summary(mcmc_ergm3)
exp(confint(mcmc_ergm3))
exp(-1.888)
# 
# # Summarize ERGM results
# for (i in seq_along(p1_ergm_list)) {
#   print(summary(p1_ergm_list[[i]]))
# }
# 
# for (i in seq_along(mcmc_ergm_list)) {
#   print(summary(mcmc_ergm_list[[i]]))
# }

summary(mcmc_ergm1)
summary(mcmc_ergm2)
library(clipr)
tidy(mcmc_ergm3) |> 
  write_clip()

par(mfrow = c(3,2))
gof_mcmc1<-gof(mcmc_ergm1)
plot(gof_mcmc1)

gof_mcmc2<-gof(mcmc_ergm2)
plot(gof_mcmc2)

gof_mcmc3<-ergm::gof(mcmc_ergm3)
plot(gof_mcmc3)

gof_mcmc4<-gof(mcmc_ergm4)
plot(gof_mcmc4)

# Perform AIC and BIC comparisons
AIC(mcmc_ergm1, mcmc_ergm2, mcmc_ergm3)

AIC_values <- sapply(p1_ergm_list,AIC)


