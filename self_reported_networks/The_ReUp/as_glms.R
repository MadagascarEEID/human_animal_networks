library(here)
library(glmmTMB)

source(here("self_reported_networks/The_ReUp/1. Animal Interaction Bipartite Full Network.R"))
source(here("self_reported_networks/The_ReUp/3. Animal Interaction Bipartite High Risk.R"))
source("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Loading Interaction Data.R")

# get all unique animals
animals <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs", "goats_sheep",
             "cows", "wild_birds", "bush_pigs", "lemurs", "tenrecs", "carnivores")

# Get all unique humans from merged survey df
humans <- unique(merged_df$social_netid)

# Create all possible combinations of humans and animals
edge_df <- expand.grid(from = humans, to = animals, stringsAsFactors = FALSE)
edge_df<- edge_df |> 
  arrange(from)

# Initialize edge column to 0
edge_df$edge <- 0

# Loop over each animal type
for (animal in animals) {

    animal_cols <- colnames(merged_df)[grepl(animal, colnames(merged_df))]
  
  if (length(animal_cols) > 0) {
    # Check if any contact occurred for each person
    merged_df[[paste0("has_", animal, "_contact")]] <- rowSums(merged_df[animal_cols], na.rm = TRUE) > 0
    
    # Update the edge_df accordingly
    edge_df$edge[edge_df$to == animal & 
                   edge_df$from %in% merged_df$social_netid[merged_df[[paste0("has_", animal, "_contact")]]]] <- 1
  }
}

# adding in animal_category
domesticated_animals <- c("poultry", "dogs", "domestic_pigs", "cows", "goats_sheep", "cats")
rodent_animals <- "rodents"
wild_animals <- c("bush_pigs", "carnivores", "tenrecs", "wild_birds", "lemurs")

edge_df<-edge_df |> 
  mutate(animal_category = ifelse(to %in% domesticated_animals, "domesticated",
                                  ifelse(to %in% rodent_animals, "rodent",
                                         ifelse(to%in%wild_animals, "wild", NA))))

# bring in human survey data
bipartite_graph_full_network_meta_data<-igraph::as_data_frame(bipartite_graph_full_network ,
                                                          what = "vertices") |> 
  rename(from = name)

# merge with edge list
full_network_df<-merge(edge_df,bipartite_graph_full_network_meta_data,
                                          by = "from")

## break down by village
full_network_df_mandena<-full_network_df |> 
  filter(village == "Mandena")

full_network_df_sarahandrano<-full_network_df |> 
  filter(village == "Sarahandrano")

full_network_df_ampandrana<-full_network_df |> 
  filter(grepl("Ampand|Andats", village))

model_mandena <- glmmTMB(edge ~
                          age+
                          gender+
                          commercial_goods+
                          house_sol+
                          grew_vanilla+
                          land_size+
                          household_size+
                          school_level+
                          animal_category+
                          animal_category:commercial_goods+
                          animal_category:house_sol+
                          animal_category:grew_vanilla+
                         (1|from),
                        data = full_network_df_mandena,
                        family = "binomial") 
summary(model_mandena)

model_sarahandrano<- glmmTMB(edge ~
                          age+
                          gender+
                          commercial_goods+
                          house_sol+
                          grew_vanilla+
                          land_size+
                         household_size+
                          school_level+
                          animal_category+
                          animal_category:commercial_goods+
                          animal_category:house_sol+
                          animal_category:grew_vanilla+
                           (1|from),
                        data = full_network_df_sarahandrano,
                        family = "binomial")

model_ampandrana <- glmmTMB(edge ~
                         age+
                          gender+
                          commercial_goods+
                          house_sol+
                          grew_vanilla+
                          land_size+
                          household_size+
                          school_level+
                          animal_category+
                          animal_category:commercial_goods+
                          animal_category:house_sol+
                          animal_category:grew_vanilla+ (1|from),
                        data = full_network_df_ampandrana,
                        family = "binomial")
AIC(model_ampandrana)
AIC(model_sarahandrano)
AIC(model_mandena)

# HIGH RISK NETWORKS ----

# get all unique animals
animals <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs", "goats_sheep",
             "cows", "wild_birds", "bush_pigs", "lemurs", "tenrecs", "carnivores")

# Get all unique humans from merged survey df
humans <- unique(merged_df$social_netid)

# Create all possible combinations of humans and animals
edge_df_high_risk <- expand.grid(from = humans, to = animals, stringsAsFactors = FALSE)
edge_df_high_risk<- edge_df_high_risk |> 
  arrange(from)

# Initialize edge column to 0
edge_df_high_risk$edge <- 0

# Loop over each animal type
for (animal in animals) {
  
  animal_cols <- colnames(merged_df)[grepl(animal, colnames(merged_df))&grepl("feces|shared_water|scratched|cooked|raw|sick|slaughtered", 
                                                                              colnames(merged_df))]
  
  if (length(animal_cols) > 0) {
    # Check if any contact occurred for each person
    merged_df[[paste0("has_", animal, "_contact")]] <- rowSums(merged_df[animal_cols], na.rm = TRUE) > 0
    
    # Update the edge_df accordingly
    edge_df_high_risk$edge[edge_df_high_risk$to == animal & 
                             edge_df_high_risk$from %in% merged_df$social_netid[merged_df[[paste0("has_", animal, "_contact")]]]] <- 1
  }
}



# adding in animal_category
domesticated_animals_1 <- c("poultry", "domestic_pigs", "cows")
domesticated_animals_2 <- c( "dogs","goats_sheep", "cats")
rodent_animals <- "rodents"
wild_animals <- c("bush_pigs", "carnivores", "tenrecs", "wild_birds", "lemurs")

edge_df_high_risk<-edge_df_high_risk |> 
  mutate(animal_category = ifelse(to %in% domesticated_animals_1, "domesticated_1",
                                  ifelse(to %in% domesticated_animals_2, "domesticated_2",
                                         ifelse(to%in%rodent_animals, "rodent", 
                                                ifelse(to%in%wild_animals, "wild", NA)))))

# bring in human survey data
bipartite_graph_high_risk_network_meta_data<-igraph::as_data_frame(bipartite_graph_high_risk ,
                                                              what = "vertices") |> 
  rename(from = name)

# merge with edge list
high_risk_network_df<-merge(edge_df_high_risk,bipartite_graph_high_risk_network_meta_data,
                       by = "from")

## break down by village
high_risk_network_df_mandena<-high_risk_network_df |> 
  filter(village == "Mandena") 

high_risk_network_df_sarahandrano<-high_risk_network_df |> 
  filter(village == "Sarahandrano")

high_risk_network_df_ampandrana<-high_risk_network_df |> 
  filter(grepl("Ampand|Andats", village))

## running models

model_mandena_high_risk <- glmmTMB(edge ~
                       age+
                       gender+
                       commercial_goods+
                       house_sol+
                       grew_vanilla+
                       land_size+
                       household_size+
                       school_level+
                       animal_category+
                       animal_category:commercial_goods+
                       animal_category:house_sol+
                       animal_category:grew_vanilla+
                         (1|from),
                     data = high_risk_network_df_mandena,
                     family = "binomial")

summary(model_mandena_high_risk)

model_sarahandrano_high_risk<- glmmTMB(edge ~
                           age+
                           gender+
                           commercial_goods+
                           house_sol+
                           grew_vanilla+
                           land_size+
                           household_size+
                           school_level+
                           animal_category+
                           animal_category:commercial_goods+
                           animal_category:house_sol+
                           animal_category:grew_vanilla+
                             (1|from),
                         data = high_risk_network_df_sarahandrano,
                         family = "binomial")

summary(model_sarahandrano_high_risk)

model_ampandrana_high_risk <- glmmTMB(edge ~
                          age+
                          gender+
                          commercial_goods+
                          house_sol+
                          grew_vanilla+
                          land_size+
                          household_size+
                          school_level+
                          animal_category+
                          animal_category:commercial_goods+
                          animal_category:house_sol+
                          animal_category:grew_vanilla+
                            (1|from),
                        data = high_risk_network_df_ampandrana,
                        family = "binomial")

# 
# 
# AIC(model_ampandrana_high_risk)
# AIC(model_sarahandrano_high_risk)
# AIC(model_mandena_high_risk)


## comparing AICs to ERGMS ----
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_ampandrana_andatsakala.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_mandena.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_sarahandrano.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_ampandrana_andatsakala_high_risk2.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_mandena_high_risk3.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_sarahandrano_high_risk2.RData")
# 
# AIC(model_ampandrana, ergm_ampandrana_andatsakala)
# AIC(model_mandena, ergm_mandena) # weird
# AIC(model_sarahandrano, ergm_sarahandrano)
# 
# 
# AIC(model_ampandrana_high_risk, ergm_ampandrana_andatsakala_high_risk) 
# AIC(model_mandena_high_risk, ergm_mandena_high_risk_lemurs)
# AIC(model_sarahandrano_high_risk, ergm_sarahandrano_high_risk)


## comparing model results...ERGM vs GLMM ----
summary(model_ampandrana)
summary(ergm_ampandrana_andatsakala)

summary(model_mandena)
summary(ergm_mandena)

summary(model_sarahandrano)
summary(ergm_sarahandrano)

summary(model_ampandrana_high_risk)
summary(ergm_ampandrana_andatsakala_high_risk)

summary(model_mandena_high_risk)
summary(ergm_mandena_high_risk_lemurs)

summary(model_sarahandrano_high_risk)
summary(ergm_sarahandrano_high_risk)

library(ROCR)

# Simulate networks from the ERGM
sim_nets <- simulate(ergm_sarahandrano, nsim = 1000, output = "network")

# Get the observed adjacency matrix
obs_mat <- as.matrix.network(ergm_sarahandrano$network)

# Get predicted probabilities (average across simulations)
predicted_probs <- Reduce("+", lapply(sim_nets, function(net) as.matrix.network(net))) / length(sim_nets)

# Flatten matrices to vectors
obs_vec <- as.vector(obs_mat)
pred_vec <- as.vector(predicted_probs)

# Remove self-loops if applicable
n <- nrow(obs_mat)
diag_index <- seq(1, length(obs_vec), by = n + 1)
obs_vec <- obs_vec[-diag_index]
pred_vec <- pred_vec[-diag_index]

# Use ROCR to compute AUC
pred <- prediction(pred_vec, obs_vec)
perf <- performance(pred, "auc")
auc_value <- perf@y.values[[1]]
auc_value

# auc for glmm
pred_probs <- predict(model_sarahandrano, type = "response", re.form = NULL)

# Observed binary outcome
observed <- model_sarahandrano$frame$edge

# Create ROCR prediction object
pred <- prediction(pred_probs, observed)
perf <- performance(pred, "auc")
auc_value <- perf@y.values[[1]]
auc_value[[1]]

