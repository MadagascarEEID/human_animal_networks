
## loading packages and files ----
library(here)
library(glmmTMB)

source(here("self_reported_networks/The_ReUp/1. Animal Interaction Bipartite Full Network.R"))
source(here("self_reported_networks/The_ReUp/3. Animal Interaction Bipartite High Risk.R"))
source("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Loading Interaction Data.R")

# FULL NETWORKS ----

### creating edge dfs----

### get all unique animals
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

### Running GLMMs ----

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

### pooling effect sizes from glmms ----


summary_df <- rbind(as.data.frame(summary(model_ampandrana)$coefficients$cond),
                    as.data.frame(summary(model_mandena)$coefficients$cond), 
                    as.data.frame(summary(model_sarahandrano)$coefficients$cond)) |> 
  rownames_to_column(var = "term") |>  # Move row names into a column named "term"
  relocate(term, .before = Estimate) |>  # Place the "term" column before "Estimate"
  dplyr::filter(!grepl("degree", term)) |> 
  mutate(Village = c(rep("Ampandrana", 17), rep("Mandena", 17), rep("Sarahandrano", 17))) 


summary_df$term <- rep(c("Edges", "Age", "Gender[Male]", "CGSOL", "HSOL",  "Vanilla Farmer",
                         "Land Size", "Household Size", "School Level", "Rodents", "Wild Animals",
                         "CGSOL:Rodents", "CGSOL:Wild Animals", "HSOL:Rodents", "HSOL:Wild Animals",
                         "Vanilla:Rodents", "Vanilla:Wild Animals"), 3)

summary_df<-summary_df |> 
  rename("Variable" = term)

library(metafor)
pool_function <- function(variable){
  
  model_df <- summary_df %>% select(Estimate, `Std. Error`, Variable, Village) %>% filter(Variable==variable)
  
  metaanalysis_model <- rma(yi = model_df$Estimate,   
                            sei = model_df$`Std. Error`, 
                            method = "FE")
  
  output <- c(metaanalysis_model$beta , metaanalysis_model$ci.lb, metaanalysis_model$ci.ub, metaanalysis_model$pval, metaanalysis_model$se, nrow(model_df))
  
  return(output)
  
}

name_age_pr <- pool_function(variable ="Age")
name_sex_pr <- pool_function(variable ="Gender[Male]")
name_vanilla_pr <- pool_function(variable ="Vanilla Farmer")
name_ls_pr <- pool_function(variable ="Land Size")
name_hs_pr <- pool_function(variable ="Household Size")
name_sl_pr <- pool_function(variable ="School Level")
name_cgsol_pr <- pool_function(variable ="CGSOL")
name_hsol_pr <- pool_function(variable ="HSOL")
name_rodents_pr <- pool_function(variable ="Rodents")
name_wild_animals_pr <- pool_function(variable ="Wild Animals")

name_hsol_rodents_pr <- pool_function(variable ="HSOL:Rodents")
name_hsol_wild_animals_pr <- pool_function(variable ="HSOL:Wild Animals")

name_vanilla_rodents_pr <- pool_function(variable ="Vanilla:Rodents")
name_vanilla_wild_animals_pr <- pool_function(variable ="Vanilla:Wild Animals")

name_cgsol_rodents_pr <- pool_function(variable ="CGSOL:Rodents")
name_cgsol_wild_animals_pr <- pool_function(variable ="CGSOL:Wild Animals")

meta_df <- as.data.frame(rbind(name_age_pr,
                               name_sex_pr,
                               name_vanilla_pr,
                               name_ls_pr,
                               name_hs_pr,
                               name_sl_pr,
                               name_cgsol_pr,
                               name_hsol_pr,
                               name_rodents_pr,
                               name_wild_animals_pr,
                               name_hsol_rodents_pr,
                               name_hsol_wild_animals_pr,
                               name_vanilla_rodents_pr,
                               name_vanilla_wild_animals_pr,
                               name_cgsol_rodents_pr,
                               name_cgsol_wild_animals_pr
))


colnames(meta_df) <- c("Estimate", "Lower", "Upper", "Pr(>|z|)","SE", "N")

meta_df$Variable <- c("Age", "Gender[Man]", "Vanilla Farmer", "Land Size",
                      "Household Size", "School Level",  "CGSOL", "HSOL", "Rodent",
                      "Wild Animal", "HSOL:Rodent","HSOL:Wild Animal",
                      "Vanilla:Rodent","Vanilla:Wild Animal",
                      "CGSOL:Rodent", "CGSOL:Wild Animal")



meta_df$lower_ci <- meta_df$Estimate - 1.96 * meta_df$SE
meta_df$upper_ci <- meta_df$Estimate + 1.96 * meta_df$SE

meta_df$lower_ci_90 <- meta_df$Estimate - qnorm(0.95) * meta_df$SE
meta_df$upper_ci_90 <- meta_df$Estimate + qnorm(0.95) * meta_df$SE

rownames(meta_df) <- NULL

meta_df <- meta_df |> 
  relocate(Variable, .before = "Estimate")



# HIGH RISK NETWORKS ----

### creating edge dfs ----

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

### running GLMMs ----

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

### pooling effect sizes from GLMMs -----

pool_function_high_risk <- function(variable){
  
  model_df <- summary_df_high_risk %>% select(Estimate, `Std. Error`, Variable, Village) %>% filter(Variable==variable)
  
  metaanalysis_model <- rma(yi = model_df$Estimate,   
                            sei = model_df$`Std. Error`, 
                            method = "FE")
  
  output <- c(metaanalysis_model$beta , metaanalysis_model$ci.lb, metaanalysis_model$ci.ub, metaanalysis_model$pval, metaanalysis_model$se, nrow(model_df))
  
  return(output)
  
}

summary_df_high_risk <- rbind(as.data.frame(summary(model_ampandrana_high_risk)$coefficients$cond ),
                              as.data.frame(summary(model_mandena_high_risk)$coefficients$cond), 
                              as.data.frame(summary(model_sarahandrano_high_risk)$coefficients$cond)) |> 
  rownames_to_column(var = "term") |>  # Move row names into a column named "term"
  relocate(term, .before = Estimate) |>  # Place the "term" column before "Estimate"
  dplyr::filter(!grepl("degree", term)) |> 
  mutate(Village = c(rep("Ampandrana", 21), rep("Mandena", 21), rep("Sarahandrano", 21))) 


summary_df_high_risk$term <- rep(c("Edges", "Age", "Gender[Male]", "CGSOL", "HSOL",  "Vanilla Farmer",
                                   "Land Size", "Household Size", "School Level", "Domesticated (Uncommon)", "Rodents", "Wild Animals",
                                   "CGSOL:Domesticated (Uncommon)","CGSOL:Rodents", "CGSOL:Wild Animals", 
                                   "HSOL:Domesticated (Uncommon)", "HSOL:Rodents", "HSOL:Wild Animals",
                                   "Vanilla:Domesticated (Uncommon)", "Vanilla:Rodents", "Vanilla:Wild Animals"), 3)

summary_df_high_risk<-summary_df_high_risk |> 
  rename("Variable" = term)


name_age_pr_high_risk <- pool_function_high_risk(variable ="Age")
name_sex_pr_high_risk <- pool_function_high_risk(variable ="Gender[Male]")
name_vanilla_pr_high_risk <- pool_function_high_risk(variable ="Vanilla Farmer")
name_ls_pr_high_risk <- pool_function_high_risk(variable ="Land Size")
name_hs_pr_high_risk <- pool_function_high_risk(variable ="Household Size")
name_sl_pr_high_risk <- pool_function_high_risk(variable ="School Level")
name_cgsol_pr_high_risk <- pool_function_high_risk(variable ="CGSOL")
name_hsol_pr_high_risk <- pool_function_high_risk(variable ="HSOL")

name_domesticated_uncommon_pr_high_risk <- pool_function_high_risk(variable = "Domesticated (Uncommon)")
name_rodents_pr_high_risk <- pool_function_high_risk(variable ="Rodents")
name_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="Wild Animals")

name_hsol_dom_uncommon_pr_high_risk <- pool_function_high_risk(variable ="HSOL:Domesticated (Uncommon)")
name_hsol_rodents_pr_high_risk <- pool_function_high_risk(variable ="HSOL:Rodents")
name_hsol_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="HSOL:Wild Animals")

name_vanilla_dom_uncommon_pr_high_risk <- pool_function_high_risk(variable ="Vanilla:Domesticated (Uncommon)")
name_vanilla_rodents_pr_high_risk <- pool_function_high_risk(variable ="Vanilla:Rodents")
name_vanilla_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="Vanilla:Wild Animals")

name_cgsol_dom_uncommon_pr_high_risk <- pool_function_high_risk(variable ="CGSOL:Domesticated (Uncommon)")
name_cgsol_rodents_pr_high_risk <- pool_function_high_risk(variable ="CGSOL:Rodents")
name_cgsol_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="CGSOL:Wild Animals")

meta_df_high_risk <- as.data.frame(rbind(name_age_pr_high_risk,
                                         name_sex_pr_high_risk,
                                         name_vanilla_pr_high_risk,
                                         name_ls_pr_high_risk,
                                         name_hs_pr_high_risk,
                                         name_sl_pr_high_risk,
                                         name_cgsol_pr_high_risk,
                                         name_hsol_pr_high_risk,
                                         name_domesticated_uncommon_pr_high_risk,
                                         name_rodents_pr_high_risk,
                                         name_wild_animals_pr_high_risk,
                                         name_hsol_dom_uncommon_pr_high_risk,
                                         name_hsol_rodents_pr_high_risk,
                                         name_hsol_wild_animals_pr_high_risk,
                                         name_vanilla_dom_uncommon_pr_high_risk,
                                         name_vanilla_rodents_pr_high_risk,
                                         name_vanilla_wild_animals_pr_high_risk,
                                         name_cgsol_dom_uncommon_pr_high_risk,
                                         name_cgsol_rodents_pr_high_risk,
                                         name_cgsol_wild_animals_pr_high_risk
))


colnames(meta_df_high_risk) <- c("Estimate", "Lower", "Upper", "Pr(>|z|)","SE", "N")

meta_df_high_risk$Variable <- c("Age", "Gender[Man]", "Vanilla Farmer", "Land Size",
                                "Household Size", "School Level",  "CGSOL", "HSOL", "Domesticated (Uncommon)",
                                "Rodent", "Wild Animal",  "HSOL:Domesticated (Uncommon)", "HSOL:Rodent","HSOL:Wild Animal",
                                "Vanilla:Domesticated (Uncommon)", "Vanilla:Rodent","Vanilla:Wild Animal",
                                "CGSOL:Domesticated (Uncommon)", "CGSOL:Rodent", "CGSOL:Wild Animal")



meta_df_high_risk$lower_ci <- meta_df_high_risk$Estimate - 1.96 * meta_df_high_risk$SE
meta_df_high_risk$upper_ci <- meta_df_high_risk$Estimate + 1.96 * meta_df_high_risk$SE

meta_df_high_risk$lower_ci_90 <- meta_df_high_risk$Estimate - qnorm(0.95) * meta_df_high_risk$SE
meta_df_high_risk$upper_ci_90 <- meta_df_high_risk$Estimate + qnorm(0.95) * meta_df_high_risk$SE

rownames(meta_df_high_risk) <- NULL

meta_df_high_risk <- meta_df_high_risk |> 
  relocate(Variable, .before = "Estimate")


# Loading ERMG results ----

load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_m.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_s.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_a.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_m.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_s.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_a.RData")



## comparing ERGM vs GLMM estimates ----
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

## AUC ----
library(ROCR)
library(sna)

compute_auc_ergm <- function(ergm_model, nsim = 500) { # simulating 500 networks
  sim_nets <- simulate(ergm_model, nsim = nsim, output = "network", verbose = FALSE)
  obs_mat <- as.matrix.network(ergm_model$network)
  pred_probs <- Reduce("+", lapply(sim_nets, function(net) as.matrix.network(net))) / nsim
  
  # Flatten to vectors
  obs_vec <- as.vector(obs_mat)
  pred_vec <- as.vector(pred_probs)
  
  # Remove self-loops
  n <- nrow(obs_mat)
  diag_index <- seq(1, length(obs_vec), by = n + 1)
  obs_vec <- obs_vec[-diag_index]
  pred_vec <- pred_vec[-diag_index]
  
  pred <- prediction(pred_vec, obs_vec)
  perf <- performance(pred, "auc")
  return(perf@y.values[[1]])
}

compute_auc_glmm <- function(glmm_model) {
  pred_probs <- predict(glmm_model, type = "response", re.form = NULL)
  observed <- glmm_model$frame$edge
  pred <- prediction(pred_probs, observed)
  perf <- performance(pred, "auc")
  return(perf@y.values[[1]])
}


auc_results <- data.frame(
  Site = character(),
  AUC_ERGM = numeric(),
  AUC_GLMM = numeric(),
  stringsAsFactors = FALSE
)

# List of village names and models
site_models <- list(
  "Ampandrana (Full Network)" = list(glmm = model_ampandrana, ergm = ergm_ampandrana_andatsakala),
  "Mandena (Full Network)" = list(glmm = model_mandena, ergm = ergm_mandena),
  "Sarahandrano (Full Network)" = list(glmm = model_sarahandrano, ergm = ergm_sarahandrano),
  "Ampandrana (High Risk)" = list(glmm = model_ampandrana_high_risk, ergm = ergm_ampandrana_andatsakala_high_risk),
  "Mandena (High Risk)" = list(glmm = model_mandena_high_risk, ergm = ergm_mandena_high_risk_lemurs),
  "Sarahandrano (High Risk)" = list(glmm = model_sarahandrano_high_risk, ergm = ergm_sarahandrano_high_risk)
)


auc_results <- data.frame(
  Village = character(),
  AUC_ERGM = numeric(),
  AUC_GLMM = numeric(),
  stringsAsFactors = FALSE
)

# List of village names and models
village_models <- list(
  "Ampandrana (Full Network)" = list(glmm = model_ampandrana, ergm = ergm_ampandrana_andatsakala),
  "Mandena (Full Network)" = list(glmm = model_mandena, ergm = ergm_mandena),
  "Sarahandrano (Full Network)" = list(glmm = model_sarahandrano, ergm = ergm_sarahandrano),
  "Ampandrana (High Risk)" = list(glmm = model_ampandrana_high_risk, ergm = ergm_ampandrana_andatsakala_high_risk),
  "Mandena (High Risk)" = list(glmm = model_mandena_high_risk, ergm = ergm_mandena_high_risk_lemurs),
  "Sarahandrano (High Risk)" = list(glmm = model_sarahandrano_high_risk, ergm = ergm_sarahandrano_high_risk)
)

# Loop through each village
for (village in names(village_models)) {
  cat("Processing:", village, "\n")
  ergm_model <- village_models[[village]]$ergm
  glmm_model <- village_models[[village]]$glmm
  
  auc_ergm <- compute_auc_ergm(ergm_model)
  auc_glmm <- compute_auc_glmm(glmm_model)
  
  auc_results <- rbind(auc_results, data.frame(
    Village = village,
    AUC_ERGM = auc_ergm,
    AUC_GLMM = auc_glmm
  ))
}

View(auc_results)