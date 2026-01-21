## loading packages and files ----
library(here)
library(ineq)

source(here("self_reported_networks/The_ReUp/1. Animal Interaction Bipartite Full Network.R"))
source(here("self_reported_networks/The_ReUp/3. Animal Interaction Bipartite High Risk.R"))

# LOADING DFs----
library(tidyverse)
demographic_health_data <- read.csv("/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/NIH Human Surveys/Survey_Demographic_Health.csv")
animal_interaction_data <- read.csv("/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/NIH Human Surveys/Survey_Animal_Interaction.csv")


# cleaning data, selecting only columns that I want, converting binaries to 0 and 1
demographic_health_data_cleaned <- demographic_health_data |> 
  mutate(across(starts_with(c("own_", "banking", "grew_crops",
                              "crops")),
                ~ifelse(. == "Yes", 1L, 0L))) |> 
  select(survey_date, social_netid, village,age,gender,school_level, household_size, main_activity, 
         starts_with("material_"), starts_with("own_"),crops_vanilla, landsize_in_daba)


animal_interaction_data_cleaned <- animal_interaction_data |>  
  mutate(across(-starts_with("social_netid"), ~ifelse(. == "Yes", 1L, 0L))) |> 
  select(-starts_with("symptoms"),-starts_with("action_"), -contains("frequency"))


merged_df <- inner_join(demographic_health_data_cleaned, animal_interaction_data_cleaned, by = "social_netid")

merged_df <- suppressWarnings(merged_df |> 
                                        mutate(material_wall_index = 0) %>%
                                        mutate(material_wall_index = if_else(material_wall == "bamboo", 0, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "rafia", 0, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "ravenala", 0, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "mud", 0, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "compacted_earth", 0, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "wood_planks", 1, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "brick_unfired", 2, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "brick_fired", 2, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "metal_sheets", 3, material_wall_index)) %>%
                                        mutate(material_wall_index = if_else(material_wall == "cement", 4, material_wall_index)) %>%
                                        mutate(material_wall_index = scale(material_wall_index)) |> 
                                        
                                        # roof construction
                                        mutate(material_roof_index = 0) %>%
                                        mutate(material_roof_index = if_else(material_roof == "bamboo", 0, material_roof_index)) %>%
                                        mutate(material_roof_index = if_else(material_roof == "thatch", 0, material_roof_index)) %>%
                                        mutate(material_roof_index = if_else(material_roof == "metal_sheets", 1, material_roof_index)) %>%
                                        mutate(material_roof_index = if_else(material_roof == "cement", 2, material_roof_index)) %>%
                                        mutate(material_roof_index = scale(material_roof_index)) |> 
                                        
                                        # floor construction
                                        mutate(material_floor_index = 0) %>%
                                        mutate(material_floor_index = if_else(material_floor == "dirt", 0, material_floor_index)) %>%
                                        mutate(material_floor_index = if_else(material_floor == "bamboo", 0, material_floor_index)) %>%
                                        mutate(material_floor_index = if_else(material_floor == "rafia", 0, material_floor_index)) %>%
                                        mutate(material_floor_index = if_else(material_floor == "ravinala", 0, material_floor_index)) %>%
                                        mutate(material_floor_index = if_else(material_floor == "wood_planks", 1, material_floor_index)) %>%
                                        mutate(material_floor_index = if_else(material_floor == "cement", 2, material_floor_index)) %>%
                                        mutate(material_floor_index = scale(material_floor_index)) |> 
                                        
                                        # house index
                                        mutate(house_sol = material_wall_index + material_roof_index + material_floor_index) |> 
                                        mutate(house_sol = scale(house_sol)) |> 
                                        
                                        # commercial goods
                                        mutate(commercial_goods = own_cellphone + own_tv + own_bicycle + own_refrigerator +
                                                 own_motorcycle + own_computer + own_generator) |> 
                                        mutate(commercial_goods = scale(commercial_goods)) |> 
                                        
                                        # other things
                                        mutate(employment_category = if_else(grepl("farm_crops", main_activity) | grepl("farm_mixed", main_activity), "Farmer", 
                                                                             ifelse(grepl("student", main_activity) | grepl("teacher", main_activity), "Education",
                                                                                    ifelse(grepl("unemployed", main_activity) | grepl("retired_teacher", main_activity), "Unemployed",
                                                                                           "Other")))) |> 
                                        mutate(school_level_numbered = ifelse(school_level == "None", 0L, 
                                                                              ifelse(school_level == "Primary", 1L,
                                                                                     ifelse(school_level == "Secondary", 2L,
                                                                                            ifelse(school_level == "Higher", 3L,
                                                                                                   NA))))) |> 
                                        rename(grew_vanilla = crops_vanilla) |> 
                                        mutate(age = scale(age)) |> 
                                        mutate(landsize_in_daba = scale(landsize_in_daba)) |> 
                                        mutate(household_size = scale(household_size)) |> 
                                        mutate(school_level_numbered = scale(school_level_numbered))) |> 
  drop_na(house_sol, commercial_goods, grew_vanilla, age, landsize_in_daba, household_size, school_level_numbered, gender)
                                
merged_df$survey_date <- as.Date(merged_df$survey_date)


n_respondents<-length(unique(merged_df$social_netid))
n_respondents




# FULL NETWORKS ----

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


### Only X% of Individuals Contribute 50% of All INTERACTIONS” ----
full_network_df |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(desc(total_edges)) |> 
  mutate(
    cum_people = row_number() / n(),
    cum_edges  = cumsum(total_edges) / sum(total_edges)
  ) |> 
  filter(cum_edges >= 0.50) |> 
  slice(1) |> 
  summarise(
    percent_of_people_for_half_interactions = 100 * cum_people
  )

### average number interactions per person ----
full_network_df |> 
  filter(!is.na(edge)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    .groups = "drop"
  ) |> 
  summarise(
    average_interactions_per_person = mean(total_edges),
    sd_interactions = sd(total_edges),
    n_people = n()
  )

### LAND SIZE DIFFERENCES ----
landsize_quintile_edges <- full_network_df |> 
  filter(!is.na(edge), !is.na(land_size)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    land_size = first(land_size),
    .groups = "drop"
  ) |> 
  mutate(rank = percent_rank(land_size),
         quintile = case_when(
           rank <= 0.20 ~ "bottom_20",
           rank >= 0.80 ~ "top_20",
           TRUE ~ NA
         )) |> 
  filter(!is.na(quintile))

t.test(total_edges ~ quintile, data = landsize_quintile_edges)


### COMMERCIAL GOODS DIFFERENCES ----
commercial_goods_quintile_edges <- full_network_df |> 
  filter(!is.na(edge), !is.na(commercial_goods)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    commercial_goods = first(commercial_goods),
    .groups = "drop"
  ) |> 
  mutate(rank = percent_rank(commercial_goods),
         quintile = case_when(
           rank <= 0.20 ~ "bottom_20",
           rank >= 0.80 ~ "top_20",
           TRUE ~ NA
         )) |> 
  filter(!is.na(quintile))

t.test(total_edges ~ quintile, data = commercial_goods_quintile_edges)


### HOUSE MATERIALS DIFFERENCES ----
house_mat_quintile_edges <- full_network_df |> 
  filter(!is.na(edge), !is.na(house_sol)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    house_sol = first(house_sol),
    .groups = "drop"
  ) |> 
  mutate(rank = percent_rank(house_sol),
         quintile = case_when(
           rank <= 0.20 ~ "bottom_20",
           rank >= 0.80 ~ "top_20",
           TRUE ~ NA
         )) |> 
  filter(!is.na(quintile))

t.test(total_edges ~ quintile, data = house_mat_quintile_edges)


### VANILLA DIFFERNCES----

## t test vanilla

edge_summary_vanilla <- full_network_df |> 
  filter(!is.na(edge), !is.na(grew_vanilla)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    grew_vanilla = first(grew_vanilla),
    .groups = "drop"
  )


t.test(total_edges ~ grew_vanilla, data = edge_summary_vanilla)


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
 


### Only X% of Individuals Contribute 50% of All INTERACTIONS” ----
high_risk_network_df |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(desc(total_edges)) |> 
  mutate(
    cum_people = row_number() / n(),
    cum_edges  = cumsum(total_edges) / sum(total_edges)
  ) |> 
  filter(cum_edges >= 0.50) |> 
  slice(1) |> 
  summarise(
    percent_of_people_for_half_interactions = 100 * cum_people
  )

### average number interactions per person ----
high_risk_network_df |> 
  filter(!is.na(edge)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    .groups = "drop"
  ) |> 
  summarise(
    average_interactions_per_person = mean(total_edges),
    sd_interactions = sd(total_edges),
    n_people = n()
  )

high_risk_network_df |> 
  filter(!is.na(edge)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    .groups = "drop"
  ) |> 
  summarise(
    average_interactions_per_person = mean(total_edges),
    sd_interactions = sd(total_edges),
    n_people = n()
  )

### LAND SIZE DIFFERENCES ----
landsize_quintile_edges <- high_risk_network_df |> 
  filter(!is.na(edge), !is.na(land_size)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    land_size = first(land_size),
    .groups = "drop"
  ) |> 
  mutate(rank = percent_rank(land_size),
         quintile = case_when(
           rank <= 0.20 ~ "bottom_20",
           rank >= 0.80 ~ "top_20",
           TRUE ~ NA
         )) |> 
  filter(!is.na(quintile))

t.test(total_edges ~ quintile, data = landsize_quintile_edges)


### COMMERCIAL GOODS DIFFERENCES ----
commercial_goods_quintile_edges <- high_risk_network_df |> 
  filter(!is.na(edge), !is.na(commercial_goods)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    commercial_goods = first(commercial_goods),
    .groups = "drop"
  ) |> 
  mutate(rank = percent_rank(commercial_goods),
         quintile = case_when(
           rank <= 0.20 ~ "bottom_20",
           rank >= 0.80 ~ "top_20",
           TRUE ~ NA
         )) |> 
  filter(!is.na(quintile))

t.test(total_edges ~ quintile, data = commercial_goods_quintile_edges)


### HOUSE MATERIALS DIFFERENCES ----
house_mat_quintile_edges <- high_risk_network_df |> 
  filter(!is.na(edge), !is.na(house_sol)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    house_sol = first(house_sol),
    .groups = "drop"
  ) |> 
  mutate(rank = percent_rank(house_sol),
         quintile = case_when(
           rank <= 0.20 ~ "bottom_20",
           rank >= 0.80 ~ "top_20",
           TRUE ~ NA
         )) |> 
  filter(!is.na(quintile))

t.test(total_edges ~ quintile, data = house_mat_quintile_edges)


### VANILLA DIFFERNCES----

## t test vanilla

edge_summary_vanilla <- high_risk_network_df |> 
  filter(!is.na(edge), !is.na(grew_vanilla)) |> 
  group_by(from) |> 
  summarise(
    total_edges = sum(edge == 1),
    grew_vanilla = first(grew_vanilla),
    .groups = "drop"
  )

t.test(total_edges ~ grew_vanilla, data = edge_summary_vanilla)












# 
# wealth_concentration <- function(df, wealth_var, top_prop = 0.2) {
#   
#   df |> 
#     filter(!is.na(.data[[wealth_var]])) |> 
#     arrange(desc(.data[[wealth_var]])) |> 
#     mutate(
#       cum_people = row_number() / n(),
#       cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#     ) |> 
#     filter(cum_people <= top_prop) |> 
#     summarise(
#       top_people_percent = 100 * max(cum_people),
#       percent_of_risk = 100 * max(cum_risk)
#     )
# }
# 
# wealth_concentration(individual_risk, "house_sol", 0.2)
# 
# wealth_concentration(individual_risk, "commercial_goods", 0.2)
# 
# wealth_concentration(individual_risk, "land_size", 0.2)
# 
# individual_risk |> 
#   filter(!is.na(grew_vanilla)) |> 
#   summarise(
#     percent_of_people_vanilla = 100 * mean(grew_vanilla == 1),
#     percent_of_risk_from_vanilla = 
#       100 * sum(total_high_risk[grew_vanilla == 1]) / sum(total_high_risk)
#   )
# 
# # Only X% of Individuals Contribute 50% of All High-Risk Exposures”
# individual_risk |> 
#   arrange(desc(total_high_risk)) |> 
#   mutate(
#     cum_people = row_number() / n(),
#     cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#   ) |> 
#   filter(cum_risk >= 0.50) |> 
#   slice(1) |> 
#   summarise(
#     percent_of_people_for_half_risk = 100 * cum_people
#   )
# 
# 
# # Bottom vs Top Wealth Strata Comparison (Clean Table)
# individual_risk |> 
#   filter(!is.na(house_sol)) |> 
#   mutate(
#     wealth_tertile = ntile(house_sol, 3)
#   ) |> 
#   group_by(wealth_tertile) |> 
#   summarise(
#     n_people = n(),
#     total_high_risk = sum(total_high_risk),
#     .groups = "drop"
#   ) |> 
#   mutate(
#     percent_of_all_risk = 
#       100 * total_high_risk / sum(total_high_risk)
#   )
# 
# 
# lorenz_overall <- individual_risk |> 
#   arrange(total_high_risk) |> 
#   mutate(
#     cum_people = row_number() / n(),
#     cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#   )
# 
# ggplot(lorenz_overall, aes(x = cum_people, y = cum_risk)) +
#   geom_line(linewidth = 1.2) +
#   geom_abline(linetype = "dashed") +
#   labs(
#     x = "Cumulative Proportion of Individuals",
#     y = "Cumulative Proportion of High-Risk Exposures",
#     title = "Lorenz Curve of High-Risk Exposure Inequality"
#   ) +
#   theme_minimal()
# 
# lorenz_house <- individual_risk |> 
#   filter(!is.na(house_sol)) |> 
#   mutate(
#     wealth_tertile = ntile(house_sol, 3)
#   ) |> 
#   group_by(wealth_tertile) |> 
#   arrange(total_high_risk, .by_group = TRUE) |> 
#   mutate(
#     cum_people = row_number() / n(),
#     cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#   ) |> 
#   ungroup()
# 
# ggplot(lorenz_house, aes(x = cum_people, y = cum_risk, group = wealth_tertile)) +
#   geom_line(linewidth = 1.2) +
#   geom_abline(linetype = "dashed") +
#   labs(
#     x = "Cumulative Proportion of Individuals",
#     y = "Cumulative Proportion of High-Risk Exposures",
#     title = "Lorenz Curves of High-Risk Exposure by House-Quality Tertile",
#     subtitle = "Dashed line = perfect equality"
#   ) +
#   theme_minimal()
# 
# 
# lorenz_commercial_goods <- individual_risk |> 
#   filter(!is.na(commercial_goods)) |> 
#   mutate(
#     wealth_tertile = ntile(commercial_goods, 3)
#   ) |> 
#   group_by(wealth_tertile) |> 
#   arrange(total_high_risk, .by_group = TRUE) |> 
#   mutate(
#     cum_people = row_number() / n(),
#     cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#   ) |> 
#   ungroup()
# 
# ggplot(lorenz_house, aes(x = cum_people, y = cum_risk, group = wealth_tertile)) +
#   geom_line(linewidth = 1.2) +
#   geom_abline(linetype = "dashed") +
#   labs(
#     x = "Cumulative Proportion of Individuals",
#     y = "Cumulative Proportion of High-Risk Exposures",
#     title = "Lorenz Curves of High-Risk Exposure by Commercial Goods Tertile",
#     subtitle = "Dashed line = perfect equality"
#   ) +
#   theme_minimal()
# 
# ### 
# 
# village_list <- list(
#   Mandena = high_risk_network_df |> filter(village == "Mandena"),
#   Sarahandrano = high_risk_network_df |> filter(village == "Sarahandrano"),
#   Ampandrana = high_risk_network_df |> filter(grepl("Ampand|Andats", village))
# )
# 
# 
# make_individual_risk <- function(df) {
#   df |> 
#     group_by(from) |> 
#     summarise(
#       total_high_risk = sum(edge == 1, na.rm = TRUE),
#       house_sol = first(house_sol),
#       commercial_goods = first(commercial_goods),
#       land_size = first(land_size),
#       grew_vanilla = first(grew_vanilla),
#       gender = first(gender),
#       .groups = "drop"
#     )
# }
# 
# wealth_concentration <- function(df, wealth_var, top_prop = 0.2) {
#   df |> 
#     filter(!is.na(.data[[wealth_var]])) |> 
#     arrange(desc(.data[[wealth_var]])) |> 
#     mutate(
#       cum_people = row_number() / n(),
#       cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#     ) |> 
#     filter(cum_people <= top_prop) |> 
#     summarise(
#       top_people_percent = 100 * max(cum_people),
#       percent_of_risk = 100 * max(cum_risk)
#     )
# }
# 
# make_lorenz <- function(df) {
#   df |> 
#     arrange(total_high_risk) |> 
#     mutate(
#       cum_people = row_number() / n(),
#       cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#     )
# }
# village_results <- imap(village_list, function(df, village_name) {
#   
#   individual_risk <- make_individual_risk(df)
#   
#   list(
#     village = village_name,
#     
#     vanilla_risk = individual_risk |> 
#       summarise(
#         percent_people_vanilla = 100 * mean(grew_vanilla == 1, na.rm = TRUE),
#         percent_risk_from_vanilla =
#           100 * sum(total_high_risk[grew_vanilla == 1], na.rm = TRUE) /
#           sum(total_high_risk)
#       ),
#     
#     half_risk = individual_risk |> 
#       arrange(desc(total_high_risk)) |> 
#       mutate(
#         cum_people = row_number() / n(),
#         cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#       ) |> 
#       filter(cum_risk >= 0.5) |> 
#       slice(1) |> 
#       summarise(percent_people_for_half_risk = 100 * cum_people),
#     
#     wealth_house = wealth_concentration(individual_risk, "house_sol", 0.2),
#     wealth_goods = wealth_concentration(individual_risk, "commercial_goods", 0.2),
#     wealth_land  = wealth_concentration(individual_risk, "land_size", 0.2),
#     
#     gini = Gini(individual_risk$total_high_risk),
#     
#     lorenz = make_lorenz(individual_risk),
#     
#     individual_risk = individual_risk
#   )
# })
# 
# 
# village_results$Mandena
# village_results$Sarahandrano
# village_results$Ampandrana
# 
# individual_risk_all <- map_dfr(village_results, "individual_risk", .id = "village")
# 
# lorenz_pooled <- make_lorenz(individual_risk_all)
# 
# ggplot(lorenz_pooled, aes(cum_people, cum_risk)) +
#   geom_line(linewidth = 1.2) +
#   geom_abline(linetype = "dashed") +
#   labs(
#     title = "Pooled Lorenz Curve of High-Risk Exposure",
#     x = "Cumulative Proportion of Individuals",
#     y = "Cumulative Proportion of High-Risk Exposures"
#   ) +
#   theme_minimal()
# 
# lorenz_by_village <- individual_risk_all |> 
#   group_by(village) |> 
#   arrange(total_high_risk, .by_group = TRUE) |> 
#   mutate(
#     cum_people = row_number() / n(),
#     cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#   ) |> 
#   ungroup()
# 
# 
# lorenz_by_village <- individual_risk_all |> 
#   group_by(village) |> 
#   arrange(total_high_risk, .by_group = TRUE) |> 
#   mutate(
#     cum_people = row_number() / n(),
#     cum_risk = cumsum(total_high_risk) / sum(total_high_risk)
#   ) |> 
#   ungroup()
# 
# ggplot(lorenz_by_village,
#        aes(cum_people, cum_risk, group = village, color = village)) +
#   geom_line(linewidth = 1.2) +
#   geom_abline(linetype = "dashed") +
#   labs(
#     title = "Village-Stratified Lorenz Curves",
#     x = "Cumulative Proportion of Individuals",
#     y = "Cumulative Proportion of High-Risk Exposures"
#   ) +
#   theme_minimal()
# 
# gini_by_village <- map_dfr(village_results, \(x) {
#   tibble(
#     village = x$village,
#     gini = x$gini,
#     n = nrow(x$individual_risk)
#   )
# })
# 
# pooled_gini <- weighted.mean(gini_by_village$gini,
#                              w = gini_by_village$n)
# 
# 
