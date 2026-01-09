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

merged_df_mandena <- suppressWarnings(merged_df |> 
                                        filter(village == "Mandena") |> 
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
                                mutate(total_animal_interactions = sum(c_across(pet_dogs:dead_goats_sheep)), na.rm=TRUE) |> 
                                mutate(employment_category = if_else(grepl("farm_crops", main_activity) | grepl("farm_mixed", main_activity), "Farmer", 
                                                                     ifelse(grepl("student", main_activity) | grepl("teacher", main_activity), "Education",
                                                                            ifelse(grepl("unemployed", main_activity) | grepl("retired_teacher", main_activity), "Unemployed",
                                                                                  "Other")))) |> 
                                mutate(num_high_risk_exposures = sum(across(contains(c("shared_water","scratched_bitten","feces", "dead",
                                                                                       "raw_undercooked", "eaten_sick"))), na.rm = TRUE)) |> 
                                mutate(high_risk_exposures_binary = ifelse(num_high_risk_exposures == 0L, 0L, 1L)) |> 
                                mutate(school_level_numbered = ifelse(school_level == "None", 0L, 
                                                                      ifelse(school_level == "Primary", 1L,
                                                                             ifelse(school_level == "Secondary", 2L,
                                                                                    ifelse(school_level == "Higher", 3L,
                                                                                           NA))))) |> 
                                rename(grew_vanilla = crops_vanilla) |> 
                              mutate(age = scale(age)) |> 
                              mutate(landsize_in_daba = scale(landsize_in_daba)) |> 
                              mutate(household_size = scale(household_size)) |> 
                              mutate(school_level_numbered = scale(school_level_numbered)) |> 
                              
  select(-na.rm))

merged_df_sarahandrano <- suppressWarnings(merged_df |> 
                                        filter(village == "Sarahandrano") |> 
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
                                        mutate(total_animal_interactions = sum(c_across(pet_dogs:dead_goats_sheep)), na.rm=TRUE) |> 
                                        mutate(employment_category = if_else(grepl("farm_crops", main_activity) | grepl("farm_mixed", main_activity), "Farmer", 
                                                                             ifelse(grepl("student", main_activity) | grepl("teacher", main_activity), "Education",
                                                                                    ifelse(grepl("unemployed", main_activity) | grepl("retired_teacher", main_activity), "Unemployed",
                                                                                           "Other")))) |> 
                                        mutate(num_high_risk_exposures = sum(across(contains(c("shared_water","scratched_bitten","feces", "dead",
                                                                                               "raw_undercooked", "eaten_sick"))), na.rm = TRUE)) |> 
                                        mutate(high_risk_exposures_binary = ifelse(num_high_risk_exposures == 0L, 0L, 1L)) |> 
                                        mutate(school_level_numbered = ifelse(school_level == "None", 0L, 
                                                                              ifelse(school_level == "Primary", 1L,
                                                                                     ifelse(school_level == "Secondary", 2L,
                                                                                            ifelse(school_level == "Higher", 3L,
                                                                                                   NA))))) |> 
                                        rename(grew_vanilla = crops_vanilla) |> 
                                        mutate(age = scale(age)) |> 
                                        mutate(landsize_in_daba = scale(landsize_in_daba)) |> 
                                        mutate(household_size = scale(household_size)) |> 
                                        mutate(school_level_numbered = scale(school_level_numbered)) |> 
                                        
                                        select(-na.rm))

merged_df_ampandrana_andatsakala <- suppressWarnings(merged_df |> 
                                        filter(grepl("Andatsakala", village) |grepl("Ampandrana", village)) |> 
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
                                        mutate(total_animal_interactions = sum(c_across(pet_dogs:dead_goats_sheep)), na.rm=TRUE) |> 
                                        mutate(employment_category = if_else(grepl("farm_crops", main_activity) | grepl("farm_mixed", main_activity), "Farmer", 
                                                                             ifelse(grepl("student", main_activity) | grepl("teacher", main_activity), "Education",
                                                                                    ifelse(grepl("unemployed", main_activity) | grepl("retired_teacher", main_activity), "Unemployed",
                                                                                           "Other")))) |> 
                                        mutate(num_high_risk_exposures = sum(across(contains(c("shared_water","scratched_bitten","feces", "dead",
                                                                                               "raw_undercooked", "eaten_sick"))), na.rm = TRUE)) |> 
                                        mutate(high_risk_exposures_binary = ifelse(num_high_risk_exposures == 0L, 0L, 1L)) |> 
                                        mutate(school_level_numbered = ifelse(school_level == "None", 0L, 
                                                                              ifelse(school_level == "Primary", 1L,
                                                                                     ifelse(school_level == "Secondary", 2L,
                                                                                            ifelse(school_level == "Higher", 3L,
                                                                                                   NA))))) |> 
                                        rename(grew_vanilla = crops_vanilla) |> 
                                        mutate(age = scale(age)) |> 
                                        mutate(landsize_in_daba = scale(landsize_in_daba)) |> 
                                        mutate(household_size = scale(household_size)) |> 
                                        mutate(school_level_numbered = scale(school_level_numbered)) |> 
                                        
                                        select(-na.rm))
merged_df <-rbind(merged_df_ampandrana_andatsakala, merged_df_mandena, merged_df_sarahandrano)

merged_df$survey_date <- as.Date(merged_df$survey_date)


n_respondents<-length(unique(merged_df$social_netid))
n_respondents


