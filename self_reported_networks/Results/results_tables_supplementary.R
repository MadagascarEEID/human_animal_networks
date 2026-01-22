# Load required packages
library(dplyr)
library(tibble)

# load ergm results
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_a.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_s.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_m.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_a.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_m.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_s.RData")


# Extract coefficient summary
ergm_coefs_a <- summary(ergm_ampandrana_andatsakala)$coefficients
ergm_coefs_m <- summary(ergm_mandena)$coefficients
ergm_coefs_s <- summary(ergm_sarahandrano)$coefficients

ergm_coefs_high_risk_a <- summary(ergm_ampandrana_andatsakala_high_risk)$coefficients
ergm_coefs_high_risk_m <- summary(ergm_mandena_high_risk_lemurs)$coefficients
ergm_coefs_high_risk_s <- summary(ergm_sarahandrano_high_risk)$coefficients

# Convert to a data frame
ergm_table_a <- as.data.frame(ergm_coefs_a) %>%
  rownames_to_column(var = "term") %>%
  rename(
    Estimate = Estimate,
    `Std. Error` = `Std. Error`,
    `z value` = `z value`,
    `p-value` = `Pr(>|z|)`
  ) %>%
  mutate(
    term = case_when(
      term == "edges" ~ "Edges",
      term == "b2cov.age" ~ "Age",
      term == "b2factor.gender.Male" ~ "Gender [Man]",
      term == "b2cov.commercial_goods" ~ "Commercial Goods",
      term == "b2cov.house_sol" ~ "House Materials",
      term == "b2factor.grew_vanilla.1" ~ "Vanilla Farmer",
      term == "b2cov.land_size" ~ "Land Size",
      term == "b2cov.household_size" ~ "Household Size",
      term == "b2cov.school_level" ~ "School Level",
      term == "b1factor.animal_category.rodent" ~ "Rodent",
      term == "b1factor.animal_category.wild" ~ "Wild Animal",
      term == "b1factor.animal_category.rodent:b2cov.commercial_goods" ~ "Commercial Goods:Rodent",
      term == "b1factor.animal_category.wild:b2cov.commercial_goods" ~ "Commercial Goods:Wild Animal",
      term == "b1factor.animal_category.rodent:b2cov.house_sol" ~ "House Materials:Rodent",
      term == "b1factor.animal_category.wild:b2cov.house_sol" ~ "House Materials:Wild Animal",
      term == "b1factor.animal_category.rodent:b2factor.grew_vanilla.1" ~ "Vanilla:Rodent",
      term == "b1factor.animal_category.wild:b2factor.grew_vanilla.1" ~ "Vanilla:Wild Animal",
      term == "b2degree2" ~ "Human Degree = 2",
      term == "b2degree3" ~ "Human Degree = 3",
      term == "b2degree4" ~ "Human Degree = 4",
      term == "b2degree5" ~ "Human Degree = 5",
      term == "b2degree6" ~ "Human Degree = 6",
      TRUE ~ term
    ),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  select(Term = term, Estimate, `Std. Error`, `z value`, `p-value`)

ergm_table_m <- as.data.frame(ergm_coefs_m) %>%
  rownames_to_column(var = "term") %>%
  rename(
    Estimate = Estimate,
    `Std. Error` = `Std. Error`,
    `z value` = `z value`,
    `p-value` = `Pr(>|z|)`
  ) %>%
  mutate(
    term = case_when(
      term == "edges" ~ "Edges",
      term == "b2cov.age" ~ "Age",
      term == "b2factor.gender.Male" ~ "Gender [Man]",
      term == "b2cov.commercial_goods" ~ "Commercial Goods",
      term == "b2cov.house_sol" ~ "House Materials",
      term == "b2factor.grew_vanilla.1" ~ "Vanilla Farmer",
      term == "b2cov.land_size" ~ "Land Size",
      term == "b2cov.household_size" ~ "Household Size",
      term == "b2cov.school_level" ~ "School Level",
      term == "b1factor.animal_category.rodent" ~ "Rodent",
      term == "b1factor.animal_category.wild" ~ "Wild Animal",
      term == "b1factor.animal_category.rodent:b2cov.commercial_goods" ~ "Commercial Goods:Rodent",
      term == "b1factor.animal_category.wild:b2cov.commercial_goods" ~ "Commercial Goods:Wild Animal",
      term == "b1factor.animal_category.rodent:b2cov.house_sol" ~ "House Materials:Rodent",
      term == "b1factor.animal_category.wild:b2cov.house_sol" ~ "House Materials:Wild Animal",
      term == "b1factor.animal_category.rodent:b2factor.grew_vanilla.1" ~ "Vanilla:Rodent",
      term == "b1factor.animal_category.wild:b2factor.grew_vanilla.1" ~ "Vanilla:Wild Animal",
      term == "b2degree2" ~ "Human Degree = 2",
      term == "b2degree3" ~ "Human Degree = 3",
      term == "b2degree4" ~ "Human Degree = 4",
      term == "b2degree5" ~ "Human Degree = 5",
      term == "b2degree6" ~ "Human Degree = 6",
      term == "b2degree7" ~ "Human Degree = 7",
      term == "b2degree8" ~ "Human Degree = 8",
      TRUE ~ term
    ),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  select(Term = term, Estimate, `Std. Error`, `z value`, `p-value`)


# View the cleaned table
print(ergm_table_m)


ergm_table_s <- as.data.frame(ergm_coefs_s) %>%
  rownames_to_column(var = "term") %>%
  rename(
    Estimate = Estimate,
    `Std. Error` = `Std. Error`,
    `z value` = `z value`,
    `p-value` = `Pr(>|z|)`
  ) %>%
  mutate(
    term = case_when(
      term == "edges" ~ "Edges",
      term == "b2cov.age" ~ "Age",
      term == "b2factor.gender.Male" ~ "Gender [Man]",
      term == "b2cov.commercial_goods" ~ "Commercial Goods",
      term == "b2cov.house_sol" ~ "House Materials",
      term == "b2factor.grew_vanilla.1" ~ "Vanilla Farmer",
      term == "b2cov.land_size" ~ "Land Size",
      term == "b2cov.household_size" ~ "Household Size",
      term == "b2cov.school_level" ~ "School Level",
      term == "b1factor.animal_category.rodent" ~ "Rodent",
      term == "b1factor.animal_category.wild" ~ "Wild Animal",
      term == "b1factor.animal_category.rodent:b2cov.commercial_goods" ~ "Commercial Goods:Rodent",
      term == "b1factor.animal_category.wild:b2cov.commercial_goods" ~ "Commercial Goods:Wild Animal",
      term == "b1factor.animal_category.rodent:b2cov.house_sol" ~ "House Materials:Rodent",
      term == "b1factor.animal_category.wild:b2cov.house_sol" ~ "House Materials:Wild Animal",
      term == "b1factor.animal_category.rodent:b2factor.grew_vanilla.1" ~ "Vanilla:Rodent",
      term == "b1factor.animal_category.wild:b2factor.grew_vanilla.1" ~ "Vanilla:Wild Animal",
      term == "b2degree3" ~ "Human Degree = 3",
      term == "b2degree4" ~ "Human Degree = 4",
      term == "b2degree5" ~ "Human Degree = 5",
      term == "b2degree6" ~ "Human Degree = 6",
      term == "b2degree7" ~ "Human Degree = 7",
      term == "b2degree8" ~ "Human Degree = 8",
      TRUE ~ term
    ),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  select(Term = term, Estimate, `Std. Error`, `z value`, `p-value`)


# View the cleaned table
print(ergm_table_s)



ergm_table_high_risk_a <- as.data.frame(ergm_coefs_high_risk_a) %>%
  rownames_to_column(var = "term") %>%
  rename(
    Estimate = Estimate,
    `Std. Error` = `Std. Error`,
    `z value` = `z value`,
    `p-value` = `Pr(>|z|)`
  ) %>%
  mutate(
    term = case_when(
      term == "edges" ~ "Edges",
      term == "b2cov.age" ~ "Age",
      term == "b2factor.gender.Male" ~ "Gender [Man]",
      term == "b2cov.commercial_goods" ~ "Commercial Goods",
      term == "b2cov.house_sol" ~ "House Materials",
      term == "b2factor.grew_vanilla.1" ~ "Vanilla Farmer",
      term == "b2cov.land_size" ~ "Land Size",
      term == "b2cov.household_size" ~ "Household Size",
      term == "b2cov.school_level" ~ "School Level",
      term == "b1factor.animal_category.domesticated_2" ~ "Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent" ~ "Rodent",
      term == "b1factor.animal_category.wild" ~ "Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2cov.commercial_goods" ~ "Commercial Goods:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2cov.commercial_goods" ~ "Commercial Goods:Rodent",
      term == "b1factor.animal_category.wild:b2cov.commercial_goods" ~ "Commercial Goods:Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2cov.house_sol" ~ "House Materials:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2cov.house_sol" ~ "House Materials:Rodent",
      term == "b1factor.animal_category.wild:b2cov.house_sol" ~ "House Materials:Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2factor.grew_vanilla.1" ~ "Vanilla:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2factor.grew_vanilla.1" ~ "Vanilla:Rodent",
      term == "b1factor.animal_category.wild:b2factor.grew_vanilla.1" ~ "Vanilla:Wild Animal",
      term == "b2degree1" ~ "Human Degree = 1",
      term == "b2degree2" ~ "Human Degree = 2",
      term == "b2degree4" ~ "Human Degree = 4",
      term == "b2degree5" ~ "Human Degree = 5",
      term == "b2degree6" ~ "Human Degree = 6",
      term == "b2degree8" ~ "Human Degree = 8",
      TRUE ~ term
    ),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  select(Term = term, Estimate, `Std. Error`, `z value`, `p-value`)


# View the cleaned table
print(ergm_table_high_risk_a)


ergm_table_high_risk_m <- as.data.frame(ergm_coefs_high_risk_m) %>%
  rownames_to_column(var = "term") %>%
  rename(
    Estimate = Estimate,
    `Std. Error` = `Std. Error`,
    `z value` = `z value`,
    `p-value` = `Pr(>|z|)`
  ) %>%
  mutate(
    term = case_when(
      term == "edges" ~ "Edges",
      term == "b2cov.age" ~ "Age",
      term == "b2factor.gender.Male" ~ "Gender [Man]",
      term == "b2cov.commercial_goods" ~ "Commercial Goods",
      term == "b2cov.house_sol" ~ "House Materials",
      term == "b2factor.grew_vanilla.1" ~ "Vanilla Farmer",
      term == "b2cov.land_size" ~ "Land Size",
      term == "b2cov.household_size" ~ "Household Size",
      term == "b2cov.school_level" ~ "School Level",
      term == "b1factor.animal_category.domesticated_2" ~ "Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent" ~ "Rodent",
      term == "b1factor.animal_category.wild" ~ "Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2cov.commercial_goods" ~ "Commercial Goods:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2cov.commercial_goods" ~ "Commercial Goods:Rodent",
      term == "b1factor.animal_category.wild:b2cov.commercial_goods" ~ "Commercial Goods:Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2cov.house_sol" ~ "House Materials:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2cov.house_sol" ~ "House Materials:Rodent",
      term == "b1factor.animal_category.wild:b2cov.house_sol" ~ "House Materials:Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2factor.grew_vanilla.1" ~ "Vanilla:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2factor.grew_vanilla.1" ~ "Vanilla:Rodent",
      term == "b1factor.animal_category.wild:b2factor.grew_vanilla.1" ~ "Vanilla:Wild Animal",
      term == "b2degree2" ~ "Human Degree = 2",
      term == "b2degree3" ~ "Human Degree = 3",
      term == "b2degree4" ~ "Human Degree = 4",
      term == "b2degree5" ~ "Human Degree = 5",
      term == "b2degree6" ~ "Human Degree = 6",
      term == "b2degree7" ~ "Human Degree = 7",
      TRUE ~ term
    ),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  select(Term = term, Estimate, `Std. Error`, `z value`, `p-value`)


# View the cleaned table
print(ergm_table_high_risk_m)


ergm_table_high_risk_s <- as.data.frame(ergm_coefs_high_risk_s) %>%
  rownames_to_column(var = "term") %>%
  rename(
    Estimate = Estimate,
    `Std. Error` = `Std. Error`,
    `z value` = `z value`,
    `p-value` = `Pr(>|z|)`
  ) %>%
  mutate(
    term = case_when(
      term == "edges" ~ "Edges",
      term == "b2cov.age" ~ "Age",
      term == "b2factor.gender.Male" ~ "Gender [Man]",
      term == "b2cov.commercial_goods" ~ "Commercial Goods",
      term == "b2cov.house_sol" ~ "House Materials",
      term == "b2factor.grew_vanilla.1" ~ "Vanilla Farmer",
      term == "b2cov.land_size" ~ "Land Size",
      term == "b2cov.household_size" ~ "Household Size",
      term == "b2cov.school_level" ~ "School Level",
      term == "b1factor.animal_category.domesticated_2" ~ "Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent" ~ "Rodent",
      term == "b1factor.animal_category.wild" ~ "Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2cov.commercial_goods" ~ "Commercial Goods:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2cov.commercial_goods" ~ "Commercial Goods:Rodent",
      term == "b1factor.animal_category.wild:b2cov.commercial_goods" ~ "Commercial Goods:Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2cov.house_sol" ~ "House Materials:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2cov.house_sol" ~ "House Materials:Rodent",
      term == "b1factor.animal_category.wild:b2cov.house_sol" ~ "House Materials:Wild Animal",
      term == "b1factor.animal_category.domesticated_2:b2factor.grew_vanilla.1" ~ "Vanilla:Domesticated (Uncommon)",
      term == "b1factor.animal_category.rodent:b2factor.grew_vanilla.1" ~ "Vanilla:Rodent",
      term == "b1factor.animal_category.wild:b2factor.grew_vanilla.1" ~ "Vanilla:Wild Animal",
      term == "b2degree2" ~ "Human Degree = 2",
      term == "b2degree3" ~ "Human Degree = 3",
      term == "b2degree4" ~ "Human Degree = 4",
      term == "b2degree5" ~ "Human Degree = 5",
      term == "b2degree6" ~ "Human Degree = 6",
      term == "b2degree7" ~ "Human Degree = 7",
      term == "b2degree8" ~ "Human Degree = 8",
      
      TRUE ~ term
    ),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  select(Term = term, Estimate, `Std. Error`, `z value`, `p-value`)


# View the cleaned table
print(ergm_table_high_risk_s)



# Optional: export
# write.csv(ergm_table, "ergm_table_clean.csv", row.names = FALSE)
# knitr::kable(ergm_table_a, format = "latex", booktabs = TRUE)
