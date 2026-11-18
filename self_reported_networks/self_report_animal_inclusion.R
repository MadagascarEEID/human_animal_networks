# loading packages
library(tidyverse)
library(kableExtra)

# loading data
data_dir <- "/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/clean_data_tables/NIH Human Surveys"

animal_interaction_data <- read.csv(file.path(data_dir, "Survey_Animal_Interaction.csv"))

demographic_health_data <- read.csv(file.path(data_dir, "Survey_Demographic_Health.csv"))

restrict_to_merged_sample <- TRUE

# wrangling animal dataset
animal_clean <- animal_interaction_data |>
  mutate(across(-social_netid, ~ ifelse(. == "Yes", 1L, 0L))) |>
  select(-starts_with("symptoms"), -starts_with("action_"),
         -contains("frequency"))

if (restrict_to_merged_sample) {
  animal_clean <- animal_clean |>
    semi_join(demographic_health_data, by = "social_netid")
}

cat("Respondents:", n_distinct(animal_clean$social_netid), "\n")

interaction_prefixes <- c("shared_water", "scratched_bitten", "cooked_handled",
                          "raw_undercooked", "eaten_sick", "slaughtered",
                          "feces", "pet", "dead", "inside", "hunted_trapped",
                          "raised", "handle")
prefix_re <- paste0("^(", paste(interaction_prefixes, collapse = "|"), ")_")

animal_long <- animal_clean |>
  pivot_longer(-social_netid, names_to = "column", values_to = "value") |>
  mutate(interaction = str_match(column, prefix_re)[, 2],
         animal      = str_remove(column, prefix_re)) |> 
  mutate(animal = recode(animal,
                         frog  = "frogs",
                         flea  = "fleas",
                         tick  = "ticks"))

unmatched <- animal_long |> filter(is.na(interaction)) |> distinct(column)

print(unmatched)

n_resp <- n_distinct(animal_clean$social_netid)

# calculating animal degree
animal_degree <- animal_long |>
  filter(!is.na(interaction)) |>
  group_by(animal, social_netid) |>
  summarise(any_contact = any(value == 1, na.rm = TRUE),
            n_types     = sum(value == 1, na.rm = TRUE),
            .groups = "drop") |>
  group_by(animal) |>
  summarise(degree           = sum(any_contact),
            total_reports    = sum(n_types),
            prop_respondents = degree / n_resp,
            .groups = "drop") |>
  arrange(desc(degree))

# Counts broken out by interaction type (animal x interaction)
animal_by_interaction <- animal_long |>
  filter(!is.na(interaction)) |>
  group_by(animal, interaction) |>
  summarise(n_yes = sum(value == 1, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = interaction, values_from = n_yes, values_fill = 0)

animal_degree_full <- animal_degree |>
  left_join(animal_by_interaction, by = "animal")

animal_interaction_types <- animal_long |>
  filter(!is.na(interaction)) |>
  group_by(animal, interaction) |>
  summarise(n_yes = sum(value == 1, na.rm = TRUE), .groups = "drop") |>
  group_by(animal) |>
  summarise(
    n_types_reported = n_distinct(interaction[n_yes > 0]),   
    types_reported   = paste(sort(interaction[n_yes > 0]), collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(desc(n_types_reported))


animal_interaction_supp_table <- animal_degree |> 
  left_join(animal_interaction_types, by = "animal")

# write.csv(animal_interaction_supp_table, "animal_interaction_counts.csv", row.names = FALSE)



# ---- Labels -------------------------------------------------------
retained_animals <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs",
                      "goats_sheep", "cows", "wild_birds", "bush_pigs",
                      "lemurs", "tenrecs", "carnivores")

animal_labels <- c(goats_sheep   = "Goats/sheep",
                   domestic_pigs = "Domestic pigs",
                   bush_pigs     = "Bush pigs",
                   wild_birds    = "Wild birds")

label_animal <- function(x) {
  ifelse(x %in% names(animal_labels), animal_labels[x],
         str_to_sentence(str_replace_all(x, "_", " ")))
}

# Rebuild the type lists from labelled interactions rather than relabelling
# the pasted string ("handle" is a substring of "cooked_handled")
types_labelled <- animal_long |>
  filter(!is.na(interaction), value == 1) |>
  distinct(animal, interaction) |>
  mutate(label = interaction_labels[interaction]) |>
  group_by(animal) |>
  summarise(types = paste(sort(label), collapse = "; "), .groups = "drop")

# ---- Assemble table -----------------------------------------------
supp_tex_df <- animal_interaction_supp_table |>
  mutate(
    retained = animal %in% retained_animals,
    Animal   = label_animal(animal),
    pct      = sprintf("%.1f", 100 * prop_respondents),
    types    = types_reported |>
      replace_na("--") |>
      str_replace_all("_", "\\\\_")
  ) |>
  arrange(desc(retained), desc(degree)) |>
  select(retained, Animal, degree, pct, total_reports, n_types_reported, types)

# Sanity check on the stated inclusion criteria
supp_tex_df |>
  filter(retained) |>
  summarise(min_respondents = min(degree), min_types = min(n_types_reported)) |>
  print()

n_retained <- sum(supp_tex_df$retained)
n_excluded <- sum(!supp_tex_df$retained)

supp_tex <- supp_tex_df |>
  select(-retained) |>
  kbl(format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      align     = c("l", "r", "r", "r", "r", "l"),
      col.names = c("Animal", "Respondents", "\\% of respondents",
                    "Total reports", "No. types", "Interaction types reported"),
      escape    = FALSE,
      caption   = paste0(
        "Self-reported human--animal interactions by animal (n = ", n_resp,
        " respondents). Animal: animal or animal group as recorded in the survey. ",
        "Respondents: number of people reporting at least one interaction of any ",
        "type with the animal. \\\\% of respondents: Respondents divided by the ",
        "total number of respondents. Total reports: sum of affirmative responses ",
        "across all interaction types, such that a person reporting multiple ",
        "interaction types with the same animal contributes multiple reports. ",
        "No. types: number of distinct interaction types (of 13 surveyed) reported ",
        "by at least one respondent. Interaction types reported: the interaction ",
        "types reported for the animal, using survey variable names. Animals ",
        "(mammals and birds) were retained as network nodes if reported by at ",
        "least 45 respondents and in at least 7 of 13 interaction types."),
      label     = "animal-counts") |>
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 12) |>
  pack_rows("Retained in networks", 1, n_retained) |>
  pack_rows("Excluded", n_retained + 1, n_retained + n_excluded)

writeLines(supp_tex, "table_animal_interaction_counts.tex")
supp_tex
