library(hrbrthemes)
library(here)
library(tidyverse)
source(here("./self_reported_networks/The_ReUp/Loading Interaction Data.R"))


animal_contact_data <- animal_interaction_data_cleaned |> 
  select(pet_dogs:dead_goats_sheep)


colnames(animal_contact_data) <- gsub("shared_water", "sharedwater", colnames(animal_contact_data))
colnames(animal_contact_data) <- gsub("scratched_bitten", "scratchedbitten", colnames(animal_contact_data))
colnames(animal_contact_data) <- gsub("hunted_trapped", "huntedtrapped", colnames(animal_contact_data))
colnames(animal_contact_data) <- gsub("cooked_handled", "cookedhandled", colnames(animal_contact_data))
colnames(animal_contact_data) <- gsub("raw_undercooked", "rawundercooked", colnames(animal_contact_data))
colnames(animal_contact_data) <- gsub("eaten_sick", "eatensick", colnames(animal_contact_data))




animals <- c("rodents", "poultry", "dogs", "cats", "domestic_pigs", "goats_sheep",
             "cows", "wild_birds", "bush_pigs", "lemurs", "tenrecs", "carnivores")


# Subset relevant columns
selected_columns <- grep(paste(animals, collapse = "|"), names(animal_contact_data), value = TRUE)
filtered_data <- animal_contact_data %>%
  select(all_of(selected_columns))

# Summarize interaction sums
interaction_sums <- filtered_data %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "interaction", values_to = "count")

# Extract animal and interaction type
interaction_sums <- interaction_sums %>%
  mutate(
    animal = sub("^[^_]+_", "", interaction),  # Extract everything after the first underscore
    interaction_type = sub("_.*", "", interaction)  # Extract everything before the first underscore
  ) %>%
  filter(animal %in% animals)

# Prepare data for heatmap
heatmap_data <- interaction_sums %>%
  group_by(interaction_type, animal) %>%
  summarize(count = sum(count), .groups = "drop")


animal_order <- c(
  "poultry", "domestic_pigs", "cows", "dogs", "goats_sheep",
  "cats", "rodents", "bush_pigs", "carnivores", "tenrecs", 
  "wild_birds", "lemurs"
)
animal_labels <- c(
  "Poultry", "Domestic Pigs", "Cows", "Dogs", "Goats/Sheep",
  "Cats", "Rodents", "Bush Pigs", "Carnivores", "Tenrecs",
  "Wild Birds", "Lemurs"
)

interaction_order <- c("sharedwater", "feces", "scratchedbitten", "cookedhandled",
                       "rawundercooked", "eatensick", "slaughtered", "raised", "pet",
                       "inside", "huntedtrapped", "handle", "dead")

interaction_labels <- c(
  "Shared Water*", "Feces*", "Scratched/Bitten*", "Cooked*", "Eaten Raw*",
  "Eaten Sick*", "Slaughtered*", "Raised", "Pet", "Inside", "Hunted/Trapped", 
  "Handled", "Dead"
)

# Modify the animal variable to set order and labels
heatmap_data <- heatmap_data %>%
  mutate(animal = factor(animal, levels = animal_order, labels = animal_labels)) |> 
  mutate(interaction_type = factor(interaction_type, levels = interaction_order, labels = interaction_labels))

# Create the heatmap
ggplot(heatmap_data, aes(x = animal, y = interaction_type, fill = count)) +
  geom_tile(color = NA) +  # Removes gridlines between tiles
  scale_fill_viridis(name = "Count", begin = .7, end = 0) +
  geom_text(aes(label = count), color = "white", size = 4) +  # Add count labels
  labs(
    title = "Heatmap of Animal Interaction Counts",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()  # Remove all gridlines
  )
  





