load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_a.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_s.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_full_village_m.RData")

library(ergm)
library(tidyverse)
library(metafor)
library(ggpubr)

summary_df <- rbind(as.data.frame(summary(ergm_ampandrana_andatsakala)$coefficients ),
                    as.data.frame(summary(ergm_mandena)$coefficients), 
                    as.data.frame(summary(ergm_sarahandrano)$coefficients)) |> 
  rownames_to_column(var = "term") |>  # Move row names into a column named "term"
  relocate(term, .before = Estimate) |>  # Place the "term" column before "Estimate"
  dplyr::filter(!grepl("degree", term)) |> 
  mutate(Village = c(rep("Ampandrana", 17), rep("Mandena", 17), rep("Sarahandrano", 17))) 


summary_df$term <- rep(c("Edges", "Age", "Gender[Male]", "CGSOL", "HSOL",  "Vanilla",
                     "Land Size", "Household Size", "School Level", "Rodents", "Wild",
                     "CGSOL:Rodents", "CGSOL:Wild", "HSOL:Rodents", "HSOL:Wild",
                     "Vanilla:Rodents", "Vanilla:Wild"), 3)

summary_df<-summary_df |> 
  rename("Variable" = term)


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
name_vanilla_pr <- pool_function(variable ="Vanilla")
name_ls_pr <- pool_function(variable ="Land Size")
name_hs_pr <- pool_function(variable ="Household Size")
name_sl_pr <- pool_function(variable ="School Level")
name_cgsol_pr <- pool_function(variable ="CGSOL")
name_hsol_pr <- pool_function(variable ="HSOL")
name_rodents_pr <- pool_function(variable ="Rodents")
name_wild_animals_pr <- pool_function(variable ="Wild")

name_hsol_rodents_pr <- pool_function(variable ="HSOL:Rodents")
name_hsol_wild_animals_pr <- pool_function(variable ="HSOL:Wild")

name_vanilla_rodents_pr <- pool_function(variable ="Vanilla:Rodents")
name_vanilla_wild_animals_pr <- pool_function(variable ="Vanilla:Wild")

name_cgsol_rodents_pr <- pool_function(variable ="CGSOL:Rodents")
name_cgsol_wild_animals_pr <- pool_function(variable ="CGSOL:Wild")

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

meta_df$Variable <- c("Age", "Gender[Man]", "Vanilla", "Land Size",
                      "Household Size", "School Level",  "Goods", "House Mat.", "Rodent",
                      "Wild", "House Mat.:Rodent","House Mat.:Wild",
                       "Vanilla:Rodent","Vanilla:Wild",
                      "Goods:Rodent", "Goods:Wild")



meta_df$lower_ci <- meta_df$Estimate - 1.96 * meta_df$SE
meta_df$upper_ci <- meta_df$Estimate + 1.96 * meta_df$SE

meta_df$lower_ci_90 <- meta_df$Estimate - qnorm(0.95) * meta_df$SE
meta_df$upper_ci_90 <- meta_df$Estimate + qnorm(0.95) * meta_df$SE

rownames(meta_df) <- NULL

meta_df <- meta_df |> 
  relocate(Variable, .before = "Estimate")

# plots metafor-----

meta_df_people <- meta_df |> 
  slice(1:8)


meta_df_people$Variable <- factor(
  meta_df_people$Variable,
  levels = rev(c(
    "House Mat.", "Goods", "Vanilla", "Land Size", 
    "Household Size", "School Level", "Gender[Man]", "Age"
  ))
)

meta_df_animal <- meta_df |> 
  slice(9:16)

meta_df_animal$Variable <- factor(
  meta_df_animal$Variable,
  levels = rev(c(
    "Rodent", "Wild", "Vanilla:Rodent","Vanilla:Wild", 
    "House Mat.:Rodent","House Mat.:Wild",
    "Goods:Rodent", "Goods:Wild"
  ))
)



people_plot_pooled_self_report <- ggplot(meta_df_people, aes(x = Estimate, y = Variable)) + 
  # Points for estimated effects with dodge position
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  # 95% CI as thinner linerange
  geom_linerange(
    aes(xmin = lower_ci, xmax = upper_ci),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  # 90% CI as thicker linerange
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) + 
  # Classic theme
  theme_classic() +
  # Labels
  labs(x = "Estimated Effect", y = NULL) + 
  # Theme adjustments
  theme(
    legend.position = "right", 
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")


animal_plot_pooled_self_report <- ggplot(meta_df_animal, aes(x = Estimate, y = Variable)) + 
  # Points for estimated effects with dodge position
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  # 95% CI as thinner linerange
  geom_linerange(
    aes(xmin = lower_ci, xmax = upper_ci),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  # 90% CI as thicker linerange
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) + 
  # Classic theme
  theme_classic() +
  # Labels
  labs(x = "Estimated Effect", y = NULL) + 
  # Theme adjustments
  theme(
    legend.position = "right", 
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")

ggarrange(people_plot_pooled_self_report,
          animal_plot_pooled_self_report, 
          labels = c("a", "b"), common.legend = TRUE)


#plotting with village included ----

summary_df_people <- summary_df |> 
  mutate(
    Variable = str_replace_all(Variable, c("CGSOL" = "Goods", "HSOL" = "House Mat.", 
                                           "Gender\\[Male\\]" = "Gender[Man]")),
    Village = case_when(
      Village == "Ampandrana" ~ "A",
      Village == "Sarahandrano" ~ "S",
      Village == "Mandena" ~ "M",
      TRUE ~ Village
    )
  ) |>
  filter(!str_detect(Variable, "odent|ild|ges")) |> 
  mutate(
    lower_ci_95 = Estimate - 1.96 * `Std. Error`,
    upper_ci_95 = Estimate + 1.96 * `Std. Error`,
    upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
    lower_ci_90 = Estimate - 1.644854 * `Std. Error`
  )

summary_df_people$Variable <- factor(summary_df_people$Variable, 
                                     levels = rev(c("House Mat.", "Goods", "Vanilla", "Land Size", 
                                                    "Household Size", "School Level", "Gender[Man]", "Age")))


full_network_a<-ggplot(summary_df_people, aes(x = Estimate, y = Variable, color = Village)) + 
  # Points for estimated effects with dodge position
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  # 95% CI as thinner linerange
  geom_linerange(
    aes(xmin = lower_ci_95, xmax = upper_ci_95),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  # 90% CI as thicker linerange
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) + 
  # Classic theme
  theme_classic() +
  # Labels
  labs(x = "Estimated Effect", y = NULL) + 
  # Colorblind-friendly palette from viridis
  scale_color_viridis_d(option = "D", end = 0.8) +  
  # Theme adjustments
  theme(
    legend.position = "top", 
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")




summary_df_animal <- summary_df |> 
  mutate(
    Variable = str_replace_all(Variable, c("CGSOL" = "Goods", "HSOL" = "House Mat.", 
                                           "Rodents" = "Rodent" )),
    Village = case_when(
      Village == "Ampandrana" ~ "A",
      Village == "Sarahandrano" ~ "S",
      Village == "Mandena" ~ "M",
      TRUE ~ Village
    )
  ) |>
  filter(str_detect(Variable, "odent|ild")) |> 
  mutate(
    lower_ci_95 = Estimate - 1.96 * `Std. Error`,
    upper_ci_95 = Estimate + 1.96 * `Std. Error`,
    upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
    lower_ci_90 = Estimate - 1.644854 * `Std. Error`
  )

summary_df_animal$Variable <- factor(summary_df_animal$Variable, 
                                     levels = rev(c("Rodent", "Wild", "Vanilla:Rodent", "Vanilla:Wild", 
                                                    "House Mat.:Rodent", "House Mat.:Wild",
                                                    "Goods:Rodent", "Goods:Wild")))
full_network_b<-ggplot(summary_df_animal, aes(x = Estimate, y = Variable, color = Village)) + 
  # Points for estimated effects with dodge position
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  # 95% CI as thinner linerange
  geom_linerange(
    aes(xmin = lower_ci_95, xmax = upper_ci_95),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  # 90% CI as thicker linerange
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) + 
  # Classic theme
  theme_classic() +
  # Labels
  labs(x = "Estimated Effect", y = NULL) + 
  # Colorblind-friendly palette from viridis
  scale_color_viridis_d(option = "D", end = 0.8) +  
  # Theme adjustments
  theme(
    legend.position = "top", 
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")


ggarrange(full_network_a,
          full_network_b, 
          labels = c("a", "b"), common.legend = TRUE)


# HIGH RISK NETWORKS ----
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_a.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_m.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_high_risk_village_s.RData")


pool_function_high_risk <- function(variable){
  
  model_df <- summary_df_high_risk %>% select(Estimate, `Std. Error`, Variable, Village) %>% filter(Variable==variable)
  
  metaanalysis_model <- rma(yi = model_df$Estimate,   
                            sei = model_df$`Std. Error`, 
                            method = "FE")
  
  output <- c(metaanalysis_model$beta , metaanalysis_model$ci.lb, metaanalysis_model$ci.ub, metaanalysis_model$pval, metaanalysis_model$se, nrow(model_df))
  
  return(output)
  
}

summary_df_high_risk <- rbind(as.data.frame(summary(ergm_ampandrana_andatsakala_high_risk)$coefficients ),
                    as.data.frame(summary(ergm_mandena_high_risk_lemurs)$coefficients), 
                    as.data.frame(summary(ergm_sarahandrano_high_risk)$coefficients)) |> 
  rownames_to_column(var = "term") |>  # Move row names into a column named "term"
  relocate(term, .before = Estimate) |>  # Place the "term" column before "Estimate"
  dplyr::filter(!grepl("degree", term)) |> 
  mutate(Village = c(rep("Ampandrana", 21), rep("Mandena", 21), rep("Sarahandrano", 21))) 


summary_df_high_risk$term <- rep(c("Edges", "Age", "Gender[Male]", "CGSOL", "HSOL",  "Vanilla",
                         "Land Size", "Household Size", "School Level", "Other Dom.", "Rodents", "Wild",
                         "CGSOL:Other Dom.","CGSOL:Rodents", "CGSOL:Wild", 
                         "HSOL:Other Dom.", "HSOL:Rodents", "HSOL:Wild",
                         "Vanilla:Other Dom.", "Vanilla:Rodents", "Vanilla:Wild"), 3)

summary_df_high_risk<-summary_df_high_risk |> 
  rename("Variable" = term)


name_age_pr_high_risk <- pool_function_high_risk(variable ="Age")
name_sex_pr_high_risk <- pool_function_high_risk(variable ="Gender[Male]")
name_vanilla_pr_high_risk <- pool_function_high_risk(variable ="Vanilla")
name_ls_pr_high_risk <- pool_function_high_risk(variable ="Land Size")
name_hs_pr_high_risk <- pool_function_high_risk(variable ="Household Size")
name_sl_pr_high_risk <- pool_function_high_risk(variable ="School Level")
name_cgsol_pr_high_risk <- pool_function_high_risk(variable ="CGSOL")
name_hsol_pr_high_risk <- pool_function_high_risk(variable ="HSOL")

name_domesticated_uncommon_pr_high_risk <- pool_function_high_risk(variable = "Other Dom.")
name_rodents_pr_high_risk <- pool_function_high_risk(variable ="Rodents")
name_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="Wild")

name_hsol_dom_uncommon_pr_high_risk <- pool_function_high_risk(variable ="HSOL:Other Dom.")
name_hsol_rodents_pr_high_risk <- pool_function_high_risk(variable ="HSOL:Rodents")
name_hsol_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="HSOL:Wild")

name_vanilla_dom_uncommon_pr_high_risk <- pool_function_high_risk(variable ="Vanilla:Other Dom.")
name_vanilla_rodents_pr_high_risk <- pool_function_high_risk(variable ="Vanilla:Rodents")
name_vanilla_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="Vanilla:Wild")

name_cgsol_dom_uncommon_pr_high_risk <- pool_function_high_risk(variable ="CGSOL:Other Dom.")
name_cgsol_rodents_pr_high_risk <- pool_function_high_risk(variable ="CGSOL:Rodents")
name_cgsol_wild_animals_pr_high_risk <- pool_function_high_risk(variable ="CGSOL:Wild")

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

meta_df_high_risk$Variable <- c("Age", "Gender[Man]", "Vanilla", "Land Size",
                      "Household Size", "School Level",  "Goods", "House Mat.", "Other Dom.",
                      "Rodent", "Wild",  "House Mat.:Other Dom.", 
                      "House Mat.:Rodent","House Mat.:Wild",
                      "Vanilla:Other Dom.", "Vanilla:Rodent","Vanilla:Wild",
                      "Goods:Other Dom.", "Goods:Rodent", 
                      "Goods:Wild")



meta_df_high_risk$lower_ci <- meta_df_high_risk$Estimate - 1.96 * meta_df_high_risk$SE
meta_df_high_risk$upper_ci <- meta_df_high_risk$Estimate + 1.96 * meta_df_high_risk$SE

meta_df_high_risk$lower_ci_90 <- meta_df_high_risk$Estimate - qnorm(0.95) * meta_df_high_risk$SE
meta_df_high_risk$upper_ci_90 <- meta_df_high_risk$Estimate + qnorm(0.95) * meta_df_high_risk$SE

rownames(meta_df_high_risk) <- NULL

meta_df_high_risk <- meta_df_high_risk |> 
  relocate(Variable, .before = "Estimate")


# plots metafor-----

meta_df_high_risk_people <- meta_df_high_risk |> 
  slice(1:8)


meta_df_high_risk_people$Variable <- factor(
  meta_df_high_risk_people$Variable,
  levels = rev(c(
    "House Mat.", "Goods", "Vanilla", "Land Size", 
    "Household Size", "School Level", "Gender[Man]", "Age"
  ))
)

meta_df_high_risk_animal <- meta_df_high_risk |> 
  slice(9:20)

meta_df_high_risk_animal$Variable <- factor(
  meta_df_high_risk_animal$Variable,
  levels = rev(c(
    "Other Dom.", "Rodent", "Wild", 
    "Vanilla:Other Dom.", "Vanilla:Rodent","Vanilla:Wild", 
    "House Mat.:Other Dom.", "House Mat.:Rodent","House Mat.:Wild",
    "Goods:Other Dom.", "Goods:Rodent", "Goods:Wild"
  ))
)



people_plot_high_risk_pooled <- ggplot(meta_df_high_risk_people, aes(x = Estimate, y = Variable)) + 
  # Points for estimated effects with dodge position
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  # 95% CI as thinner linerange
  geom_linerange(
    aes(xmin = lower_ci, xmax = upper_ci),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  # 90% CI as thicker linerange
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) + 
  # Classic theme
  theme_classic() +
  # Labels
  labs(x = "Estimated Effect", y = NULL) + 
  # Theme adjustments
  theme(
    legend.position = "right", 
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")


animal_plot_high_risk_pooled <- ggplot(meta_df_high_risk_animal, aes(x = Estimate, y = Variable)) + 
  # Points for estimated effects with dodge position
  geom_point(position = position_dodge(width = 0.5), size = 3) + 
  # 95% CI as thinner linerange
  geom_linerange(
    aes(xmin = lower_ci, xmax = upper_ci),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  # 90% CI as thicker linerange
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) + 
  # Classic theme
  theme_classic() +
  # Labels
  labs(x = "Estimated Effect", y = NULL) + 
  # Theme adjustments
  theme(
    legend.position = "right", 
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")

ggarrange(people_plot_high_risk_pooled,
          animal_plot_high_risk_pooled, 
          labels = c("a", "b"), common.legend = TRUE)


#plotting with village included ----
summary_df_high_risk_people <- summary_df_high_risk |>
  mutate(
    Variable = str_replace_all(Variable, c("CGSOL" = "Goods", "HSOL" = "House Mat.",
                                           "Gender\\[Male\\]" = "Gender[Man]")),
    Village = case_when(
      Village == "Ampandrana" ~ "A",
      Village == "Sarahandrano" ~ "S",
      Village == "Mandena" ~ "M",
      TRUE ~ Village
    )
  ) |>
  filter(!str_detect(Variable, "om|odent|ild|ges")) |>
  mutate(
    lower_ci_95 = Estimate - 1.96 * `Std. Error`,
    upper_ci_95 = Estimate + 1.96 * `Std. Error`,
    upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
    lower_ci_90 = Estimate - 1.644854 * `Std. Error`
  )

summary_df_high_risk_people$Variable <- factor(summary_df_high_risk_people$Variable,
                                               levels = rev(c("House Mat.", "Goods", "Vanilla", "Land Size",
                                                              "Household Size", "School Level", "Gender[Man]", "Age")))

high_risk_network_a <- ggplot(summary_df_high_risk_people, aes(x = Estimate, y = Variable, color = Village)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_linerange(
    aes(xmin = lower_ci_95, xmax = upper_ci_95),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) +
  theme_classic() +
  labs(x = "Estimated Effect", y = NULL) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  theme(
    legend.position = "top",
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")


# ---- High Risk Animal ----
summary_df_high_risk_animal <- summary_df_high_risk |>
  mutate(
    Variable = str_replace_all(Variable, c("CGSOL" = "Goods", "HSOL" = "House Mat.",
                                           "Rodents" = "Rodent")),
    Village = case_when(
      Village == "Ampandrana" ~ "A",
      Village == "Sarahandrano" ~ "S",
      Village == "Mandena" ~ "M",
      TRUE ~ Village
    )
  ) |>
  filter(str_detect(Variable, "om|odent|ild")) |>
  mutate(
    lower_ci_95 = Estimate - 1.96 * `Std. Error`,
    upper_ci_95 = Estimate + 1.96 * `Std. Error`,
    upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
    lower_ci_90 = Estimate - 1.644854 * `Std. Error`
  )

summary_df_high_risk_animal$Variable <- factor(summary_df_high_risk_animal$Variable,
                                               levels = rev(c("Other Dom.", "Rodent", "Wild",
                                                              "Vanilla:Other Dom.", "Vanilla:Rodent", "Vanilla:Wild",
                                                              "House Mat.:Other Dom.", "House Mat.:Rodent", "House Mat.:Wild",
                                                              "Goods:Other Dom.", "Goods:Rodent", "Goods:Wild")))

high_risk_network_b <- ggplot(summary_df_high_risk_animal, aes(x = Estimate, y = Variable, color = Village)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_linerange(
    aes(xmin = lower_ci_95, xmax = upper_ci_95),
    position = position_dodge(0.5), linewidth = 0.4
  ) +
  geom_linerange(
    aes(xmin = lower_ci_90, xmax = upper_ci_90),
    position = position_dodge(0.5), linewidth = 1.2
  ) +
  theme_classic() +
  labs(x = "Estimated Effect", y = NULL) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  theme(
    legend.position = "top",
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")

# ---- Combine Plots ----
ggarrange(high_risk_network_a,
          high_risk_network_b,
          labels = c("a", "b"), common.legend = TRUE)


