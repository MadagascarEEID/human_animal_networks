load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_ampandrana_andatsakala.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_sarahandrano.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_mandena.RData")

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


summary_df$term <- rep(c("Edges", "Age", "Gender[Male]", "CGSOL", "HSOL",  "Vanilla Farmer",
                     "Land Size", "Household Size", "School Level", "Rodents", "Wild Animals",
                     "CGSOL:Rodents", "CGSOL:Wild Animals", "HSOL:Rodents", "HSOL:Wild Animals",
                     "Vanilla:Rodents", "Vanilla:Wild Animals"), 3)

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
View(meta_df)

# plots metafor-----

meta_df_people <- meta_df |> 
  slice(1:8)


meta_df_people$Variable <- factor(
  meta_df_people$Variable,
  levels = rev(c(
    "HSOL", "CGSOL", "Vanilla Farmer", "Land Size", 
    "Household Size", "School Level", "Gender[Man]", "Age"
  ))
)

meta_df_animal <- meta_df |> 
  slice(9:16)

meta_df_animal$Variable <- factor(
  meta_df_animal$Variable,
  levels = rev(c(
    "Rodent", "Wild Animal", "Vanilla:Rodent","Vanilla:Wild Animal", 
    "HSOL:Rodent","HSOL:Wild Animal",
    "CGSOL:Rodent", "CGSOL:Wild Animal"
  ))
)



people_plot_pooled <- ggplot(meta_df_people, aes(x = Estimate, y = Variable)) + 
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


animal_plot_pooled <- ggplot(meta_df_animal, aes(x = Estimate, y = Variable)) + 
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

ggarrange(people_plot_pooled,
          animal_plot_pooled, 
          labels = c("a", "b"), common.legend = TRUE)


#plotting with village included ----

summary_df_people<-summary_df |> 
  filter(!str_detect(Variable, "odent|ild|ges")) |> 
  mutate(lower_ci_95 = Estimate - 1.96 * `Std. Error`,
         upper_ci_95 = Estimate + 1.96 * `Std. Error`,
         upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
         lower_ci_90 = Estimate - 1.644854 * `Std. Error`,
  ) 

# Set factor levels for 'term' in summary_df_high_risk_no_cluster
summary_df_people$Variable <- factor(summary_df_people$Variable, 
                                     levels = rev(c("HSOL", "CGSOL", "Vanilla Farmer", "Land Size", 
                                                    "Household Size", "School Level", "Gender[Male]", "Age")))


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




summary_df_animal<-summary_df |> 
  filter(str_detect(Variable, "odent|ild")) |> 
  mutate(lower_ci_95 = Estimate - 1.96 * `Std. Error`,
         upper_ci_95 = Estimate + 1.96 * `Std. Error`,
         upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
         lower_ci_90 = Estimate - 1.644854 * `Std. Error`,
  ) 

# Set factor levels for 'term' in summary_df_high_risk_no_cluster
summary_df_animal$Variable <- factor(summary_df_animal$Variable, 
                                     levels = rev(c("Rodents", "Wild Animals", "Vanilla:Rodents","Vanilla:Wild Animals", 
                                                    "HSOL:Rodents","HSOL:Wild Animals",
                                                    "CGSOL:Rodents", "CGSOL:Wild Animals")))


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
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_ampandrana_andatsakala_high_risk2.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_sarahandrano_high_risk2.RData")
load("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/Results/ergm_mandena_high_risk3.RData")


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
View(meta_df_high_risk)


# plots metafor-----

meta_df_high_risk_people <- meta_df_high_risk |> 
  slice(1:8)


meta_df_high_risk_people$Variable <- factor(
  meta_df_high_risk_people$Variable,
  levels = rev(c(
    "HSOL", "CGSOL", "Vanilla Farmer", "Land Size", 
    "Household Size", "School Level", "Gender[Man]", "Age"
  ))
)

meta_df_high_risk_animal <- meta_df_high_risk |> 
  slice(9:20)

meta_df_high_risk_animal$Variable <- factor(
  meta_df_high_risk_animal$Variable,
  levels = rev(c(
    "Domesticated (Uncommon)", "Rodent", "Wild Animal", 
    
    "Vanilla:Domesticated (Uncommon)", "Vanilla:Rodent","Vanilla:Wild Animal", 
    "HSOL:Domesticated (Uncommon)", "HSOL:Rodent","HSOL:Wild Animal",
    "CGSOL:Domesticated (Uncommon)", "CGSOL:Rodent", "CGSOL:Wild Animal"
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

summary_df_high_risk_people<-summary_df_high_risk|> 
  filter(!str_detect(Variable, "omestic|odent|ild|ges")) |> 
  mutate(lower_ci_95 = Estimate - 1.96 * `Std. Error`,
         upper_ci_95 = Estimate + 1.96 * `Std. Error`,
         upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
         lower_ci_90 = Estimate - 1.644854 * `Std. Error`,
  ) 

# Set factor levels for 'term' in summary_df_high_risk_no_cluster
summary_df_high_risk_people$Variable <- factor(summary_df_high_risk_people$Variable, 
                                     levels = rev(c("HSOL", "CGSOL", "Vanilla Farmer", "Land Size", 
                                                    "Household Size", "School Level", "Gender[Male]", "Age")))


high_risk_network_a<-ggplot(summary_df_high_risk_people, aes(x = Estimate, y = Variable, color = Village)) + 
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




summary_df_high_risk_animal<-summary_df_high_risk |> 
  filter(str_detect(Variable, "omestic|odent|ild")) |> 
  mutate(lower_ci_95 = Estimate - 1.96 * `Std. Error`,
         upper_ci_95 = Estimate + 1.96 * `Std. Error`,
         upper_ci_90 = Estimate + 1.644854 * `Std. Error`,
         lower_ci_90 = Estimate - 1.644854 * `Std. Error`,
  ) 

# Set factor levels for 'term' in summary_df_high_risk_no_cluster
summary_df_high_risk_animal$Variable <- factor(summary_df_high_risk_animal$Variable, 
                                     levels = rev(c("Domesticated (Uncommon)", "Rodents", "Wild Animals", 
                                                    "Vanilla:Domesticated (Uncommon)", "Vanilla:Rodents","Vanilla:Wild Animals", 
                                                    "HSOL:Domesticated (Uncommon)", "HSOL:Rodents","HSOL:Wild Animals",
                                                    "CGSOL:Domesticated (Uncommon)",  "CGSOL:Rodents", "CGSOL:Wild Animals")))


high_risk_network_b<-ggplot(summary_df_high_risk_animal, aes(x = Estimate, y = Variable, color = Village)) + 
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


ggarrange(high_risk_network_a,
          high_risk_network_b, 
          labels = c("a", "b"), common.legend = TRUE)


