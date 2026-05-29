library(tidyverse);library(metafor);library(ggpubr); library(grid); library(gridExtra) 
# source("~/Desktop/human_animal_networks/self_reported_networks/pooling effect sizes self-report.R")
summary_df <- read.csv("~/Desktop/human_animal_networks/gps_networks/outputs/summary_df_spatial.csv")


pool_function <- function(variable){
  
  model_df <- summary_df %>% select(Estimate, `Std..Error`, Variable, Village) %>% filter(Variable==variable)
  
  metaanalysis_model <- rma(yi = model_df$Estimate,   
                            sei = model_df$`Std..Error`, 
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
name_zebu_pr <- pool_function(variable ="Zebu")
name_hsol_zebu_pr <- pool_function(variable ="HSOL:Zebu")
name_vanilla_zebu_pr <- pool_function(variable ="Vanilla:Zebu")
name_cgsol_zebu_pr <- pool_function(variable ="CGSOL:Zebu")
name_animal_sex_pr <- pool_function(variable ="Animal Sex[Male]")


meta_df <- as.data.frame(rbind(name_age_pr,
                               name_sex_pr,
                               name_vanilla_pr,
                               name_ls_pr,
                               name_hs_pr,
                               name_sl_pr,
                               name_cgsol_pr,
                               name_hsol_pr,
                               name_zebu_pr,
                               name_hsol_zebu_pr,
                               name_vanilla_zebu_pr,
                               name_cgsol_zebu_pr,
                               name_animal_sex_pr))


colnames(meta_df) <- c("Estimate", "Lower", "Upper", "Pr(>|z|)","SE", "N")

meta_df$Variable <- c("Age", "Gender[Man]", "Vanilla", "Land Size", "Household Size", "School Level",  "Goods", "House Mat.", "Cattle", "House Mat.:Cattle", "Vanilla:Cattle", "Goods:Cattle", "Animal Sex[Male]")



meta_df$lower_ci <- meta_df$Estimate - 1.96 * meta_df$SE
meta_df$upper_ci <- meta_df$Estimate + 1.96 * meta_df$SE

meta_df$lower_ci_90 <- meta_df$Estimate - qnorm(0.95) * meta_df$SE
meta_df$upper_ci_90 <- meta_df$Estimate + qnorm(0.95) * meta_df$SE


meta_df_filtered <- meta_df %>% filter(Variable !="Edges", Variable !="Cattle", Variable !="House Mat.:Cattle", Variable !="Goods:Cattle", Variable !="Vanilla:Cattle", Variable !="Degree 1",Variable !="Degree 2",Variable !="Degree 3",Variable !="Degree 4", Variable !="Animal Sex[Male]")

meta_df_filtered$Variable <- factor(meta_df_filtered$Variable, levels = c("Age", "Gender[Man]", "School Level", "Household Size", "Land Size", "Vanilla", "Goods", "House Mat."))

people_plot_pooled <- ggplot(meta_df_filtered, aes(x = Estimate, y = Variable)) + 
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
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")


meta_df_filtered2 <- meta_df %>% filter(Variable =="Cattle"| Variable =="House Mat.:Cattle"| Variable =="Goods:Cattle"| Variable =="Vanilla:Cattle"| Variable =="Animal Sex[Male]")
meta_df_filtered2$Variable <- factor(meta_df_filtered2$Variable, levels = c("Animal Sex[Male]", "Goods:Cattle", "House Mat.:Cattle", "Vanilla:Cattle", "Cattle"))

animals_plot_pooled <- ggplot(meta_df_filtered2, aes(x = Estimate, y = Variable)) + 
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
  # Colorblind-friendly palette from viridis
  scale_color_viridis_d(option = "D", end = 0.8) +  
  # Theme adjustments
  theme(
    legend.position = "right", 
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")

ggarrange(people_plot_pooled, animals_plot_pooled, labels = c("a", "b"), common.legend = TRUE)

### SELF REPORTED----
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_full_village_a.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_full_village_m.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_full_village_s.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_hr_village_a.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_hr_village_m.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_hr_village_s.RData")

build_summary_df <- function(ergm_list, term_labels) {
  bind_rows(lapply(names(ergm_list), function(vname) {
    coef_df <- as.data.frame(
      summary(ergm_list[[vname]])$coefficients
    ) |>
      rownames_to_column("raw_term") |>
      filter(!grepl("degree", raw_term))
    
    if (nrow(coef_df) != length(term_labels))
      stop("Term count mismatch for village: ", vname,
           " (", nrow(coef_df), " vs ", length(term_labels), ")")
    
    coef_df$Variable <- term_labels
    coef_df$Village  <- vname
    coef_df
  })) |>
    relocate(Variable, Village, .before = Estimate)
}

# ------------------------------------------------------------------
# pool_one() / pool_all(): fixed-effects meta-analysis.
# ------------------------------------------------------------------
pool_one <- function(summary_df, variable) {
  d   <- summary_df |> filter(Variable == variable)
  fit <- rma(yi = d$Estimate, sei = d$`Std. Error`, method = "FE")
  data.frame(Variable = variable, Estimate = as.numeric(fit$beta),
             Lower = fit$ci.lb, Upper = fit$ci.ub,
             `Pr(>|z|)` = fit$pval, SE = fit$se,
             N = nrow(d), check.names = FALSE)
}

pool_all <- function(summary_df) {
  vars <- setdiff(unique(summary_df$Variable), "Edges")
  bind_rows(lapply(vars, \(v) pool_one(summary_df, v))) |>
    mutate(lower_ci    = Estimate - 1.96        * SE,
           upper_ci    = Estimate + 1.96        * SE,
           lower_ci_90 = Estimate - qnorm(0.95) * SE,
           upper_ci_90 = Estimate + qnorm(0.95) * SE)
}

# ------------------------------------------------------------------
# Plotting helpers
# ------------------------------------------------------------------
coef_plot <- function(df, var_levels, color_var = NULL) {
  df$Variable <- factor(df$Variable, levels = rev(var_levels))
  aes_base <- if (!is.null(color_var))
    aes(x = Estimate, y = Variable, color = .data[[color_var]])
  else
    aes(x = Estimate, y = Variable)
  
  ggplot(df, aes_base) +
    geom_point(position = position_dodge(0.5), size = 3) +
    geom_linerange(aes(xmin = lower_ci,    xmax = upper_ci),
                   position = position_dodge(0.5), linewidth = 0.4) +
    geom_linerange(aes(xmin = lower_ci_90, xmax = upper_ci_90),
                   position = position_dodge(0.5), linewidth = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    labs(x = "Estimated Effect", y = NULL) +
    theme_classic() +
    theme(legend.position = "top", axis.title.x = element_text(size = 12))
}

rename_vars <- function(df) {
  df |> mutate(Variable = str_replace_all(
    Variable, c("CGSOL" = "Goods", "HSOL" = "House Mat.",
                "Gender\\[Male\\]" = "Gender[Man]",
                "Rodents" = "Rodent")))
}

rename_villages <- function(df) {
  df |> mutate(Village = recode(Village,
                                "Ampandrana"   = "A",
                                "Mandena"      = "M",
                                "Sarahandrano" = "S"))
}

add_ci_cols <- function(df, se_col = "SE") {
  df |> mutate(
    lower_ci    = Estimate - 1.96        * .data[[se_col]],
    upper_ci    = Estimate + 1.96        * .data[[se_col]],
    lower_ci_90 = Estimate - qnorm(0.95) * .data[[se_col]],
    upper_ci_90 = Estimate + qnorm(0.95) * .data[[se_col]]
  )
}

people_levels <- c("House Mat.", "Goods", "Vanilla", "Land Size",
                   "Household Size", "School Level", "Gender[Man]", "Age")



full_term_labels <- c(
  "Edges", "Age", "Gender[Male]", "CGSOL", "HSOL", "Vanilla",
  "Land Size", "Household Size", "School Level",
  "Rodents", "Wild",
  "CGSOL:Rodents", "CGSOL:Wild", "HSOL:Rodents", "HSOL:Wild",
  "Vanilla:Rodents", "Vanilla:Wild"
)

summary_df_full <- build_summary_df(
  list(Ampandrana = ergm_full_amp, Mandena = ergm_full_man,
       Sarahandrano = ergm_full_sar),
  full_term_labels
)

summary(ergm_full_amp)
summary(ergm_full_man)
summary(ergm_full_sar)


meta_df_full <- pool_all(summary_df_full)
print(meta_df_full)

animal_levels_full <- c("Rodent", "Wild",
                        "Vanilla:Rodent",    "Vanilla:Wild",
                        "House Mat.:Rodent", "House Mat.:Wild",
                        "Goods:Rodent",      "Goods:Wild")

### Pooled Full Self Reported----
meta_full_r <- meta_df_full |> rename_vars()
ggarrange(
  meta_full_r |> filter(Variable %in% people_levels) |>
    coef_plot(people_levels),
  meta_full_r |> filter(Variable %in% animal_levels_full) |>
    coef_plot(animal_levels_full),
  labels = c("a", "b"), common.legend = TRUE
)

people_plot_pooled_self_report <- meta_full_r |> 
  filter(Variable %in% people_levels) |>
  coef_plot(people_levels) +
  theme(axis.title.x = element_blank())

animal_plot_pooled_self_report <- meta_full_r |> 
  filter(Variable %in% animal_levels_full) |>
  coef_plot(animal_levels_full) +
  theme(axis.title.x = element_blank())


## HIgh risk self report----

hr_term_labels <- c(
  "Edges", "Age", "Gender[Male]", "CGSOL", "HSOL", "Vanilla",
  "Land Size", "Household Size", "School Level",
  "Other Dom.", "Rodents", "Wild",
  "CGSOL:Other Dom.", "CGSOL:Rodents", "CGSOL:Wild",
  "HSOL:Other Dom.",  "HSOL:Rodents",  "HSOL:Wild",
  "Vanilla:Other Dom.", "Vanilla:Rodents", "Vanilla:Wild"
)

summary_df_hr <- build_summary_df(
  list(Ampandrana = ergm_hr_amp, Mandena = ergm_hr_man,
       Sarahandrano = ergm_hr_sar),
  hr_term_labels
)

summary(ergm_hr_amp)
summary(ergm_hr_man)
summary(ergm_hr_sar)

meta_df_hr <- pool_all(summary_df_hr)
print(meta_df_hr)

animal_levels_hr <- c(
  "Other Dom.", "Rodent", "Wild",
  "Vanilla:Other Dom.", "Vanilla:Rodent",    "Vanilla:Wild",
  "House Mat.:Other Dom.", "House Mat.:Rodent", "House Mat.:Wild",
  "Goods:Other Dom.",   "Goods:Rodent",      "Goods:Wild"
)

# Pooled
meta_hr_r <- meta_df_hr |> rename_vars()
ggarrange(
  meta_hr_r |> filter(Variable %in% people_levels) |>
    coef_plot(people_levels),
  meta_hr_r |> filter(Variable %in% animal_levels_hr) |>
    coef_plot(animal_levels_hr),
  labels = c("a", "b"), common.legend = TRUE
)

people_plot_high_risk_pooled <- meta_hr_r |> 
  filter(Variable %in% people_levels) |>
  coef_plot(people_levels)

animal_plot_high_risk_pooled <- meta_hr_r |> 
  filter(Variable %in% animal_levels_hr) |>
  coef_plot(animal_levels_hr)



# ggsave("combined_plot.png",
#        plot = ggarrange(people_plot_pooled + theme(axis.title.x = element_blank()), animals_plot_pooled+ theme(axis.title.x = element_blank()),
#                  people_plot_pooled_self_report+ theme(axis.title.x = element_blank()), animal_plot_pooled_self_report+ theme(axis.title.x = element_blank()), 
#                  people_plot_high_risk_pooled, animal_plot_high_risk_pooled,
#                  ncol = 2, nrow = 3,
#                  labels = c("a", "b", "c", "d", "e", "f"), 
#                  common.legend = TRUE),
#        width = 10,       # in inches
#        height = 12,      # in inches
#        dpi = 300)        # resolution for publication


library(ggpubr)
library(grid)   
library(cowplot)

# Header row (spans correctly above each column)
header <- plot_grid(
  ggdraw() + draw_label("Sociodemographic Variables", fontface = "bold", size = 13),
  ggdraw() + draw_label("Animal Variables",           fontface = "bold", size = 13),
  ncol = 2
)

# Row labels
row_labels <- plot_grid(
  ggdraw() + draw_label("Spatial",                 fontface = "bold", size = 11, angle = 90),
  ggdraw() + draw_label("Full Self-Reported",      fontface = "bold", size = 11, angle = 90),
  ggdraw() + draw_label("High-Risk Self-Reported", fontface = "bold", size = 11, angle = 90),
  ncol = 1, nrow = 3
)

# Main panels (ggarrange handles the 2x3 grid)
combined <- ggarrange(
  people_plot_pooled             + theme(axis.title.x = element_blank()),
  animals_plot_pooled            + theme(axis.title.x = element_blank()),
  people_plot_pooled_self_report + theme(axis.title.x = element_blank()),
  animal_plot_pooled_self_report + theme(axis.title.x = element_blank()),
  people_plot_high_risk_pooled,
  animal_plot_high_risk_pooled,
  ncol = 2, nrow = 3,
  labels = c("a", "b", "c", "d", "e", "f"),
  common.legend = FALSE
)

# Attach row labels to the left of the panels
panels_with_labels <- plot_grid(
  row_labels, combined,
  ncol = 2,
  rel_widths = c(0.06, 1)
)

# Stack header on top
final_plot <- plot_grid(
  header,
  panels_with_labels,
  ncol = 1,
  rel_heights = c(0.04, 1)
)

# ggsave("combined_plot_annotated_May_29_2026.jpeg",
#        plot = final_plot,
#        width = 10,
#        height = 12,
#        dpi = 300,
#        bg = "white")

