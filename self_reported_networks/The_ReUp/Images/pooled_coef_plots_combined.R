library(tidyverse);library(metafor);library(ggpubr)
source("~/Desktop/human_animal_networks/self_reported_networks/The_ReUp/pooling effect sizes self-report.R")
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



ggsave("combined_plot.png",
       plot = ggarrange(people_plot_pooled + theme(axis.title.x = element_blank()), animals_plot_pooled+ theme(axis.title.x = element_blank()),
                 people_plot_pooled_self_report+ theme(axis.title.x = element_blank()), animal_plot_pooled_self_report+ theme(axis.title.x = element_blank()), 
                 people_plot_high_risk_pooled, animal_plot_high_risk_pooled,
                 ncol = 2, nrow = 3,
                 labels = c("a", "b", "c", "d", "e", "f"), 
                 common.legend = TRUE),
       width = 10,       # in inches
       height = 12,      # in inches
       dpi = 300)        # resolution for publication


