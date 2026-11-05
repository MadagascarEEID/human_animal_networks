# =====================================================================
# Predicted-probability curves + pooled coefficients for
#   spatial (GPS) | full self-reported | high-risk self-reported ERGMs
# =====================================================================

# loading ergm model objects ----
## spatial
load("/Users/levkolinski/Downloads/ergm_outputs (1)/ergm_sara2.RData")
load("/Users/levkolinski/Downloads/ergm_outputs (1)/ergm_mandena2.RData")
load("/Users/levkolinski/Downloads/ergm_outputs (1)/ergm_anda2.RData")

## self-reported
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_full_village_a_nexus.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_full_village_m_nexus.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_full_village_s_nexus.RData")

load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_hr_village_a_nexus.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_hr_village_m_nexus.RData")
load("/Users/levkolinski/Desktop/human_animal_networks/self_reported_networks/Results/ergm_hr_village_s_nexus.RData")

# loading packages ----
library(ergm)
library(MASS)
library(tidyverse)
library(ggpubr)
library(metafor)
library(writexl)

set.seed(24)
n_draws <- 1000
n_grid  <- 30
chunk   <- 5000   # rows of the dyad matrix processed at once (spatial nets are large)

# model list ----
model_list <- list(
  spatial = list(Ampandrana   = ergm_anda,
                 Mandena      = ergm_mandena,
                 Sarahandrano = ergm_sara),
  full = list(Ampandrana   = ergm_full_amp,
              Mandena      = ergm_full_man,
              Sarahandrano = ergm_full_sar),
  hr   = list(Ampandrana   = ergm_hr_amp,
              Mandena      = ergm_hr_man,
              Sarahandrano = ergm_hr_sar)
)

# per-network term names ----
self_rep_cov <- c(`Age`            = "b2cov.age",
                  `Gender (man)`   = "b2factor.gender.Male",
                  `School level`   = "b2cov.school_level")

self_rep_wealth <- c(`Land size`      = "b2cov.land_size",
                     Goods        = "b2cov.commercial_goods",
                     `House Mat.` = "b2cov.house_sol",
                     Vanilla      = "b2factor.grew_vanilla.1")

net_spec <- list(
  spatial = list(
    wealth     = c(`Land size`      = "b1cov.land",
                    Goods        = "b1cov.cgsol",
                   `House Mat.` = "b1cov.hsol",
                   Vanilla      = "b1factor.vanilla.Yes"),
    covariates = c(`Age`               = "b1cov.age",
                   `Gender (man)`      = "b1factor.gender.Male",
                   `School level`      = "b1cov.school",
                   `Animal sex (male)` = "b2factor.sex.Male"),
    cat_prefix = "b2factor.species.",
    ref        = "not_cattle"),
  full = list(wealth = self_rep_wealth, covariates = self_rep_cov,
              cat_prefix = "b1factor.animal_category.", ref = "domesticated"),
  hr   = list(wealth = self_rep_wealth, covariates = self_rep_cov,
              cat_prefix = "b1factor.animal_category.", ref = "domesticated_1")
)

is_binary <- function(term) grepl("factor\\.", term)

# levels / colors / labels ----
cat_labels <- c(all            = "All animals (average)",
                Zebu           = "Cattle (zebu)",
                not_cattle     = "Non-cattle animals",
                domesticated   = "Domesticated",
                domesticated_1 = "Domesticated 1 (poultry, pigs, cattle)",
                domesticated_2 = "Domesticated 2 (dogs, goats/sheep, cats)",
                rodent         = "Rodents",
                wild           = "Wild")

cat_colors <- c("Cattle (zebu)"                            = "#7b3fb5",  # purple
                "Non-cattle animals"                       = "#009e9e",  # teal
                "Domesticated"                             = "#2a78d6",
                "Domesticated 1 (poultry, pigs, cattle)"   = "#2a78d6",
                "Domesticated 2 (dogs, goats/sheep, cats)" = "#eb6834",
                "Rodents"                                  = "#1baf7a",
                "Wild"                                     = "#eda100",
                "All animals (average)"                    = "#444444")


cat_lines  <- c("Cattle (zebu)"                            = "solid",
                "Non-cattle animals"                       = "dotted",
                "Domesticated"                             = "solid",
                "Domesticated 1 (poultry, pigs, cattle)"   = "solid",
                "Domesticated 2 (dogs, goats/sheep, cats)" = "longdash",
                "Rodents"                                  = "dotdash",
                "Wild"                                     = "dashed",
                "All animals (average)"                    = "solid")

# helper functions ----

# dyad-level change-statistic matrix, columns in coef order
dyad_matrix <- function(fit) {
  nw <- fit$network
  f  <- fit$formula
  f[[2]] <- as.name("nw")
  environment(f) <- environment()
  P <- ergmMPLE(f, output = "dyadlist")$predictor
  stopifnot(all(names(coef(fit)) %in% colnames(P)))
  P[, names(coef(fit)), drop = FALSE]
}

# animal category of each dyad, read off the category main-effect columns
dyad_category <- function(X, prefix, ref) {
  cn <- colnames(X)
  cat_cols <- cn[startsWith(cn, prefix) & !grepl(":", cn, fixed = TRUE)]
  out <- rep(ref, nrow(X))
  for (cc in cat_cols) out[X[, cc] == 1] <- sub(prefix, "", cc, fixed = TRUE)
  out
}

# number of animal nodes = number of nodes in the non-human mode
n_animal_nodes <- function(fit, spec) {
  nw <- fit$network
  n1 <- nw %n% "bipartite"
  human_mode1 <- startsWith(spec$wealth[[1]], "b1")
  if (human_mode1) network.size(nw) - n1 else n1
}

# set a wealth term to new values and rebuild every interaction that
# contains it -- works for both "cat:wealth" and "wealth:cat" ordering
set_wealth <- function(X, term, new_vals) {
  X[, term] <- new_vals
  for (ic in grep(":", colnames(X), fixed = TRUE, value = TRUE)) {
    parts <- strsplit(ic, ":", fixed = TRUE)[[1]]
    if (!term %in% parts) next
    other <- setdiff(parts, term)
    stopifnot(length(other) == 1, other %in% colnames(X))
    X[, ic] <- X[, other] * new_vals
  }
  X
}

# mean predicted probability (or difference X1 - X0) by category and overall,
# for the point estimate + every draw; processed in chunks to limit memory
pp_group_means <- function(X1, B, cats, X0 = NULL, chunk = 5000) {
  tB  <- t(B)
  lev <- sort(unique(cats))
  S   <- matrix(0, length(lev), nrow(B), dimnames = list(lev, NULL))
  for (r in split(seq_len(nrow(X1)), ceiling(seq_len(nrow(X1)) / chunk))) {
    P <- plogis(X1[r, , drop = FALSE] %*% tB)
    if (!is.null(X0)) P <- P - plogis(X0[r, , drop = FALSE] %*% tB)
    s <- rowsum(P, cats[r])
    S[rownames(s), ] <- S[rownames(s), ] + s
  }
  rbind(S / as.numeric(table(cats)[lev]),
        all = colSums(S) / length(cats))
}

summarise_draws <- function(M, extra = NULL) {
  tibble(category = rownames(M),
         estimate = M[, 1],
         se       = apply(M[, -1, drop = FALSE], 1, sd)) |>
    bind_cols(extra)
}

# predicted-probability curve by category, one village
curve_one <- function(p, term, grid) {
  map_dfr(grid, \(g)
          pp_group_means(set_wealth(p$X, term, rep(g, nrow(p$X))), p$B, p$cats) |>
            summarise_draws(tibble(value = g)))
}

# change in predicted probability (+1 SD, or 0 -> 1 for binary), one village
category_contrast <- function(p, term) {
  X <- p$X
  if (is_binary(term)) {
    X1 <- set_wealth(X, term, rep(1, nrow(X)))
    X0 <- set_wealth(X, term, rep(0, nrow(X)))
  } else {
    X0 <- X
    X1 <- set_wealth(X, term, X[, term] + 1)
  }
  D  <- pp_group_means(X1, p$B, p$cats, X0 = X0)   # absolute change
  P0 <- pp_group_means(X0, p$B, p$cats)            # baseline probability
  R  <- D / P0                                     # relative change
  sd_rows <- \(M) apply(M[, -1, drop = FALSE], 1, sd)
  
  summarise_draws(D) |>
    mutate(base = P0[, 1], base_se = sd_rows(P0),
           rel  = R[, 1],  rel_se  = sd_rows(R))
}


# prep: change stats and coefficient draws, once per model ----
prep <- imap(model_list, function(mods, net) {
  spec <- net_spec[[net]]
  imap(mods, function(fit, vil) {
    message("Preparing: ", net, " | ", vil)
    X <- dyad_matrix(fit)
    list(X         = X,
         cats      = dyad_category(X, spec$cat_prefix, spec$ref),
         B         = rbind(coef(fit), mvrnorm(n_draws, coef(fit), vcov(fit))),
         n_animals = n_animal_nodes(fit, spec))
  })
})

# sanity check: category counts per village
iwalk(prep, \(vs, net) iwalk(vs, \(p, vil) {
  cat(net, vil, "| animals:", p$n_animals, "\n"); print(table(p$cats))
}))

# contrasts (by category and overall) ----
contrasts_by_village <- map_dfr(names(prep), \(net) {
  wt <- net_spec[[net]]$wealth
  map_dfr(names(wt), \(w) imap_dfr(prep[[net]], \(p, vil)
                                   category_contrast(p, wt[[w]]) |>
                                     mutate(network = net, village = vil, wealth = w,
                                            n_animals = p$n_animals)))
})

pool_fe <- function(d) {
  fe <- \(y, s) rma(yi = y, sei = s, method = "FE")
  f  <- fe(d$estimate, d$se)
  fb <- fe(d$base,     d$base_se)
  fr <- fe(d$rel,      d$rel_se)
  tibble(pp       = 100 * as.numeric(f$beta),  lo      = 100 * f$ci.lb,  hi      = 100 * f$ci.ub,
         p        = f$pval, I2 = f$I2,
         base_pct = 100 * as.numeric(fb$beta), base_lo = 100 * fb$ci.lb, base_hi = 100 * fb$ci.ub,
         rel_pct  = 100 * as.numeric(fr$beta), rel_lo  = 100 * fr$ci.lb, rel_hi  = 100 * fr$ci.ub)
}

contrasts_pooled <- contrasts_by_village |>
  group_by(network, wealth, category) |>
  group_modify(~ pool_fe(.x)) |>
  ungroup() |>
  mutate(category = cat_labels[category])

contrasts_pooled |> print(n = Inf)

write.csv(contrasts_pooled, "predprob_contrasts_pooled_wland.csv",
          row.names = FALSE)

# change in expected number of animal partners per person
# (= n_animals x mean change across all pairs; n_animals differs by network/village)
degree_change_pooled <- contrasts_by_village |>
  filter(category == "all") |>
  mutate(estimate = estimate * n_animals, se = se * n_animals) |>
  group_by(network, wealth) |>
  group_modify(~ {
    f <- rma(yi = .x$estimate, sei = .x$se, method = "FE")
    tibble(d_degree = as.numeric(f$beta), lo = f$ci.lb, hi = f$ci.ub,
           p = f$pval, I2 = f$I2)
  }) |> ungroup()
degree_change_pooled

# curves: grids, per village, pooled pointwise ----
grid_for <- function(net, term) {
  if (is_binary(term)) return(c(0, 1))
  vals <- unlist(map(prep[[net]], ~ .x$X[, term]))
  q <- quantile(vals, c(0.05, 0.95), na.rm = TRUE)
  seq(q[1], q[2], length.out = n_grid)
}

curves_by_village <- map_dfr(names(prep), function(net) {
  wt <- net_spec[[net]]$wealth
  map_dfr(names(wt), function(w) {
    term <- wt[[w]]
    grid <- grid_for(net, term)
    imap_dfr(prep[[net]], function(p, vil) {
      message("Curves: ", net, " | ", vil, " | ", w)
      curve_one(p, term, grid) |>
        mutate(network = net, village = vil, wealth = w)
    })
  })
})

village_w <- contrasts_by_village |>
  transmute(network, wealth, category, village, w = 1 / se^2)

curves_pooled <- curves_by_village |>
  left_join(village_w, by = c("network", "wealth", "category", "village")) |>
  group_by(network, wealth, category, value) |>
  summarise(Estimate = sum(w * estimate) / sum(w),
            SE       = sqrt(sum(w^2 * se^2)) / sum(w),
            .groups  = "drop") |>
  mutate(Lower = pmax(0, Estimate - 1.96 * SE),
         Upper = pmin(1, Estimate + 1.96 * SE),
         category = factor(cat_labels[category], levels = cat_labels))

sig_levels <- c("p < 0.05", "p < 0.1", "p ≥ 0.1")
sig_lines  <- c("p < 0.05" = "solid", "p < 0.1" = "longdash", "p ≥ 0.1" = "dotted")

sig_lookup <- contrasts_pooled |>
  transmute(network, wealth,
            category = factor(category, levels = cat_labels),
            sig = factor(case_when(p < 0.05 ~ "p < 0.05",
                                   p < 0.1  ~ "p < 0.1",
                                   TRUE     ~ "p ≥ 0.1"),
                         levels = sig_levels))

curves_pooled <- curves_pooled |>
  select(-any_of(c("sig", "sig.x", "sig.y"))) |>  
  left_join(sig_lookup, by = c("network", "wealth", "category"))

# plotting ----
theme_curves <- theme_classic() +
  theme(strip.background = element_blank(),
        strip.text       = element_text(face = "bold"),
        legend.position  = "bottom",
        legend.title     = element_blank(),
        panel.spacing    = unit(1.2, "lines"))

# panel a: pooled log-odds coefficients for non-wealth covariates
pool_coef <- function(net) {
  terms <- net_spec[[net]]$covariates
  imap_dfr(terms, function(term, lab) {
    est <- map_dbl(model_list[[net]], ~ coef(.x)[[term]])
    se  <- map_dbl(model_list[[net]], ~ sqrt(vcov(.x)[term, term]))
    f   <- rma(yi = est, sei = se, method = "FE")
    b   <- as.numeric(f$beta)
    tibble(Variable = lab, Estimate = b, SE = f$se, p = f$pval,
           lower_ci = f$ci.lb, upper_ci = f$ci.ub,
           lower_90 = b - qnorm(0.95) * f$se,
           upper_90 = b + qnorm(0.95) * f$se)
  })
}

coef_panel <- function(net) {
  pool_coef(net) |>
    mutate(Variable = factor(Variable,
                             levels = rev(names(net_spec[[net]]$covariates)))) |>
    ggplot(aes(x = Estimate, y = Variable)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_linerange(aes(xmin = lower_ci, xmax = upper_ci), linewidth = 0.4) +
    geom_linerange(aes(xmin = lower_90, xmax = upper_90), linewidth = 1.2) +
    geom_point(size = 2) +
    labs(x = "Estimated effect (log-odds)", y = NULL) +
    theme_classic()
}
# panel b: predicted-probability curves (continuous) + vanilla (binary)

predprob_panels <- function(net) {
  d    <- filter(curves_pooled, network == net) |> droplevels()
  cont <- filter(d, wealth != "Vanilla")
  van  <- filter(d, wealth == "Vanilla") |>
    mutate(grew  = factor(ifelse(value == 1, "Yes", "No"), levels = c("No", "Yes")),
           panel = "Vanilla")
  is_all <- \(x) startsWith(as.character(x), "All animals")
  dodge  <- position_dodge(0.3)
  ylim   <- if (net == "spatial") c(0, 0.15) else if (net == "spatial_15") c(0, 0.25) else if (net == "spatial_5") c(0,0.1) else c(0,1) 
  
  p_cont <- ggplot(cont, aes(x = value, y = Estimate, color = category,
                             fill = category, group = category)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15,
                color = NA, show.legend = FALSE) +
    geom_line(aes(linetype = sig, linewidth = is_all(category))) +
    scale_linewidth_manual(values = c(`FALSE` = 0.8, `TRUE` = 1.4), guide = "none") +
    scale_color_manual(values = cat_colors) +
    scale_fill_manual(values = cat_colors, guide = "none") +
    scale_linetype_manual(values = sig_lines, drop = FALSE) +
    facet_wrap(~ wealth, scales = "free_x") +
    coord_cartesian(ylim = ylim) +
    labs(x = "Wealth (SD from village mean)",
         y = "Predicted probability of an edge") +
    guides(color    = guide_legend(nrow = 2, order = 1),
           linetype = "none") +
    theme_curves
  
  p_van <- ggplot(van, aes(x = grew, y = Estimate, color = category,
                           group = category)) +
    geom_line(aes(linetype = sig, linewidth = is_all(category)), position = dodge) +
    scale_linewidth_manual(values = c(`FALSE` = 0.5, `TRUE` = 1.0), guide = "none") +
    geom_pointrange(aes(ymin = Lower, ymax = Upper), size = 0.4,
                    linetype = "solid", position = dodge) +
    scale_color_manual(values = cat_colors) +
    scale_linetype_manual(values = sig_lines, drop = FALSE, guide = "none") +
    facet_wrap(~ panel) +
    coord_cartesian(ylim = ylim) +
    labs(x = "Grew vanilla", y = NULL) +
    theme_curves
  
  list(cont = p_cont, van = p_van)
}

# one network: a (coefficients) | b (predicted probabilities)
fig_combined <- function(net, labs = c("a", "b")) {
  pp <- predprob_panels(net)
  ggarrange(coef_panel(net), pp$cont, pp$van,
            nrow = 1, widths = c(1.15, 2, 1),
            labels = c(labs, ""),
            common.legend = TRUE, legend = "bottom",
            legend.grob = get_legend(pp$cont))
}

fig_combined("spatial")
fig_combined("full")
fig_combined("hr")


## sig legend ----
key_df <- expand_grid(sig = factor(sig_levels, levels = sig_levels), x = 1:2) |>
  mutate(y = 1)

sig_key <- get_legend(
  ggplot(key_df, aes(x, y, linetype = sig, group = sig)) +
    geom_line(color = "gray30", linewidth = 0.8) +
    scale_linetype_manual(values = sig_lines) +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.key.width = unit(2, "lines"))
)

row_lab <- function(p, lab) {
  annotate_figure(p, left = text_grob(lab, rot = 90, face = "bold", size = 12))
}

col_header <- ggarrange(
  text_grob("Covariate effects\n(pooled log-odds)",
            face = "bold", size = 13),
  text_grob("Predicted edge probabilities across wealth indicators",
            face = "bold", size = 13),
  nrow = 1, widths = c(1.15, 3)          # matches widths in fig_combined(): 1.15 | 2 + 1
) |> row_lab(" ")                        # blank row label so the header lines up with the rows

fig_all <- function() {
  ggarrange(col_header,
            row_lab(fig_combined("spatial", c("a", "b")), "Spatial Networks"),
            row_lab(fig_combined("full",    c("c", "d")), "Full Self-Reported Networks"),
            # row_lab(fig_combined("hr",      c("e", "f")), "High-Risk Self-Reported Networks"),
            as_ggplot(sig_key),
            ncol = 1, heights = c(0.12, 1, 1, 0.06))
}

fig_all()

fig_hr <- function() {
  ggarrange(col_header,
            row_lab(fig_combined("hr",      c("a", "b")), "High-Risk Self-Reported Networks"),
            as_ggplot(sig_key),
            ncol = 1, heights = c(0.12, 1, 0.06))
}

fig_hr()

ggsave("predicted_probs_hr_supp.png", fig_hr(),
       width = 12, height = 6, dpi = 300)

ggsave("predicted_probs_all_networks.pdf", fig_all(),
width = 12, height = 10, dpi = 300)

ggsave("predicted_probs_all_networks.png", fig_all(),
       width = 12, height = 10, dpi = 300)


## Saving pooled coefs in csv----
covariate_pooled <- map_dfr(names(model_list), \(net)
                            pool_coef(net) |> mutate(network = net, .before = 1)) |>
  mutate(OR    = exp(Estimate),
         OR_lo = exp(lower_ci),
         OR_hi = exp(upper_ci))

covariate_pooled |> print(n = Inf)
write.csv(covariate_pooled, "covariate_coefs_pooled.csv", row.names = FALSE)

## WRITING EXCEL SHEETS WITH MODEL ESTIMATES ----
# village-level: every coefficient from every model ----
# village_coefs <- imap_dfr(model_list, \(mods, net) imap_dfr(mods, \(fit, vil) {
#   s <- summary(fit)$coefficients
#   tibble(network  = net,
#          village  = vil,
#          term     = rownames(s),
#          estimate = s[, "Estimate"],
#          se       = s[, "Std. Error"],
#          mcmc_pct = if ("MCMC %" %in% colnames(s)) s[, "MCMC %"] else NA_real_,
#          z        = s[, "z value"],
#          p        = s[, "Pr(>|z|)"])
# })) |>
#   mutate(term      = fct_inorder(term),                     # keep model order
#          term_type = if_else(grepl("degree", term), "structural (degree)", "covariate"),
#          OR    = exp(estimate),
#          OR_lo = exp(estimate - 1.96 * se),
#          OR_hi = exp(estimate + 1.96 * se)) |> 
#   mutate(across(where(is.character), ~ str_replace_all(.x, "Ampandrana", "A")),
#          across(where(is.character), ~ str_replace_all(.x, "Mandena", "M")),
#          across(where(is.character), ~ str_replace_all(.x, "Sarahandrano", "S")))
# 
# # pooled: FE meta-analysis of each term across the villages that have it ----
# pooled_coefs <- village_coefs |>
#   filter(is.finite(estimate), is.finite(se)) |>            # drops -Inf boundary terms
#   group_by(network, term, term_type) |>
#   group_modify(~ {
#     f <- rma(yi = .x$estimate, sei = .x$se, method = "FE")
#     tibble(n_villages = nrow(.x),
#            villages   = paste(.x$village, collapse = "; "),
#            estimate   = as.numeric(f$beta), se = f$se,
#            lo = f$ci.lb, hi = f$ci.ub, p = f$pval,
#            I2 = if (nrow(.x) > 1) f$I2 else NA_real_)
#   }) |>
#   ungroup() |>
#   mutate(OR = exp(estimate), OR_lo = exp(lo), OR_hi = exp(hi)) |>
#   arrange(network, term) 
#          
# 
# # one pooled + one village sheet per network ----
# sheets <- list()
# for (net in names(model_list)) {
#   sheets[[paste0(net, "_pooled")]]  <- pooled_coefs  |> filter(network == net) |>
#     mutate(term = as.character(term))
#   sheets[[paste0(net, "_village")]] <- village_coefs |> filter(network == net) |>
#     arrange(term, village) |>
#     mutate(term = as.character(term))
# }
# 
# sheets[["notes"]] <- tibble(note = c(
#   "Estimates are log-odds from ERGMs fit separately in each village.",
#   "Pooled = fixed-effects inverse-variance meta-analysis (metafor::rma, method = 'FE') across villages.",
#   "OR = exp(estimate); OR CIs are exponentiated 95% CIs.",
#   "Terms with non-finite estimates or SEs (e.g., degree terms with zero observed counts, estimated at -Inf) are excluded from pooling but shown on village sheets.",
#   "Degree terms are structural controls and differ across villages; n_villages shows how many villages each pooled term is based on.",
#   "Wealth main effects are slopes for the reference animal category; interaction terms are differences in slope relative to the reference."
# ))
# 
# write_xlsx(sheets, "ergm_coefficients_all_networks.xlsx")


### SPATIAL NETWORK SENSITIVITY TESTING ----
load("/Users/levkolinski/Downloads/Sensitivity_tests/ergm_anda_sensitivity_5.RData")
ergm_anda_5 <- ergm_anda

load("/Users/levkolinski/Downloads/Sensitivity_tests/ergm_anda_sensitivity_15.RData")

load("/Users/levkolinski/Downloads/Sensitivity_tests/ergm_mandena_sensitivity_5.RData")
ergm_mandena_5 <- ergm_mandena

load("/Users/levkolinski/Downloads/Sensitivity_tests/ergm_mandena_sensitivity_15.RData")

load("/Users/levkolinski/Downloads/Sensitivity_tests/ergm_sara_sensitivity_5.RData")
ergm_sara_5 <- ergm_sara

load("/Users/levkolinski/Downloads/Sensitivity_tests/ergm_sara_sensitivity_15.RData")

# ### b1degree22 and 23 are infinite estimates from ergm_anda_5....dropping from fit model
# nw    <- fit$network
# 
# ctrl <- fit$control
# ctrl$init <- coef(fit)[is.finite(coef(fit))]   # start from the old estimates, minus 22/23
# or ctrl$init <- NULL to start fresh
# 
# ergm_anda_5_refit <- ergm(f_new, control = ctrl)
# 
# f_new <- update(fit$formula, nw ~ . - b1degree(22) - b1degree(23)) 
# environment(f_new) <- environment()
# 
# 
# stopifnot(all(is.finite(coef(ergm_anda_5_refit))),
#           all(is.finite(diag(vcov(ergm_anda_5_refit)))))
# 
# round(cbind(original = coef(fit)[names(coef(ergm_anda_5_refit))],
#             refit    = coef(ergm_anda_5_refit)), 4)       # should be near-identical
# 
# model_list_spatial_sensitivity$spatial_5$Ampandrana <- ergm_anda_5_refit
# save(ergm_anda_5_refit,
#      file = "/Users/levkolinski/Downloads/Sensitivity_tests/ergm_anda_sensitivity_5_refit.RData")

load("/Users/levkolinski/Downloads/Sensitivity_tests/ergm_anda_sensitivity_5_refit.RData")

# model list ----
model_list_spatial_sensitivity <- list(
  spatial_5  = list(Ampandrana   = ergm_anda_5_refit,
                    Mandena      = ergm_mandena_5,
                    Sarahandrano = ergm_sara_5),
  spatial_15 = list(Ampandrana   = ergm_anda,
                    Mandena      = ergm_mandena,
                    Sarahandrano = ergm_sara))


net_spec_spatial_sensitivity <- list(
  spatial_5 = list(
    wealth     = c(`Land size`      = "b1cov.land",
                   Goods        = "b1cov.cgsol",
                   `House Mat.` = "b1cov.hsol",
                   Vanilla      = "b1factor.vanilla.Yes"),
    covariates = c(`Age`               = "b1cov.age",
                   `Gender (man)`      = "b1factor.gender.Male",
                   `School level`      = "b1cov.school",
                   `Animal sex (male)` = "b2factor.sex.Male"),
    cat_prefix = "b2factor.species.",
    ref        = "not_cattle"),
  spatial_15 = list(
    wealth     = c(`Land size`      = "b1cov.land",
                   Goods        = "b1cov.cgsol",
                   `House Mat.` = "b1cov.hsol",
                   Vanilla      = "b1factor.vanilla.Yes"),
    covariates = c(`Age`               = "b1cov.age",
                   `Gender (man)`      = "b1factor.gender.Male",
                   `School level`      = "b1cov.school",
                   `Animal sex (male)` = "b2factor.sex.Male"),
    cat_prefix = "b2factor.species.",
    ref        = "not_cattle"))


# prep: change stats and coefficient draws, once per model ----
prep_spatial_sensitivity <- imap(model_list_spatial_sensitivity, function(mods, net) {
  spec <- net_spec_spatial_sensitivity[[net]]
  imap(mods, function(fit, vil) {
    message("Preparing: ", net, " | ", vil)
    X <- dyad_matrix(fit)
    list(X         = X,
         cats      = dyad_category(X, spec$cat_prefix, spec$ref),
         B         = rbind(coef(fit), mvrnorm(n_draws, coef(fit), vcov(fit))),
         n_animals = n_animal_nodes(fit, spec))
  })
})

# sanity check: category counts per village
iwalk(prep_spatial_sensitivity, \(vs, net) iwalk(vs, \(p, vil) {
  cat(net, vil, "| animals:", p$n_animals, "\n"); print(table(p$cats))
}))

# contrasts (by category and overall) ----
contrasts_by_village_spatial_sensitivity <- map_dfr(names(prep_spatial_sensitivity), \(net) {
  wt <- net_spec_spatial_sensitivity[[net]]$wealth
  map_dfr(names(wt), \(w) imap_dfr(prep_spatial_sensitivity[[net]], \(p, vil)
                                   category_contrast(p, wt[[w]]) |>
                                     mutate(network = net, village = vil, wealth = w,
                                            n_animals = p$n_animals)))
})


contrasts_pooled_spatial_sensitivity <- contrasts_by_village_spatial_sensitivity |>
  group_by(network, wealth, category) |>
  group_modify(~ pool_fe(.x)) |>
  ungroup() |>
  mutate(category = cat_labels[category])


write.csv(contrasts_pooled_spatial_sensitivity, "predprob_contrasts_pooled_spatial_sensitivity.csv",
          row.names = FALSE)

# change in expected number of animal partners per person
# (= n_animals x mean change across all pairs; n_animals differs by network/village)
degree_change_pooled_spatial_sensitivity <- contrasts_by_village_spatial_sensitivity |>
  filter(category == "all") |>
  mutate(estimate = estimate * n_animals, se = se * n_animals) |>
  group_by(network, wealth) |>
  group_modify(~ {
    f <- rma(yi = .x$estimate, sei = .x$se, method = "FE")
    tibble(d_degree = as.numeric(f$beta), lo = f$ci.lb, hi = f$ci.ub,
           p = f$pval, I2 = f$I2)
  }) |> ungroup()
degree_change_pooled_spatial_sensitivity

# curves: grids, per village, pooled pointwise ----
grid_for_sens <- function(net, term) {
  if (is_binary(term)) return(c(0, 1))
  vals <- unlist(map(prep_spatial_sensitivity[[net]], ~ .x$X[, term]))
  q <- quantile(vals, c(0.05, 0.95), na.rm = TRUE)
  seq(q[1], q[2], length.out = n_grid)
}

curves_by_village_spatial_sensitivity <- map_dfr(names(prep_spatial_sensitivity), function(net) {
  wt <- net_spec_spatial_sensitivity[[net]]$wealth
  map_dfr(names(wt), function(w) {
    term <- wt[[w]]
    grid <- grid_for_sens(net, term)
    imap_dfr(prep_spatial_sensitivity[[net]], function(p, vil) {
      message("Curves: ", net, " | ", vil, " | ", w)
      curve_one(p, term, grid) |>
        mutate(network = net, village = vil, wealth = w)
    })
  })
})

village_w_sens <- contrasts_by_village_spatial_sensitivity |>
  transmute(network, wealth, category, village, w = 1 / se^2)

curves_pooled_spatial_sensitivity <- curves_by_village_spatial_sensitivity |>
  left_join(village_w_sens, by = c("network", "wealth", "category", "village")) |>
  group_by(network, wealth, category, value) |>
  summarise(Estimate = sum(w * estimate) / sum(w),
            SE       = sqrt(sum(w^2 * se^2)) / sum(w),
            .groups  = "drop") |>
  mutate(Lower = pmax(0, Estimate - 1.96 * SE),
         Upper = pmin(1, Estimate + 1.96 * SE),
         category = factor(cat_labels[category], levels = cat_labels))

sig_levels <- c("p < 0.05", "p < 0.1", "p ≥ 0.1")
sig_lines  <- c("p < 0.05" = "solid", "p < 0.1" = "longdash", "p ≥ 0.1" = "dotted")

sig_lookup_sens <- contrasts_pooled_spatial_sensitivity |>
  transmute(network, wealth,
            category = factor(category, levels = cat_labels),
            sig = factor(case_when(p < 0.05 ~ "p < 0.05",
                                   p < 0.1  ~ "p < 0.1",
                                   TRUE     ~ "p ≥ 0.1"),
                         levels = sig_levels))

curves_pooled_spatial_sensitivity <- curves_pooled_spatial_sensitivity |>
  left_join(sig_lookup_sens, by = c("network", "wealth", "category"))

stopifnot(!anyNA(curves_pooled_spatial_sensitivity$sig))


## renaming objects for plotting purposes.....
curves_pooled <- curves_pooled_spatial_sensitivity
model_list    <- model_list_spatial_sensitivity
net_spec      <- net_spec_spatial_sensitivity

fig_combined("spatial_5")
fig_combined("spatial_15")


fig_all_spatial_sensitivity <- function() {
  ggarrange(col_header,
            row_lab(fig_combined("spatial_5",  c("a", "b")), "Spatial networks (5%)"),
            row_lab(fig_combined("spatial_15", c("c", "d")), "Spatial networks (15%)"),
            as_ggplot(sig_key),
            ncol = 1, heights = c(0.12, 1, 1, 0.06))
}



fig_all_spatial_sensitivity()
ggsave("predicted_probs_spatial_sensitivity.pdf", fig_all_spatial_sensitivity(),
       width = 12, height = 10, dpi = 300)
