# human_animal_networks

Organizing analyses of GPS and self-reported animal interaction networks


## gps_reported_networks

This folder contains code for generating bipartite networks based on GPS-based contact between people and animals. 

-   **0_Correlation.Rmd** contains exploratory analysis to investigate correlations between degree in spatial and self-reported networks. 

-   **1_NetworkConstruction.Rmd** constructs edge lists for binary bipartite networks between people and domestic animals, according to their GPS overlaps

-   **2_ERGM2.Rmd** ERGM for odds of edge formation between a human and a domestic animal individual in the spatial bipartite networks. 

-   **outputs** contains network edge lists



## self_reported_networks

This folder contains code for generating bipartite networks based on respondents' self-reported animal interactions. 

-  **human_animal_networks_combined_nexus.Rmd** main pipeline for the self-reported networks: constructs full 
and high-risk bipartite networks, fits village-level ERGMs, checks diagnostics (VIF, MCMC, GOF), 
pools effect estimates with fixed-effects meta-analysis, and computes descriptive statistics. 

-  **supplementary_mlm.Rmd** supplementary analysis of edge data using multilevel logistic regression
  
-  **full_network_sensitivity_v2.Rmd** sensitivity testing for unusual interactions in the full self-reported data

-   **high_risk_network_sensitivity_v2.Rmd** sensitivity testing for unusual interactions in the high-risk self-reported data


#### /Results
Contains network edge lists

#### /Images
Contains code for generating figures

- **animal_exposure_heat_map.R** creates heat map of self-reported interactions

- **network_visualizations_bipartite.R** updated network visualizations using the 'bipartite' package

## predicted_probs.R

Translates ERGM estimates from all three networks (spatial, full self-reported, high-risk self-reported) into predicted probabilities of human–animal edges, and produces the main multipanel results figure and coefficient tables.

-   **Inputs:** village-level ERGM objects for the spatial networks (`ergm_anda2`, `ergm_mandena2`, `ergm_sara2`) and the self-reported networks (`*_nexus.RData`). Network-specific term names, animal categories, and reference levels are set in `net_spec` (spatial: people = b1 mode, cattle vs. non-cattle; self-reported: people = b2 mode, domesticated/rodent/wild, with domesticated split into two groups in the high-risk network).
-   **Predicted probabilities:** for each village model, computes the conditional probability of every person–animal dyad from the model's change statistics, holding other covariates at observed values. Wealth indicators (land size, commercial goods, house materials) are varied across their 5th–95th percentiles; vanilla is set to 0/1. Uncertainty comes from 1,000 multivariate-normal draws of the coefficients.
-   **Contrasts:** change in predicted probability per 1 SD increase (or vanilla vs. not) by animal category and across all animals, with baseline probability and relative change. Pooled across villages with fixed-effects meta-analysis (`metafor`). Also reports change in expected degree.
-   **Figure:** pooled covariate coefficients (left) and predicted-probability curves by animal category (right) for each network; line type shows significance of the pooled contrast (solid p < 0.05, long-dashed p < 0.1, dotted p ≥ 0.1).
-   **Spatial sensitivity analysis:** repeats the pipeline for spatial networks built with 5% and 15% overlap thresholds (5% Ampandrana model refit without degree terms with zero observed counts).
-   **Outputs:**
    -   `predprob_contrasts_pooled_wland.csv`: pooled contrasts, baselines, relative changes
    -   `predprob_contrasts_pooled_spatial_sensitivity.csv`: same, for 5% and 15% spatial networks
    -   `covariate_coefs_pooled.csv`: pooled non-wealth covariate coefficients (log-odds and ORs)
    -   `ergm_coefficients_all_networks.xlsx`: all pooled and village-level ERGM coefficients, plus supplementary MLM sheets
    -   `predicted_probs_all_networks.pdf`, `predicted_probs_spatial_sensitivity.pdf`: main and sensitivity figures
