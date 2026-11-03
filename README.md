# human_animal_networks

Organizing analyses of GPS and self-reported animal interaction networks


## gps_reported_networks

This folder contains code for generating bipartite networks based on GPS-based contact between people and animals. 

-   **0_Correlation.Rmd** contains exploratory analysis to investigate correlations between degree in spatial and self-reported networks. 

-   **1_NetworkConstruction.Rmd** constructs edge lists for binary bipartite networks between people and domestic animals, according to their GPS overlaps

-   **2_ERGM.Rmd** ERGM for odds of edge formation between a human and a domestic animal individual in the spatial bipartite networks. 

-   **3. EffectSizePooling.R** pools effect sizes across villages for the ERGM coefficients. 

-   **outputs** contains network edge lists and ERGM results.



## self_reported_networks

This folder contains code for generating bipartite networks based on respondents' self-reported animal interactions. 

-  **human_animal_networks_combined.Rmd** constructs networks for full and high-risk self reported networks; runs ERGMS; checks diagnostics; pools effect estimates and generates coef plots
  
-  **supplementary_mlm.Rmd** supplementary analysis of edge data using multilevel logistic regression
  
-  **full_network_sensitivity_v2.Rmd** sensitivity testing for unusual interactions in the full self-reported data

-   **high_risk_network_sensitivity_v2.Rmd** sensitivity testing for unusual interactions in the high-risk self-reported data


#### /Results
Contains all edge lists and ERGM outputs

#### /Images
Contains code for generating figures

- **animal_exposure_heat_map.R** creates heat map of self-reported interactions

- **network_figures_take_2.R** original network visualizations (old)

- **network_visualizations_bipartite.R** updated network visualizations (spring 2026) using the 'bipartite' package

- **pooled_coef_plots_combined.R** combines coef plots into single multipanel figure


