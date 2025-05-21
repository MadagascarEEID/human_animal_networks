# human_animal_networks

Organizing analyses of GPS and self-reported animal interaction networks

## self_reported_networks

This folder contains Lev's code for generating bipartite networks based on respondents' self-reported animal interactions. All code is in progress.

### Archive

This folder contains old code....not used for analysis.

-   **"1. Animal Interaction Bipartite.R"** creates a full bipartite network between individual respondents on one mode and (nonhuman) animal species on the other mode. An edge exists between a human and an animal species if a respondent reports any interaction with that animal species.

-   **"2. Animal Interaction ERGM.R"** ERGM for odds of edge formation between a human and an animal species in the full bipartite network

-   **"3. Animal High Risk ERGM.R"** ERGM for odds of edge formation between a human and an animal species in a bipartite network restricted to interactions likely to bring a human into contact with an animal's bodily fluids.

-   **"4. Visuals and Descriptive Stats.R"** Generates figures and descriptive statistics for full and high-risk networks.

-   **"Loading Interaction Data.R"** loads survey data, calculates demographic variables.

### The_ReUp

This folder contains up-to-date code for finalized analyses.

-   **Loading Interaction Data.R** sources all demographic and animal interaction survey data to be used to construct self-reported networks
  
-   **"1. Animal Interaction Bipartite Full Network.R"** creates a full bipartite network between individual respondents on one mode and (nonhuman) animal species on the other mode. An edge exists between a human and an animal species if a respondent reports any interaction with that animal species.

-   **"2. Animal Interaction ERMG Full Network"** ERGM for odds of edge formation between a human and an animal species in the full bipartite network

-   **"3. Animal Interaction Bipartite High Risk.R"** creates a subset bipartite network between individual respondents on one mode and (nonhuman) animal species on the other mode. An edge exists between a human and an animal species if a respondent reports an interaction likely to bring  a human into contact with an animal's bodily fluids (i.e., a "high risk" interaction)

-   **"4. Animal Interaction ERMG High Risk.R"** EGM for odds of edge formation between a human and an animal species in the high risk bipartite network

-   **Descriptive Network Stats.R** Generates descriptive statistics for full and high-risk networks.

-   **pooling effect sizes self-report.R** meta regressions to pool village-specific ERGMs into aggregated models

-   **as_glms.R** Recreates network analyses as generalized linear mixed-effect models; compares fits of ERGMs vs. GLMMs with area under curve calculations.


