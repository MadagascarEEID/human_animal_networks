# human_animal_networks

Organizing analyses of GPS and self-reported animal interaction networks

## self_reported_networks

This folder contains Lev's code for generating bipartite networks based on respondents' self-reported animal interactions. All code is in progress.

-   **"1. Animal Interaction Bipartite.R"** creates a full bipartite network between individual respondents on one mode and (nonhuman) animal species on the other mode. An edge exists between a human and an animal species if a respondent reports any interaction with that animal species.

-   **"2. Animal Interaction ERGM.R"** ERGM for odds of edge formation between a human and an animal species in the full bipartite network

-   **"3. Animal High Risk ERGM.R"** ERGM for odds of edge formation between a human and an animal species in a bipartite network restricted to interactions likely to bring a human into contact with an animal's bodily fluids.

-   **"4. Visuals and Descriptive Stats.R"** Generates figures and descriptive statistics for full and high-risk networks.

-   **"Loading Interaction Data.R"** loads survey data, calculates demographic variables.
