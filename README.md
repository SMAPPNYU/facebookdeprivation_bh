# facebookdeprivation_bh IN PROGRESS

Replication materials: _Growing Closer or Further Apart? Exposure to Social Media in Post-Conflict Settings_
--------------

This github repository contains the replication materials for the paper "Growing Closer or Further Apart? Exposure to Social Media in Post-Conflict Settings," forthcoming in XX, authored by Nejla Asimovic, Jonathan Nagler, Richard Bonneau, and Joshua Tucker, all members of the Center for Social Media and Politics [CSMAP] at NYU.


> __Abstract:__
Despite the belief that social media is altering inter-group dynamics - bringing people closer or further alienating groups - the impact of social media on inter-ethnic attitudes has yet to be rigorously evaluated, especially within areas with tenuous inter-ethnic relations. We present
a pre-registered evaluation of the effects of exposure to social media during the
week of genocide remembrance in July 2019 on a set of inter-ethnic attitudes of
the online users. Focusing on Bosnia-Herzegovina (BiH), we randomly subset users to deactivate
Facebook accounts and assess inter-ethnic attitudes as a primary
outcome of interest. We find evidence that, counter pre-registered expectations, people who remained active on Facebook report higher regard for ethnic out-groups than those who deactivated their profiles, but this effect was highly conditional on the composition of one’s offline environment. If that environment is characterized by few to none opportunities for inter-group contact, deactivation may move users to an environment even more homogeneous, and echo chambers even more pronounced, than the ones they experience online. We also extend the analysis to include measures of factual news knowledge and subjective well-being, thus representing the first experimental study of the welfare effects of social media conducted outside of the context of advanced democracies. Our findings suggest that deactivation from social media leads to an increase in subjective well-being and a decrease in news knowledge, replicating results for recent research in the United States in a very different context, thus increasing our confidence in the generalizability of these effects. 

## Data

- bosnia_data.csv contains the anonymized dataset used for all the analysis. Personal information about individual users has anonymized to ensure participant privacy, and the original Facebook URLs have been replaced by random numbers. 

- sept18_active_attrition_c.xlsx contains drop-out participants from the control group

- sept18_deactive_attrition_c.xlsx contains drop-out participants from the treatment group

To download the data files, go to XX.


## Code
Below is the code necessary for replicating all the figures/tables in the main text of the paper and supplementary analysis.

- code_review_megan.R - Table S1-S8

- maintext_plots.R generates all the figures in the main text (Fig 1, Fig 2 and Fig 3); the models from which it derives coefficients and standard errors are specified within the code_review_megan.R script 
- heterogenous_Effects_megan.R - Fig S2 [S1 are just images]
- offline_networks_megan.R  - TABLE S11-S18
- offline_online_interaction_megan.R - Fig S6, Fig S5.A
- fb_substitutes_megan.R - Fig S3, S4, S7
- online_networks_megan.R - Tables S19-S21 + Fig S5
- offline_online_interaction.R






