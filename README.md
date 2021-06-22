Replication material: _Testing the Effects of Facebook Usage in an Ethnically Polarized Setting_
--------------

This repository contains the replication material for the paper "Testing the Effects of Facebook Usage in an Ethnically Polarized Setting" forthcoming in Proceedings of the National Academy of Sciences of the United States of America, authored by Nejla Asimovic, Jonathan Nagler, Richard Bonneau, and Joshua A. Tucker, all members of the Center for Social Media and Politics [CSMaP] at NYU.


> __Abstract:__
Despite the belief that social media is altering intergroup dynamics – bringing people closer or further alienating them from one another – the impact of social media on inter-ethnic attitudes has yet to be rigorously evaluated, especially within areas with tenuous inter-ethnic relations. We report results from a randomized controlled trial in Bosnia and Herzegovina (BiH), exploring the effects of exposure to social media during a
week of genocide remembrance in July 2019 on a set of inter-ethnic attitudes of Facebook users. We find evidence that, counter pre-registered expectations, people who deactivated their Facebook profiles report lower regard for ethnic out-groups than those who remained active. Moreover, we present additional evidence suggesting that this effect is conditional on the level of ethnic heterogeneity of respondents' residence.
We also extend the analysis to include measures of subjective well-being and knowledge of news. Here, we find that Facebook deactivation leads to suggestive improvements in subjective well-being and a decrease in the knowledge of current events, replicating results from recent research in the U.S. in a very different context, thus increasing our confidence in the generalizability of these effects. 

## Install

To run the code, initiate: `git clone https://github.com/SMAPPNYU/facebookdeprivation_bh.git`


## Data

- `replication_data.csv` contains the anonymized dataset used in the analysis (necessary for running all the scripts below). Personal information about individual users has been anonymized to ensure participant privacy, and identifying information has been taken out. 

Codebook is also included within the "data" folder.

## Scripts [code]
Within the "scripts" folder is the code necessary for replicating all the figures/tables in the main text of the paper and supplementary analysis.

- **`01_analysis.R/`** - main script listing the necessary packages, uploading the dataset, creating indicators and specifying the models; also contains code for replicating Table S1-S9, analyzing baseline characteristics of the attrition sample, and testing the sensitivity of results to outliers
	* `1.1-heterogenous-effects.R` - replicates Fig S2
	* `1.2-fb-substitutes.R` - replicates Fig S3 and S4
	* `1.3-offline-networks.R` - replicates Tables S10-S17
	* `1.4-online-networks.R` - replicates Tables S18-S20 + Fig S5
	* `1.5-offline-online-interaction.R` - replicates Table S21, S22; Fig S6
	* `1.6-pol-disaffection.R` - replicates Table S23, Fig S7 and S8
      
- **`02_maintext.R/`** generates all the figures in the main text (Fig 1, Fig 2 and Fig 3); the models from which it derives coefficients and standard errors are specified within the above-mentioned analysis (and are sourced at the beginning of each script into the global environment)
	
	
-  **`03_count_network_diversity.py`** – script estimating proportion of each ethnic group within online networks/friend lists of users who shared their online data with our research team. Lists are filtered to friends made prior to July 8, 2019, the week the experiment started to capture only pre-treatment friendships. To respect and ensure the privacy of our participants, we cannot publicly share the raw files with names but instead provide aggregated proportions for each user within the main dataset (these proportions – variables "online_bosniak", "online_serbian", "online_croat" – are then used in the analysis of network diversity, as in the script `1.4_online_networks.R`). 

Within the "figures" folder are the three main figures presented in the main text of the paper.

## Authors

Repository prepared by Nejla Asimovic.

Paper can be cited as [BIBTEX]: 
```@article {Asimovice2022819118,
	author = {Asimovic, Nejla and Nagler, Jonathan and Bonneau, Richard and Tucker, Joshua A.},
	title = {Testing the effects of Facebook usage in an ethnically polarized setting},
	volume = {118},
	number = {25},
	elocation-id = {e2022819118},
	year = {2021},
	doi = {10.1073/pnas.2022819118},
	publisher = {National Academy of Sciences},
	issn = {0027-8424},
	URL = {https://www.pnas.org/content/118/25/e2022819118},
	eprint = {https://www.pnas.org/content/118/25/e2022819118.full.pdf},
	journal = {Proceedings of the National Academy of Sciences}
	}
```
