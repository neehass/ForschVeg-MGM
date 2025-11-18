
# Synergistic effects between global warming and water quality change on modelled macrophyte species richness 

**Research Compendium**

<!-- badges: start -->
<!-- badges: end -->

*This repository is a R package which includes all input data from MGM, output data from MGM, analysis files and results to reproduce the following publications given in the Journal reference.*


## Journal references

*Release 0.9* corresponds to a *Preprint*:

- Anne Lewerentz, Markus Hoffmann, Thomas Hovestadt, et al. Potential change in the future spatial distribution of submerged macrophyte species and species richness: the role of today's lake type and strength of compounded environmental change. Authorea. May 31, 2022.
DOI: 10.22541/au.165401091.12520929/v1



## Data source

Source of raw data is: 

- Observed data: 
  - *Makrophyten_WRRL_05-17_nurMakrophytes.csv*:  Bayerisches Landesamt für Umwelt, www.lfu.bayern.de (Published under *Licence CC BY 4.0*)
  - *Morphology*: Bayerisches Landesamt für Umwelt, www.lfu.bayern.de (Published under *Licence CC BY 4.0*)
  - *Indexmakrophyten_Gruppenzuordnung*: Melzer, A. (1999). Aquatic macrophytes as tools for lake management. In D. M. Harper, B. Brierley, A. J. D. Ferguson, & G. Phillips (Eds.), The Ecological Bases for Lake and Reservoir Management (pp. 181–190). Springer Netherlands.
- MGM output: 
  - Output of Macrophytes Growth Model. The input files and the code to create virtual species is stored in the folder `MGMinput/`





## Structure of research compendium

-   `data-raw/`: Raw datasets of model output (*MGMoutput*) and observed data and R code to generate data in preparation files `data/`
-   `data/`: Cleaned data used for the analysis
-   `analysis/`: R code in .Rmd files to reproduce tables, figures and analysis of main file and Supplementary material





## How to reproduce the results
### Installation

To install the package in R follow this code:
``` r
# install.packages("devtools") # install devtools if you don't have it already
devtools::install_github("AnneLew/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness")
library("LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness")
```

### Scripts
To reproduce the results run the R scripts in the following order:
| Order | Script Name                          | Description                                                                                                                                                                             |
|-------|--------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1     | `data-raw/DATASET.R`                | Preparation of the datasets stored in folder *data* |
| 2     | `analysis/analysis.Rmd`              | Reproduces figures, tables and analysis used in main manuscript                                                                                                                         |
| 3     | `analysis/SupplementaryAnalysis.Rmd` | Reproduces figures, tables and analysis shown in SupplementaryMaterial                                                                                                                  |
