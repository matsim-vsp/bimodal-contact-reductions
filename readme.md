This is the repository for the 2025 study ``Trimodal Contact Reductions and the Persistence of Social Homophily during the COVID-19 pandemic''.

The repository contains all material to reproduce the analysis.
The survey in the original German is available on [OSF](https://osf.io/rtjzu), as is the anonymized data ([OSF](https://osf.io/7vzgd/))

## Folder structure

-   `R/`: Contains most of the analysis of the data.
-   `python`: Contains the fitting of the data and the model comparison.

## Top level files

- `DataCleaningPrepForContactAnalysis.R`: Contains the cleaning of the anonymized data. Is called in most subsequent scripts to prepare the necessary data
- `WholeSampleContactAnalysis.R`: Contains contact analysis for whole sample. Contains code to produce Figure 1.
- `Subanalysis_Careful.R`: Contains subanalysis by risk perception groups (risk-averse vs risk-tolerant participants). Contains code to produce Figure 3, Figure 4, Figure 5, and Figure 6.
- `Subanalysis_Age.R`: Contains subanalysis by age bracket (18-39, 40-59, 60+). Contains code to produce Figure 14 and Figure 15
- `Subanalysis_Gender.R`: Contains subanalysis by gender. Contains code to produce Figure 16 and Figure 17.
- `Subanalysis_Comorbidities.R`: Contains subanalysis by presence/absence of comorbidity. Contains code to produce Figure 18 and Figure 19.
- `PolymodAnalysis.R`: Contains simple comparison of survey data and the [POLYMOD](https://doi.org/10.1371/journal.pmed.0050074) study. Contains code to produce Figure 8 and Figure 9.
- `TestDistribution.R`: Applies Kolmogorov-Smirnov-tests and permutation tests to test for difference in distributions. 

## Miscellaneous files
- `DataComposition.R` : Contains some preliminary analysis and data validation. Figures produced in this script are used in presentations, but are not part of the manuscript.
- `mytheme.R`: Creates a personalized theme that is applied to all figures. 

## Instructions for reproducibility

TO DO
