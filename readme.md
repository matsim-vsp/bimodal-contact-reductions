This is the repository for the 2025 study ``Trimodal Contact Reductions and the Persistence of Social Homophily during the COVID-19 pandemic''.

The repository contains all material to reproduce the analysis.
The survey in the original German is available on [OSF](https://osf.io/rtjzu), as is the anonymized data ([OSF](https://osf.io/7vzgd/))

## Folder structure

-   `R/`: Contains most of the analysis of the data.
-   `python`: Contains the fitting of the data and the model comparison.

## Top level files

- `DataCleaningPrepForContactAnalysis.R`: Contains the cleaning of the anonymized data. Is called in most subsequent scripts to prepare the necessary data

- `ContactAnalysis_FullSample.R`: Contains contact analysis for whole sample. Contains code to produce Figure 1, Supplementary Figure 2, Supplementary Figure 8, Supplementary Figure  [X].

- `DemographicExploration.R` First demographic exploration of the survey data. Explores age, gender, householdsize, and comorbiditiy distribution of sample. Contains the code to produce Supplementary Figure 1 [X].

- `PolymodAnalysis.R` Analysis of German [POLYMOD](https://doi.org/10.1371/journal.pmed.0050074) data. Explores demographic distributions of German sample, depicts their distribution of work and leisure contacts. Contains the code to produce Table **XX** and Figure **XX**.

- `Subanalysis_Careful.R` Analysis of contact data, differentiated by risk perception group. Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the two subpopulations. Contains the code to produce Figures **XX, XX, and XX**.

- `Subanalysis_Age.R` Analysis of contact data, differentiated by age group (18-39, 40-59, 60+). Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the three age groups. Contains the code to produce Figures **XX, XX, and XX**.

- `Subanalysis_Gender.R` Analysis of contact data, differentiated by gender (female, male; as only 7 participants reported their gender as *diverse*, they are excluded from this analysis). Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the genders. Contains the code to produce Figures **XX, XX, and XX**.

- `Subanalysis_Comorbidities.R` Analysis of contact data, differentiated by presence/absence of comorbidity. Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the two subpopulations. Contains the code to produce Figures **XX, XX, and XX**.

- `TestDistribution.R`: Applies Kolmogorov-Smirnov-tests and permutation tests to test for difference in distributions. 

## Miscellaneous files
- `DataComposition.R` : Contains some preliminary analysis and data validation. Figures produced in this script are used in presentations, but are not part of the manuscript.
- `mytheme.R`: Creates a personalized theme that is applied to all figures. 

## Instructions for reproducibility

TO DO
