This is the repository for the 2025 study _Bimodal Contact Reductions and Social Homophily during COVID-19_.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15783898.svg)](https://doi.org/10.5281/zenodo.15783898)

The repository contains all material to reproduce the analysis.
The [survey in the original German](https://osf.io/rtjzu) and the [anonymized data](https://osf.io/7vzgd/) are available on OSF.

## Folder structure

-   `R/`: Contains most of the analysis of the data.
-   `python`: Contains the fitting of the data and the model comparison.

## Top level files

- `DataPrep.R`: Contains the cleaning of the anonymized data. Is called in most subsequent scripts.

- `ContactAnalysis_FullSample.R`: Contains contact analysis for whole sample. Contains code to produce Figure 1, Supplementary Figure 2, Supplementary Figure 9, Supplementary Figure 10.

- `DemographicExploration.R` First demographic exploration of the survey data. Explores age, gender, householdsize, and comorbiditiy distribution of sample. Contains the code to produce Supplementary Figure 1.

- `PolymodAnalysis.R` Analysis of German [POLYMOD](https://doi.org/10.1371/journal.pmed.0050074) data. Explores demographic distributions of German sample, depicts their distribution of work and leisure contacts. Contains the code to produce Supplementary Table 1.

- `ContactAnalysis_RiskPerception.R` Analysis of contact data, differentiated by risk perception group. Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the two subpopulations. Contains the code to produce Figures 3, 4, 5, and Supplementary Figure 4.

- `ContactAnalysis_Age.R` Analysis of contact data, differentiated by age group (18-39, 40-59, 60+). Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the three age groups. Contains the code to produce Supplementary Figures 10, 11, and 12.

- `ContactAnalysis_Gender.R` Analysis of contact data, differentiated by gender (female, male; as only 7 participants reported their gender as *diverse*, they are excluded from this analysis). Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the genders. Contains the code to produce Supplementary Figures 14 and 15.

- `ContactAnalysis_Comorbidities.R` Analysis of contact data, differentiated by presence/absence of comorbidity. Depicts number of contacts, distribution of number of infections, and ECDF of timing of first infection for the two subpopulations. Contains the code to produce Supplementary Figure 16 and 17.

- `AccuracyofParticipants.R`Compares the number of contacts participants reported their closests contacts (CCs) to the number of contacts the CCs reported for themselves. Contains the code to produce Supplementary Figure 6,7, and 8.

- `KolmogorovSmirnovTest.R`: Applies Kolmogorov-Smirnov-tests and permutation tests to test for difference in distributions of change of number of contacts.   

- `GroupassignmentSankey.R`: Takes Bayesian model outputs as inputs. Based on fitting, assigns groups _Strong reduction_, _Intermediate Reduction_, and _Little Change_ to reported no. of contacts.

- `SankeyPlots.R`: Takes output from GroupAssignmentSankey.R as input. Converts data to appropiate format for Sankey plots. Contains the code to produce Figure 2. 

- `CorrelationAnalysis-SocialHomophily.R`: Computes correlation coefficients for participants' and CC's number of contacts. Produces Table 3 and Figure 6.

- `BayesianReductionModel.ipynb`: Bayesian reduction model to fit one to four distributions to the contact data. Leave-one-out cross validation is used for model comparison. 

## Miscellaneous files
- `mytheme.R`: Creates a personalized theme that is applied to all figures. 

## Instructions for reproducibility

1. Put the preprocessed data from OSF into the folder called `data`. The cleaned data set called cleaned_data.rds.
2. Run the scripts in the R folder to reproduce the plots.
3. The resulting plots can be found in the plots folder. Each scripts generates a PDF and PNG version of the plot.
