This is the repository for the 2024 studies "TITLE Twitter Paper" and "TITLE 2nd order contact paper".

It contains all materials to reproduce our analyses. Anonymized data is available at our OSF repository: https://osf.io/8wvy3/

## Folder structure

-   `data/`: Put the preprocessed data from OSF here.
-   `AnalysisSP`: Contains the analysis of the data.
-   `Complete Surveys`: Contains the figures.

It is split between the two studies: `ExploratoryAnalysis.R` contains all analyses for "TITLE Twitter Paper", while everything else is
for "TITLE 2nd order contact paper".

## Top level files

-  `Preprocessing.Rmd`: This file contains the code for preprocessing the data.
-  `survey_415684_R_syntax_file.R`: This file contains the automatic labeling of data from the LimeSurvey export
-  `rename.R`: This file contains the code for renaming the variables in the data.

## Instructions for reproducibility

This project uses `renv` for reproducibility. You can install the libraries we used by calling `renv::restore()` from the R terminal.

1. Clone the repository.
2. Call `renv::restore()` from the R terminal.
3. Create a folder called "data" and put the preprocessed data from OSF there.
4. All analysis scripts in the `AnalysisSP` folder should run now.
