# About
This directory contains the code required to process the data from the 
EqualStrength Name Survey.  The study pre-registration can be found here: 

Ghekiere, A., Martiniello, B., Suarez, A., van Oosten, S., Fernández-Reino, M., Vass-Vigh, V., … Atay, P. (2023, December 13). The perception of names in experimental studies on ethnic origin: a cross-national validation in Europe. [https://doi.org/10.17605/OSF.IO/HSWG6](https://doi.org/10.17605/OSF.IO/HSWG6)

## Usage

> [!IMPORTANT]  
> The datasets are currently under embargo and not included in this repository yet.

- The sub-directory [data/raw](data/raw) contains the raw data file from all countries
- The sub-directory [data](data) contains the final dataset in different formats
- [ES2_NameSurvey_1_Prep.R](ES2_NameSurvey_1_Prep.R) contains the R code
required to:
    1) Import RAW datafiles and merge them into a single file
    2) Import Name Survey dictionary
    3) Generating variables with congruence between response and dictionary
    4) Export the common dataset

## Authors
EqualStrength team

## Version control
16-Feb: Initial version
20-Feb: Document and adjust duplicate entries caused by a correction in the spelling of four names in the second round.
21-Feb: Add column of region congruence rate in all country tables
23-Feb: Re-run the results for Spain based on weighted data (Weight variable calculated by Spain)
28-Feb: Adjust ladder variable for Spain and add score/rank for Surinamese names in the Netherlands

