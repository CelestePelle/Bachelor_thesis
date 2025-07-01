# Estimating excess mortality during the COVID-19 pandemic using time series analysis

Welcome to this repository containing the datasets and R code used in my research on estimating excess mortality of a selection of fifteen countries during the COVID-19 pandemic using time series analysis. The purpose of this repository is to provide all the materials needed to reproduce the analyses in my thesis.

In this repository you will find the processed data files under **Datasets**, and the R scripts that use the data sets.  

## The datasets
The **Datasets** folder contains the following datasets and tables: 
- locations_temperatures: Contains the coordinates used to access the temperature data for the selected countries.
- Temperatures.csv: Contains the daily minimum and maximum temperatures for the selected countries analyzed in the research, from December 2014 till the end of 2023. 
- WHO_official.csv: Contains the total number of reported COVID-19 deaths from the beginning of the pandemic until the end of 2021 in the column "Cumulative_deaths" for the selection of countries.
- excess_mortality.csv: Contains the computed excess mortality presented as a dataframe.
- world_mortality.csv: Contains the all-cause mortality per time unit for all countries available in the World Mortality Dataset as of 11/02/2025.


## R scripts
The **R codes folder** contains the following scripts: 
- Cross_validation.R: Performs the cross validation as discussed in the thesis. 
- Excess_mortality.R: Fits the models and computes the excess mortality.
- Variable_selection.R: Performs the Stepwise Backward Selection method using the likelihood ratio test (anova()) and the AIC to select a subset of the variables, such that the kept variables are significant or improve the model. 