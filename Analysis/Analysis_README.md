This folder contains the main codes for the analyses of the processed Identification and Network measures. The order of execution of the files is as follows -

* 1_data_cleaning.R : This program cleans all the input files (specified below) and pools the data sets to create a final data set. The validation checks are also run.

  ############ This file does the following ############
1. Set the directory path and load the libraries
2. Read the Tech firm files
3. Read the Staffing firm files
4. Read the Design firm files
5. Descriptive Statistics (Table 4)
6. Correlation Matrix 
7. Preparing data for validation analyses
8. Merging data from all three firms and creating final data set used for regressions
9. Creating the cultural fit data set
10. The DV of the raw i_we variable

* 2_main_regression_code.R : This program runs the main analyses and robustness checks.
  
  ############ This file does the following ############
1. Set the directory path and load the libraries
2. Read the final processed file
3. Print unique user counts and total number of observations for each company
4. Different specifications of regression models (including the finally selected specification for the Between-person and Within-person)
5. Dropping observations based on Number of Tokens and running the regression models
6. Correlation Matrix (Table 5)
7. Robustness checks
8. Correlation with survey measures
   
