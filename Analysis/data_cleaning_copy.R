##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

############ This file does the following ############
# 1. Set the directory path and load the libraries
# 2. Read the Tech firm files
# 3. Read the Staffing firm files
# 4. Read the Design firm files
# 5. Descriptive Statistics (Table 4)
# 6. Correlation Matrix 
# 7. Preparing data for validation analyses
# 8. Merging data from all three firms and creating final data set used for regressions
# 9. Creating the cultural fit data set
# 10. The DV of the raw i_we variable

#####################################
############ INPUT FILES ############
#####################################
#This file reads the following input files for each firm-
  
##### TECH firm #####
# 1. embedding file - embeddings_quarterly_50d_mincount50.csv
# 2. hr and network files -
#   2a. pronoun_count.csv
#   2b. tech_network_embed.csv
#   2c. tech_diversity_bridging_all.csv
##### Design firm #####
# 3. embedding file - hash_embeddings_50_all_quarterly_internal.csv
# 4. hr and network files -
#   4a. pronouns_count_all_internal.csv
#   4b. design_network_embedded_all_internal.csv
#   4c. design_diversity_bridging_all_internal.csv
##### Staffing firm #####
# 5. embedding file - embeddings_high_prob_eng_08_quarterly_50d_mincount50_v2.csv
# 6. hr and network files -
#   6a. longitudinal_hr.csv
#   6b. effort_quarterly_all.csv
#   6c. staffing_num_counts.csv
#   6d. staffing_network_embedded.csv
#   6e. staffing_diversity_bridging_all.csv

######################################
############ OUTPUT FILES ############
######################################
# This file generates two output files  
#       1. final_processed_data.csv
#       2. cf_all.csv
# These two files are used to run the regressions in the files main_regression_code.R

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#1. Set the directory path and load the libraries

#set path
print(getwd())
setwd("/Users/sarayuvyakaranam/Desktop/identification_final_analysis_codes")
print(getwd())

#install.packages('stargazer')
#install.packages('ggplot2')
#install.packages('survival')
#install.packages('ggfortify')
#install.packages('lfe')
#install.packages('car')
#install.packages('dplyr')
#install.packages('DataCombine')
#install.packages('Hmisc')
#install.packages('OneR')
#install.packages('DescTools')
#install.packages('multiwayvcov')
#install.packages('psych')
#install.packages('tidyr')
#install.packages('corrplot')
#install.packages('xtable')

#---
#title: "R Notebook"
#output: html_notebook
#---
#```{r, warning=FALSE}
#rm(list=ls())
library(stargazer)
library(ggplot2)
library(survival)
library(ggfortify)
library(lfe)
# qqPlot
library(car)
# piping
library(dplyr)
# slide
library(DataCombine)
# function %nin%
library(Hmisc)
# merge
library(data.table)
# bin
library(OneR)
# Winsorize
library(DescTools)
# coeftest
library(lmtest)
library(multiwayvcov)
library(psych)
library(tidyr)
library(corrplot)
library(xtable)
#```

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#Read tech files
########################## Tech firm
home_dir = getwd()
print(home_dir)
data_tech = read.csv("data_files/embeddings_quarterly_50d_mincount50.csv")
View(data_tech)
####checking count start
nrow(data_tech)
ncol(data_tech)
####checking count end

#######histogram of the DV######
hist(data_tech$i_we)

names(data_tech)[1] = 'user_id'
names(data_tech)[names(data_tech)=='female'] = 'gender' #rename the column called female
#This loop iterates over the specified columns ("manager", "sales", "marketing", "tech") and converts their data type to a factor. 
#This is often done to indicate that these columns contain categorical data (e.g., job titles) rather than numerical data.
for (i in c("manager", "sales", "marketing", "tech")) {
  data_tech[,i] = as.factor(data_tech[,i])
}
data_tech = data_tech[order(data_tech$user_id, data_tech$quarter),] #sorts the data_tech DataFrame by the "user_id" and "quarter" columns
data_tech$log_tenure = log(data_tech$tenure_days+1) #take log of tenure
data_tech$time2 = data_tech$tenure_days #copy the value of tenure in days to a new column called time2
data_tech = slide(data_tech, Var="time2", GroupVar="user_id", slideBy=-1, NewVar="time1") #for each user, the corresponding value in "time1" will be the "time2" value from the previous quarter.
data_tech$time1 = ifelse(is.na(data_tech$time1), 0, data_tech$time1) #address NA values in time1 col, replace all the NAs with 0
# leave time2 as the last column
data_tech = data_tech %>% dplyr::select(-time2, everything()) #moves the "time2" column to the last position in the DataFrame. time2 contains the original time
data_tech$multi_exit = ifelse(data_tech$vol_exit_event == 1, 'vol', ifelse(data_tech$invol_exit_event == 1, 'invol', 'censor')) #create a new col called multi_exit
data_tech$exit = ifelse(data_tech$multi_exit != 'censor', 1, 0) #create new column called exit
data_tech$multi_exit = as.factor(data_tech$multi_exit) #converts the "multi_exit" column to a factor data type

# add changing into a manager
data_tech$manager = as.numeric(as.character(data_tech$manager))
data_tech$manager = ifelse(is.na(data_tech$manager), 0, data_tech$manager)
data_tech = data_tech[order(data_tech$user_id, data_tech$quarter),]
data_tech$user_id = as.factor(data_tech$user_id)
data_tech$gender = as.factor(ifelse(data_tech$gender == 1, 'F', ifelse(data_tech$gender == 0, 'M', NA)))
data_tech$gender = relevel(data_tech$gender, ref = 'M')
data_tech$department = ifelse(!is.finite(data_tech$tech) & !is.finite(data_tech$sales) & !is.finite(data_tech$marketing), NA, data_tech$department)
data_tech = data_tech %>% 
  group_by(user_id) %>% 
  fill(gender, .direction='downup') %>%
  as.data.frame()
data_tech$company = 'Tech'

#num_we_counts, num_i_count variables
pronouns = read.csv('data_files/pronoun_count.csv')
data_tech = merge(data_tech, pronouns)

#Read local clustering
df_network_embed_tech = read.csv("data_files/tech_network_embed.csv")
names(df_network_embed_tech)[1] = 'user_id'
df_network_embed_tech = merge(data_tech, df_network_embed_tech)

#Read global reach files
df_comm_diversity_tech = read.csv("data_files/tech_diversity_bridging_all.csv")
names(df_comm_diversity_tech)[1] = 'user_id'
for (n in c("comm_diversity_unweighted", "comm_diversity_all_neighbors_unweighted", "comm_diversity_weighted", "comm_diversity_all_neighbors_weighted")) {
  df_comm_diversity_tech[,n][df_comm_diversity_tech[,n] == 0] = NA
  df_comm_diversity_tech[,n] = 1 - df_comm_diversity_tech[,n]
}

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#Read design files

########################## Design firm
home_dir = getwd()
print(home_dir)
data_design = read.csv('data_files/hash_embeddings_50_all_quarterly_internal.csv')

#######histogram of the DV######
hist(data_design$i_we)

data_design = data_design[grepl('U[0-9]+', data_design$anon_id), ] #create a new dataframe that contains only the rows that have this condition for user id
data_design$ethnicity = as.character(data_design$ethnicity)
data_design$ethnicity[grepl('I', data_design$ethnicity)] = "Other"
data_design$ethnicity[grepl('N', data_design$ethnicity)] = "Other"
data_design$ethnicity[grepl('U', data_design$ethnicity)] = "Other"
data_design$ethnicity[grepl('T', data_design$ethnicity)] = "Other"
data_design$ethnicity = as.factor(data_design$ethnicity)
data_design$ethnicity = relevel(data_design$ethnicity, ref = 'W')
data_design$gender = relevel(as.factor(data_design$gender), ref = 'M')
data_design$hire_date = as.Date(data_design$hire_date)
data_design$snapshot_date = as.Date(data_design$snapshot_date)
data_design$log_sal = log(data_design$annual_salary)
data_design[data_design$gender=='', 'gender'] = NA
data_design[data_design$ethnicity=='', 'ethnicity'] = NA
data_design[data_design$department=='', 'department'] = NA

# dropping individuals with no HR data
data_design = data_design[is.finite(data_design$snapshot_date),] #remove rows that dont have a snapshot_date
data_design = data_design[order(data_design$anon_id, data_design$quarter),] #This expression creates a vector of indices that would sort the DataFrame by the anon_id and quarter columns in ascending order

#dropping rehires 
data_design = slide(data_design, Var="hire_date", GroupVar="anon_id", slideBy=-1, NewVar="past_hire_date") #essentially creates a column that holds the hire date from the previous quarter for each individual.
data_design$past_hire_date = ifelse(is.na(data_design$past_hire_date), data_design$hire_date, data_design$past_hire_date) #line handles potential NA values in the past_hire_date column
data_design$rehired = ifelse(data_design$hire_date != data_design$past_hire_date, 1, NA) #create a new column called rehired, if hire_date is different from past_hire_date, it indicates a rehire event (1).
rehired = data_design %>% group_by(anon_id) %>% transmute(rehired=na.locf(rehired, na.rm=FALSE)) #lines address the issue of NA values in the rehired column for the first quarter of each individual's data
data_design$rehired = rehired$rehired
data_design = data_design[-which(data_design$rehired == 1),] #This line filters the data_design DataFrame to exclude rows where rehired is 1, indicating a rehire event

data_design$job_function = as.character(data_design$job_title) #Converts the job_title column to character format
data_design$job_function[grepl('Director', data_design$job_title)] = "Director"
data_design$job_function[grepl('Executive Director', data_design$job_title)] = "Executive Director"
data_design$job_function[grepl('Chief', data_design$job_title)] = "Chief"
names(data_design)[names(data_design) == 'is_supervisor'] = 'manager'
data_design$manager = as.factor(data_design$manager)
data_design$time2 = as.numeric(data_design$snapshot_date - data_design$hire_date)
data_design$tenure_days = data_design$time2 
data_design$log_tenure = log(data_design$tenure_days+1) 
data_design = slide(data_design, Var="time2", GroupVar="anon_id", slideBy=-1, NewVar="time1") #Essentially, "time1" holds the tenure days from the previous quarter.
data_design$time1 = ifelse(is.na(data_design$time1) & is.finite(data_design$snapshot_date), 0, data_design$time1)
data_design = data_design %>% dplyr::select(-time2, everything()) #select all columns in the data_design "time2"

data_design$multi_exit = ifelse(data_design$exit_vol == 1, 'vol', ifelse(data_design$exit_invol == 1, 'invol', 'censor'))
data_design$exit = ifelse(data_design$multi_exit != 'censor', 1, 0)
data_design$multi_exit = as.factor(data_design$multi_exit)
data_design$tenure_days = as.numeric(data_design$snapshot_date - data_design$hire_date)
data_design$department = as.character(data_design$department)
data_design$department[grepl('Offsite', data_design$department)] = 'Offsite'
data_design$department[grepl('Healthcare', data_design$department)] = 'Healthcare'
data_design$department[grepl('Learning', data_design$department)] = 'Learning'
data_design$department[grepl('CSD', data_design$department)] = 'CSD'
data_design$department[grepl('Pair', data_design$department)] = 'Other'
data_design$department[grepl('Preowned', data_design$department)] = 'Other'
data_design$department[grepl('Admin', data_design$department)] = 'Admin'
data_design$department[grepl('Concierge', data_design$department)] = 'Other'
data_design$department[grepl('Facilities', data_design$department)] = 'Other'
data_design$department[grepl('Refurbishing', data_design$department)] = 'Other'
# several people had multiple genders, which seemed more like typos
data_design[data_design$anon_id == 'U1017',]$gender = 'F'
data_design[data_design$anon_id == 'U546',]$gender = 'F'
data_design[data_design$anon_id == 'U780',]$gender = 'F'
data_design[data_design$anon_id == 'U809',]$gender = 'F'
data_design$company = 'Design'

pronouns = read.csv('data_files/pronouns_count_all_internal.csv')
data_design = merge(data_design, pronouns)

#Read local clustering file
df_network_embed_design = read.csv('data_files/design_network_embedded_all_internal.csv')
df_network_embed_design = merge(data_design, df_network_embed_design)

#Read global reach file
df_comm_diversity_design = read.csv('data_files/design_diversity_bridging_all_internal.csv') 
#starts a loop over all the columns and replaces NA values with 0, and subtracts 1 from the value
for (n in c("comm_diversity_unweighted", "comm_diversity_all_neighbors_unweighted", "comm_diversity_weighted", "comm_diversity_all_neighbors_weighted")) {
  df_comm_diversity_design[,n][df_comm_diversity_design[,n] == 0] = NA
  df_comm_diversity_design[,n] = 1 - df_comm_diversity_design[,n]
}

names(data_design)[1] = 'user_id'
names(df_network_embed_design)[1] = 'user_id'
names(df_comm_diversity_design)[1] = 'user_id'

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# Read Staffing firm files

##########################Staffing firm
home_dir = getwd()
print(home_dir)
#trying with different mincount output files
#df_quarterly = read.csv("embeddings_high_prob_eng_08_quarterly_50d_mincount200.csv")
#df_quarterly = read.csv("embeddings_high_prob_eng_08_quarterly_50d_mincount50.csv")
df_quarterly = read.csv("data_files/embeddings_high_prob_eng_08_quarterly_50d_mincount50_v2.csv")

#######histogram of the DV######
hist(df_quarterly$i_we_internal)

#Reading hr files
long_hr = read.csv("data_files/longitudinal_hr.csv")
names(long_hr)
names(df_quarterly)
df_quarterly = df_quarterly[, c("num_tokens","user_id", "uid", "quarter", "i_we_internal", "mael_avg")]
names(df_quarterly)[names(df_quarterly) == 'i_we_internal'] = "i_we"
df_quarterly = merge(df_quarterly, long_hr, by=c("uid", "quarter"))

df_quarterly = df_quarterly %>% rename(ethnicity = race)
df_quarterly$ethnicity = as.character(df_quarterly$ethnicity)
df_quarterly$ethnicity[df_quarterly$ethnicity %in% c('Black or African American', 'Missing', 'Native Hawaiian or Other Pacific Islander')] = 'Other'
df_quarterly$ethnicity = as.factor(df_quarterly$ethnicity)
df_quarterly$gender = as.factor(ifelse(df_quarterly$gender == "Male", "M", ifelse(df_quarterly$gender == "Female", "F", NA)))
df_quarterly$gender = relevel(df_quarterly$gender, "M")
df_quarterly$work_country = relevel(as.factor(df_quarterly$work_country), 'U.S.A.')
df_quarterly = df_quarterly[df_quarterly$tenure >= 0,]
df_quarterly$exit = as.factor(df_quarterly$exit)
names(df_quarterly)[names(df_quarterly)=='tenure'] = 'tenure_days'
df_quarterly$time2 = df_quarterly$tenure_days
df_quarterly$log_tenure = log(df_quarterly$tenure_days+1)
df_quarterly = slide(df_quarterly, Var="time2", GroupVar="user_id", slideBy=-1, NewVar="time1")
df_quarterly$time1 = ifelse(is.na(df_quarterly$time1), 0, df_quarterly$time1)
df_quarterly = df_quarterly %>% dplyr::select(-time2, everything())
names(df_quarterly)

effort_df = read.csv("data_files/effort_quarterly_all.csv")
names(effort_df)
df_quarterly = merge(df_quarterly, effort_df[, c("user_id", "quarter", "num_messages", "num_messages_weekend", "num_messages_post_work", "avg_response_time", "num_working_weekends",
                                                 "peer_standardized_num_messages_weekend", "peer_standardized_num_messages_post_work", "peer_standardized_num_working_weekends",
                                                 "department_standardized_num_messages_weekend", "department_standardized_num_messages_post_work", "department_standardized_num_working_weekends")], by=c("user_id", "quarter"))
names(df_quarterly)

#reads num_i and num_we
df_quarterly_alternative = read.csv("data_files/staffing_num_counts.csv")
df_quarterly = merge(df_quarterly, df_quarterly_alternative[, c("user_id", "quarter", "num_we_words", "num_i_words")], by=c("user_id", "quarter"))
names(df_quarterly)[names(df_quarterly) == "num_we_words"] = "num_we_count"
names(df_quarterly)[names(df_quarterly) == "num_i_words"] = "num_i_count"

data_staffing = df_quarterly[df_quarterly$work_country != "India",]
data_staffing$department = droplevels(as.factor(data_staffing$department))
data_staffing$department = as.character(data_staffing$department)
data_staffing$department[grepl('IT', data_staffing$department)] = 'IT'
data_staffing$department[grepl('Operations', data_staffing$department)] = 'Operations'
data_staffing$department[grepl('Direct Hire', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Employee Relations', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Training', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Managed Services', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Administration', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Immigration', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Legal & Contracts', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Marketing', data_staffing$department)] = 'Other'
data_staffing$department[grepl('Learning', data_staffing$department)] = 'Other'
data_staffing$company = 'Staffing'

#Reading local clustering file
df_network_embed_staffing = read.csv('data_files/staffing_network_embedded.csv')
df_network_embed_staffing = merge(data_staffing, df_network_embed_staffing)
df_network_embed_staffing = df_network_embed_staffing[df_network_embed_staffing$work_country != 'India',]

#Reading global reach file
df_comm_diversity_staffing = read.csv('data_files/staffing_diversity_bridging_all.csv')
df_comm_diversity_staffing = merge(data_staffing, df_comm_diversity_staffing)
df_comm_diversity_staffing = df_comm_diversity_staffing[df_comm_diversity_staffing$work_country != 'India',]
for (n in c("comm_diversity_unweighted", "comm_diversity_all_neighbors_unweighted", "comm_diversity_weighted", "comm_diversity_all_neighbors_weighted")) {
  df_comm_diversity_staffing[,n][df_comm_diversity_staffing[,n] == 0] = NA
  df_comm_diversity_staffing[,n] = 1 - df_comm_diversity_staffing[,n]
}
df_comm_diversity_staffing = df_comm_diversity_staffing[, c( "user_id", "quarter", "ego_bridging_unweighted", "ego_bridging_all_neighbors_unweighted", "global_bridging_unweighted", "global_bridging_all_neighbors_unweighted", "comm_diversity_unweighted", "comm_diversity_all_neighbors_unweighted", "n_comm_unweighted", "ego_bridging_weighted", "ego_bridging_all_neighbors_weighted", "global_bridging_weighted", "global_bridging_all_neighbors_weighted", "comm_diversity_weighted", "comm_diversity_all_neighbors_weighted", "n_comm_weighted", "department_bridging_unweighted", "department_bridging_all_neighbors_unweighted", "department_bridging_weighted", "department_bridging_all_neighbors_weighted")]

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# Descriptive Statistics (Table 4)

descriptive_stats = function(df, vars, ndigits) {
  d = round(describe(df[, c(vars)]), ndigits)
  return(cbind(d$mean, d$sd, d$min, d$max))
}

print(typeof(merge(df_network_embed_staffing, df_comm_diversity_staffing)$i_we))
print(typeof(merge(df_network_embed_staffing, df_comm_diversity_staffing)$weighted_clustering))
print(typeof(merge(df_network_embed_staffing, df_comm_diversity_staffing)$comm_diversity_all_neighbors_unweighted))
print(typeof(merge(df_network_embed_staffing, df_comm_diversity_staffing)$tenure_days))
print(typeof(merge(df_network_embed_staffing, df_comm_diversity_staffing)$num_tokens))
print(typeof(merge(df_network_embed_staffing, df_comm_diversity_staffing)$unweighted_degree))
names(df_network_embed_staffing)
names(df_comm_diversity_staffing)
staffing_descriptives = descriptive_stats(merge(df_network_embed_staffing, df_comm_diversity_staffing), c("i_we", "weighted_clustering", "comm_diversity_all_neighbors_unweighted", "tenure_days", "num_tokens", "unweighted_degree"), 2)
tech_descriptives = descriptive_stats(merge(df_network_embed_tech, df_comm_diversity_tech), c("i_we", "weighted_clustering", "comm_diversity_all_neighbors_unweighted", "tenure_days", "num_tokens", "unweighted_degree"), 2)
names(df_network_embed_tech)
names(df_comm_diversity_tech)
design_descriptives = descriptive_stats(merge(df_network_embed_design, df_comm_diversity_design), c("i_we", "weighted_clustering", "comm_diversity_all_neighbors_unweighted", "tenure_days", "num_tokens", "unweighted_degree"), 2)
staffing_descriptives
tech_descriptives
design_descriptives

cols = c('i_we', 'weighted_clustering', 'comm_diversity_all_neighbors_unweighted', "tenure_days", "num_tokens", "unweighted_degree")
df_pooled = rbind(merge(df_network_embed_tech, df_comm_diversity_tech)[, cols], merge(df_network_embed_design, df_comm_diversity_design)[, cols], merge(df_network_embed_staffing, df_comm_diversity_staffing)[, cols])
names(df_pooled)
pooled_descriptives = descriptive_stats(df_pooled, c("i_we", "weighted_clustering", "comm_diversity_all_neighbors_unweighted", "tenure_days", "num_tokens", "unweighted_degree"), 2)

all_desc = cbind(tech_descriptives, design_descriptives, staffing_descriptives, pooled_descriptives)
all_desc
rownames(all_desc) = c("Organizational Identification", "Local Clustering", "Global Reach", "Tenure (Days)", "Number of Tokens","Network Size")
colnames(all_desc) = rep(c("Mean", "SD", "Min", "Max"), 4)
all_desc
xtable(all_desc,fontsize = 2)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# Correlation Matrix (Table 5)

####################main network variables
###Regression variables correlation and p values
all_vars = cbind(df_pooled[, c('i_we', 'weighted_clustering', 'comm_diversity_all_neighbors_unweighted', 'tenure_days', 'num_tokens', 'unweighted_degree')])
all_vars
colnames(all_vars) = c('Organizational Identification', 'Local Clustering', 'Global Reach', 'Tenure (Days)', 'Number of Tokens', 'Network Size')

corr_mat = corr.test(all_vars, method='spearman', adjust='none')
# Print the table in LaTeX format
xtable(corr_mat$r, digits = 3)
xtable(corr_mat$p, digits = 3)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#Preparing data for validation analyses

cols = c("user_id", "quarter", "i_we", "time1", "time2", "exit", "tenure_days", "company", "gender", "department", "num_tokens", "num_messages", "num_we_count", "num_i_count", "num_messages_weekend", "num_messages_post_work", "num_working_weekends", "peer_standardized_num_messages_weekend", "peer_standardized_num_messages_post_work", "peer_standardized_num_working_weekends", "department_standardized_num_messages_weekend", "department_standardized_num_messages_post_work", "department_standardized_num_working_weekends", "avg_response_time")
names(data_tech)
names(data_design)
names(data_staffing)
df_pooled = rbind(data_design[, cols], data_staffing[, cols], data_tech[, cols])

df_pooled$company = as.factor(df_pooled$company)
df_pooled$gender = droplevels(df_pooled$gender)
df_pooled$exit = as.numeric(df_pooled$exit)
df_pooled$department = droplevels(as.factor(tolower(df_pooled$department)))

df_pooled$num_messages_off = df_pooled$num_messages_post_work + df_pooled$num_messages_weekend
df_pooled$peer_standardized_num_messages_off = df_pooled$peer_standardized_num_messages_weekend+df_pooled$peer_standardized_num_messages_post_work
df_pooled$department_standardized_num_messages_off = df_pooled$department_standardized_num_messages_weekend+df_pooled$department_standardized_num_messages_post_work
df_pooled$gender_numeric = ifelse(df_pooled$gender == 'F', 1, 0)

df_pooled$i_we = c(scale(df_pooled$i_we))
df_pooled$num_tokens = scale(log(df_pooled$num_tokens+1))
df_pooled$log_tenure = scale(log(df_pooled$tenure_days+1))

df_pooled$num_work_emails = scale(log(df_pooled$num_messages - df_pooled$num_messages_off + 1))
df_pooled$num_messages_off = scale(log(df_pooled$num_messages_off+1))
df_pooled$num_messages = scale(log(df_pooled$num_messages + 1))
df_pooled$peer_standardized_num_messages_off = scale(log(df_pooled$peer_standardized_num_messages_off-min(df_pooled$peer_standardized_num_messages_off,na.rm=T)+0.001))
df_pooled$department_standardized_num_messages_off = scale(log(df_pooled$department_standardized_num_messages_off-min(df_pooled$department_standardized_num_messages_off,na.rm=T)+0.001))

#exiters = df_pooled[is.finite(df_pooled$exit) & df_pooled$exit == 1, 'user_id']

df_pooled$norm_we = df_pooled$num_we_count / (df_pooled$num_tokens+1)
df_pooled$num_we_count = c(scale(log(df_pooled$num_we_count + 1)))
df_pooled$num_i_count = c(scale(log(df_pooled$num_i_count + 1)))

#Running validation analyses
df_pooled$gender = relevel(as.factor(df_pooled$gender), "M")

#running Cox Proportional Hazard Model
View(df_pooled)
mod_surv = coxph(Surv(time1, time2, exit) ~ i_we + gender + num_tokens + company + department + cluster(user_id), data=df_pooled[which(df_pooled$time2 != df_pooled$time1),], control = coxph.control(iter.max=200))
summary(mod_surv)

############ Table 1 output
stargazer(mod_surv, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level"), notes.append=F, title="Cox Proportional Hazard Models of Exit", add.lines = list(c("Department Controls", "Yes"), c("Company Fixed effects", "Yes")), dep.var.labels = "Employee Exit", covariate.labels = c("Organizational Identification", "Female", "Number of Tokens"), omit = c('department', 'company'),type = "text")

df_pooled$quarter = as.factor(df_pooled$quarter)
df_pooled$company = as.factor(df_pooled$company)
# num tokens not used as control as it is highly collinear with num work emails
#corr.test(df_pooled$num_work_emails, df_pooled$num_messages_off)

effort_within_mods = list(felm(peer_standardized_num_messages_off ~ i_we + num_work_emails | user_id+department+quarter:company | 0 | user_id, data=df_pooled),
                          felm(department_standardized_num_messages_off ~ i_we + num_work_emails | user_id+department+quarter:company | 0 | user_id, data=df_pooled))

effort_between_mods = list(
  felm(peer_standardized_num_messages_off ~ i_we + num_work_emails + gender | department+quarter:company | 0 | user_id, data=df_pooled),
  felm(department_standardized_num_messages_off ~ i_we + num_work_emails + gender | department+quarter:company | 0 | user_id, data=df_pooled))

#Displaying validation analyses
# Table 3 in the paper
stargazer(effort_within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level"), notes.append=F, title="Number of Off-Hour Emails: Within-Person Models", add.lines = list(c("Person Fixed effects", "Yes", "Yes", "Yes"), c("Department Controls", "Yes", "Yes", "Yes"), c("Company-by-Quarter Fixed effects", "Yes", "Yes", "Yes")), dep.var.labels = c("Peer-Standardized Number of Off-Hour Emails", "Department-Standardized Number of Off-Hour Emails"), covariate.labels = c("Organizational Identification", "Number of At-Work Emails"),type = "text")
# Table 2 in the paper
stargazer(effort_between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level"), notes.append=F, title="Number of Off-Hour Emails: Between-Person Models", add.lines = list(c("Person Fixed effects", "No", "No", "No"), c("Department Controls", "Yes", "Yes", "Yes"), c("Company-by-Quarter Fixed effects", "Yes", "Yes", "Yes")), dep.var.labels = c("Peer-Standardized Number of Off-Hour Emails", "Department-Standardized Number of Off-Hour Emails"), covariate.labels = c("Organizational Identification", "Number of At-Work Emails", "Female"),type = "text")

#latex output
# Table 3 in the paper
stargazer(effort_between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level"), notes.append=F, title="Number of Off-Hour Emails: Between-Person Models", add.lines = list(c("Person Fixed effects", "No", "No", "No"), c("Department Controls", "Yes", "Yes", "Yes"), c("Company-by-Quarter Fixed effects", "Yes", "Yes", "Yes")), dep.var.labels = c("Peer-Standardized Number of Off-Hour Emails", "Department-Standardized Number of Off-Hour Emails"), covariate.labels = c("Organizational Identification", "Number of At-Work Emails", "Female"))
# Table 2 in the paper
stargazer(effort_within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level"), notes.append=F, title="Number of Off-Hour Emails: Within-Person Models", add.lines = list(c("Person Fixed effects", "Yes", "Yes", "Yes"), c("Department Controls", "Yes", "Yes", "Yes"), c("Company-by-Quarter Fixed effects", "Yes", "Yes", "Yes")), dep.var.labels = c("Peer-Standardized Number of Off-Hour Emails", "Department-Standardized Number of Off-Hour Emails"), covariate.labels = c("Organizational Identification", "Number of At-Work Emails"))
#merge Table 2 and Table 3 together
stargazer(effort_between_mods,effort_within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level"), notes.append=F, title="Number of Off-Hour Emails: Between-Person and Within-Person Models", 
          add.lines = list(c("Gender Fixed effects", "Yes", "Yes", "No", "No"),c("Person Fixed effects", "No", "No", "Yes", "Yes"),
                           c("Department Controls", "Yes", "Yes", "Yes","Yes"), c("Company-by-Quarter Fixed effects", "Yes", "Yes", "Yes","Yes")), 
          dep.var.labels = c("Peer-Standardized Number of Off-Hour Emails", "Department-Standardized Number of Off-Hour Emails","Peer-Standardized Number of Off-Hour Emails", "Department-Standardized Number of Off-Hour Emails"), 
          covariate.labels = c("Organizational Identification", "Number of During-Work-Hour Emails", "Female"))


##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#Merging data from all three firms and creating final data set used for regressions

df_network_embed_tech$job_title = NA
df_network_embed_staffing$manager = NA
df_network_embed_tech$ethnicity = NA
df_network_embed_staffing$age = df_network_embed_staffing$year - df_network_embed_staffing$year_of_birth 
df_network_embed_design$age = df_network_embed_design$tenure_days / 365 + df_network_embed_design$entry_age

cols = c("user_id", "quarter", network_measures, "tenure_days", "log_tenure", "i_we", "gender", "department", "num_tokens", "company", "job_title", "num_we_count", "num_i_count", "manager")

#global reach measures
network_measures = c('weighted_degree', 'unweighted_degree', 'weighted_clustering')
cols = c("user_id", "quarter", "tenure_days", network_measures, "log_tenure", "i_we", "gender", "department", "num_tokens", "company", "job_title", "num_we_count", "num_i_count", "manager")
df_network_embed_tech$job_title = NA
df_network_embed_staffing$manager = NA

df_network_embed_tech$ethnicity = NA
df_network_embed_staffing$age = df_network_embed_staffing$year - df_network_embed_staffing$year_of_birth 
df_network_embed_design$age = df_network_embed_design$tenure_days / 365 + df_network_embed_design$entry_age


#merge local clustering files
df_network_embed = rbind(df_network_embed_design[, cols], df_network_embed_staffing[, cols], df_network_embed_tech[, cols])
#merge global reach files
df_comm_diversity = rbind(df_comm_diversity_tech, df_comm_diversity_staffing, df_comm_diversity_design)
#merge local clusteriung files and global reach files
df_network_embed = merge(df_network_embed, df_comm_diversity)
print(colnames(df_network_embed))
####checking count start
nrow(df_network_embed)
ncol(df_network_embed)
####checking count end

#distribution of raw network variables
hist(df_network_embed$tenure_days)
hist(df_network_embed$unweighted_degree)
hist(df_network_embed$weighted_clustering)
hist(df_network_embed$comm_diversity_all_neighbors_unweighted)
hist(df_network_embed$i_we)

#twentfifth_percentile <- quantile(df_network_embed$num_tokens, probs = 0.25)
#ninetieth_percentile <- quantile(df_network_embed$num_tokens, probs = 0.9)
#print(tenth_percentile)
#print(twentfifth_percentile)
describe(df_network_embed$num_tokens)

# this is where we insert descriptives
df_network_embed$i_we = scale(df_network_embed$i_we)
df_network_embed$gender = as.factor(droplevels(df_network_embed$gender))
df_network_embed$log_tenure = scale(log(df_network_embed$tenure_days + 1))
df_network_embed$company = as.factor(df_network_embed$company)
df_network_embed$num_tokens = scale(log(df_network_embed$num_tokens + 1))
df_network_embed$we_i_ratio = scale(log(1 + (df_network_embed$num_we_count / (df_network_embed$num_i_count + 1)))) #not working because staffing num_i counts and num_we counts file is missing
df_network_embed$num_i_count = scale(log(df_network_embed$num_i_count + 1)) #not working
df_network_embed$num_we_count = scale(log(df_network_embed$num_we_count + 1)) #not working
df_network_embed$weighted_degree = log(df_network_embed$weighted_degree+1)
df_network_embed$unweighted_degree = log(df_network_embed$unweighted_degree+1)
df_network_embed$weighted_clustering = log(df_network_embed$weighted_clustering+0.00001)
#df_network$comm_diversity_all_neighbors_unweighted = log(df_network$comm_diversity_all_neighbors_unweighted+0.00001)
df_network_embed$comm_diversity_all_neighbors_transformed <- log(df_network_embed$comm_diversity_all_neighbors_unweighted + 0.00001) #take log of global 


df_network_embed = df_network_embed[order(df_network_embed$user_id, df_network_embed$quarter),]

#slide local clustering variables
network_measures = c('weighted_degree', 'unweighted_degree', 'weighted_clustering')

for (n in c(network_measures)) {
  df_network_embed[,n] = scale(df_network_embed[,n])
  #df_network_embed[,n] = Winsorize(df_network_embed[,n], na.rm=T)
  df_network_embed = slide(df_network_embed, Var=n, GroupVar="user_id", slideBy=-1, NewVar=paste0("past_", n), reminder=FALSE)
}

##slide global reach variables
network_measures = c("comm_diversity_all_neighbors_unweighted", "comm_diversity_all_neighbors_weighted")
print(colnames(df_network_embed))
for (n in network_measures) {
  df_network_embed[,n] = scale(df_network_embed[,n])
  df_network_embed = slide(df_network_embed, Var=n, GroupVar="user_id", slideBy=-1, NewVar=paste0("past_", n), reminder=FALSE)
}

#seeing the scaled histograms
hist(df_network_embed$weighted_clustering)
hist(df_network_embed$comm_diversity_all_neighbors_unweighted) #unlogged global reach
hist(df_network_embed$comm_diversity_all_neighbors_transformed) #logged global reach

#logg i_we and create a new variable
df_network_embed = slide(df_network_embed, Var='i_we', GroupVar="user_id", slideBy=-1, NewVar='past_i_we', reminder=FALSE)
names(df_network_embed)
####checking count start
nrow(df_network_embed)
ncol(df_network_embed)
####checking count end
write.csv(df_network_embed, file = 'data_files/final_processed_data.csv', row.names = F)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#Preparing Cultural Fit Measures
#Creating the cultural fit data set
tech_cf = read.csv("data_files/tech_cf.csv")
design_cf = read.csv('data_files/design_cf_all_internal.csv')
staffing_cf = read.csv("data_files/staffing_cf.csv")
names(tech_cf) = names(design_cf)  = c("user_id", "quarter", "vanilla_cf")
df_cf = rbind(tech_cf, design_cf, staffing_cf)
hist(df_cf$vanilla_cf)
write.csv(df_cf, file = 'data_files/cf_all.csv', row.names = F)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#######################
#histogram of the raw DV
#histogram of all the DV from each data set
par(mfrow = c(1,3))
hist(data_tech$i_we, main = "Tech Firm", xlab = "Identification",breaks = 12,cex.lab=1.5, cex.axis=1.2, cex.main=3, cex.sub=1,ylim=c(0,800))
hist(data_design$i_we, main = "Design Firm", xlab = "Identification",breaks = 12,cex.lab=1.5, cex.axis=1.2, cex.main=3, cex.sub=1,ylim=c(0,800))
hist(df_quarterly$i_we, main = "Staffing Firm", xlab = "Identification",breaks = 8,cex.lab=1.5, cex.axis=1.2, cex.main=3, cex.sub=1,ylim=c(0,800))
