##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#This file contains the following
# 1. Set the directory path and load the libraries
# 2. Read the final processed file
# 3. Print unique user counts and total number of observations for each company
# 4. Different specifications of regression models (including the finally selected specification for the Between-person and Within-person)
# 5. Dropping observations based on Number of Tokens and running the regression models
# 6. Correlation Matrix (Table 5)
# 7. Robustness checks
# 8. Correlation with survey measures

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#set the directory path and load the libraries

#set path
print(getwd())
setwd("/Users/sarayuvyakaranam/Desktop/identification_final_analysis_codes")
print(getwd())

#install.packages("ggthemes")

#rm(list=ls())
library(lfe)
library(psych)
library(corrplot)
library(DescTools)
# slide
library(DataCombine)
# bin
library(OneR)
library(stargazer)
library(margins)
library(sjPlot)
library(ggplot2)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#read the final processed file
df_network = read.csv('data_files/final_processed_data.csv')
names(df_network)
####checking count start
nrow(df_network)
ncol(df_network)
####checking count end
#df_cf = read.csv('data_files/cf_all.csv')

df_network = merge(df_network, df_cf, by = c("user_id", "quarter"), all.x=T)
print(colnames(df_network))
####checking count start
nrow(df_network)
ncol(df_network)
####checking count end

names(df_network)

df_network$quarter = as.factor(df_network$quarter)
df_network$company = as.factor(df_network$company)
df_network = df_network[order(df_network$user_id, df_network$quarter),]
df_network = df_network[, c("user_id", "quarter", "i_we", "log_tenure", "num_tokens", "company", "gender", "department", "job_title", "manager", "unweighted_degree", "past_unweighted_degree",  "weighted_clustering", "past_weighted_clustering", 'comm_diversity_all_neighbors_unweighted', 'past_comm_diversity_all_neighbors_unweighted', 'vanilla_cf','comm_diversity_all_neighbors_transformed')]
names(df_network) = c("user_id", "quarter", "i_we", "log_tenure", "log_num_tokens", "company", "gender", "department", "job_title", "manager", "log_network_size", "past_log_network_size", "log_clustering", "past_log_clustering",'global_reach', "past_global_reach",'vanilla_cf','global_reach_logged')

#remove NAs
df_network = df_network[!(is.na(df_network$log_tenure)) & !(is.na(df_network$department)) & !(is.na(df_network$i_we)),]
names(df_network)

####checking count start
nrow(df_network) #10971
ncol(df_network)
####checking count end

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#print counts
tech_df <- df_network[df_network$company == 'Tech',]
nrow(tech_df) #4685
design_df <- df_network[df_network$company == 'Design',]
nrow(design_df) #3112
staffing_df <- df_network[df_network$company == 'Staffing',]
nrow(staffing_df) #2994

#find number of employees per company
names(tech_df) 
length(unique(tech_df$user_id)) #800
names(design_df)
length(unique(design_df$user_id)) #434
names(staffing_df)
length(unique(staffing_df$user_id)) #390

#######################
#histogram of the raw DV
#histogram of all the DV from each data set
par(mfrow = c(1,3))
hist(tech_df$i_we, main = "Tech Firm", xlab = "Identification",breaks = 12,cex.lab=1.5, cex.axis=1.2, cex.main=3, cex.sub=1,ylim=c(0,1200))
hist(design_df$i_we, main = "Design Firm", xlab = "Identification",breaks = 12,cex.lab=1.5, cex.axis=1.2, cex.main=3, cex.sub=1,ylim=c(0,1200))
hist(staffing_df$i_we, main = "Staffing Firm", xlab = "Identification",breaks = 12,cex.lab=1.5, cex.axis=1.2, cex.main=3, cex.sub=1,ylim=c(0,1200))


##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
#different specifications of models run on the data

############################## FINAL SET OF MODELS START HERE #########################

#these models include Global Reach that is not logged and also include num_tokens, and the standard errors are clustered at the person level 
within_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)
within_mods_lagged = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + past_log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + past_global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + past_log_clustering + past_global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)

between_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

between_mods_lagged = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + past_log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + past_global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + past_log_clustering + past_global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Within-Person", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach", "Global Reach: 25-50%", "Global Reach: 50-75%", "Global Reach: 75-100%"),type = "text")
stargazer(within_mods_lagged, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Lagged Local Clustering and Global Reach, Within-Person", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Lagged Local Clustering", "Lagged Global Reach", "Lagged Global Reach: 25-50%", "Lagged Global Reach: 50-75%", "Lagged Global Reach: 75-100%"),type = "text")
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Between-Person", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach", "Global Reach: 25-50%", "Global Reach: 50-75%", "Global Reach: 75-100%"),type = "text")
stargazer(between_mods_lagged, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Lagged Local Clustering and Global Reach, Between-Person", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Lagged Local Clustering", "Lagged Global Reach", "Lagged Global Reach: 25-50%", "Lagged Global Reach: 50-75%", "Lagged Global Reach: 75-100%"),type = "text")

#latex output
stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Within-Person", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach", "Global Reach: 25-50%", "Global Reach: 50-75%", "Global Reach: 75-100%"))
stargazer(within_mods_lagged, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Lagged Local Clustering and Global Reach, Within-Person", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Lagged Local Clustering", "Lagged Global Reach", "Lagged Global Reach: 25-50%", "Lagged Global Reach: 50-75%", "Lagged Global Reach: 75-100%"))
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Between-Person", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach", "Global Reach: 25-50%", "Global Reach: 50-75%", "Global Reach: 75-100%"))
stargazer(between_mods_lagged, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Lagged Local Clustering and Global Reach, Between-Person", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Lagged Local Clustering", "Lagged Global Reach", "Lagged Global Reach: 25-50%", "Lagged Global Reach: 50-75%", "Lagged Global Reach: 75-100%"))


############################## FINAL SET OF MODELS ENDS HERE #########################

#models without number of tokens variable
within_mods = list(
  felm(i_we ~ log_tenure + log_network_size | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + log_clustering*global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)

between_mods = list(
  felm(i_we ~ log_tenure + log_network_size | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_network_size + log_clustering*global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person and quarter level", "Tenure, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach without Including Number of Tokens, Within-Person Models", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Network Size", "Local Clustering", "Global Reach"),type = "text")
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person and quarter level", "Tenure,Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach without Including Number of Tokens, Between-Person Models", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Network Size", "Local Clustering", "Global Reach"),type = "text")

#latex output
stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach without Including Number of Tokens, Within-Person Models", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Network Size", "Local Clustering", "Global Reach"))
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach without Including Number of Tokens, Between-Person Models", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Network Size", "Local Clustering", "Global Reach"))

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#Dropping observations based on Number of Tokens and running the regression models

describe(df_network$log_num_tokens)
twentfifth_percentile <- quantile(df_network$log_num_tokens, probs = 0.25)
drop_25perc <- df_network$log_num_tokens >= twentfifth_percentile
dropped_25perc <- df_network[drop_25perc, ]

####checking count start
nrow(dropped_25perc) 
ncol(dropped_25perc)
####checking count end

describe(df_network$log_num_tokens)
seventyfifth_percentile <- quantile(df_network$log_num_tokens, probs = 0.75)
drop_75perc <- df_network$log_num_tokens <= seventyfifth_percentile
dropped_75perc <- df_network[drop_75perc, ]

####checking count start
nrow(dropped_75perc) 
ncol(dropped_75perc)
####checking count end

within_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | user_id + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | user_id + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_25perc)
)

between_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | gender + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | gender + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | gender + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=dropped_25perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_25perc)
)

stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations below the 25th percentile value of Number of Tokens, Within-Person Models", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"),type="text")
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations below the 25th percentile value of Number of Tokens, Between-Person Models", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"),type="text")

#latex output
stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations below the 25th percentile value of Number of Tokens, Within-Person Models", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"))
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations below the 25th percentile value of Number of Tokens, Between-Person Models", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens","Network Size", "Local Clustering", "Global Reach"))


within_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | user_id + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | user_id + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_75perc)
)

between_mods = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size | gender + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | gender + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | gender + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=dropped_75perc),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | user_id + department + quarter:company | 0 | user_id, data=dropped_75perc)
)

stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations above the 75th percentile value of Number of Tokens, Within-Person Models", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"),type="text")
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations above the 75th percentile value of Number of Tokens, Between-Person Models", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"),type="text")

#latex output
stargazer(within_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations above the 75th percentile value of Number of Tokens, Within-Person Models", add.lines = list(c("Person Fixed effects", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"))
stargazer(between_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Local Clustering and Global Reach, Dropping observations above the 75th percentile value of Number of Tokens, Between-Person Models", add.lines = list(c("Gender Controls", rep("Yes", length(within_mods))), c("Department Controls", rep("Yes", length(within_mods))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_mods)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens","Network Size", "Local Clustering", "Global Reach"))

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# Correlation Matrix (Table 5)

####################main network variables
###Regression variables correlation and p values
all_vars = cbind(df_network[, c('i_we', 'log_clustering', 'global_reach', 'log_tenure', 'log_num_tokens', 'log_network_size')])
all_vars
colnames(all_vars) = c('Organizational Identification', 'Local Clustering', 'Global Reach', 'Tenure (Days)', 'Number of Tokens', 'Network Size')

corr_mat = corr.test(all_vars, method='pearson', adjust='none')
# Print the table in LaTeX format
xtable(corr_mat$r, digits = 3)
xtable(corr_mat$p, digits = 3)

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

#Robustness Checks
df_network = read.csv('data_files/final_processed_data.csv')
df_cf = read.csv('data_files/cf_all.csv')

df_network = merge(df_network, df_cf, by = c("user_id", "quarter"), all.x=T)

df_network$quarter = as.factor(df_network$quarter)
df_network$company = as.factor(df_network$company)

df_network = df_network[order(df_network$user_id, df_network$quarter),]
df_network = df_network[, c("user_id", "quarter", "i_we", "log_tenure", "num_tokens", "company", "gender", "department", "job_title", "manager", "we_i_ratio", "num_we_count", "num_i_count", "unweighted_degree", "past_unweighted_degree",  "weighted_clustering", "past_weighted_clustering", 'comm_diversity_all_neighbors_unweighted', 'past_comm_diversity_all_neighbors_unweighted', 'vanilla_cf')]
names(df_network) = c("user_id", "quarter", "i_we", "log_tenure", "log_num_tokens", "company", "gender", "department", "job_title", "manager", "log_we_i", "log_num_we", "log_num_i", "log_network_size", "past_log_network_size",  "log_clustering", "past_log_clustering", 'global_reach', 'past_global_reach', 'vanilla_cf')
df_network = df_network[!(is.na(df_network$log_tenure)) & !(is.na(df_network$department)) & !(is.na(df_network$i_we)),]

####checking count start
nrow(df_network) #10971
ncol(df_network)
####checking count end

sum(is.na(df_network$vanilla_cf)) #1
na_rows <- which(is.na(df_network$vanilla_cf))
print(df_network[na_rows, ])

#now running the models. These models are Tables A1 through A8 in the appendix

within_manager_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | user_id + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Staffing',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | user_id + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Staffing',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | user_id + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Staffing',])
)

within_we_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)

within_ratio_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)

between_manager_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering | gender + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Staffing',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + global_reach | gender + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Staffing',]),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering + global_reach | gender + department + manager + quarter:company | 0 | user_id, data=df_network[df_network$company != 'Staffing',])
)

between_we_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_num_we + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

between_ratio_controls = list(
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_we_i + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

within_cf_models = list(
  felm(i_we ~ log_tenure + log_num_tokens + vanilla_cf | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering + global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network))

between_cf_models = list(
  felm(i_we ~ log_tenure + log_num_tokens + vanilla_cf | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + vanilla_cf + log_clustering + global_reach | gender + department + quarter:company | 0 | user_id, data=df_network)
)

#latex output with merging output tables
#We controls
stargazer(between_we_controls,within_we_controls, digits=3, header=F, star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", 
                                                          "Standard errors clustered at the person level", "Tenure, Number of Tokens, 
                                                          Network Size, Number of We-Words, and Local Clustering are logged due to right skewed distributions"), 
          notes.append=F, title="Identification on Local Clustering and Global Reach Controlling for Number of We-Words: Between-Person and Within-Person Models", 
          add.lines = list(c("Gender Controls", "Yes","Yes","Yes","No","No","No"), 
                           c("Person Controls", "No","No","No","Yes","Yes","Yes"),
                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes")), 
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Number of We-Words ", "Local Clustering", "Global Reach"))

#Ratio of We to I control
stargazer(between_ratio_controls,within_ratio_controls, digits=3, header=F, star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(.1, .05, .01, .001), 
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, Ratio of Number of We-Words to I-Words, and Local Clustering are logged due to right skewed distributions"), 
          notes.append=F, title="Identification on Local Clustering and Global Reach Controlling for Ratio of Number of We-Words to Number of I-Words, 
          Between-Person and Within-Person Models", add.lines = list(c("Gender Controls", "Yes","Yes","Yes","No","No","No"), 
                                           c("Person Controls", "No","No","No","Yes","Yes","Yes"),
                                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), 
                                           c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes")), 
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", 
                             "Ratio of Number of We-Words to Number of I-Words", 
                             "Local Clustering", "Global Reach"))
#Managerial Status
stargazer(between_manager_controls,within_manager_controls, digits=3, 
          header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), 
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", 
                    "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local 
                    Clustering are logged due to right skewed distributions"), 
          notes.append=F, 
          title="Identification on Local Clustering and Global Reach Controlling for Managerial Status: Between-Person and Within-Person Models", 
          add.lines = list(c("Gender Controls", "Yes","Yes","Yes","No","No","No"), 
                           c("Person Controls", "No","No","No","Yes","Yes","Yes"), 
                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Manager Controls", "Yes","Yes","Yes","Yes","Yes","Yes"), c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes")),
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"))

#Linguistic Fit Models
stargazer(between_cf_models,within_cf_models, digits=3, header=F, 
          star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), 
          notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", 
                    "Tenure, Number of Tokens, Network Size, and Local Clustering are logged"), 
          notes.append=F, title="Identification on Linguistic Fit: Between-Person and Within-Person Models", 
          add.lines = list(c("Gender Controls", "Yes","Yes","Yes","Yes","No","No","No","No"), 
                           c("Person Controls", "No","No","No","No","Yes","Yes","Yes","Yes"), 
                           c("Department Controls", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"), 
                           c("Company-by-Quarter Fixed effects", "Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")), 
          dep.var.labels = "Organizational Identification", 
          covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Linguistic Fit", "Local Clustering", "Global Reach"))


#models with and interaction term
interaction_mods = list(felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | gender + department + quarter:company | 0 | user_id, data=df_network),
  felm(i_we ~ log_tenure + log_num_tokens + log_network_size + log_clustering*global_reach | user_id + department + quarter:company | 0 | user_id, data=df_network)
)

stargazer(interaction_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Clustering and Bridging Controlling for Managerial Status, Within-Person", add.lines = list(c("Person Fixed Effects", rep("Yes", length(within_manager_controls))), c("Department Controls", rep("Yes", length(within_manager_controls))), c("Manager Controls", rep("Yes", length(within_manager_controls))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_manager_controls)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"),type="text")
#latex output (change FE text in overleaf)
stargazer(interaction_mods, digits=3, header=F, star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), notes = c("$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001", "Standard errors clustered at the person level", "Tenure, Number of Tokens, Network Size, and Local Clustering are logged due to right skewed distributions"), notes.append=F, title="Identification on Clustering and Bridging Controlling for Managerial Status, Within-Person", add.lines = list(c("Person Fixed Effects", rep("Yes", length(within_manager_controls))), c("Department Controls", rep("Yes", length(within_manager_controls))), c("Manager Controls", rep("Yes", length(within_manager_controls))), c("Company-by-Quarter Fixed effects", rep("Yes", length(within_manager_controls)))), dep.var.labels = "Organizational Identification", covariate.labels=c("Tenure", "Number of Tokens", "Network Size", "Local Clustering", "Global Reach"))

##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
