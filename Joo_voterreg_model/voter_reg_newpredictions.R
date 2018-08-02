source('config/init.R')
source('config/pgConnect.R')
source('config/voter_regfunctions.R') # all functions are here
source('config/data_source.R') # data sources
pg <- pgConnect()

# TO UPDATE BASED ON MODEL PERFORMANCE
library(lubridate)
library(reshape2)
library(caret)
library(MLmetrics)
library(nnet)
library(xgboost)
library(gbm)
library(randomForest)
library(zoo)
library(rlang)
library(dplyr)

options(mc.cores = parallel::detectCores())

##################### Load Data ########################

load_data(days_refreshed = 0)
list2env(data_list, .GlobalEnv)

################# Create master dataset ################

# concern that some variables might not contain enough information? 

create_master <- function(...){
  
  # predicted variable 
  voter_reg_status <- predicted_feature(users, turbo, campaign, phoenix, email)
  
  # predictor variables based on initial modeling culling (more info available in voter_reg.Rmd file)
  gender <- gender_feature(users)
  age <- age_feature(users)
  ses <- ses_feature(users)
  state_politics <- dem_rep_feature(users)
  email_opened <- 
    email_feature(email) %>% 
    select(northstar_id, email_opened)
  time_mostactive <- time_mostactive_feature(phoenix, mel) 
  gambit_signup_sitelogin <- 
    MAM_action_feature(mel) %>%
    select(northstar_id, messaged_gambit, signup, site_login) 
  last_action <- last_action_feature(mel)
  
  # create master dataset
  master <- plyr::join_all(list(voter_reg_status, 
                                gender,
                                age,
                                ses,
                                state_politics,
                                email_opened,
                                time_mostactive,
                                gambit_signup_sitelogin,
                                last_action),
                           by = "northstar_id",
                           type = "left")
  
  # clean up: replace NAs 
    # NA-replacement for numeric predictors 
    vector_mean <- "age" # age or any other appropriate column that should be imputed with mean
    vector_zeros <- # vector of column names 
    colnames(master %>% 
               select_if(is.numeric) %>% 
               select(-age)) 
    master <- replace_na_numeric(master, vector_mean, vector_zeros) 
  
    # NA-replacement for factor variables
    master <- replace_na_else(master) 
    
  # Save untransformed dataset
  saveRDS(master, "for_quasar.rds")
  
  return(master)
  
}

################ Center and scale data ##################

preprocvalues <- preProcess(master, method = c("center", "scale")) 
master <- predict(preprocvalues, master) 


################ Final prep for modeling ################# 

# extract data with no status for predictions 
predictions_only  <- 
  master %>% 
  filter(is.na(voter_reg_status)) %>%
  select(-voter_reg_status)


################### Predictions ###########################

voter_reg_prediction <- voter_reg_predict(predictions_only)


########### Append to existing quasar table #################

channel <- pgConnect()

dbWriteTable(channel, 
             c("public", "voter_reg_predictions"), 
             voter_reg_prediction, 
             append=T, row.names=F)



















