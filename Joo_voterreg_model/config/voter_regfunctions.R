source('config/data_source.R')
source('config/init.R')
source('config/pgConnect.R')

#################################################################
################ Initial Modeling & Prediction ##################
#################################################################

###################### Load Data Set ############################

load_data <- function(days_refreshed) { # set number of days since last running the query
  if (difftime(Sys.Date(), file.mtime("Rdata/users.rds")) > days_refreshed) {
    
    # run queries for latest data from Quasar
    users <- runQuery(users_query)
    phoenix <- runQuery(phoenix_query)
    mel <- runQuery(mel_query)
    campaign <- runQuery(ca_query)
    campaign_info <- runQuery(ca_info_query)
    email <- runQuery(email_query)
    sms <- runQuery(SMS_mes)
    turbo <- runQuery(turbo_query)  
    
    # remove northstar_ids that are under 18, do not live in the US, and are unsubscribed based on users data
    mel <- remove18(mel, users)
    phoenix <- remove18(phoenix, users)
    campaign <- remove18(campaign, users)
    email <- remove18(email, users)
    sms <- remove18(sms, users)
    turbo <- remove18(turbo, users)
    
    # save the files as Rdata files
    saveRDS(users, "Rdata/users.rds")
    saveRDS(phoenix, "Rdata/phoenix.rds")
    saveRDS(mel, "Rdata/mel.rds")
    saveRDS(campaign, "Rdata/campaign.rds")
    saveRDS(campaign_info, "Rdata/campaign_info.rds")
    saveRDS(email, "Rdata/email.rds")
    saveRDS(sms, "Rdata/sms.rds")
    saveRDS(turbo, "Rdata/turbo.rds")
    
    data_list <- list(users = users, 
                      phoenix = phoenix,
                      mel = mel,
                      campaign = campaign,
                      campaign_info = campaign_info,
                      email = email,
                      sms = sms,
                      turbo = turbo)
    
  } else {
    
    # load the files 
    users <- readRDS("Rdata/users.rds")
    phoenix <- readRDS("Rdata/phoenix.rds")
    mel <- readRDS("Rdata/mel.rds")
    campaign <- readRDS("Rdata/campaign.rds")
    campaign_info <- readRDS("Rdata/campaign_info.rds")
    email <- readRDS("Rdata/email.rds")
    sms <- readRDS("Rdata/sms.rds")
    turbo <- readRDS("Rdata/turbo.rds")
    
    data_list <- list(users = users, 
                      phoenix = phoenix,
                      mel = mel,
                      campaign = campaign,
                      campaign_info = campaign_info,
                      email = email,
                      sms = sms,
                      turbo = turbo)
    
  }
  
  return(data_list)
    
}


################## Initial Data Wrangling #######################

# remove northstar_ids that are under 18, do not live in the US, and are unsubscribed
remove18 <- function(x, users) {
  output <- 
    x %>% 
    semi_join(users, by = "northstar_id") # returns all rows from x where there are matching values in y, keeping just columns from x
  
  return(output)
}

################## Predicted Feature #########################

predicted_feature <- function(users, turbo, campaign, phoenix, email) {
  # (this code will be obsolete once the two statuses are combined & deduped in public.users)
  
  # voter reg status from users 
  fromusers <- 
    users %>%
    select(northstar_id, voter_registration_status) %>%
    rename(voter_reg_status = voter_registration_status) %>%
    mutate(voter_reg_status = replace(voter_reg_status, 
                                      voter_reg_status %in% c("confirmed", "unregistered"),
                                      NA)) 
  
  # voter reg status from turbo
  fromturbo <- 
    turbo %>% 
    select(northstar_id, ds_vr_status) %>%
    rename(voter_reg_status = ds_vr_status) %>%
    mutate(voter_reg_status = replace(voter_reg_status, 
                                      voter_reg_status %in% c("register-OVR", "register-form"),
                                      "registration_complete"),
           voter_reg_status = replace(voter_reg_status,
                                      voter_reg_status %in% c("confirmed", "unregistered"),
                                      NA)) 
  
  # combine: registration_complete > registration_started > ineligible
  users_turbo <- 
    bind_rows(fromusers, fromturbo) %>% 
    distinct() %>% 
    mutate(voter_reg_status = case_when(
      voter_reg_status == "registration_complete" ~ "registration_complete",
      voter_reg_status == "uncertain" ~ "registration_started",
      voter_reg_status == "ineligible" ~ "ineligible",
      TRUE ~ NA_character_
    ),
    ordered = case_when(
      voter_reg_status == "registration_complete" ~ 1,
      voter_reg_status == "registration_started" ~ 2,
      voter_reg_status == "ineligible" ~ 3,
      TRUE ~ 4
      )) %>%
    group_by(northstar_id) %>%
    dplyr::slice(which.min(ordered)) %>%
    select(northstar_id, voter_reg_status)
  
  # voter reg status (any interaction) from campaign, phoenix, email 
  voter_reg_emails <- read.csv("voter_reg_emails.csv") # contains template_ids of all emails re: voter reg 
  
  # campaign_run_ids & campaign_ids for voter registration campaigns 
  voter_runids <- c("8022", # gtm
                     "7060", "7944", "8105", "8128", "8152", # lose your v-card
                     "6223", "8103", # school the vote
                     "8151", # red, white, and booth
                     "7952", # do something about gun violence (members asked to register in the affirmation)
                     "7928") # defend dreamers (members asked to register in the affirmation) 
  voter_ids <- c("8017", # gtm
                 "7059", # lose your v-card
                 "822", # school the vote
                 "8129") #red, white, and booth
  voter_reg_template_ids <- voter_reg_emails$template_id # template ids for voter registration emails 
  
  fromcampaign <- 
    campaign %>%
    filter(campaign_run_id %in% voter_runids & 
             (!is.na(signup_created_at) | !is.na(post_attribution_date))) %>%
    select(northstar_id)
  fromphoenix <- 
    phoenix %>%
    filter(campaign_id %in% voter_ids) %>%
    select(northstar_id)
  fromemail <- 
    email %>%
    filter(event_type != "email_unsubscribed" 
           & template_id %in% voter_reg_template_ids) %>%
    select(northstar_id)
  all_interactions <- 
    bind_rows(fromcampaign, fromphoenix, fromemail) %>%
    distinct() %>%
    mutate(voter_reg_status = "any_interaction")
  
  # combine all: registration_complete > registration_started > any_interaction > ineligible
  output <- 
    bind_rows(users_turbo, all_interactions) %>% 
    mutate(ordered = case_when(
      voter_reg_status == "registration_complete" ~ 1,
      voter_reg_status == "registration_started" ~ 2,
      voter_reg_status == "any_interaction" ~ 3,
      voter_reg_status == "ineligible" ~ 4,
      TRUE ~ 5
    )) %>% 
    dplyr::slice(which.min(ordered)) %>% 
    select(northstar_id, voter_reg_status)
  
  return(output)
  
}


################## Feature Extraction #######################

# gender feature (extracts gender using list "name_gender.csv")
gender_feature <- function(users) {
  gender_list <- read.csv("name_gender.csv")
  users$first_name <- tolower(users$first_name)
  gender_list$name <- tolower(gender_list$name)
  gender <- 
    gender_list %>% 
    select(name, gender) %>%
    rename(first_name = name)
  output <- 
    users %>% 
    separate(first_name, 
             into = c("first_name", "name_remove"), 
             extra = "merge", 
             fill = "right") %>% 
    select(northstar_id, first_name) %>%
    left_join(gender, by = "first_name") %>%
    select(northstar_id, gender)
  
  return(output)
}

# age feature 
age_feature <- function(users) {
  output <- 
    users %>% 
    select(northstar_id, birthdate) %>%
    mutate(age = as.integer(time_length(difftime(Sys.Date(), users$birthdate), "years"))) %>% 
    select(-birthdate)
  
  return(output)
}

# SES feature (extracts ses_status using list "zipcode_ses.csv")
ses_feature <- function(users) {
  ses_list <- read.csv("zipcode_ses.csv")
  users$zipcode <- as.integer(users$zipcode)
  output <- 
    users %>% 
    left_join(ses_list, by = "zipcode") %>%
    select(northstar_id, socioeconomic_status) %>%
    rename(ses_status = socioeconomic_status)
  
  return(output)
}

# 2016 election results feature from state variable 
dem_rep_feature <- function(users) {
  # from 2016 election 
  dems <- c("CA", "CO", "CT", "DE", "HI", "IL", "ME", "MD", "MA", "MN",
            "NV", "NH", "NJ", "NM", "NY", "OR", "VT", "VA", "WA") 
  reps <- c("AL", "AK", "AZ", "AR", "FL", "GA", "ID", "IN", "IA", "KS", "KY", 
            "LA", "MI", "MS", "MO", "MT", "NE", "NC", "ND", "OH", "OK", "PA",
            "RI", "SC", "SD", "TN", "TX", "UT", "WV", "WI", "WY")
  state_list <- as.data.frame(c(dems, reps), stringsAsFactors = FALSE)
  dems_reps_list <- 
    state_list %>% 
    mutate(election_2016 = ifelse(state_list$`c(dems, reps)` %in% dems, 
                                  "democratic", "republican")) %>%
    rename(state = `c(dems, reps)`)
  output <- 
    users %>% 
    select(northstar_id, state) %>% 
    left_join(dems_reps_list, by = "state") %>%
    select(-state)
  
  return(output)
  
}

# returns columns of email clicked, opened, and unsubscribed per northstar_id 
email_feature <- function(email) {
  output <- 
    email %>% 
    filter(event_type != "email_converted") %>%
    group_by(northstar_id, event_type) %>%
    tally %>% 
    spread(key = event_type, value = n, fill = 0) %>%
    rowwise() %>%
    mutate(email_total = sum(email_clicked, email_opened)) # removes email_unsubscribed variable in email total count
  
  return(output)
}

# returns columns of SMS action by type & total number of actions per northstar_id 
sms_total_feature <- function(sms) {
  output <-
    sms %>% 
    group_by(northstar_id) %>%
    tally %>%
    rename(sms_total = n)
  
  return(output)
}

# returns columns of browser size by northstar_id 
browser_size_feature <- function(phoenix) {
  output <-
    phoenix %>% 
    select(northstar_id, browser_size) %>%
    group_by(northstar_id, browser_size) %>% 
    tally %>%
    spread(key = browser_size, value = n, fill = 0) %>%
    rename(browser_large = large,
           browser_medium = medium,
           browser_small = small)
  
  return(output)
}

# returns total # of actions per northstar_id 
total_actions_feature <- function(phoenix, mel) {
  web_actions <- 
    phoenix %>%
    filter(event_name %in% c("view", "visit", "open modal")) %>%
    select(northstar_id)
  mel_actions <- 
    mel %>% 
    select(northstar_id)
  output <- 
    bind_rows(web_actions, mel) %>% 
    group_by(northstar_id) %>% 
    summarise(total_actions = n())
  
  return(output)
}

# how many reportbacks a user has done 
rb_feature <- function(campaign, users) {
  rb <- 
    campaign %>% 
    filter(post_status == "accepted") %>%
    select(northstar_id, reportback_volume) %>%
    group_by(northstar_id) %>% 
    summarise(total_rbs = n())
  output <- 
    users %>%
    select(northstar_id) %>%
    left_join(rb, by = "northstar_id")
  
  return(output)  
}

# when user is most active 
## if user active during multiple times, categorized as "all_day"
time_mostactive_feature <- function(phoenix, mel) {
  web_time <- 
    phoenix %>% 
    filter(event_name %in% c("view", "visit", "open modal")) %>% 
    select(northstar_id, event_datetime) %>% 
    rename(timestamp = event_datetime)
  mel_time <- 
    mel %>% 
    select(northstar_id, timestamp)
  most_active <- 
    bind_rows(web_time, mel_time) %>% 
    mutate(time_active = case_when(
      hour(timestamp) %in% c(0:5) ~ "night",
      hour(timestamp) %in% c(6:11) ~ "morning",
      hour(timestamp) %in% c(12:17) ~ "afternoon",
      hour(timestamp) %in% c(18:23) ~ "evening",
      TRUE ~ NA_character_
    )) %>% 
    select(-timestamp) %>% 
    group_by(northstar_id, time_active) %>% 
    summarise(freq = n()) %>% 
    filter(freq == max(freq)) %>% 
    mutate(occur = n()) %>%
    mutate(time_active = ifelse(occur > 1, "all_day", time_active)) %>% 
    distinct(northstar_id, time_active)
  
  return(most_active)
}


# monthly active membership eligible action count by action type 
MAM_action_feature <- function(mel) {
  output <- 
    mel %>%
    select(northstar_id, action_type) %>%
    filter(action_type != "registered") %>%
    group_by(northstar_id, action_type) %>%
    tally %>% 
    spread(key = action_type, value = n, fill = 0) %>% 
    rename(fb_share_completed = `facebook share completed`)
  
  return(output)
}

# how many days ago was their last activity? (the lower the value, the most recently active the user)
last_action_feature <- function(mel) { 
  output <- 
    mel %>%
    group_by(northstar_id) %>% 
    dplyr::slice(which.max(timestamp)) %>% 
    select(northstar_id, timestamp) %>% 
    mutate(days_since_lastactive = as.numeric(difftime(Sys.Date(), timestamp, units = "days"))) %>%
    select(-timestamp) 
  
  return(output)
  
}

# what cause spaces have they participated in? 
cause_space_feature <- function(users, campaign, campaign_info) {
  campaign_with_id <- 
    campaign %>% 
    filter(post_status == "accepted") %>% 
    select(northstar_id, campaign_run_id) %>% 
    left_join(campaign_info, by = "campaign_run_id") %>% 
    select(northstar_id, campaign_cause_type) %>% 
    filter(!is.na(campaign_cause_type)) %>%
    mutate(cause_area = case_when(
      grepl("Environment", campaign_cause_type) |
        grepl("Disasters", campaign_cause_type) ~ "cause_area_environment",
      grepl("Health", campaign_cause_type) |
        grepl("Relationships", campaign_cause_type) |
        grepl("Violence", campaign_cause_type) ~ "cause_area_health",
      grepl("Animals", campaign_cause_type) ~ "cause_area_animals", 
      grepl("Bullying", campaign_cause_type) | 
        grepl("Education", campaign_cause_type) | 
        grepl("Homelessness", campaign_cause_type) |
        grepl("Poverty", campaign_cause_type) | 
        grepl("Discrimination", campaign_cause_type) | 
        grepl("Education", campaign_cause_type) ~ "cause_area_social",
      TRUE ~ NA_character_
    )) %>% 
    select(northstar_id, cause_area) %>% 
    filter(!is.na(cause_area)) %>% 
    group_by(northstar_id, cause_area) %>% 
    tally %>% 
    spread(key = cause_area, value = n, fill = 0)
  
  output <- 
    users %>% 
    select(northstar_id) %>%
    left_join(campaign_with_id, by = "northstar_id")
  
  return(output)
}

######################### Pre-processing ########################

# replace all NAs in numeric columns with 0's OR means 
replace_na_numeric <- function(data, vector_mean, vector_zeros) {  
  vector_mean # should input a vector with names of variables for which you'd want to replace NA's with means
  vector_zeros # should input a vector with names of all variables for which you'd want to replace NA's with 0
  
  data_imputed <- 
    data %>% 
    mutate_at(vars(vector_mean), na.aggregate) %>% 
    mutate_at(vars(vector_zeros), funs(replace(., is.na(.), 0)))
  
  return(data_imputed)
}

# replace all other NAs in categorical variables (except northstar_id) with "Unknown"
replace_na_else <- function(data) {
  non_num <- 
    data %>%
    select_if(negate(is.numeric)) %>%
    select(-c(northstar_id, voter_reg_status))
  non_num_names <- names(non_num)
  output <- 
    data %>%
    mutate_at(vars(non_num_names), as.character) %>% 
    mutate_at(vars(non_num_names), funs(replace(., is.na(.), "Unknown"))) %>%
    mutate_at(vars(non_num_names), factor)
  
  return(output)
}

# removing nzv variables - returns list: 
## 1) table with metrics of variance & near zero variance (nzv) and 
## 2) dataframe that has eliminated nzv variables  

remove_nzv <- function(x) {
  nzv <- nearZeroVar(x, saveMetrics = TRUE)
  nzv2 <- nearZeroVar(x, names = TRUE)
  nzv2 <- nzv2[!nzv2 %in% "voter_reg_status"] # voter_reg_status = nzv 
  filtered_df <- select(x, -nzv2)
  
  output_list <- list(nzv, filtered_df)
  
  return(output_list)
}

# removing correlation variables- returns list:
## 1) correlation matrix 
## 2) names of highly correlated variables 
## 3) dataframe htat has eliminated highly correlated variables

remove_highcor <- function(x) {
  numeric_only <- 
    x %>%
    select_if(is.numeric) # selects only numeric variables for correlation matrix 
  df_corr <- cor(numeric_only) # correlation matrix 
  high_corr <- findCorrelation(df_corr, cutoff = .75, names = TRUE) # spits out variables above the cutoff 
  x_filtered <- select(x, -high_corr)
  
  output_list<- list(df_corr, high_corr, x_filtered)
  
  return(output_list) # returns list with corr matrix, names of highly corr variables, and data frame without them
}

# Create final master dataset that has four categories:
# registration_complete, registration_started, any_interaction, no_interaction

master_final <- function(master) {
  # filter the three levels of interest 
  only_three <- 
    master %>% 
    filter(voter_reg_status %in% c("registration_complete", "registration_started", "any_interaction"))
  
  # get info we need to sample randomly from the NA category 
  most_common <- names(which.max(table(only_three$voter_reg_status))) # get value most frequent in voter_reg_status
  sample_size <- sum(only_three$voter_reg_status == most_common) # get its frequency 
  
  # randomly sample from the NA rows 
  no_interaction_sample <- 
    master %>% 
    filter(is.na(voter_reg_status)) %>% 
    sample_n(sample_size, replace = FALSE) %>% 
    replace_na(list(voter_reg_status = "no_interaction"))
  
  # combine to get the final dataset 
  combined <- 
    bind_rows(only_three, no_interaction_sample)
  
  # create ordered factor levels 
  combined$voter_reg_status <- 
    factor(combined$voter_reg_status,
           levels = c("no_interaction", "any_interaction", "registration_started", "registration_complete"),
           labels = c("no_interaction", "any_interaction", "registration_started", "registration_complete"))
  
  return(combined)
}

# Balancing training dataset 

balance_data <- function(training_data, number_per_group) {
  # sample without replacement for no_interaction & any_interaction 
  no_any_interaction <- 
    training_data %>% 
    filter(voter_reg_status %in% c("no_interaction", "any_interaction")) %>% 
    group_by(voter_reg_status) %>% 
    sample_n(number_per_group)
  
  # registration_complete: keep all reg_complete rows + sample with placement to meet numbers_per_group threshold
  reg_complete <- 
    training_data %>% 
    filter(voter_reg_status == "registration_complete")
  number_to_sample <- number_per_group - nrow(reg_complete)
  reg_complete_samp <- 
    reg_complete %>% 
    sample_n(size = number_to_sample, replace = TRUE)
  all_reg_complete <- 
    bind_rows(reg_complete, reg_complete_samp)
  
  # registration_started: keep all reg_started rows + sample with placement to meet numbers_per_group threshold 
  reg_started <- 
    training_data %>% 
    filter(voter_reg_status == "registration_started")
  number_to_sample <- number_per_group - nrow(reg_started)
  reg_started_samp <- 
    reg_started %>% 
    sample_n(size = number_to_sample, replace = TRUE)
  all_reg_started <- 
    bind_rows(reg_started, reg_started_samp)
  
  # bind all of them to create a balanced training set 
  training <- 
    bind_rows(no_any_interaction,
              all_reg_complete,
              all_reg_started)
  
  return(training)
} 



###################### Modeling & Performance Evaluation ##########################

# transforming training and testing data to appropriate class for xgboost modeling

xgboost_data_transform_function <- function(data) {
  xgboost_master <- 
    data %>% 
    ungroup() %>% 
    mutate_if(is.factor, as.numeric) %>% 
    select_if(is.numeric) # xgboost only takes integer/numeric predictors (this also removes northstar_id in test data)
  xgboost_label <- xgboost_master$voter_reg_status # predicted variable 
  xgboost_label <- xgboost_label - 1 # "classes" must start at 0, not 1 
  xgboost_data <- data.matrix(xgboost_master) # xgboost only takes matrices or xgb.DMatrix 
  xgboost_data <- xgboost_data[, !(colnames(xgboost_data) %in% "voter_reg_status")] # remove voter_reg in predictor matrix 
  xgboost_matrix <- xgb.DMatrix(data = xgboost_data, label = xgboost_label) # construct a xgb.DMatrix object
  
  return(xgboost_matrix)
}


# gbm predictions vector (make it comparable to the test data)

gbm_transform_function <- function(gbm_pred) {
  gbm_pred <- as.data.frame(gbm_pred)
  gbm_pred <- colnames(gbm_pred)[max.col(gbm_pred)]
  gbm_pred <- str_split(gbm_pred, "[.]" , simplify = TRUE)[, 1]
  gbm_pred <- factor(gbm_pred,
                     levels = c("no_interaction", "any_interaction", "registration_started", "registration_complete"),
                     labels = c("no_interaction", "any_interaction", "registration_started", "registration_complete"))
  
  return(gbm_pred)
}


# Creating a list with confusion matrix + accuracy rate & AUC rate dataframe 

mod_perf_function <- function(model_name, predictions, test_data) { # model_name must be in quotes 
  test_ref <- test_data$voter_reg_status
  results <- 
    confusionMatrix(predictions, test_ref)
  
  # confusion matrix 
  confusion_mat <- results$table
  
  # data frame with performance results 
  accuracy_metric <- results$overall["Accuracy"]
  AUC_metric <- results$byClass[, "Balanced Accuracy"]
  
  accur_df <- 
    as.data.frame(accuracy_metric) %>% 
    rename(!!quo_name(model_name) := accuracy_metric)
  
  AUC_df <- 
    as.data.frame(AUC_metric) %>% 
    rename(!!quo_name(model_name) := AUC_metric)
  
  metric <- c("Overall accuracy", 
              "AUC: no_interaction", 
              "AUC: any_interaction", 
              "AUC: registration_started", 
              "AUC: registration_complete")
  performance <- bind_rows(accur_df, AUC_df)
  perf_df <- cbind(metric, performance)
  
  perf_list <- list(confusion_mat, perf_df)
  
  return(perf_list) 
}
 

# Comparing performance of all models 

model_comparison <- function(rf_results, ord_logit_results, nnet_results, 
                             xgboost_results, xgboost_tree_results, gbm_results) { 
  df <- 
    rf_results[[2]] %>% 
    left_join(ord_logit_results[[2]], by = "metric") %>%
    left_join(nnet_results[[2]], by = "metric") %>%
    left_join(xgboost_results[[2]], by = "metric") %>% 
    left_join(xgboost_tree_results[[2]], by = "metric") %>%
    left_join(gbm_results[[2]], by = "metric") 
    
  return(df)
}

# Ranking models with best performance 

model_select <- function(performance_df, which_metric) { # which_metric must be in quotes
  ranked_df <- 
    performance_df %>% 
    filter(metric == which_metric) %>%
    select_if(is.numeric) %>% 
    gather(key = "model", value = "performance") %>%
    arrange(desc(performance))
  
  return(ranked_df)
}

# Making final predictions to load into Quasar 

voter_reg_predict <- function(new_data, latest_untransformed.rds) {
  best_model <- readRDS("models/best_model_ordlogit.rds")
  
  predictions <- predict(best_model, 
                         newdata = new_data, 
                         type = "class")
  all_predictions <- 
    cbind(new_data, predictions) %>% 
    select(northstar_id, predictions)
  
  full_data <- readRDS(latest_untransformed.rds)
  quasar_table<- 
    full_data %>% 
    full_join(all_predictions, by = "northstar_id")
  
  return(quasar_table)
}


#################################################################
################ Prediction for New Users #######################
#################################################################

####################### Load Latest Data #########################

new_users_only <- function(...) {
  
  # 1. load from quasar's voter_reg_predictions table 
  last_voterreg_table <- runQuery(voter_reg_table_query)
  
  # 2. query for newest data from quasar & remove under 18, abroad, and unsubscribed 
  users <- runQuery(users_query)
  phoenix <- runQuery(phoenix_query)
  mel <- runQuery(mel_query)
  campaign <- runQuery(ca_query)
  campaign_info <- runQuery(ca_info_query)
  email <- runQuery(email_query)
  sms <- runQuery(SMS_mes)
  turbo <- runQuery(turbo_query)  
  
  mel <- remove18(mel, users)
  phoenix <- remove18(phoenix, users)
  campaign <- remove18(campaign, users)
  email <- remove18(email, users)
  sms <- remove18(sms, users)
  turbo <- remove18(turbo, users)
  
  # 3. extract only the newest users and their actions
  users <- 
    users %>% 
    anti_join(last_voterreg_table, by = "northstar_id")
  phoenix <- 
    phoenix %>% 
    anti_join(last_voterreg_table, by = "northstar_id")
  mel <- 
    mel %>% 
    anti_join(last_voterreg_table, by = "northstar_id")
  campaign <- 
    campaign %>% 
    anti_join(last_voterreg_table, by = "northstar_id") 
  email <- 
    email %>%
    anti_join(last_voterreg_table, by = "northstar_id")
  sms <- 
    sms %>% 
    anti_join(last_voterreg_table, by = "northstar_id")
  turbo <- 
    turbo %>%
    anti_join(last_voterreg_table, by = "northstar_id")
  
  data_list <- list(users = users, 
                    phoenix = phoenix,
                    mel = mel,
                    campaign = campaign,
                    campaign_info = campaign_info,
                    email = email,
                    sms = sms,
                    turbo = turbo)
  
  return(data_list)
  
}

################# Create master dataset for new members ################

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
  master <- 
    users %>% 
    select(northstar_id) %>% 
    left_join(voter_reg_status, by = "northstar_id") %>% 
    left_join(gender, by = "northstar_id") %>% 
    left_join(age, by = "northstar_id") %>% 
    left_join(ses, by = "northstar_id") %>% 
    left_join(state_politics, by = "northstar_id") %>% 
    left_join(email_opened, by = "northstar_id") %>% 
    left_join(time_mostactive, by = "northstar_id") %>% 
    left_join(gambit_signup_sitelogin, by = "northstar_id") %>% 
    left_join(last_action, by = "northstar_id")
  
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
  saveRDS(master, "future_predictions/join_quasar.rds")
  
  return(master)
  
}























