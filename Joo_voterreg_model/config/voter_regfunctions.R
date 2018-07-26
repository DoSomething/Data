################## Initial Data Wrangling #######################

# remove northstar_ids that are under 18, do not live in the US, and are unsubscribed
remove18 <- function(x, y) {
  output <- x %>% semi_join(y, by = "northstar_id") 
  # semi_join returns all rows from x where there are matching values in y, keeping just columns from x
  
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
  
  # combine: registration > uncertain > ineligible
  users_turbo <- 
    fromusers %>%
    left_join(fromturbo, by = "northstar_id") %>%
    mutate(voter_reg_status = case_when(
      voter_reg_status.x == "registration_complete" | voter_reg_status.y == "registration_complete" ~ "registration_complete",
      voter_reg_status.x == "uncertain" | voter_reg_status.y == "uncertain" ~ "uncertain",
      voter_reg_status.y == "ineligible" ~ "ineligible",
      TRUE ~ NA_character_
    )) %>%
    select(northstar_id, voter_reg_status)
  
  # voter reg status (any interaction) from campaign, phoenix, email 
  voter_reg_emails <- read.csv("voter_reg_emails.csv")
  voter_reg_ids <- c("8017", "7059", "822", "8129", "7951", "7927") # campaign ids for voter registration campaigns 
  voter_reg_template_ids <- voter_reg_emails$template_id # template ids for voter registration emails 
  
  fromcampaign <- 
    campaign %>%
    filter(campaign_id %in% voter_reg_ids & 
             (!is.na(signup_created_at) | !is.na(post_attribution_date))) %>%
    select(northstar_id)
  fromphoenix <- 
    phoenix %>%
    filter(campaign_id %in% voter_reg_ids) %>%
    select(northstar_id)
  fromemail <- 
    email %>%
    filter(event_type != "email_unsubscribed" 
           & template_id %in% voter_reg_template_ids) %>%
    select(northstar_id)
  all_interactions <- 
    bind_rows(fromcampaign, fromphoenix, fromemail) %>%
    distinct() %>%
    mutate(voter_reg_status = "interaction")
  
  # combine all: registration_complete > uncertain > interaction > ineligible
  output <- 
    users_turbo %>%
    left_join(all_interactions, by = "northstar_id") %>%
    mutate(voter_reg_status = case_when(
      voter_reg_status.x == "registration_complete" | voter_reg_status.y == "registration_complete" ~ "registration_complete",
      voter_reg_status.x == "uncertain" | voter_reg_status.y == "uncertain" ~ "registration_started",
      voter_reg_status.y == "interaction" ~ "any_interaction",
      voter_reg_status.x == "ineligible" ~ "ineligible",
      TRUE ~ NA_character_
    )) %>%
    select(northstar_id, voter_reg_status)
  
  return(output)
  
}



################## Feature Extraction #######################

# gender feature (extracts gender using list "name_gender.csv" - can be substituted with another gender list)
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

# SES feature 
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

# state + Democrat vs. Republican feature 

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
    left_join(dems_reps_list, by = "state")
  
  return(output)
  
}

# returns columns of email clicked, converted, opened, and unsubscribed per northstar_id 
email_feature <- function(email) {
  output <- 
    email %>% 
    group_by(northstar_id, event_type) %>%
    tally %>% 
    spread(key = event_type, value = n, fill = 0) %>%
    rowwise() %>%
    mutate(email_total = sum(email_clicked, email_converted, email_opened)) # removes email_unsubscribed variable in email total count
  
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

total_actions_feature <- function(phoenix, campaign, email, sms) {
  web <- 
    phoenix %>% 
    filter(event_name %in% c("view", "visit", "open modal")) %>%
    select(northstar_id)
  signup <- 
    campaign %>% 
    filter(!is.na(signup_created_at)) %>% 
    select(northstar_id)
  post <- 
    campaign %>%
    filter(!is.na(post_attribution_date)) %>%
    select(northstar_id)
  email_action <- 
    email %>%
    filter(event_type != "email_unsubscribed") %>%
    select(northstar_id)
  sms_action <- 
    sms %>%
    select(northstar_id)
  output <- 
    bind_rows(web, signup, post, email_action, sms_action) %>%
    group_by(northstar_id) %>% 
    summarise(total_action = n())
  
  return(output)
}

# whether or not someone has reportedback ever (according to campaigns) 

rb_feature <- function(campaign, users) {
  rb <- 
    campaign %>% 
    filter(post_status == "accepted") %>%
    distinct(northstar_id) %>%
    mutate(reportback_status = "Yes")
  output <- 
    users %>%
    select(northstar_id) %>%
    full_join(rb, by = "northstar_id") %>%
    replace_na(list(reportback_status = "No")) # is a character vector, leave until replace_na function later to change to factor
  
  return(output)  
}

# time of activity (uses member event log) 

timeday_feature <- function(mel, users) {
  time <- 
    mel %>%
    select(northstar_id, timestamp) %>%
    drop_na() %>%
    mutate(time_active = case_when(
      hour(timestamp) %in% c(0:5) ~ "night_activities",
      hour(timestamp) %in% c(6:11) ~ "morning_activities",
      hour(timestamp) %in% c(12:17) ~ "afternoon_activities",
      hour(timestamp) %in% c(18:23) ~ "evening_activities",
      TRUE ~ NA_character_
    )) %>%
    group_by(northstar_id, time_active) %>%
    tally %>%
    spread(key = time_active, value = n, fill = 0)
  output <-
    users %>%
    select(northstar_id) %>%
    left_join(time, by = "northstar_id")
  
  return(output)
  
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

# how many months ago was their last activity? 

last_action_feature <- function(mel) { 
  output <- 
    mel %>%
    group_by(northstar_id) %>% 
    dplyr::slice(which.max(timestamp)) %>% 
    select(northstar_id, timestamp) %>% 
    mutate(month_diff = time_length(difftime(Sys.Date(), timestamp, units = "days"), "month")) %>%
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
        grepl("Disasters", campaign_cause_type) ~ "Environment",
      grepl("Health", campaign_cause_type) |
        grepl("Relationships", campaign_cause_type) |
        grepl("Violence", campaign_cause_type) ~ "Health",
      grepl("Animals", campaign_cause_type) ~ "Animals", 
      grepl("Bullying", campaign_cause_type) | 
        grepl("Education", campaign_cause_type) | 
        grepl("Homelessness", campaign_cause_type) |
        grepl("Poverty", campaign_cause_type) | 
        grepl("Discrimination", campaign_cause_type) | 
        grepl("Education", campaign_cause_type) ~ "Social",
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

# replace all NAs in numeric columns with 0's
replace_na_numeric <- function(data) {
  output <- 
    data %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
  return(output)
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

# multiple imputation to deal with missing data for categorical variables (something that we want to do?)

multi_imput <- function(master) {
  imp <- mice(master, m = 5, method = "polyreg", seed = 12345)
  output <- complete(imp)
  
  return(output)
}

# removing nzv variables - returns list: 
# 1) table with metrics of variance & near zero variance (nzv) and 
# 2) dataframe that has eliminated nzv variables  

remove_nzv <- function(x) {
  nzv <- nearZeroVar(x, saveMetrics = TRUE)
  nzv2 <- nearZeroVar(x, names = TRUE)
  nzv2 <- nzv2[!nzv2 %in% "voter_reg_status"] # voter_reg_status = nzv 
  filtered_df <- select(x, -nzv2)
  
  output_list <- list(nzv, filtered_df)
  
  return(output_list)
}

# removing correlation variables- returns list:
# 1) correlation matrix 
# 2) names of highly correlated variables 
# 3) dataframe htat has eliminated highly correlated variables

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



###################### Modeling ##########################

# Calculating AUC 

AUC_function <- function(predictions, test_data, class) { # class must be in quotes for input
  class_pred <- ifelse(predictions == class, 1, 0)
  class_test <- ifelse(test_data$voter_reg_status == class, 1, 0)
  output <- AUC(y_pred = class_pred, y_true = class_test)
  
  return(output)
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










