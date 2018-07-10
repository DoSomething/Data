################## Initial Data Wrangling #######################

# remove northstar_ids that are under 18, do not live in the US, and are unsubscribed
remove18 <- function(x, y) {
  x %>% semi_join(y, by = "northstar_id")
}

# replace all NAs in numeric columns with 0's
replace_na_numeric <- function(data) {
  num_cols <- sapply(data, is.numeric)
  data[, num_cols] <- 
    apply(data[, num_cols], 2, function(x){replace(x, is.na(x), 0)})
  
  return(data)
}

################## Predicted Feature #########################

predicted <- function(users) {
  predicted_col <- 
    users %>%
    select(northstar_id, voter_registration_status)
  
  return(predicted_col)
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
  users$first_name <- sapply(strsplit(users$first_name, "-|\\s"), function(x) x[[1]])
  gender_col <- 
    users %>% 
    left_join(gender, by = "first_name") %>%
    select(northstar_id, gender)
  
  return(gender_col)
}

# age feature 
age_feature <- function(users) {
  age_col <- 
    users %>% 
    mutate(age = as.integer(time_length(difftime(Sys.Date(), users$birthdate), "years"))) %>% 
    select(northstar_id, age)
  
  return(age_col)
}

# returns columns of email clicked, converted, opened, and unsubscribed per northstar_id 
email_feature <- function(email) {
  email_col <- 
    email %>% 
    group_by(northstar_id, event_type) %>%
    tally %>% 
    spread(key = event_type, value = n, fill = 0) %>%
    rowwise() %>%
    mutate(email_total = sum(email_clicked, email_converted, email_opened)) # removes email_unsubscribed variable in email total count
  
  return(email_col)
}

# returns columns of SMS action by type & total number of actions per northstar_id 

sms_feature <- function(sms) {
  sms_col <-
    sms %>% 
    group_by(northstar_id, action_type) %>%
    tally %>%
    spread(key = action_type, value = n, fill = 0) %>% 
    rowwise() %>%
    mutate(SMS_total = sum(`SMS link click`, `SMS message`))
  
  return(sms_col)
}

# returns columns of browser size by northstar_id 

browser_size_feature <- function(phoenix) {
  browser_col <-
    phoenix %>% 
    select(northstar_id, browser_size) %>%
    group_by(northstar_id, browser_size) %>% 
    tally %>%
    spread(key = browser_size, value = n, fill = 0)
  
  return(browser_col)
}

# returns total # of actions per northstar_id 

total_actions_feature <- function(phoenix, campaign, email, sms) {
  web <- 
    phoenix %>% 
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
  total_action <- 
    bind_rows(web, signup, post, email_action, sms_action) %>%
    group_by(northstar_id) %>% 
    summarise(total_action = n())
  
  return(total_action)
}


######################### Pre-processing ########################

# removing near zero-variance variables 

nzv <- nearZeroVar(df, saveMetrics = T)
nzv # if nzv holds true, consider eliminating 

nzv2 <- nearZeroVar(df)

filtered_df <- df[, -nzv2]


remove_nzv <- function(x) {
  nzv <- nearZeroVar(x)
  filtered_df <- x[, -nzv]
  
  return(filtered_df)
}























