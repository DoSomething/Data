# https://trello.com/c/GS8uKdxp/1154-active-members-goal-for-defend-dreamers
source('config/init.R')
source('config/mySQLConfig.R')

getCurrentActive <- function() {
  
  query <- "SELECT 
              count(*) as count
            FROM quasar.users u
            WHERE (u.customer_io_subscription_status = 'subscribed' 
            OR u.moco_current_status = 'active')"
  out <- runQuery(query)
 
  return(as.numeric(out))
  
}

getActiveCounts <- function() {
  
  out <- tbl_dt(data.frame())
  
  for (i in 1:length(sr$start_date)) {
 
    date <- as.Date(sr[i,start_date])
    
    query <- paste0("SELECT 
                      count(*)
                    FROM quasar.users u
                    WHERE (u.customer_io_subscription_status = 'subscribed' 
	                  OR u.moco_current_status = 'active')
                    AND u.northstar_created_at_timestamp < '", date,"'")
    val <- as.numeric(runQuery(query))
    
    temp <- 
      data.frame(
        campaign_run_id = sr[i,campaign_run_id], 
        activeCount = val
        ) %>% 
      tbl_dt()
    
    out <- out %>% bind_rows(temp)
      
  }
  
  return(out)
}

pctChangeRequired <- function(original, target) {
  x <- (target-original)/original
  return(x)
}

qres <- runQuery('Scripts/defendDreamersForecast.sql')

sr <- 
  qres %>% 
  mutate(
    start_date = as.Date(start_date)
  )

activeCounts <- getActiveCounts()

currentActive <- getCurrentActive()

estimates <-
  sr %>% 
  left_join(
    activeCounts %>% 
      mutate(
        inflate = pctChangeRequired(activeCount, currentActive)
      )
  ) %>% 
  mutate(
    rbToday = applyPctChange(reportbacks, inflate),
    suToday = applyPctChange(signups, inflate)
  ) %>% 
  group_by(TYPE) %>% 
  summarise(
    estSignup = mean(suToday),
    estReportback = mean(rbToday)
  )
    