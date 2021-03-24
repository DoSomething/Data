source('config/init.R')
library(XML)
library(openxlsx)

doc <- xmlParse('Data/moco_message_sample.xml')

rootNode <- xmlRoot(doc)

xmlList <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))

data <- data.frame(t(xmlList),row.names=NULL)

flattened <-
  data.frame(do.call(c, unlist(data, recursive=F))) %>%
  as.tibble()
  # setNames(
  #   c('phone_number','carrier','profile_id','body','message_template_id',
  #     'keyword','opt_in_path_id','received_at','campaign_name','profile_first_name',
  #     'profile_last_name','profile_phone_number','profile_email','profile_created_at',
  #     'profile_updated_at','profile_opted_out_at','profile_opted_out_source',
  #     'profile_source','profile_address','profile_location',
  #     'profile_custom_columns','profile_integrations','profile_clicks')
  #   )

profile <-
  flattened %>%
  select(starts_with('profile')) %>%
  rename(profile_id = profile.profile.text)

colnames(profile) <- gsub('profile.1.profile.','',colnames(profile))

messages <-
  flattened %>%
  select(-starts_with('profile.1')) %>%
  setNames(c('phone_number','carrier_name','profile_id','body','template_id',
             'keyword','opt_in_path_id','receieved_at','camapign_name'))

wb <- createWorkbook()

addWorksheet(wb, 'messages')
addWorksheet(wb, 'profile')
writeData(wb, 'messages', messages, rowNames=F)
writeData(wb, 'profile', profile, rowNames=F)
saveWorkbook(
  wb,
  paste0('Data/moco_messages_modeled.xlsx'),
  overwrite = TRUE
)