source('config/init.R')
library(jsonlite)
library(xlsx)

event <- fromJSON('Data/cio_sample_email.json')
customer <- fromJSON('Data/cio_sample_customer.json')

eventdf <- tbl_df(t(unlist(event, recursive=T)))
customerdf <- tbl_df(t(unlist(customer, recursive=T)))

customerdf %<>%
  rename(customer_id = data.customer_id)

eventMod <-
  eventdf %>%
  select(
    event_id, event_type, timestamp,
    data.variables.event_id, data.variables.event_name,
    data.variables.event.page,
    data.customer_id, data.variables.customer.created_at,
    data.variables.customer.name, data.variables.customer.plan_name,
    data.email_address, data.email_id,
    data.message_id, data.message_name, data.subject,
    data.campaign_id, data.campaign_name, data.template_id
  )

tableBlink <-
  eventMod %>%
  select(event_id, event_type, timestamp) %>%
  setNames(c('blink_id', 'event_type', 'timestamp'))

tableCustomer <-
  eventMod %>%
  select(
    data.customer_id, data.variables.customer.created_at, data.email_address,
    data.variables.customer.name, data.variables.customer.plan_name) %>%
  setNames(
    c('customer_id', 'created_at', 'email_address', 'name', 'plan_name')
  )

tableEvent <-
  eventMod %>%
  select(
    event_id,
    data.variables.event_id, data.variables.event_name, data.variables.event.page,
    data.customer_id, data.email_id, data.message_id, data.message_name,
    data.subject, data.campaign_id, data.campaign_name, data.template_id
  ) %>%
  setNames(
    c('blink_id', 'event_id', 'event_name', 'event_page', 'customer_id',
      'email_id', 'message_id', 'message_name', 'subject', 'campaign_id',
      'campaign_name', 'template_id')
  )

customerEvent <-
  customerdf %>%
  rename(data.customer_id = customer_id)

write.xlsx(tableBlink, file = 'Data/cioModel.xlsx', sheetName = 'Blink', row.names = F)
write.xlsx(tableCustomer, file = 'Data/cioModel.xlsx', sheetName = 'Customer', row.names = F, append = T)
write.xlsx(customerEvent, file = 'Data/cioModel.xlsx', sheetName = 'CustomerEvent', row.names = F, append = T)
write.xlsx(tableEvent, file = 'Data/cioModel.xlsx', sheetName = 'emailEvent', row.names = F, append = T)

