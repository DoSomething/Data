#https://trello.com/c/aZZsQXiX/1127-sms-subscriber-growth-breakdown
leads <-
  read_csv('Data/DoSomething.org_-_July_2017_Leads.csv') %>%
  select(email) %>%
  filter(!duplicated(email))

# leads <-
#   leads %>% 
#   filter(nchar(gsub("[^']", "", email))==0)

ems <- prepQueryObjects(leads$email)

query <- 
    "SELECT
      u.northstar_id,
      u.email
    FROM quasar.users u 
    WHERE u.email IS NOT NULL"

found <- runQuery(query)

found %<>% filter(!duplicated(email))

notFound <- 
  leads %>% 
  left_join(found) %>% 
  filter(is.na(northstar_id)) %>% 
  select(email) 

saveCSV(notFound)
