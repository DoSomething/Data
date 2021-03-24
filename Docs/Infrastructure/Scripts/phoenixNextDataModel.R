source('config/init.R')
library(jsonlite)
library(xlsx)

pna <- fromJSON('Data/phoenix_next_sample_a.json')
pnb <- fromJSON('Data/phoenix_next_sample_b.json')
pnc <- fromJSON('Data/phoenix_next_sample_c.json')

session <-
  data_frame(
    id = c(pna$page$sessionId, pnc$page$sessionId, pnc$page$sessionId),
    timestamp = c(pna$page$landingTimestamp, pnc$page$landingTimestamp, pnc$page$landingTimestamp),
    northstar_id = c(NA, NA, pnc$user$northstarId),
    device_id = c(pna$user$deviceId, pnb$user$deviceId, pnc$user$deviceId),
    referrer_path = c(NA, pnb$page$referrer$path, pnc$page$referrer$path),
    referrer_host = c(NA, pnb$page$referrer$host, pnc$page$referrer$host),
    referrer_href = c(NA, pnb$page$referrer$href, pnc$page$referrer$href),
    referrer_from_session = c(NA, NA, pnc$page$referrer$query$from_session),
    referrer_source = c(pna$page$query$source, NA, NA),
    referrer_utm_medium = c(pna$page$query$utm_medium, NA, NA),
    referrer_utm_source = c(pna$page$query$utm_source, NA, NA),
    referrer_utm_campaign = c(pna$page$query$utm_campaign, NA, NA),
    browser_size = c(pna$browser, pnb$browser, pnc$browser)
  )

event <-
  data_frame(
    event_id = c(pna$meta$event_id, pnb$meta$event_id, pnc$meta$event_id),
    timestamp = c(pna$meta$timestamp, pnb$meta$timestamp, pnc$meta$timestamp),
    event_name = c(pna$event$name, pnb$event$name, pnc$event$name),
    event_source = c(pna$event$source, pnb$event$source, pnc$event$source),
    path = c(pna$page$path, pnb$page$path, pnc$page$path),
    host = c(pna$page$host, pnb$page$host, pnc$page$host),
    href = c(pna$page$href, pnb$page$href, pnc$page$href),
    session_id = c(pna$page$sessionId, pnc$page$sessionId, pnc$page$sessionId),
    keen_id = c(pna$keen$id, pnb$keen$id, pnc$keen$id)
  )

keen <-
  data_frame(
    id = c(pna$keen$id, pnb$keen$id, pnc$keen$id),
    timestamp = c(pna$keen$timestamp, pnb$keen$timestamp, pnc$keen$timestamp),
    created_at = c(pna$keen$created_at, pnb$keen$created_at, pnc$keen$created_at)
  )

write.xlsx(data.frame(event), file = 'Data/phoenixNextModel.xlsx', sheetName = 'Event', row.names = F)
write.xlsx(data.frame(session), file = 'Data/phoenixNextModel.xlsx', sheetName = 'Session', row.names = F, append = T)
write.xlsx(data.frame(keen), file = 'Data/phoenixNextModel.xlsx', sheetName = 'Keen', row.names = F, append = T)
