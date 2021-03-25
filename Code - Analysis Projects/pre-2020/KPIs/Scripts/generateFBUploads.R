source('config/init.R')
source('config/mySQLConfig.R')
library(PKI)

q <-
  "SELECT
    u.northstar_id,
    u.email,
    u.mobile
  FROM quasar.users u
  WHERE
  (u.customer_io_subscription_status = 'subscribed'
  OR u.sms_status IN ('active','less'))
  AND u.country in ('US','')
  "

qres <- runQuery(q, which='mysql')

qClean <-
  qres %>%
  mutate(mobileClean = paste0('01',cleanPhone(mobile)))

# t <- qres %>% head(.) %$% northstar_id
#
# key <- PKI.genRSAkey(2048)
#
# x <- charToRaw(t[[1]])
#
# e <- PKI.encrypt(x, key)
# y <- PKI.decrypt(e, key)
# print(rawToChar(y))

email <- qClean %>% filter(email != '')
phone <- qClean %>% filter(mobileClean != '01NA')
emailPhone <- qClean %>% filter(email != '' & mobileClean != '01NA')

saveCSV(email %>% select(email))
saveCSV(phone %>% select(mobileClean))
saveCSV(emailPhone, select(email, mobileClean))

## TODO: Append country code for facebook