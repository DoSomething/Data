library(openxlsx)

# Q1 ----------------------------------------------------------------------

# How many people that DoSomething registered did not opt-in to messaging through the Rock The Vote flow?

optin_age <-
  tj %>%
  filter(registered==T & started_registration>'2019-04-10') %>%
  group_by(registered_my, age) %>%
  summarise(
    registrations = n(),
    pct_opt_in = sum(opt_in_to_partner_email=='Yes') / n()
  )
optin_source <-
  tj %>%
  filter(registered==T & started_registration>'2019-04-10') %>%
  group_by(registered_my, source) %>%
  summarise(
    registrations=n(),
    pct_opt_in = sum(opt_in_to_partner_email=='Yes') / n()
  )
optin_source_details <-
  tj %>%
  filter(registered==T & started_registration>'2019-04-10') %>%
  group_by(registered_my, source, source_details) %>%
  summarise(
    registrations=n(),
    pct_opt_in = sum(opt_in_to_partner_email=='Yes') / n()
  )

write.xlsx(list('optin_overall'=optin.ova,
                'optin_age'=optin_age,
                'optin_source'=optin_source,
                'optin_source_details'=optin_source_details),
           file = 'optin.xlsx')

# Q2 ----------------------------------------------------------------------

# How many people that DoSomething registered have actively unsubscribed from our messaging?

unsub_age <-
  tj %>%
  filter(registered==T & !is.na(date_of_birth)) %>%
  group_by(registered_my, age) %>%
  summarise(
    registrations = n(),
    pct_unsub = sum(unsubscribed==T) / n()
  )

unsub_source <-
  tj %>%
  filter(registered==T) %>%
  group_by(registered_my, source) %>%
  summarise(
    registrations=n(),
    pct_unsub = sum(unsubscribed==T) / n()
  )

unsub_source_details <-
  tj %>%
  filter(registered==T) %>%
  group_by(registered_my, source, source_details) %>%
  summarise(
    registrations=n(),
    pct_unsub = sum(unsubscribed==T) / n()
  )
write.xlsx(list('unsub_overall'=unsub.ova,
                'unsub_age'=unsub_age,
                'unsub_source'=unsub_source,
                'unsub_source_details'=unsub_source_details),
           file = 'unsub_rates.xlsx')

# Q3 ----------------------------------------------------------------------

# For existing members/self-reported…
# - What was the average length of their membership before unsubscribing?
# - Were there trends on email only v. sms only v. both?
# Q4 ----------------------------------------------------------------------

# For new members…
# - Did they unsubscribe on the same day that they started receiving DoSomething messaging?
#   - What was the average length of time between their account created and unsubscribing?
#   - Any sources that had higher unsubscribe rates than other sources?


write.xlsx(
  list('existing_communication_type'=ttunsub.subtype,
       'existing_usersource_type'=existing_usersource_type,
       'new_source'=nunsub.o),
  file = 'time_to_unsub.xlsx')
