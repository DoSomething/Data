source('config/init.R')
source('config/pgConnect.R')
library(glue)
library(janitor)
library(lubridate)
library(viridis)
pg <- pgConnect()

processReferralColumn <- function(dat) {

  maxSep <- max(as.numeric(names(table(str_count(dat$tracking_source, ',')))))+1

  parsedSep <-
    dat %>%
    select(post_id, tracking_source) %>%
    mutate(
      tracking_source = gsub('sourcedetails','source_details',tracking_source),
      tracking_source = gsub('iframe\\?r=','',tracking_source),
      tracking_source = gsub('iframe','',tracking_source)
    ) %>%
    separate(tracking_source, LETTERS[1:maxSep], ',',remove = F) %>%
    mutate(
      source =
        case_when(
          grepl('ads',tolower(A)) ~ 'ads',
          grepl('email',tolower(A)) ~ 'email',
          grepl('source:',tolower(C)) ~ gsub(".*:",'',C),
          grepl('source=',tolower(C)) ~ gsub(".*=",'',C),
          grepl('source:',tolower(D)) ~ gsub(".*:",'',D),
          grepl('source=',tolower(D)) ~ gsub(".*=",'',D),
          TRUE ~ 'no_attribution'
        ),
      source_details =
        case_when(
          grepl('_details:',tolower(D)) ~ gsub(".*:",'',D),
          grepl('_details=',tolower(D)) ~ gsub(".*=",'',D),
          grepl('_details:',tolower(E)) ~ gsub(".*:",'',E),
          grepl('_details=',tolower(E)) ~ gsub(".*=",'',E),
          TRUE ~ ''
        ),
      source = case_when(
        source == 'sms_share' ~ 'sms',
        grepl('referral=true', tracking_source) ~ 'web',
        grepl('niche', source_details) ~ 'partner',
        grepl('source',source) ~ 'no_attribution',
        source=='ema0il' ~ 'email',
        TRUE ~ source
      ),
      source_details = case_when(
        grepl('referral=true', source_details) ~ 'referral',
        TRUE ~ source_details
      )
    ) %>%
    select(-A,-B,-C,-D,-E,-`F`)

  return(parsedSep)

}

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

raw <-
  read_csv('registrants-report_through_20200420.csv') %>%
  clean_names() %>%
  select(status, tracking_source, date_of_birth, email=email_address, home_state,
         party, race, opt_in_to_partner_email, started_registration) %>%
  mutate(
    started_registration = as.POSIXct(started_registration)
  )

q <- "
  SELECT
    u.northstar_id AS nsid,
    u.cio_status,
    u.cio_status_timestamp,
    u.sms_status,
    u.created_at,
    u.source AS user_source,
    CASE
      WHEN u.sms_status IN ('active', 'less', 'pending')
         AND (u.cio_status IS DISTINCT FROM 'customer_subscribed')
         THEN 'sms_only'
      WHEN u.cio_status = 'customer_subscribed'
         AND (u.sms_status NOT IN ('active', 'less', 'pending') OR u.sms_status IS NULL)
         THEN 'email_only'
      ELSE 'both' END AS subscribe_type,
    rtv.post_id,
    rtv.tracking_source,
    rtv.finish_with_state,
    COALESCE(rtv.email, u.email) as email,
    rtv.zip,
    rtv.started_registration,
    case when p.status ilike '%register%' then 'Complete' else rtv.status end as status
  FROM public.rock_the_vote rtv
  LEFT JOIN public.posts p ON rtv.post_id=p.id
  LEFT JOIN public.users u ON p.northstar_id=u.northstar_id
"
rtv <- runQuery(q)

rfparsed <- processReferralColumn(rtv)

nomatch <-
  raw %>%
  anti_join(rtv, by=c('email','started_registration','status')) %>%
  distinct()

tj <-
  rtv %>%
  inner_join(raw, by=c('email','started_registration','status')) %>%
  distinct() %>%
  filter(!is.na(cio_status)) %>%
  mutate(
    created_via_vr =
      if_else(user_source=='importer-client',T,F,missing = F),
    age =
      as.Date(date_of_birth, '%m/%d/%Y') %>%
      age(., as.Date(started_registration), 'years'),
    registered =
      if_else(status=='Complete', T, F),
    receiving_emails =
      if_else(cio_status=='customer_subscribed',T,F,F),
    unsubscribed =
      if_else((opt_in_to_partner_email=='Yes' & receiving_emails==F) |
                (started_registration<='2019-04-10' & receiving_emails==F), T, F),
    registered_my =
      round_date(started_registration, unit='month'),
    time_to_unsub =
      as.numeric(difftime(cio_status_timestamp, started_registration, units = 'days')),
    days_since_created =
      as.numeric(difftime(max(started_registration), created_at, units='days')),
    population =
      case_when(
        registered==T & created_via_vr==T & unsubscribed==T ~ 'new-registered-unsubscribed',
        registered==T & created_via_vr==F & unsubscribed==T ~ 'old-registered-unsubscribed',
        registered==T & created_via_vr==T & unsubscribed==F ~ 'new-registered-subscribed',
        registered==T & created_via_vr==F & unsubscribed==F ~ 'old-registered-subscribed',
        registered==F ~ 'unregistered'
      ),
    age_bucks =
      case_when(
        age < 30 ~ '18-30',
        age < 45 ~ '31-45',
        age < 65 ~ '45-65',
        age >=65 ~ '65+'
      ),
    relationship_length =
      if_else(unsubscribed==T, time_to_unsub, days_since_created)
  ) %>%
  left_join(rfparsed) %>%
  # some people unsubbed emails but still registered on site via ads/sms; toss for now
  filter(relationship_length>0 & (age>=17 | is.na(age)))

# How many people that DoSomething registered did not opt-in to messaging through the Rock The Vote flow?

optin.ova <-
  tj %>%
  filter(registered==T & started_registration>'2019-04-10') %>%
  group_by(registered_my) %>%
  summarise(
    registrations = n(),
    pct_opt_in = sum(opt_in_to_partner_email=='Yes') / n()
  )

ggplot(optin.ova, aes(x=as.Date(registered_my), y=pct_opt_in)) +
  geom_line() + geom_point(aes(size=registrations), show.legend=F) +
  labs(x='Time', y='% Opt In') +
  ylim(0,1) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_linedraw() + theme(axis.text.x = element_text(angle=35, hjust=1))

optin.age <-
  tj %>%
  filter(registered==T & started_registration>'2019-04-10') %>%
  group_by(registered_my, age_bucks) %>%
  summarise(
    registrations = n(),
    pct_opt_in = sum(opt_in_to_partner_email=='Yes') / n()
  )

ggplot(optin.age, aes(x=as.Date(registered_my), y=pct_opt_in, color=age_bucks)) +
  geom_line() + geom_point(aes(size=registrations), alpha=.8, show.legend=F) +
  labs(x='', y='% Opt In', color='Age Groups') + ylim(0,1) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_linedraw() + theme(axis.text.x = element_text(angle=35, hjust=1))

optin.source <-
  tj %>%
  filter(registered==T & started_registration>'2019-04-10' &
         source %in% c('ads','web','sms','email','no_attribution','partner','influencer')) %>%
  group_by(registered_my, source) %>%
  summarise(
    registrations=n(),
    pct_opt_in = sum(opt_in_to_partner_email=='Yes') / n()
  ) %>%
  filter(registrations>20)

ggplot(optin.source, aes(x=as.Date(registered_my), y=pct_opt_in, color=source)) +
  geom_line() + geom_point(aes(size=registrations), alpha=.8, show.legend=F) +
  labs(x='', y='% Opt In', color='Age Groups') + ylim(0,1) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_linedraw() + theme(axis.text.x = element_text(angle=35, hjust=1))

# TODO: Source Detail
# TODO: .csv dumps

# How many people that DoSomething registered have actively unsubscribed from our messaging?

unsub.ova <-
  tj %>%
  filter(registered==T) %>%
  group_by(registered_my) %>%
  summarise(
    registrations = n(),
    pct_unsub = sum(unsubscribed==T) / n()
  )

ggplot(unsub.ova, aes(x=as.Date(registered_my), y=pct_unsub)) +
  geom_line() + geom_point(aes(size=registrations), show.legend=F) +
  labs(x='Time', y='% Unsubscribed') +
  ylim(0,1) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_linedraw() + theme(axis.text.x = element_text(angle=35, hjust=1))

unsub.age <-
  tj %>%
  filter(registered==T & !is.na(date_of_birth)) %>%
  group_by(registered_my, age_bucks) %>%
  summarise(
    registrations = n(),
    pct_unsub = sum(unsubscribed==T) / n()
  )

ggplot(unsub.age, aes(x=as.Date(registered_my), y=pct_unsub, color=age_bucks)) +
  geom_line() + geom_point(aes(size=registrations), alpha=.8, show.legend=F) +
  labs(x='', y='% Unsubscribe', color='Age Groups') + ylim(0,1) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_linedraw() + theme(axis.text.x = element_text(angle=35, hjust=1))

unsub.source <-
  tj %>%
  filter(registered==T & source %in%
           c('ads','web','sms','email','no_attribution','partner','influencer')) %>%
  group_by(registered_my, source) %>%
  summarise(
    registrations=n(),
    pct_unsub = sum(unsubscribed==T) / n()
  ) %>%
  filter(registrations>20)

ggplot(unsub.source, aes(x=as.Date(registered_my), y=pct_unsub, color=source)) +
  geom_line() + geom_point(aes(size=registrations), alpha=.8, show.legend=F) +
  labs(x='', y='% Unsubscribe', color='Age Groups') + ylim(0,1) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme_linedraw() + theme(axis.text.x = element_text(angle=35, hjust=1))

# TODO: Source Detail
# TODO: .csv dumps