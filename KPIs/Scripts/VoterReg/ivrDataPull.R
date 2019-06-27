# https://trello.com/c/nGa4Sx5i/1525-csv-of-2018-voter-registrations
nsids=unique(tvrtv$nsid)
q <-
  glue_sql(
    "SELECT
      u.northstar_id AS nsid,
      u.first_name,
      u.last_name,
      u.address_street_1,
      u.address_street_2,
      u.city,
      u.state,
      u.zipcode,
      u.birthdate
    FROM users u
    WHERE u.northstar_id IN ({nsids*})",
    .con=pg,
    nsids=nsids
  )

userFields <- runQuery(q)

out <-
  tvrtv %>%
  filter(grepl('register',ds_vr_status) & created_at < '2019-01-01') %>%
  mutate(
    source =
      case_when(
        source=='ads' ~ 'PAID',
        source %in% c('influencer','partner','company') ~ 'PARTNER',
        TRUE ~ 'PROGRAM'
        ),
    type = 'ONLINE'
  ) %>%
  left_join(userFields) %>%
  select(
    first_name, last_name, address_street_1, address_street_2,
    city, state, zipcode, birthdate, source, type, registration_date=created_at
  )

saveCSV(out)
