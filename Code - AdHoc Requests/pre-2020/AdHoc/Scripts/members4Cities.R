# https://trello.com/c/RfyCC2aD/1164-data-request-of-members-in-4-cities
source('config/init.R')
source('config/mySQLConfig.R')

zips <- 
  read_csv('Data/specificCityZips.csv') %>% 
  mutate_if(is.integer,as.character)

meltZips <-
  zips %>% 
  melt() %>% 
  filter(!is.na(value))

zipList <- prepQueryObjects(meltZips$value)

q <- 
  paste0(
    "SELECT *
    FROM
      (SELECT 
  	  u.northstar_id,
      u.birthdate,
      COALESCE(NULLIF(u.addr_zip, ''), NULLIF(m.addr_zip, ''), NULLIF(m.loc_zip,'')) AS addr_zip
      FROM quasar.moco_profile_import m
      LEFT JOIN quasar.users u 
      ON m.moco_id = u.moco_commons_profile_id
      WHERE u.moco_commons_profile_id IS NOT NULL) a
    WHERE a.addr_zip in", zipList
  )

qres <- runQuery(q)

dat <-
  qres %>% 
  mutate(
    age = floor((Sys.Date()-as.Date(qres$birthdate))/365.25),
    Over17 = case_when(as.Date(qres$birthdate)<='1970-01-01' ~ 'Unknown',
                       age >= 17 ~ 'Over 17',
                       age < 17 ~ 'Under 17',
                       is.difftime(age) ~ 'Unknown'),
    City = case_when(addr_zip %in% zips$Akron ~ 'Akron',
                     addr_zip %in% zips$Charlotte ~ 'Charlotte',
                     addr_zip %in% zips$GrandForks ~ 'Grand Forks',
                     addr_zip %in% zips$WestPalmBeach ~ 'West Palm Beach',
                     is.character(addr_zip) ~ 'None')
  ) %>% 
  group_by(City, Over17) %>% 
  summarise(Count = n())

saveCSV(dat, desktop=T)
