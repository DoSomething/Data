library(tidyverse)
library(lettercase)
library(scales)
library(openxlsx)
library(data.table)

#Lowercase cities so all text is consistent/easier to search
footlocker<- read.xlsx('~/Documents/Footlocker/1-10-18-FL-external-yes-apps.xlsx')
  # %>%mutate(
  #   city=str_lowercase(city),
  #   participation=str_lower_case(participation))
footlocker_lowcase<-mutate_all(footlocker, funs(tolower))
#Rename zip to zipcode so it matches Sohaib's zipcode code
colnames(footlocker_lowcase)[10] <-"zipcode"

#Pull Sohaib's zipcode/socioeconomic status code
irs <-
  read_csv('~/Documents/Footlocker/IRS 2015 data.csv', col_types = list( "zipcode" = col_character())) %>%
  setNames(c('state','zipcode','income', 'returns','n_people_hh','n_people_soc_tier','garbage1','notes','garbage2')) %>%
  select(-garbage1, -garbage2, -notes) %>%
  mutate(
    zipcode = as.character(
      ifelse(nchar(zipcode)==4, paste0('0',zipcode),
             ifelse(nchar(zipcode)==3, paste0('00',zipcode), zipcode)
      )
    )
  )

weightedIncome <-
  irs %>%
  group_by(zipcode) %>%
  summarise(
    weightedAverageIncome = round(weighted.mean(income, w=returns)),
    state = max(state)
  ) %>%
  mutate(
    socioeconomic_status =
      case_when(weightedAverageIncome == 1 ~ 'Poor',
                weightedAverageIncome == 2 ~ 'Low Income',
                weightedAverageIncome == 3 ~ 'Middle Income',
                weightedAverageIncome == 4 ~ 'Middle High',
                weightedAverageIncome == 5 ~ 'High Income',
                weightedAverageIncome == 6 ~ 'Super Income')
  )

zip<-weightedIncome%>%
  dplyr::select(zipcode, socioeconomic_status)

# #rename zipcode 'zip' to join with footlocker
# names(zip)[names(zip) == 'zipcode']<- 'zip'
#Merge zipcodes and socio-economic categories with footlocker csv
footlocker_zip <-
  footlocker_lowcase %>%
  left_join(zip, by = 'zipcode')

#Quantifying markets
cities<- read.csv('~/Documents/Footlocker/Footlocker market cities.csv')

#check class
# class(cities$zipcode)
footlocker_cities<-cities%>%
mutate(
  zipcode=as.character.numeric_version(zipcode),
  market=as.character.numeric_version(market))

#Merge market cities
#Merge zipcodes and socio-economic categories with footlocker csv
footlocker_markets <-
  footlocker_zip %>%
  left_join(footlocker_cities, by = 'zipcode')

#recode missing and transform test scores and GPA as numeric
footlocker_markets<-footlocker_markets%>%
mutate(
  market=
    case_when(market==1 ~ 1,
    is.na(market) ~ 0),
  test_score=as.numeric(test_score),
  gpa=as.numeric(gpa)
)

#Quantifying values for other scholarship criteria (race, GPA, test scores, income, sports)
footlocker_quantify<-footlocker_markets%>%
    # dplyr::select(city, state, participation,race,socioeconomic_status,gpa, test_score, test_type)%>%
    mutate(
      race_nonwhite=ifelse((race %like% 'blac')|
                  (race %like% 'afri')|
                  (race %like% 'his')|
                  (race %like% 'asia')|
                  (race %like% 'lat')|
                  (race %like% 'ind')|
                  (race %like% 'nat')|
                  (race %like% 'alas')|
                  (race %like% 'span'),1,0),
      gpa_rec =
        case_when(gpa<=3.5 ~ '1',
                  gpa>3.5 & gpa<=3.75 ~ '2',
                  gpa>3.75 & gpa<4 ~ '3',
                  gpa>=4 ~ '4'),
      test =
        case_when(test_score<20 & test_type == 'act' ~ '1',
                  test_score>=20 & test_score<22 & test_type == 'act' ~ '2',
                  test_score>=22 & test_score<31 & test_type == 'act' ~ '3',
                  test_score>=31 & test_score<37 & test_type == 'act' ~ '4',
                  test_score == 'null' ~ '0',
                  test_type == 'null' ~ '0',
                  test_type == 'prefer not to submit scores' ~ '0',
                  test_score<930 & test_type == 'sat (out of 1600)' ~ '1',
                  test_score>=930 & test_score <=1049 & test_type == 'sat (out of 1600)' ~ '2',
                  test_score>=1050 & test_score <=1299 & test_type == 'sat (out of 1600)' ~ '3',
                  test_score>=1300 & test_score<=1600 & test_type == 'sat (out of 1600)' ~ '4',
                  test_score<1390 & test_type == 'sat (out of 2400)' ~ '1',
                  test_score>=1390 & test_score <=1569 & test_type == 'sat (out of 2400)' ~ '2',
                  test_score>=1570 & test_score <=1949 & test_type == 'sat (out of 2400)' ~ '3',
                  test_score>=1950 & test_score<=2400 & test_type == 'sat (out of 2400)' ~ '4',
                  test_score<950 & test_type == 'psat' ~ '1',
                  test_score>=950 & test_score <=1059 & test_type == 'psat' ~ '2',
                  test_score>=1060 & test_score <=1269 & test_type == 'psat' ~ '3',
                  test_score>=1269 & test_score<=1520 & test_type == 'psat' ~ '4'),
      income=
        case_when(socioeconomic_status=='Low Income' | socioeconomic_status=='Poor'~1,
                  socioeconomic_status=='Middle Income' | socioeconomic_status=='Middle High' |
                  socioeconomic_status=='High Income' | socioeconomic_status=='Super Income'|
                  is.na(socioeconomic_status) ~ 0),
      basketball=ifelse(participation %like% 'bask' | accomplishments %like% 'bask' | activities %like% 'bask', 1,0),
      volleyball=ifelse(participation %like% 'volley' | accomplishments %like% 'volley' | activities %like% 'volley',1,0),
      track=ifelse(participation %like% 'track' | accomplishments %like% 'track' | activities %like% 'track'
      | participation %like% 'field' | accomplishments %like% 'field' | activities %like% 'field',1,0),
      cross_country=ifelse(participation %like% 'cross'| accomplishments %like% 'cross' | activities %like% 'cross', 1,0),
      crew=ifelse(participation %like% 'crew'| accomplishments %like% 'crew' | activities %like% 'crew', 1,0),
      soccer=ifelse(participation %like% 'socc' | accomplishments %like% 'socc' | activities %like% 'socc', 1,0),
      tennis=ifelse(participation %like% 'tenn'| accomplishments %like% 'tenn' | activities %like% 'tenn', 1,0),
      football=ifelse(participation %like% 'foot'| accomplishments %like% 'foot' | activities %like% 'foot', 1,0),
      lacrosse=ifelse(participation %like% 'lacros'| accomplishments %like% 'lacros' | activities %like% 'lacros', 1,0),
      softball=ifelse(participation %like% 'soft'| accomplishments %like% 'soft' | activities %like% 'soft', 1,0),
      cheer=ifelse(participation %like% 'cheer'| accomplishments %like% 'cheer' | activities %like% 'cheer', 1,0),
      martial_arts=ifelse(participation %like% 'marti'| accomplishments %like% 'marti' | activities %like% 'marti', 1,0),
      hockey=ifelse(participation %like% 'hock'| accomplishments %like% 'hock' | activities %like% 'hock', 1,0),
      field_hockey=ifelse(participation %like% 'field hoc'| accomplishments %like% 'field hoc' | activities %like% 'field hoc', 1,0),
      swim=ifelse(participation %like% 'swim'| accomplishments %like% 'swim' | activities %like% 'swim', 1,0),
      golf=ifelse(participation %like% 'golf'| accomplishments %like% 'golf' | activities %like% 'golf', 1,0),
      baseball=ifelse(participation %like% 'base'| accomplishments %like% 'base' | activities %like% 'base', 1,0),
      dance=ifelse(participation %like% 'dance'| accomplishments %like% 'dance' | activities %like% 'dance', 1,0),
      badminton=ifelse(participation %like% 'badmin'| accomplishments %like% 'badmin' | activities %like% 'badmin', 1,0),
      boxing=ifelse(participation %like% 'box'| accomplishments %like% 'box' | activities %like% 'box', 1,0),
      rowing=ifelse(participation %like% 'row'| accomplishments %like% 'row' | activities %like% 'row', 1,0),
      karate=ifelse(participation %like% 'karate'| accomplishments %like% 'karate' | activities %like% 'karate', 1,0),
      rifle=ifelse(participation %like% 'rifl'| accomplishments %like% 'rifl' | activities %like% 'rifl', 1,0),
      taekwondo=ifelse(participation %like% 'tae'| accomplishments %like% 'tae' | activities %like% 'tae', 1,0),
      wrestling=ifelse(participation %like% 'wrest'| accomplishments %like% 'wrest' | activities %like% 'wrest', 1,0),
      rugby=ifelse(participation %like% 'rugb'| accomplishments %like% 'rugb' | activities %like% 'rugb', 1,0),
      wheelchair=ifelse(participation %like% 'wheelchair'| accomplishments %like% 'wheelchair' | activities %like% 'wheel', 1,0),
      total_sports=(basketball+volleyball+track+cross_country+crew+soccer+tennis+football+lacrosse+
                    softball+cheer+martial_arts+hockey+field_hockey+swim+golf+baseball+dance+badminton+
                    boxing+rowing+karate+rifle+taekwondo+wrestling+rugby+wheelchair))

#Quantify a sports score for those with only one sport (will need to read individually through those with 'NA'
#because those are students who reported multiple sports or sports that are not on the list)
footlocker_sports<-footlocker_quantify%>%
  mutate(
          basketball_score=ifelse(basketball==1,5,0),
          volleyball_score=ifelse(volleyball==1,3,0),
          track_score=ifelse(track==1,4,0),
          cross_country_score=ifelse(cross_country==1,4,0),
          crew_score=ifelse(crew==1,3,0),
          soccer_score=ifelse(soccer==1,3,0),
          tennis_score=ifelse(tennis==1,3,0),
          football_score=ifelse(football==1,3,0),
          lacrosse_score=ifelse(lacrosse==1,3,0),
          softball_score=ifelse(softball==1,3,0),
          cheer_score=ifelse(cheer==1,2,0),
          martial_arts_score=ifelse(martial_arts==1,1,0),
          hockey_score=ifelse(hockey==1,2,0),
          field_hockey_score=ifelse(field_hockey==1,2,0),
          swim_score=ifelse(swim==1,2,0),
          golf_score=ifelse(golf==1,1,0),
          baseball_score=ifelse(baseball==1,3,0),
          dance_score=ifelse(dance==1,1,0),
          badminton_score=ifelse(badminton==1,1,0),
          boxing_score=ifelse(boxing==1,1,0),
          rowing_score=ifelse(rowing==1,1,0),
          karate_score=ifelse(karate==1,1,0),
          rifle_score=ifelse(rifle==1,1,0),
          taekwondo_score=ifelse(taekwondo==1,1,0),
          wrestling_score=ifelse(wrestling==1,2,0),
          rugby_score=ifelse(rugby==1,2,0),
          wheelchair_score=ifelse(wheelchair==1,1,0),
          multiplesports=ifelse(total_sports>1,1,0),
          basketball_run=ifelse(basketball==1 | track==1 | cross_country==1,"yes","no"),
          total_sports_score = basketball_score + volleyball_score + track_score + cross_country_score +
          crew_score + soccer_score + tennis_score + football_score + lacrosse_score + softball_score +
          cheer_score + martial_arts_score + hockey_score + field_hockey_score + swim_score + golf_score +
          baseball_score + dance_score + badminton_score + boxing_score + rowing_score + karate_score + rifle_score
          + taekwondo_score + wrestling_score + rugby_score + wheelchair_score)

#Transform new quantifying variables to numeric
footlocker_sports$total_sports_score<-as.numeric(as.character(footlocker_sports$sports_score))
footlocker_sports$market<-as.numeric(as.character(footlocker_sports$market))
footlocker_sports$race_nonwhite<-as.numeric(as.character(footlocker_sports$race_nonwhite))
footlocker_sports$income<-as.numeric(as.character(footlocker_sports$income))
footlocker_sports$test<-as.numeric(as.character(footlocker_sports$test))
footlocker_sports$gpa_rec<-as.numeric(as.character(footlocker_sports$gpa_rec))

#Calculate Total Scores
Footlocker_totalscore <-footlocker_sports%>%
  mutate(
      total_score = market + total_sports_score + gpa_rec + test + race_nonwhite + income)

#Get column names to select
colnames(Footlocker_totalscore)
#
# Footlocker_totalscore_final <-Footlocker_totalscore%>%
#   dplyr::select(1:7,9, 11:19,54:71,8,49,20,21,52,23,50,22,51,10,48,53,72:74)

Footlocker_totalscore_final <-Footlocker_totalscore%>%
  dplyr::select(1:7,9,11:19,24:55,91:117,20,21,61,23,59,22,60,8,57:58,10,62,56,90,118:121)

# Footlocker_totalscore_final<-str_ucfirst('first_name')

#Export to csv
write.csv(Footlocker_totalscore_final, file = "Footlocker Total Scores 2.csv")

#Transform character gpa values to numeric with decimals
# footlocker_zip$gpa <-as.numeric(footlocker_zip$gpa)

# market=ifelse((city %like% 'york' & state == 'ny')|
#             (city %like% 'brookl' & state == 'ny')|
#             (city %like% 'staten' & state == 'ny')|
#             (city %like% 'quee' & state == 'ny')|
#             (city %like% 'manhat' & state == 'ny')|
#             (city %like% 'miam' & state == 'fl')|
#             (city %like% 'angeles' & state == 'ca')|
#             (city %like% 'chica' & state == 'il')|
#             (city %like% 'philad' & state == 'pa')|
#             (city %like% 'houst' & state == 'tx')|
#             (city %like% 'dalla' & state == 'tx')|
#             (city %like% 'worth' & state == 'tx')|
#             (city %like% 'atlan' & state == 'ga')|
#             (city %like% 'detro' & state == 'mi')|
#             (city %like% 'washing' & state == 'dc')|
#             (city %like% 'hagers' & state == 'md')|
#             (city %like% 'francis' & state == 'ca')|
#             (city %like% 'oaklan' & state == 'ca')|
#             (city %like% 'orlan' & state == 'fl')|
#             (city %like% 'dayton' & state == 'fl')|
#             (city %like% 'melbour' & state == 'fl')|
#             (city %like% 'tampa' & state == 'fl')|
#             (city %like% 'petersbur' & state == 'fl')|
#             (city %like% 'sarasot' & state == 'fl')|
#             (city %like% 'bost' & state == 'ma')|
#             (city %like% 'manchest' & state == 'ma')|
#             (city %like% 'minneap' & state == 'mn')|
#             (city %like% 'paul' & state == 'mn')|
#             (city %like% 'sacrem' & state == 'ca')|
#             (city %like% 'stockto' & state == 'ca')|
#             (city %like% 'modest' & state == 'ca')|
#             (city %like% 'antonio' & state == 'tx')|
#             (city %like% 'balti' & state == 'md'),1,0),

# footlocker_sports<-footlocker_quantify%>%
#   mutate(
#       sports_score =
#         case_when(basketball==1 & total_sports==1 ~ 5,
#                   volleyball== 1 & total_sports==1 ~ 3,
#                   track== 1 & total_sports==1 ~ 4,
#                   cross_country== 1 & total_sports==1 ~ 4,
#                   crew==1 & total_sports==1 ~ 3,
#                   soccer==1 & total_sports==1 ~ 3,
#                   tennis==1 & total_sports==1 ~ 3,
#                   football==1 & total_sports==1 ~ 3,
#                   lacrosse==1 & total_sports==1 ~ 3,
#                   softball==1 & total_sports==1 ~ 3,
#                   cheer==1 & total_sports==1 ~ 2,
#                   martial_arts==1 & total_sports==1 ~ 1,
#                   hockey==1 & total_sports==1 ~ 2,
#                   field_hockey==1 & total_sports==1 ~ 2,
#                   swim==1 & total_sports==1 ~ 2,
#                   golf==1 & total_sports==1 ~ 1,
#                   baseball==1 & total_sports==1 ~ 3,
#                   dance==1 & total_sports==1 ~ 1)


#Identify those with multiple sports and read through to determine priority sport
# footlocker_sports<-footlocker_zip%>%
#   dplyr::select(participation,essay1, essay2)%>%
#   mutate(
#     basketball=ifelse(participation %like% 'bask',1,0),
#     volleyball=ifelse(participation %like% 'volley',1,0),
#     track=ifelse((participation %like% 'track') | (participation %like% 'field'),1,0),
#     cross_country=ifelse(participation %like% 'cross',1,0),
#     crew=ifelse(participation %like% 'crew',1,0),
#     soccer=ifelse(participation %like% 'socc',1,0),
#     tennis=ifelse(participation %like% 'tenn',1,0),
#     football=ifelse(participation %like% 'foot',1,0),
#     lacrosse=ifelse(participation %like% 'lacros',1,0),
#     softball=ifelse(participation %like% 'soft',1,0),
#     cheer=ifelse(participation %like% 'cheer',1,0),
#     martial_arts=ifelse(participation %like% 'marti',1,0),
#     hockey=ifelse(participation %like% 'hock',1,0),
#     field_hockey=ifelse(participation %like% 'field hoc',1,0),
#     swim=ifelse(participation %like% 'swim',1,0),
#     golf=ifelse(participation %like% 'golf',1,0),
#     baseball=ifelse(participation %like% 'base',1,0),
#     dance=ifelse(participation %like% 'dance',1,0),
#     total_sports=(basketball+volleyball+track+cross_country+crew+soccer+tennis+football+lacrosse+
#                     softball+cheer+martial_arts+hockey+field_hockey+swim+golf+baseball+dance))


#
# sports<-footlocker_quantify%>%
#   dplyr::select(basketball:total_sports)%>%
#   sports =ifelse(sports$basketball==1 & sports$total_sports==1,5,ifelse(sports$volleyball==1 & sports$total_sports==1,3,NA))
#
#
# sum_sports<-rowSums(sports)
#
# #Convert to character
# class(footlocker$city)
# attach(footlocker)
# as.character(footlocker$city)
# tolower(city)
#
# loc<-as.character(footlocker$city)
# grep("N",city)
