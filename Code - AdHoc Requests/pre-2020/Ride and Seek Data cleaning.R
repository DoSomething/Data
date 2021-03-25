library(tidyverse)
library(data.table)
library(gmodels)
library(dbplyr)

#############################
###### CONTROL SURVEYS ######
#############################

#Oct 25 Control
control_10_25<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Control/Ride and Seek - Control Question Pro_10_25_2017_clean.csv', stringsAsFactors=FALSE)
control_10_25$group<- "control"
control_10_25$nsid<- NA
control_10_25$network_id<- NA
control_10_25$start_date<- NA
#get column names
colnames(control_10_25)
#drop irrelevant vars
control_10_25<-control_10_25%>%
  dplyr::select(1:3,5,15:17,23:64)

#Nov 2 Control
control_11_02<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Control/Ride and Seek - Control Question Pro_11_02_2017_clean.csv', stringsAsFactors=FALSE)
control_11_02$group<- "control"
control_11_02$nsid<- NA
control_11_02$network_id<- NA
control_11_02$start_date<- NA
#drop irrelevant vars
control_11_02<-control_11_02%>%
  dplyr::select(1:3,5,15:17,23:64)

#Typeform Dec 7th
control_12_07<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Control/Ride And Seek - Control Typeform_12_7_2017.csv',stringsAsFactors=FALSE)
control_12_07$group<- "control"
control_12_07$ip_address<- NA
control_12_07$time_to_complete<- NA
control_12_07$country<- NA
control_12_07$state<- NA
control_12_07$response_status<- "NA"
control_12_07$device_survey<- "NA"
control_12_07$operating_system<- "NA"
control_12_07$language<- "NA"
#get column names
colnames(control_12_07)

#Typeform Dec 20th
control_12_20<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Control/Ride And Seek - Control Typeform_12_20_2017.csv', stringsAsFactors=FALSE)
control_12_20$group<- "control"
control_12_20$ip_address<- NA
control_12_20$time_to_complete<- NA
control_12_20$country<- NA
control_12_20$state<- NA
control_12_20$response_status<- NA
control_12_20$device_survey<- NA
control_12_20$operating_system<- NA
control_12_20$language<- NA



#############################
###### EXPERIMENT SURVEYS ######
#############################

#Oct 19th
experiment_10_19<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Experiment/Ride and Seek - Experiment QuestionPro_10_19_2017_clean.csv',stringsAsFactors=FALSE)
experiment_10_19$group<- "experiment"
experiment_10_19$nsid<- NA
experiment_10_19$network_id<- NA
experiment_10_19$start_date<- NA
#get column names
colnames(experiment_10_19)
#drop irrelevant vars
experiment_10_19<-experiment_10_19%>%
  dplyr::select(1:3,5,15:17,23:64)

#Oct 21st
experiment_10_21<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Experiment/Ride and Seek - Experiment QuestionPro_10_21_2017_clean.csv', stringsAsFactors=FALSE)
experiment_10_21$group<- "experiment"
experiment_10_21$nsid<- "NA"
experiment_10_21$network_id<- "NA"
experiment_10_21$start_date<- "NA"
#drop irrelevant vars
experiment_10_21<-experiment_10_21%>%
  dplyr::select(1:3,5,15:17,23:64)

#Oct 25th
experiment_10_25<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Experiment/Ride and Seek - Experiment QuestionPro_10_25_2017_clean.csv',stringsAsFactors=FALSE)
experiment_10_25$group<- "experiment"
experiment_10_25$nsid<- NA
experiment_10_25$network_id<- NA
experiment_10_25$start_date<- NA
#drop irrelevant vars
experiment_10_25<-experiment_10_25%>%
  dplyr::select(1:3,5,15:17,23:64)

#Nov 2nd
experiment_11_02<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Experiment/Ride and Seek - Experiment QuestionPro_11_02_2017_clean.csv',stringsAsFactors=FALSE)
experiment_11_02$group<- "experiment"
experiment_11_02$nsid<- "NA"
experiment_11_02$network_id<- "NA"
experiment_11_02$start_date<- "NA"
#drop irrelevant vars
experiment_11_02<-experiment_11_02%>%
  dplyr::select(1:3,5,15:17,23:64)

#Typeform Dec 7th
experiment_12_07<-read.csv('~/Documents/Ride and Seek/Data/Ride and Seek Experiment/Ride And Seek - Experiment Typeform_12_07_2017.csv',stringsAsFactors=FALSE)
experiment_12_07$group<- "experiment"
experiment_12_07$ip_address<- NA
experiment_12_07$time_to_complete<- NA
experiment_12_07$country<- NA
experiment_12_07$state<- NA
experiment_12_07$response_status<- NA
experiment_12_07$device_survey<- NA
experiment_12_07$operating_system<- NA
experiment_12_07$language<- NA

#rename QP vars
desiredColnames <- c('survey_id',
                     'ip_address',
                     'date_submitted',
                     'time_to_complete',
                     'country',
                     'state',
                     'response_status',
                     'device_survey',
                     'operating_system',
                     'language',
                     'age',
                     'gender',
                     'region',
                     'license',
                     'vehicle',
                     'driving_freq',
                     'seatbelt_driver',
                     'seatbelt_front',
                     'seatbelt_back',
                     'danger_front',
                     'danger_back',
                     'danger_distract',
                     'intervene_driving',
                     'intervene_passenger',
                     'distract_talking',
                     'distract_eating',
                     'distract_phone',
                     'distract_read',
                     'distract_send',
                     'distract_grooming',
                     'distract_radio',
                     'distract_cd',
                     'distract_navigate',
                     'distract_social',
                     'distract_pets',
                     'distract_none',
                     'witnessed_noseatbelt',
                     'noseatbelt_relationship',
                     'noseatbelt_other',
                     'convinced',
                     'convinced_asked',
                     'convinced_stats',
                     'convinced_jokes',
                     'convinced_other',
                     'mobile',
                     'group',
                     'nsid',
                     'network_id',
                     'start_date')

#rename QP vars
desiredColnames_typeform <- c('survey_id',
                     'age',
                     'gender',
                     'region',
                     'license',
                     'vehicle',
                     'driving_freq',
                     'seatbelt_driver',
                     'seatbelt_front',
                     'seatbelt_back',
                     'danger_front',
                     'danger_back',
                     'danger_distract',
                     'intervene_driving',
                     'intervene_passenger',
                     'distract_talking',
                     'distract_eating',
                     'distract_phone',
                     'distract_read',
                     'distract_send',
                     'distract_grooming',
                     'distract_radio',
                     'distract_cd',
                     'distract_navigate',
                     'distract_social',
                     'distract_pets',
                     'distract_none',
                     'witnessed_noseatbelt',
                     'noseatbelt_relationship',
                     'noseatbelt_other',
                     'convinced',
                     'convinced_asked',
                     'convinced_stats',
                     'convinced_jokes',
                     'convinced_other',
                     'mobile',
                     'nsid',
                     'start_date',
                     'date_submitted',
                     'network_id',
                     'group',
                     'ip_address',
                     'time_to_complete',
                     'country',
                     'state',
                     'response_status',
                     'device_survey',
                     'operating_system',
                     'language'
                     )


#Set names for control surveys
for (i in 1:length(colnames(control_10_25))) {
  colnames(control_10_25)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(control_11_02))) {
  colnames(control_11_02)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(experiment_10_19))) {
  colnames(experiment_10_19)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(experiment_10_21))) {
  colnames(experiment_10_21)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(experiment_10_25))) {
  colnames(experiment_10_25)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(experiment_11_02))) {
  colnames(experiment_11_02)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(experiment_12_07))) {
  colnames(experiment_12_07) <- desiredColnames_typeform
}

for (i in 1:length(colnames(control_12_07))) {
  colnames(control_12_07)[i] <- desiredColnames_typeform[i]
}

for (i in 1:length(colnames(control_12_20))) {
  colnames(control_12_20)[i] <- desiredColnames_typeform[i]
}

control_10_25 %>%
  setNames(desiredColnames)

control_11_02 %>%
  setNames(desiredColnames)

experiment_10_19 %>%
  setNames(desiredColnames)

experiment_10_21 %>%
  setNames(desiredColnames)

experiment_10_25 %>%
  setNames(desiredColnames)

experiment_11_02 %>%
  setNames(desiredColnames)

experiment_12_07 %>%
  setNames(desiredColnames)

control_12_07 %>%
  setNames(desiredColnames_typeform)

control_12_20 %>%
  setNames(desiredColnames_typeform)

#order alphabetically
# control_10_25<-control_10_25[,order(colnames(control_10_25))]
# control_11_02<-control_11_02[,order(colnames(control_11_02))]
# control_12_07<-control_12_07[,order(colnames(control_12_07))]
# control_12_20<-control_12_20[,order(colnames(control_12_20))]

#merge files
control_all<-rbind(control_10_25,control_11_02,control_12_07,control_12_20)

experiment_all<-rbind(experiment_10_19, experiment_10_21,  experiment_10_25, experiment_11_02, experiment_12_07)

test_1207 <- experiment_12_07 %>% dplyr::select(How.old.are.you.) %>% setNames('age')
test_1102 <- experiment_11_02 %>% dplyr::select(age)
test_1019 <- experiment_10_19 %>% dplyr::select(age)

experiment_test <- rbind(test_1207, test_1102)

all_RS<-rbind(control_all, experiment_all)

write.csv(all_RS, file = "Ride and Seek.csv")

# all_RS<-all_RS%>%
#   mutate(
#     case_when(gender == 'Female' ~ 1,
#               gender == 'Male' ~ 2,
#               gender == 'Genderqueer/Non-binary' ~ 3,
#               gender == 'Prefer not to say' ~ 4))

CrossTable(all_RS$age, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$gender, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$region, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$license, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$vehicle, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$driving_freq, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$seatbelt_driver, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$seatbelt_front, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$seatbelt_back, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(all_RS$danger_back, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$danger_front, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$danger_front, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(all_RS$intervene_driving, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$witnessed_noseatbelt, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(all_RS$convinced, all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(all_RS$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

all_RS_final<-all_RS%>%
  mutate(age=as.numeric(age),
         danger_back=as.character.numeric_version(danger_back))

CrossTable(all_RS_final$age, all_RS_final$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(all_RS_final$danger_back, all_RS_final$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

class(all_RS$age)

# surv %>%
#   mutate_at(vars(starts_with('danger')), function(x) substr(x, 1, 1))

speeding<-rbind(experiment_10_19, experiment_10_21,  experiment_10_25, experiment_11_02)%>%
  dplyr::select(mobile)

write.csv(speeding, file = "Speeding.csv")
