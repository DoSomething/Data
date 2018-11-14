source('config/init.R')
library(gmodels)
library(dplyr)
library(ggpubr)
library(gplots)

#Upload surveys
talk_control<- read.csv('~/Documents/The Talk/HPV Survey 9.25.2018.csv')
talk_experiment<- read.csv('~/Documents/The Talk/The Talk.csv')

#Rename control column names
colnames(talk_control)

desiredColnames <- c('typeform_id', 
                     'heardof_HPV', 
                     'importance_vaccinated', 
                     'HPV_affects_males_females', 
                     'HPV_protects', 
                     'comfortable_asking_doctor', 
                     'comfortable_talking_parents', 
                     'likelihood_start_convo', 
                     'talked_friends', 
                     'talked_guardian', 
                     'talked_family', 
                     'talked_teacher', 
                     'talked_doctor', 
                     'talked_na', 
                     'talked_other',
                     'age', 
                     'gender', 
                     'gender_other', 
                     'race_white', 
                     'race_hispanic', 
                     'race_black', 
                     'race_nativeamerican', 
                     'race_asian', 
                     'race_pacific', 
                     'race_other', 
                     'zipcode', 
                     'region', 
                     'nsid', 
                     'start_date', 
                     'submit_id', 
                     'network_id')

#Set names for control
for (i in 1:length(colnames(talk_control))) {
  colnames(talk_control)[i] <- desiredColnames[i]
}

#Rename Experiment column names
colnames(talk_experiment)

desiredColnames_ex <- c('typeform_id', 
                     'heardof_HPV', 
                     'importance_vaccinated', 
                     'HPV_affects_males_females', 
                     'HPV_protects', 
                     'comfortable_asking_doctor', 
                     'comfortable_talking_parents', 
                     'likelihood_start_convo', 
                     'talked_friends', 
                     'talked_guardian', 
                     'talked_family', 
                     'talked_teacher', 
                     'talked_doctor', 
                     'talked_na', 
                     'talked_other',
                     'created_card',
                     'printed_card',
                     'read_7reasons',
                     'read_how_to_talk',
                     'shared_guides_friends',
                     'shared_social',
                     'used_finder',
                     'none_of_above',
                     'Other_action',
                     'reason_notparticipating',
                     'thought_differently',
                     'how_thought_differently',
                     'age', 
                     'gender', 
                     'gender_other', 
                     'race_white', 
                     'race_hispanic', 
                     'race_black', 
                     'race_nativeamerican', 
                     'race_asian', 
                     'race_pacific', 
                     'race_other', 
                     'zipcode', 
                     'region', 
                     'nsid', 
                     'start_date', 
                     'submit_id', 
                     'network_id')

#Set names for control
for (i in 1:length(colnames(talk_experiment))) {
  colnames(talk_experiment)[i] <- desiredColnames_ex[i]
}

#remove duplicates
control<- talk_control%>%
  filter(!duplicated(nsid))%>%
  mutate(group='control')

experiment<-talk_experiment%>%
  filter(!duplicated(nsid))%>%
  mutate(group='experiment')

#Join surveys on variables that are the same in both
colnames(talk_control)
colnames(talk_experiment)

control_merge <- control

experiment_merge <- experiment%>%
  select(1:15,28:44)

#recode variables
talk_merged <- rbind(control_merge,experiment_merge)%>%
  mutate(comfortable_parents = 
           case_when(
             comfortable_talking_parents=='Very uncomfortable' ~ 1,
             comfortable_talking_parents=='Uncomfortable' ~ 2,
             comfortable_talking_parents=='Neutral' ~3,
             comfortable_talking_parents=='Comfortable' ~4,
             comfortable_talking_parents=='Very comfortable'~5),
          start_convo =
            case_when(
              likelihood_start_convo=='Very unlikely' ~1,
              likelihood_start_convo=='Unlikely'~2,
              likelihood_start_convo=='Not sure'~3,
              likelihood_start_convo=='Likely'~4,
              likelihood_start_convo=='Very likely'~5),
         gender_cat=
           case_when(gender=='Woman' ~ 'Woman',
                     gender=='Man' ~ 'Man',
                     TRUE ~ 'Other'),
         gender_dummy=
           case_when(gender=='Woman' ~ 'Woman',
                     gender=='Man' ~ 'Man'),
         hpv_important = 
           case_when(
             importance_vaccinated=='Not important' ~ 1,
             importance_vaccinated=='Slightly important' ~ 2,
             importance_vaccinated=='Moderately important'~3,
             importance_vaccinated=='Important'~4,
             importance_vaccinated=='Very important'~5),
         talked =
           case_when(
             talked_friends=='Friends' |
             talked_family=='Parent/guardian' ~ 1,
             TRUE ~ 0),
         age_rec = 
           case_when(
             age =='13' ~ 13,
             age=='14' ~ 14,
             age=='15' ~ 15,
             age=='16' ~ 16,
             age=='17' ~ 17,
             age=='18' ~ 18,
             age=='19' ~ 19,
             age=='20' ~ 20,
             age=='21' ~ 21,
             age=='22' ~ 22,
             age=='23' ~ 23,
             age=='24' ~ 24,
             age=='25' ~ 25)
  ) 

#Exclude older than 25 and younger than 13
talk_merged <- talk_merged%>%
  filter(age!='Older than 25'& age!='Younger than 13' & age!='choice 16')

control_descriptives<-talk_merged%>%
  filter(age!='Older than 25'& age!='Younger than 13' & age!='choice 16' & group=='control')

experiment_descriptives<-talk_merged%>%
  filter(age!='Older than 25'& age!='Younger than 13' & age!='choice 16' & group=='experiment')

##########################################
########Experiment vs. Control############
###########################################

#Control Heard of HPV
count(control_descriptives, heardof_HPV, sort = TRUE)%>% mutate(p=n/sum(n))

#Experiment Heard of HPV before campaign
count(experiment_descriptives, heardof_HPV, sort = TRUE)%>% mutate(p=n/sum(n))

#Experiment vs. control - Demographics
CrossTable(talk_merged$age, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$gender_dummy, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$gender_cat, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$region, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


#Experiment vs. control - Outcomes
CrossTable(talk_merged$importance_vaccinated, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$HPV_affects_males_females, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$HPV_protects, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$comfortable_asking_doctor, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$comfortable_talking_parents, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$likelihood_start_convo, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(talk_merged$talked_friends, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$talked_family, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$talked_teacher, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(talk_merged$talked_doctor, talk_merged$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Check significance
t.test(talk_merged$hpv_important~talk_merged$group)
t.test(talk_merged$HPV_affects_males_females~talk_merged$group)
t.test(talk_merged$HPV_protects~talk_merged$group)
t.test(talk_merged$comfortable_asking_doctor~talk_merged$group)
t.test(talk_merged$comfortable_parents~talk_merged$group)
t.test(talk_merged$start_convo~talk_merged$group)
t.test(talk_merged$age_rec~talk_merged$group)

#########################################
#########Differences in Gender###########
########################################
CrossTable(experiment_descriptives$heardof_HPV, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$hpv_important, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$HPV_affects_males_females, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$HPV_protects, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$comfortable_asking_doctor, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$comfortable_parents, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$start_convo, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))


CrossTable(experiment_descriptives$talked_friends, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_family, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_teacher, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_doctor, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_guardian, experiment_descriptives$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


#########################################
#########Differences in Region###########
#########################################
CrossTable(experiment_descriptives$heardof_HPV, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$hpv_important, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$HPV_affects_males_females, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE,fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$HPV_protects, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE,format= c("SPSS"))
CrossTable(experiment_descriptives$comfortable_asking_doctor, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$comfortable_parents, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE,fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$start_convo, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE,fisher=TRUE, format= c("SPSS"))

CrossTable(experiment_descriptives$talked_friends, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_family, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_teacher, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_doctor, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_guardian, experiment_descriptives$region, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


# 
# group_by(experiment_descriptives, region) %>%
#   summarise(
#     count = n(),
#     mean = mean(comfortable_talking_parents, na.rm = TRUE),
#     sd = sd(comfortable_talking_parents, na.rm = TRUE)
#   )
# 
# #boxplot
# ggboxplot(experiment_descriptives, x = "region", y = "comfortable_talking_parents", 
#           color = "region", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("Rural", "Suburban", "Urban"),
#           ylab = "Comfortable", xlab = "Region")
# #plot means
# plotmeans(comfortable_talking_parents ~ region, data = experiment_descriptives, frame = FALSE,
#           xlab = "Region", ylab = "Comfortable talking to parents",
#           main="Mean Plot with 95% CI") 
# 
# #anova test for significance
# parents_region <- aov(comfortable_talking_parents ~ region, data = experiment_descriptives)
# summary(parents_region)


#########################################
#########Differences in Age###########
#########################################
experiment_descriptives <- experiment_descriptives%>%
  mutate(age_cat=
           case_when(
             age_rec<17 ~ '13 -16 years old',
             (age_rec>16 & age_rec<19) ~ '17-18 years old',
             age_rec>=19 ~ '19 years or older'))

#check coding
#CrossTable(experiment_descriptives$age_rec, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(experiment_descriptives$heardof_HPV, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE,fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$hpv_important, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE,fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$HPV_affects_males_females, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE,format= c("SPSS"))
CrossTable(experiment_descriptives$HPV_protects, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$comfortable_asking_doctor, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$comfortable_parents, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$start_convo, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, fisher=TRUE,format= c("SPSS"))

CrossTable(experiment_descriptives$talked_friends, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_family, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_teacher, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_doctor, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked_guardian, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(experiment_descriptives$talked, experiment_descriptives$age_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


tests <- aov(comfortable_talking_parents ~ age_cat, data = experiment_descriptives)
summary(tests)






