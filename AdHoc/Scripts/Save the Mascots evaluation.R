source('config/init.R')
library(httr)
library(jsonlite)
library(gmodels)

### Grab API key from environment var
key <- Sys.getenv('TYPEFORM_KEY')

# Get a list of all our typeforms and their unique IDs
allForms <- paste0('https://api.typeform.com/v1/forms?key=',key)
res <- GET(url = allForms)
json <- httr::content(res, as = "text")
allTypeForms <- fromJSON(json)

# Grab the ID for the DoSomething Smoking survey
allTypeForms %>% 
  filter(name == 'DS Smoking Survey') %>% 
  select(id) %>% as.character() -> feedback

smsq1Key <- 'IRoJVI'

# Submit request for that survey
smoking_control<- paste0('https://api.typeform.com/v1/form/',smsq1Key,'?key=',key)
res <- GET(url = smoking_control)
json <- content(res, as = "text")
feedbackResults <- fromJSON(json)

# Grab questions and answers (including hidden fields) from request
questions <- as.tibble(feedbackResults$questions)
answers <- as.tibble(cbind(feedbackResults$responses$hidden, feedbackResults$responses$answers))

answers %<>%
  filter(!is.na(id))

control_smoking <- answers

control_smoking <- control_smoking %>%
  rename(policy_knowledge = list_IMBP0hfaYPs0_choice,
         writtenpolicy = list_ljLnPfuufQpl_choice,
         writtenpolicy_other = list_ljLnPfuufQpl_other,
         tobacco_free = list_FZ8nP5cxp8oZ_choice,
         best_policy = list_W4Dr0z5VVGPM_choice,
         ecigg = list_SV0NHb4lBF7p_choice,
         secondsmoke = opinionscale_GRkvtSNtVq1G,
         secondsmoke_bother = list_FffhhtF5FjyW_choice,
         secondsmoke_exposire = list_e4arU6uOnAto_choice,
         ciggbutt = opinionscale_D3J6E6Q4L8T7,
          quit = list_aT9n0mxRcfxB_choice,
         gender = list_ACvjJ4FPW1Uy_choice,
         gender_other = list_ACvjJ4FPW1Uy_other,
          quit_cessation = opinionscale_xlQYdYwPA5TB,
          quit_campus = opinionscale_DuzkbQYAEqIZ,
         quit_patches = opinionscale_rksQBwIoObYH,
         quit_groups = opinionscale_VDuKMnlhoWUn)

control_smoking$group <- "control"
control_smoking$gender_other <- NULL

control_smoking <- control_smoking%>%
  mutate(secondsmoke=as.numeric(secondsmoke),
         ciggbutt=as.numeric(ciggbutt),
         quit_campus=as.numeric(quit_campus),
         quit_cessation=as.numeric(quit_cessation),
         quit_patches=as.numeric(quit_patches),
         quit_groups=as.numeric(quit_groups))

#Check freqs
control_smoking%>%count(gender)
policy <- table(control_smoking$policy_knowledge)
prop.table(policy)
CrossTable(control_smoking$tobacco_free, control_smoking$secondsmoke, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
           
# control_smoking <- read.csv('~/Documents/Data Requests/Save the Mascots/DS Smoking Survey.csv')

# Get a list of all our typeforms and their unique IDs
allForms <- paste0('https://api.typeform.com/v1/forms?key=',key)
res <- GET(url = allForms)
json <- httr::content(res, as = "text")
allTypeForms <- fromJSON(json)

# Grab the ID for the DoSomething Smoking survey
allTypeForms %>% 
  filter(name == 'Save the Mascots') %>% 
  select(id) %>% as.character() -> feedback

smsq1Key <- 'l2EuCx'

# Submit request for that survey
smoking<- paste0('https://api.typeform.com/v1/form/',smsq1Key,'?key=',key)
res <- GET(url = smoking)
json <- content(res, as = "text")
feedbackResults <- fromJSON(json)
# Grab questions and answers (including hidden fields) from request
questions_savemascots<- as.tibble(feedbackResults$questions)
answers_savemascots <- as.tibble(cbind(feedbackResults$responses$hidden, feedbackResults$responses$answers))

answers_savemascots %<>%
  filter(!is.na(id))

save_the_mascots <- answers_savemascots

save_the_mascots <- save_the_mascots %>%
  rename(policy_knowledge = list_cjCXigJPf0Ru_choice,
         writtenpolicy = list_UVgCeUFghSkj_choice,
         writtenpolicy_other = list_UVgCeUFghSkj_other,
         tobacco_free = list_dkx4P2LINn2b_choice,
         best_policy = list_iG9lLlUojtGb_choice,
         ecigg = list_vWagxb66KlaB_choice,
         secondsmoke = opinionscale_riCZZ60X8pMD,
         secondsmoke_bother = list_YHQVaZKs9R2c_choice,
         secondsmoke_exposire = list_kaz9ko80bHBa_choice,
         ciggbutt = opinionscale_ikynb4zz3R2P,
         quit = list_sJYHVlENTtY5_choice,
         gender = list_pcbzrcu1E1w4_choice,
         quit_cessation = opinionscale_stFSegivtNqz,
         quit_campus = opinionscale_o3MZnpM2zUaZ,
         quit_patches = opinionscale_ZtHXH1vRwgsu,
         quit_groups = opinionscale_TudQ0t48k06L)

save_the_mascots$group<- "campaign"
save_the_mascots$textfield_F7N38h8xWxEV <- NULL

save_the_mascots <- save_the_mascots%>%
  mutate(secondsmoke=as.numeric(secondsmoke),
         ciggbutt=as.numeric(ciggbutt),
         quit_campus=as.numeric(quit_campus),
         quit_cessation=as.numeric(quit_cessation),
         quit_patches=as.numeric(quit_patches),
         quit_groups=as.numeric(quit_groups))

#Merge control and save the mascots data
smoking_eval <- rbind(control_smoking, save_the_mascots)

CrossTable(smoking_eval$policy_knowledge, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(smoking_eval$writtenpolicy, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(smoking_eval$tobacco_free, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(smoking_eval$best_policy, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(smoking_eval$ecigg, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(smoking_eval$secondsmoke, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
t.test(smoking_eval$secondsmoke~smoking_eval$group)
CrossTable(smoking_eval$secondsmoke_bother, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(smoking_eval$secondsmoke_exposire, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(smoking_eval$ciggbutt, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
t.test(smoking_eval$ciggbutt~smoking_eval$group)
CrossTable(smoking_eval$quit, smoking_eval$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


