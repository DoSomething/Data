source('Scripts/init.R')
source('Scripts/DSUtils.R')
library(xlsx)

myLetters <- data.table(ChildAgeInterest = LETTERS[1:5])
myLetters[,childAgeInterestRef := as.numeric(row.names(myLetters)) - 1]

fullList <-
  data.table(
    Mobile.Number = NA, TipTime = NA, Updated = NA, Child1DOB = NA, 
    Child1Age = NA, Child1FirstName = NA,
    Child1FirstNameSeg01 = NA, Child1FirstNameSeg12 = NA, Child1FirstNameSeg23 = NA, Child1FirstNameSeg34 = NA,
    Child1FirstNameSeg45 = NA,
    Child2DOB = NA, Child2Age = NA,	Child2FirstName = NA,
    Child2FirstNameSeg01 = NA, Child2FirstNameSeg12 = NA, Child2FirstNameSeg23 = NA, Child2FirstNameSeg34 = NA,
    Child2FirstNameSeg45 = NA,
    Child3DOB = NA, Child3Age = NA, Child3FirstName = NA,
    Child3FirstNameSeg01 = NA, Child3FirstNameSeg12 = NA, Child3FirstNameSeg23 = NA, Child3FirstNameSeg34 = NA,
    Child3FirstNameSeg45 = NA,
    Child4DOB = NA, Child4Age = NA, Child4FirstName = NA,
    Child4FirstNameSeg01 = NA, Child4FirstNameSeg12 = NA, Child4FirstNameSeg23 = NA, Child4FirstNameSeg34 = NA,
    Child4FirstNameSeg45 = NA,
    ChildAgeInterest = NA, ContentTrack = NA, Language = NA, NumOfChildren = NA
  )

tmi.x <- read.xlsx('~/Downloads/DOWNLOAD_Week_Nine.xlsx', 1) %>%
  tbl_dt() %>%
  select(-starts_with('NA')) %>%
  filter(!is.na(Mobile.Number)) 

tmi.x <- rbind(tmi.x, fullList, fill=T) %>%
  left_join(myLetters) %>%
  filter(!is.na(Mobile.Number)) %>%
  mutate(
    firstOfMonth = as.Date(paste0(substr(Sys.Date(), 1, 7), '-01')),
    Child1Age = round( as.numeric((firstOfMonth - Child1DOB)) / 365),
    Child2Age = round( as.numeric((firstOfMonth - Child2DOB)) / 365),
    Child3Age = round( as.numeric((firstOfMonth - Child3DOB)) / 365),
    Child4Age = round( as.numeric((firstOfMonth - Child4DOB)) / 365)
  ) %>%
  mutate(
    Child1Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child1Age),
    Child2Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child2Age),
    Child3Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child3Age),
    Child4Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child4Age),
    Child1FirstName = ifelse(!(NumOfChildren %in% seq(1,4,1)), 'your child', as.character(Child1FirstName)),
    Child2FirstName = as.character(Child2FirstName),
    Child3FirstName = as.character(Child3FirstName),
    Child4FirstName = as.character(Child4FirstName)
  ) %>%
  mutate(
    Child1FirstNameSeg01 = ifelse(Child1Age < 1, Child1FirstName, NA),
    Child1FirstNameSeg12 = ifelse(Child1Age >= 1 & Child1Age < 2 , Child1FirstName, NA),
    Child1FirstNameSeg23 = ifelse(Child1Age >= 2 & Child1Age < 3, Child1FirstName, NA),
    Child1FirstNameSeg34 = ifelse(Child1Age >= 3 & Child1Age < 4, Child1FirstName, NA),
    Child1FirstNameSeg45 = ifelse(Child1Age >= 4, Child1FirstName, NA),
    
    Child2FirstNameSeg01 = ifelse(Child2Age < 1, Child2FirstName, NA),
    Child2FirstNameSeg12 = ifelse(Child2Age >= 1 & Child2Age < 2 , Child2FirstName, NA),
    Child2FirstNameSeg23 = ifelse(Child2Age >= 2 & Child2Age < 3, Child2FirstName, NA),
    Child2FirstNameSeg34 = ifelse(Child2Age >= 3 & Child2Age < 4, Child2FirstName, NA),
    Child2FirstNameSeg45 = ifelse(Child2Age >= 4, Child2FirstName, NA),
    
    Child3FirstNameSeg01 = ifelse(Child3Age < 1, Child3FirstName, NA),
    Child3FirstNameSeg12 = ifelse(Child3Age >= 1 & Child3Age < 2 , Child3FirstName, NA),
    Child3FirstNameSeg23 = ifelse(Child3Age >= 2 & Child3Age < 3, Child3FirstName, NA),
    Child3FirstNameSeg34 = ifelse(Child3Age >= 3 & Child3Age < 4, Child3FirstName, NA),
    Child3FirstNameSeg45 = ifelse(Child3Age >= 4, Child3FirstName, NA),
    
    Child4FirstNameSeg01 = ifelse(Child4Age < 1, Child4FirstName, NA),
    Child4FirstNameSeg12 = ifelse(Child4Age >= 1 & Child4Age < 2 , Child4FirstName, NA),
    Child4FirstNameSeg23 = ifelse(Child4Age >= 2 & Child4Age < 3, Child4FirstName, NA),
    Child4FirstNameSeg34 = ifelse(Child4Age >= 3 & Child4Age < 4, Child4FirstName, NA),
    Child4FirstNameSeg45 = ifelse(Child4Age >= 4, Child4FirstName, NA)
  ) %>%
  mutate(
    ContentTrack = as.character(ContentTrack),
    ContentTrack = ifelse(Language == 'ESP' & is.na(ContentTrack), 'Direct', ContentTrack)
  ) %>%
  select(
    -childAgeInterestRef
  )

EngDirect <-
  tmi.x %>%
  filter(
   Language == 'ENG' & ContentTrack == 'Direct' 
  )

EngMotiv <-
  tmi.x %>%
  filter(
    Language == 'ENG' & ContentTrack == 'Motivational' 
  )

ESPDirect <-
  tmi.x %>%
  filter(
    Language == 'ESP' & ContentTrack == 'Direct' 
  )

write.xlsx(EngDirect, file = 'out.xlsx', sheetName = 'ENG Direct', row.names=F, showNA=F)
write.xlsx(EngMotiv, file = 'out.xlsx', sheetName = 'ENG Motive', row.names=F, append=T, showNA=F)
write.xlsx(ESPDirect, file = 'out.xlsx', sheetName = 'ESP Direct', row.names=F, append=T, showNA=F)
write.xlsx(tmi.x, file = 'out.xlsx', sheetName = 'All', row.names=F, append=T, showNA=F)

