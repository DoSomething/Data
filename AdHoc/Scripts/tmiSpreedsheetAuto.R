source('Scripts/init.R')
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

tmi.x <- read.csv('Data/bezos2017-08-10.csv') %>%
  tbl_dt() %>%
  select(-starts_with('NA')) %>%
  rename(Mobile.Number = device_address) %>%
  filter(!is.na(Mobile.Number)) 

tmi.x <- rbind(tmi.x, fullList, fill=T) %>%
  left_join(myLetters) %>%
  filter(!is.na(Mobile.Number)) %>%
  mutate(
    TipTime = ifelse(TipTime == 'Morning', 'AM', toupper(TipTime)),
    Child1DOB = as.character(Child1DOB),
    Child1DOB = 
      as.Date(
        ifelse(nchar(Child1DOB) == 7, as.Date(paste0('01/', Child1DOB), '%d/%m/%Y'), 
               ifelse(nchar(Child1DOB) == 8, as.Date(Child1DOB, '%d/%m/%y'),
                      ifelse(nchar(Child1DOB) == 10, as.Date(Child1DOB, '%m/%d/%Y'), NA))),
        origin='1970-01-01'),
    
    Child2DOB = as.character(Child2DOB),
    Child2DOB = 
      as.Date(
        ifelse(nchar(Child2DOB) == 7, as.Date(paste0('01/', Child2DOB), '%d/%m/%Y'), 
               ifelse(nchar(Child2DOB) == 8, as.Date(Child2DOB, '%d/%m/%y'),
                      ifelse(nchar(Child2DOB) == 10, as.Date(Child2DOB, '%m/%d/%Y'), NA))),
        origin='1970-01-01'),
    
    Child3DOB = as.character(Child3DOB),
    Child3DOB = 
      as.Date(
        ifelse(nchar(Child3DOB) == 7, as.Date(paste0('01/', Child3DOB), '%d/%m/%Y'), 
               ifelse(nchar(Child3DOB) == 8, as.Date(Child3DOB, '%d/%m/%y'),
                      ifelse(nchar(Child3DOB) == 10, as.Date(Child3DOB, '%m/%d/%Y'), NA))),
        origin='1970-01-01'),
    
    Child4DOB = as.character(Child4DOB),
    Child4DOB = 
      as.Date(
        ifelse(nchar(Child4DOB) == 7, as.Date(paste0('01/', Child4DOB), '%d/%m/%Y'), 
               ifelse(nchar(Child4DOB) == 8, as.Date(Child4DOB, '%d/%m/%y'),
                      ifelse(nchar(Child4DOB) == 10, as.Date(Child4DOB, '%m/%d/%Y'), NA))),
        origin='1970-01-01')
  ) %>%
  mutate(
    firstOfMonth = as.Date(paste0(substr(Sys.Date(), 1, 7), '-01')),
    Child1Age = pmin(pmax(floor( as.numeric((firstOfMonth - Child1DOB)) / 365), 0), 4),
    Child2Age = pmin(pmax(floor( as.numeric((firstOfMonth - Child2DOB)) / 365), 0), 4),
    Child3Age = pmin(pmax(floor( as.numeric((firstOfMonth - Child3DOB)) / 365), 0), 4),
    Child4Age = pmin(pmax(floor( as.numeric((firstOfMonth - Child4DOB)) / 365), 0), 4)
  ) %>%
  mutate(
    Child1Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child1Age),
    # Child2Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child2Age),
    # Child3Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child3Age),
    # Child4Age = ifelse(!(NumOfChildren %in% seq(1,4,1)), childAgeInterestRef, Child4Age),
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
    ContentTrack = ifelse(Language == 'ESP' & is.na(ContentTrack), 'Direct', ContentTrack),
    Language = as.character(Language)
  ) %>%
  select(
    -childAgeInterestRef, -starts_with('X')
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

ContentTracks <- c('Direct','Motivational')
TipTimes <- c('AM','PM')
Languages <- c('ENG','ESP')
Ages <- c(0,1,2,3,4)

allOptions <- data.table(expand.grid(ContentTrack = ContentTracks, TipTime = TipTimes, Language = Languages, Age = Ages))

write.xlsx(EngDirect, file = 'out.xlsx', sheetName = 'ENG Direct', row.names=F, showNA=F)
write.xlsx(EngMotiv, file = 'out.xlsx', sheetName = 'ENG Motive', row.names=F, append=T, showNA=F)
write.xlsx(ESPDirect, file = 'out.xlsx', sheetName = 'ESP Direct', row.names=F, append=T, showNA=F)
write.xlsx(tmi.x, file = 'out.xlsx', sheetName = 'All', row.names=F, append=T, showNA=F)

for (i in 1:nrow(allOptions)) {
  
  out <-
    tmi.x %>%
    filter(
      ContentTrack == allOptions[i,ContentTrack] & TipTime == allOptions[i,TipTime] & Language == allOptions[i,Language] &
      (Child1Age == allOptions[i,Age] | Child2Age == allOptions[i,Age] | Child3Age  == allOptions[i,Age] | Child4Age  == allOptions[i,Age])
    )
  
  if (nrow(out) > 0) {
    
    write.xlsx(
      out, 
      file = 'out.xlsx', 
      sheetName = paste(allOptions[i,ContentTrack],allOptions[i,TipTime],allOptions[i,Language], allOptions[i,Age], sep='_'), 
      row.names=F, append=T, showNA=F
    )
    
  }
  
}
