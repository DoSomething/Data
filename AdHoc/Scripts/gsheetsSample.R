source('config/init.R')
source('config/mySQLConfig.R')
library(googlesheets)

#List all the workbooks I have access to
gs_ls()

#Grab key of workbook I'm interested in
rbSheetKey <- 
  gs_ls() %>% 
  filter(grepl('Reportbacks Aster',sheet_title)) %>% 
  select(sheet_key) %>% 
  as.character()

#Grab the workbook of interest by key
rbWB <- 
  gs_key(rbSheetKey)

#Get list of sheet names in sheet
sheetNames <-
  rbWB %>% 
  gs_ws_ls()

#Grab the exact data frame of interest from the workbook
rbTotalsSheet <-
  rbWB %>% 
  gs_read(sheetNames[1])