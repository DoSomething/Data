library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

inInterval <- function(x, interval){ 
  stopifnot(length(interval) == 2L) 
  interval[1] <= x & x <= interval[2] 
} 

mtable <- function(x) {
  table(x, exclude = NULL)
}

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

saveCSV <- function(dat, desktop = F) {
  datName <- deparse(substitute(dat))
  if (desktop == T) {
    write.csv(dat, row.names = F, na = '.',
              file = paste0('~/Desktop/',datName,'_', gsub("[[:punct:]]|\\ ", "", Sys.time()),'.csv'))
  } else {
    write.csv(dat, row.names = F, na = '.',
              file = paste0(getwd(),'/',datName,'_', gsub("[[:punct:]]|\\ ", "", Sys.time()),'.csv'))
    
  }
}
