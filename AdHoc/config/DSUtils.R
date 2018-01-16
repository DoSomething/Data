library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#####Quick write csv without timestamp#####
saveCSV <- function(dat, desktop = F) {
  datName <- deparse(substitute(dat))
  if (desktop == T) {
    write.csv(
      dat, 
      row.names = F, 
      na = '.',
      file = paste0(
        '~/Desktop/',datName,'_', gsub("[[:punct:]]|\\ ", "", Sys.time()),'.csv'
        )
      )
  } else {
    write.csv(
      dat, 
      row.names = F, 
      na = '.',
      file = paste0(
        getwd(),'/',datName,'_', gsub("[[:punct:]]|\\ ", "", Sys.time()),'.csv'
        )
      )
    
  }
}

#####Format Query#####
getQuery <- function(path, wd = T) {
  if (wd ==T) {
    query <- paste(readLines(paste0(getwd(),'/',path)), collapse="\n")
  } else {
    query <- paste(readLines(paste0(path)), collapse="\n")
  }
}

runQuery <- function(query, which=c('pg','mysql')) {
  require(RMySQL)
  require(RPostgreSQL)
  source('config/mySQLConfig.R')
  source('config/pgConnect.R')
  
  if(grepl('.sql', query)) {
    
    q <- getQuery(query)
    
  } else {
    
    q <- query
    
  }
  
  if (which=='mysql') {
    connection <- quasarConnect()
  } else {
    connection <- pgConnect()
  }
  
  out <- tbl_df(dbGetQuery(connection, q))
  
  return(out)
  
}

#####Save multiple data frame to tabs in spreadsheet#####
save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, row.names = F, showNA = F, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, row.names = F, showNA = F, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

#####Get Matrix of missing values#####
get_missing_matrix = function(dt, vintage_indicator, binwidth = NULL, rounding = NULL, id_val ){
  dt_melt= data.table(melt(dt, id = (id_val)))
  tb = dt_melt[ , .SD[, list(pct_miss = sum(is.na(value))/.N)],
                by = c('variable',vintage_indicator)]
  return(tb)  
}

#####Rescale numeric feature to range between 0 and 1#####
scalerange <- function(x){(x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T))}

#####Reduce levels of a factor using decision tree#####
recat <- function(df, outcome, feature, compar=0.001) {
  part_obj <- rpart(formula = paste0(outcome, '~', feature), data = df, control = list(cp = compar)) 
  frame <- data.frame(part_obj$csplit)
  if (length(frame)==0) stop("No meaningful difference between levels with respect to outcome. Consider increasing complexity parameter or removing feature")
  levels <- levels(df[[feature]])
  names(frame) <- levels
  frame <- data.frame(t(frame))
  frame$path <- as.factor(apply(frame, 1, paste, collapse=""))
  frame$feature <- row.names(frame)
  unique_paths <- length(unique(frame$path))
  codes <- toupper(letters[1:unique_paths])
  levels(frame$path) <- codes
  frame <- subset(frame, select = c(path,feature))
  names(frame) <- c(paste0(feature,'_category'),feature)
  return(frame)
}

#####Get scaled mean values of numeric/logical features over time#####
get_mean_matrix = function(dt, vintage_indicator, binwidth = NULL, rounding = NULL, id_val ){
  dt <- data.frame(dt)[sapply(data.frame(dt),function(x) is.numeric(x)|is.logical(x))]
  scaled <- data.frame(apply(dt[-c(which(colnames(dt)==vintage_indicator))], 2, scalerange))
  dt <- dt[which(colnames(dt)==vintage_indicator)]
  dt <- cbind(dt,scaled)
  dt_melt = data.table(melt(dt, id = (id_val)))
  tb = dt_melt[ , .SD[, list(mean_value = mean(value, na.rm=T))],
                by = c('variable',vintage_indicator)]
  return(tb)  
}

#####Fill Null values with means#####
fillNAs = function(df) {
  dfClass = sapply(df, class)
  df = as.data.frame(df)
  jMax = ncol(df)
  for (j in 1:jMax)
  {
    #factors get assigned 'Missing'
    if (dfClass[j] %in% c('character','factor') & any(is.na(df[, j])))
    {
      set(df, which(is.na(df[[j]])), j, 'Missing')
    }
    #logicals get assigned false, and create a new column for whether the original was NA
    if (dfClass[j] == 'logical' & any(is.na(df[, j])))
    {
      if (any(is.na(df[, j])))
      {
        df[[paste0(names(df)[[j]], '_missing')]] = is.na(df[, j])
      }
      set(df, which(is.na(df[[j]])), j, F)
    }
    #numerics get assigned the column mean from the build sample, and create a new column for whether the original was NA
    if (dfClass[j] %in% c('numeric','integer') & any(is.na(df[, j])))
    {
      if (any(is.na(df[, j])))
      {
        df[[paste0(names(df)[[j]],'_missing')]] = is.na(df[, j])
      }
      set(df, which(is.na(df[[j]])), j, mean(df[[j]], na.rm=T))
    }
  }
  return(data.table(df))
} 

#####Convert character features to factors when levels are fewer than X#####
char_to_fact = function(df, levels) {
  dfClass = sapply(df, class)
  dflengthunique = sapply(df, function(x) length(unique(x)))
  df = as.data.frame(df)
  jMax = ncol(df)
  for (j in 1:jMax)
  {
    if (dfClass[j] == 'character' & dflengthunique[j] < levels)
    {
      df[[j]] <- factor(df[[j]])
    }
  }
  return(data.table(df))
}

####Is X within 2 numbers####
inInterval <- function(x, interval){ 
  stopifnot(length(interval) == 2L) 
  interval[1] <= x & x <= interval[2] 
} 

####Percent Change between Old and New Value####
pctChange <- function(Old, New) {
  pctChange <- ifelse(is.na(Old) | is.na(New), NA,
                      ifelse(Old == New, 0, 
                             ifelse(Old < 0 & New < 0, ((New - Old) / Old) * -1,
                                    ifelse(Old <= 0 | New < 0, 0, (New - Old) / Old))))
  return(pctChange)
}

##Apply Percent Change##
applyPctChange <- function(Value, percentChange) {
  out <- Value + (Value*percentChange)
  return(out)
}

####Nest preferred table call####
mtable <- function (..., exclude = if (useNA == "no") c(NA, NaN), useNA = c("no", 
                                                                            "ifany", "always"), dnn = list.names(...), deparse.level = 1) 
{
  list.names <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if (is.null(nm)) 
      seq_along(l)
    else nm == ""
    dep <- vapply(l[fixup], function(x) switch(deparse.level + 
                                                 1, "", if (is.symbol(x)) as.character(x) else "", 
                                               deparse(x, nlines = 1)[1L]), "")
    if (is.null(nm)) 
      dep
    else {
      nm[fixup] <- dep
      nm
    }
  }
  miss.use <- missing(useNA)
  miss.exc <- missing(exclude)
  useNA <- if (miss.use && !miss.exc && !match(NA, exclude, 
                                               nomatch = 0L)) 
    "ifany"
  else match.arg(useNA)
  doNA <- useNA != "no"
  if (!miss.use && !miss.exc && doNA && match(NA, exclude, 
                                              nomatch = 0L)) 
    warning("'exclude' containing NA and 'useNA' != \"no\"' are a bit contradicting")
  args <- list(...)
  if (!length(args)) 
    stop("nothing to tabulate")
  if (length(args) == 1L && is.list(args[[1L]])) {
    args <- args[[1L]]
    if (length(dnn) != length(args)) 
      dnn <- if (!is.null(argn <- names(args))) 
        argn
    else paste(dnn[1L], seq_along(args), sep = ".")
  }
  bin <- 0L
  lens <- NULL
  dims <- integer()
  pd <- 1L
  dn <- NULL
  for (a in args) {
    if (is.null(lens)) 
      lens <- length(a)
    else if (length(a) != lens) 
      stop("all arguments must have the same length")
    fact.a <- is.factor(a)
    if (doNA) 
      aNA <- anyNA(a)
    if (!fact.a) {
      a0 <- a
      a <- factor(a, exclude = exclude)
    }
    add.na <- doNA
    if (add.na) {
      ifany <- (useNA == "ifany")
      anNAc <- anyNA(a)
      add.na <- if (!ifany || anNAc) {
        ll <- levels(a)
        if (add.ll <- !anyNA(ll)) {
          ll <- c(ll, NA)
          TRUE
        }
        else if (!ifany && !anNAc) 
          FALSE
        else TRUE
      }
      else FALSE
    }
    if (add.na) 
      a <- factor(a, levels = ll, exclude = NULL)
    else ll <- levels(a)
    a <- as.integer(a)
    if (fact.a && !miss.exc) {
      ll <- ll[keep <- which(match(ll, exclude, nomatch = 0L) == 
                               0L)]
      a <- match(a, keep)
    }
    else if (!fact.a && add.na) {
      if (ifany && !aNA && add.ll) {
        ll <- ll[!is.na(ll)]
        is.na(a) <- match(a0, c(exclude, NA), nomatch = 0L) > 
          0L
      }
      else {
        is.na(a) <- match(a0, exclude, nomatch = 0L) > 
          0L
      }
    }
    nl <- length(ll)
    dims <- c(dims, nl)
    if (prod(dims) > .Machine$integer.max) 
      stop("attempt to make a table with >= 2^31 elements")
    dn <- c(dn, list(ll))
    bin <- bin + pd * (a - 1L)
    pd <- pd * nl
  }
  names(dn) <- dnn
  bin <- bin[!is.na(bin)]
  if (length(bin)) 
    bin <- bin + 1L
  y <- array(tabulate(bin, pd), dims, dimnames = dn)
  class(y) <- "table"
  y
}

colOneToRowName <- function(dat) {
  dat <- data.frame(dat)
  row.names(dat) <- dat[[1]]
  dat[[1]] = NULL
  return(dat)
}

firstDayOfMonth <- function(x) {
  x <- as.character(x)
  x <- substr(x, 1, 8)
  x <- paste0(x, '01')
  x <- as.Date(x)
  return(x)
}

reverseString <- function(x) {
  x <- paste(rev(substring(x,1:nchar(x),1:nchar(x))),collapse="") 
  return(x)
}

prepQueryObjects <- function(x) {
  
  items <- as.character(x)
  out <- "("
  
  for (i in 1:length(items)) {
    temp <- paste0("'", items[i], "',")  
    out <- paste0(out, temp)  
  }
  out <- substr(out, 1, nchar(out)-1)
  out <- paste0(out, ')')
  
  return(out)
}

#garbage
# cleanDOB <- function(x) {
#   browser()
#   month.subs <- substr(month.name, 1, 3)
#   x = as.character(x)
#   x = gsub("-", "/", x)
#   x = gsub("[^0-9\\/\\-]", "", x) 
#   slashCount <- nchar(gsub("[^\\/]","",x))
#   bigYFirst = ifelse(grepl('/', substr(x,1,4))==F, T, F)
#   bigYLast = ifelse(grepl('/', substr(x,nchar(x)-3,nchar(x)))==F, T, F)
#   x = 
#     as.Date(
#       ifelse(substr(x,1,3) %in% month.subs, as.Date(paste0('01-', substr(x, 1, 3), substr(x, 4, 6)), format='%d-%b-%y'), 
#              ifelse(nchar(gsub("[^\\/]", "", x))==2 & nchar(x)==7, as.Date(x, format='%m/%d/%y'),
#                     ifelse(nchar(gsub("[^\\/]", "", x))==1 & nchar(x)==7, as.Date(paste0('01/', x), format='%d/%m/%Y'),
#                            ifelse(nchar(x) %in% c(7), as.Date(x, format='%m/%d/%y'),
#                                   ifelse(grepl('/', x), as.Date(x, format='%m/%d/%Y'), 
#                                          ifelse(grepl('-', x), as.Date(x, format='%m-%d-%Y'), NA)))))),
#       origin='1970-01-01')
#   return(x)
# }

cleanDOB <- function(x) {
  slashCount <- nchar(gsub("[^\\/]","",x))
  out <- ifelse(slashCount == 1, 
                as.Date(paste0('01/',x), '%d/%m/%Y'), 
                as.Date(x, '%m/%d/%Y'))
  return(as.Date(out, origin='1970-01-01'))
}

cleanPhone <- function(Phone) {
  
  Phone = as.numeric(gsub("[^0-9]", "", Phone))
  Phone = ifelse(
    nchar(Phone) == 11 & 
      substr(Phone, 1, 1)==1, 
    substr(Phone, 2, nchar(Phone)), 
    Phone
  )
  Phone = ifelse(nchar(Phone) > 10, substr(Phone, nchar(Phone) - 9 , nchar(Phone)), Phone)
  return(as.character(Phone))
}
