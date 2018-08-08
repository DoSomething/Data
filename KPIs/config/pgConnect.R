library(RPostgreSQL)

pgConnect <- function(QA=F) {

  drv <- dbDriver("PostgreSQL")
  user=Sys.getenv('QUASAR_USERNAME')

  if (QA==T) {
    pw=Sys.getenv('QUASAR_QA_PW')
    host="ec2-18-211-176-65.compute-1.amazonaws.com"
    db="deh9oi912c63mc"
  } else {
    pw=Sys.getenv('QUASAR_PG_PW')
    host="ec2-52-201-106-141.compute-1.amazonaws.com"
    db="d4e7vm204dkmar"
  }

  channel <-
    dbConnect(
      drv,
      user = user,
      password=pw,
      dbname = paste0('dbname=',db,' sslmode=require'),
      host = host,
      port = 5432#,
      # sslmode="required"
    )

  return(channel)

}

