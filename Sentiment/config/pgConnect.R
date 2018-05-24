library(RPostgreSQL)

pgConnect <- function() {

  drv <- dbDriver("PostgreSQL")
  user=Sys.getenv('QUASAR_USERNAME')
  pw=Sys.getenv('QUASAR_PG_PW')

  channel <-
    dbConnect(
      drv,
      user = user,
      password=pw,
      dbname = 'dbname=d4e7vm204dkmar sslmode=require',
      host = "ec2-52-201-106-141.compute-1.amazonaws.com",
      port = 5432#,
      # sslmode="required"
    )

  return(channel)

}

