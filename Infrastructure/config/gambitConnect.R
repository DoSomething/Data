connectGambit <- function() {
  require(mongolite)

  host = "aws-us-east-1-portal.28.dblayer.com"
  host1 = "aws-us-east-1-portal.29.dblayer.com"
  port = "21952"
  db = "conversation-api"
  username = "quasar-ro"
  password = Sys.getenv("GAMBIT_MONGO_PW")
  collection = "compose?authSource=conversations-api&ssl=true"

  url =
    paste0(
      "mongodb://",
      username, ":",
      password, "@",
      host, ":",
      port, ",",
      host1, ":",
      port, "/",
      collection
      )

  mongocon <- mongo(collection = "conversations", url = url, db=db,
                    verbose = T, options = ssl_options(weak_cert_validation = T))

  return(mongocon)

}

gam <- connectGambit()
