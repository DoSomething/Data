source('config/init.R')
library(httr)
library(jsonlite)

endpoints <- fromJSON('https://track.customer.io/')

un=Sys.getenv('CUSTOMER_IO_SITE_ID')
pw=Sys.getenv('CUSTOMER_IO_TOKEN')
base='https://track.customer.io/api/v1/'
mail='customers/593660b6a0bfad6d443e19a3'

cioCall <- paste0(base, mail)

response <- content(GET(cioCall, authenticate(un,pw, type = "basic")), 'text')
