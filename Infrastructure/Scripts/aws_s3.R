source('config/init.R')

library(jsonlite)

library(aws.s3)

bucketlist()

get_bucket('ds-keen-io')

a <-
    get_object(
    '58cbf99954532c1f35f4331f/58cbf99954532c1f35f4331f/2017-04-21T01:10:00.000Z/signup created/58cbf99954532c1f35f4331f-signup created-2017-04-21T01:10:00.000Z.json.gz',
    bucket='ds-keen-io')