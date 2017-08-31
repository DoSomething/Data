import boto3
import botocore

s3 = boto3.resource('s3')

for bucket in s3.buckets.all():
    print(bucket.name)

keen_bucket = 'ds-keen-io'
bucket = s3.Bucket('keen_bucket')

key = 'n58cbf99954532c1f35f4331f/2017-04-14T21%3A15%3A00.000Z/action/58cbf99954532c1f35f4331f-action-2017-04-14T21%3A15%3A00.000Z.json.gz'


s3.Bucket(keen_bucket).download_file(key, '58cbf99954532c1f35f4331f-action-2017-04-14T21%3A15%3A00.000Z.json.gz')

