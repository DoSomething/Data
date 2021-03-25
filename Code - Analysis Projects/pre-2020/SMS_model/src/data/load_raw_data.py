import gzip

import pandas as pd

import utils as u

conn = u.load_connection()
cur = conn.cursor()

gambit_2018_sql = '''
COPY (
    SELECT
        o.user_id,
        o.campaign_id,
        o.conversation_id,
        o.created_at AS outbound_created_at,
        o.message_id AS outbound_message_id,
        o.template AS template,
        o.broadcast_id as outbound_broadcast_id,
        i.broadcast_id as inbound_broadcast_id,
        i.inbound_created_at,
        i.inbound_message_id
    FROM gambit_messages_outbound o
    LEFT JOIN (
        SELECT user_id, conversation_id, campaign_id,
            created_at AS inbound_created_at,
            message_id AS inbound_message_id,
            broadcast_id
        FROM gambit_messages_inbound
        WHERE created_at >= '2018-01-01'
        AND created_at < '2019-01-01'
        ) i
    ON o.conversation_id = i.conversation_id
    AND o.broadcast_id = i.broadcast_id
    AND o.user_id = i.user_id
    WHERE o.user_id NOT IN (
            SELECT DISTINCT user_id
            FROM gambit_messages_outbound
            WHERE "template" = 'subscriptionStatusStop'
            AND user_id IS NOT NULL)
    AND o.created_at >= '2018-01-01'
    AND o.created_at < '2019-01-01'
    AND o.broadcast_id IS NOT NULL
    AND o.broadcast_id NOT IN (
        SELECT DISTINCT broadcast_id
        FROM gambit_messages_outbound
        WHERE "template" = 'autoReplyBroadcast'
    )
) TO STDOUT WITH CSV HEADER DELIMITER ','
'''
with gzip.open('data/raw/gambit.csv.gz', 'w') as f:
    cur.copy_expert(gambit_2018_sql, f)

print('Wrote raw csv file to data/raw\n')

df_users = pd.read_csv(
    'data/raw/gambit.csv.gz', compression='gzip')

user_ids = list(df_users['user_id'].unique()[:1000000])

df_users[df_users['user_id'].isin(user_ids)].to_csv(
    'data/raw/gambit_sample.csv.gz', compression='gzip', index=False
)

print('Wrote sample of raw csv file to data/raw\n')

#  Fields removed to reduce file size and load times
#  o.macro AS outbound_macro
#  o.text AS outbound_text,
#  o.topic AS outbound_topic,
#  i.attachment_url,
#  i.inbound_macro,
#  i.total_segments,
#  i.inbound_text,
#  i.inbound_topic

#  in left join for inbound table:
#  macro AS inbound_macro,
#  total_segments,
#  "text" AS inbound_text,
#  topic AS inbound_topic,
#  attachment_url,
