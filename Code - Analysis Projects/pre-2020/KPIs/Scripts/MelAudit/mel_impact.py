import os

import numpy as np
import pandas as pd

import utils


# loads MEL data
if os.path.exists('member_event_log.csv.gz'):
    print('Loading file...')
    df_mel = pd.read_csv(
        'member_event_log.csv.gz',
        compression='gzip',
        low_memory=False
    )
    df_mel['timestamp'] = pd.to_datetime(df_mel['timestamp'], utc=True)
else:
    print('Pulling data from MEL...')
    conn = utils.load_connection(
        db_user_env='DB_USER',
        db_pw_env='DB_PW',
        db_name_env='DB_NAME',
        db_host_env='DB_HOST'
    )
    mel_sql = '''
        select *
        from public.member_event_log
        where "timestamp" between '2018-01-01' and '2018-11-01'
    '''
    df_mel = pd.read_sql(mel_sql, con=conn)
    df_mel['timestamp'] = pd.to_datetime(df_mel['timestamp'], utc=True)
    df_mel.to_csv('member_event_log.csv.gz', compression='gzip', index=False)


print('Count pre-cleaning: ')
start_count = utils.print_monthly_mamcount(df_mel)
print(start_count, '\n')


# remove 'clicked link' email duplicates that are 4 hours ahead
df_mel = df_mel.sort_values(['northstar_id', 'timestamp'])

df_mel = df_mel[~(
    (df_mel.duplicated(
        ['northstar_id', 'action_type', 'action_serial_id'],
        keep='first')
    ) &
    (df_mel['action_type'] == 'clicked_link')
)]


step1_count = utils.print_monthly_mamcount(df_mel)
print('Removing email duplicates...')
print(step1_count)
print('...deltas..')
print(step1_count - start_count, '\n')


# join bertly clicks to northstar users to remove bad/null ID values
def find_length(x):
    if isinstance(x, float):
        return 0
    else:
        return len(x)

df_mel['ns_len'] = df_mel['northstar_id'].apply(find_length)

df_mel = df_mel[~(
    ((df_mel['northstar_id'].isnull()) |
     (df_mel['northstar_id'] == '') |
     (df_mel['ns_len'] < 24)) &
    ((df_mel['action_type'].str.contains('bertly')) |
     (df_mel['action_type'] == 'clicked_link')
     )
)]

step2_count = utils.print_monthly_mamcount(df_mel)
print('Removing null/bad NS ids...')
print(step2_count)
print('...deltas...')
print(step2_count - step1_count, '\n')


df_mel = df_mel[~(
    (df_mel['action_type'] == 'signup') &
    (df_mel['source'].str.contains('runscope'))
)]


step3_count = utils.print_monthly_mamcount(df_mel)
print('Remove runscope signups...')
print(step3_count)
print('...deltas...')
print(step3_count - step2_count, '\n')
