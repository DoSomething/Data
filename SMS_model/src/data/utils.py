import os

import pandas as pd
import psycopg2


# functions/variables to support db querying #
def load_connection(
        db_user='DB_USER',
        db_pw='DB_PW',
        db_name='DB_NAME',
        db_host='DB_HOST'):
    user = os.environ.get(db_user)
    pwd = os.environ.get(db_pw)
    db = os.environ.get(db_name)
    host = os.environ.get(db_host)
    return psycopg2.connect(database=db, user=user, password=pwd, host=host)


def sql_stringify_list(li):
    sql_str = ''

    if isinstance(li[0], str):
        for item in li:
            if item != li[-1]:
                sql_str += "'" + item + "', "
            else:
                sql_str += "'" + item + "'"
    else:
        for l in li:
            if item != li[-1]:
                sql_str += str(int(item)) + ", "
            else:
                sql_str += str(int(item))
    return sql_str


user_info_sql = '''
    SELECT
        2018 - date_part('YEAR', u.birthdate) AS age,
        EXTRACT(DAY FROM ('2018-12-31' - u.created_at)) AS length_of_membership_days,
        u.source,
        u.state,
        u.sms_status,
        u.northstar_id,
        u.voter_registration_status,
        u.cio_status,
        s.id as signup_id,
        s.campaign_id,
        s.created_at as signup_created_at,
        r.post_created_at,
        r.post_id,
        r.post_type
    FROM public.users u
    LEFT JOIN (
            SELECT northstar_id, campaign_id, max(id) as id, max(created_at) as created_at
        FROM signups 
        WHERE created_at >= '2018-01-01'
        AND created_at < '2019-01-01'
        GROUP BY northstar_id, campaign_id) s
    ON u.northstar_id = s.northstar_id
    LEFT JOIN (
        SELECT northstar_id, signup_id, min(post_created_at) as post_created_at, min(post_id) as post_id, min(post_type) AS post_type 
        FROM reportbacks
        GROUP BY northstar_id, signup_id) r
    ON u.northstar_id = r.northstar_id
        and s.id = r.signup_id
    WHERE u.northstar_id in ({});
'''


campaign_causes_sql = '''
    SELECT s.northstar_id, s.campaign_id, c.campaign_cause_type
    FROM signups s
    LEFT JOIN (SELECT campaign_id, campaign_cause_type FROM campaign_info) c
    ON s.campaign_id::bigint = c.campaign_id
    WHERE s.campaign_id <> 'NA'
    AND s.created_at >= '2018-01-01'
'''


# group by functions #
def non_null_unique(ser):
    '''Returns the number of unique values in series, excluding nulls'''
    return len(ser[ser.notnull()].unique())


def top_val(ser):
    if len(ser.value_counts()):
        return ser.value_counts().index[0]
    return None


# dataset processing/cleaning functions #
def build_target(df):
    # find number of messages sent and received from each user
    df_users = df.groupby('northstar_id').agg({
        'outbound_message_id': lambda x: len(x.unique()),
        'inbound_message_id': non_null_unique,
    })

    # target variable identifies a user active if they responded
    # at least once
    df_users['target'] = (df_users['inbound_message_id'] > 0) * 1

    # also interesting to know percentage response
    df_users['percent_response'] = (
        df_users['inbound_message_id'] / df_users['outbound_message_id'])
    return (
        df_users['target'].reset_index(),
        df_users['percent_response'].reset_index())


def load_data(sql, output_filepath, user_ids):
    conn = load_connection()
    sql_query = sql.format(user_ids)

    df_sql = pd.read_sql(sql_query, conn)
    df_sql.to_csv(
        output_filepath,
        compression='gzip',
        index=False)

    return df_sql


state_abbrev_mapping = {
    'ALABAMA': 'AL',
    'ALASKA': 'AK',
    'ARIZONA': 'AZ',
    'ARKANSAS': 'AR',
    'CALIFORNIA': 'CA',
    'COLORADO': 'CO',
    'CONNECTICUT': 'CT',
    'DELAWARE': 'DE',
    'FLORIDA': 'FL',
    'GEORGIA': 'GA',
    'HAWAII': 'HI',
    'IDAHO': 'ID',
    'ILLINOIS': 'IL',
    'INDIANA': 'IN',
    'IOWA': 'IA',
    'KANSAS': 'KS',
    'KENTUCKY': 'KY',
    'LOUISIANA': 'LA',
    'MAINE': 'ME',
    'MARYLAND': 'MD',
    'MASSACHUSETTS': 'MA',
    'MICHIGAN': 'MI',
    'MINNESOTA': 'MN',
    'MISSISSIPPI': 'MS',
    'MISSOURI': 'MO',
    'MONTANA': 'MT',
    'NEBRASKA': 'NE',
    'NEVADA': 'NV',
    'NEW HAMPSHIRE': 'NH',
    'NEW JERSEY': 'NJ',
    'NEW MEXICO': 'NM',
    'NEW YORK': 'NY',
    'NORTH CAROLINA': 'NC',
    'NORTH DAKOTA': 'ND',
    'OHIO': 'OH',
    'OKLAHOMA': 'OK',
    'OREGON': 'OR',
    'PENNSYLVANIA': 'PA',
    'RHODE ISLAND': 'RI',
    'SOUTH CAROLINA': 'SC',
    'SOUTH DAKOTA': 'SD',
    'TENNESSEE': 'TN',
    'TEXAS': 'TX',
    'UTAH': 'UT',
    'VERMONT': 'VT',
    'VIRGINIA': 'VA',
    'WASHINGTON': 'WA',
    'WEST VIRGINIA': 'WV',
    'WISCONSIN': 'WI',
    'WYOMING': 'WY',
    'AMERICAN SAMOA': 'AS',
    'DISTRICT OF COLUMBIA': 'DC',
    'FEDERATED STATES OF MICRONESIA': 'FM',
    'GUAM': 'GU',
    'MARSHALL ISLANDS': 'MH',
    'NORTHERN MARIANA ISLANDS': 'MP',
    'PALAU': 'PW',
    'PUERTO RICO': 'PR',
    'VIRGIN ISLANDS': 'VI'
}


def map_state_to_abbrev(val):
    '''Note that this will exclude any foreign users'''
    if (val not in state_abbrev_mapping.values()) and (
            val in state_abbrev_mapping.keys()):
        return state_abbrev_mapping[val]
    if (val not in state_abbrev_mapping.values()) and (
            val not in state_abbrev_mapping.keys()):
        return None
    return val


def process_user_data(df):
    df_user_activity = df.groupby('northstar_id').agg({
        'signup_id': {'num_signups': non_null_unique},
        'signup_created_at': {'most_recent_signup': 'max'},
        'post_id': {'num_rbs': non_null_unique},
        'post_created_at': {'most_recent_post': 'max'},
        'post_type': {'post_type': top_val}
    })

    df_user_activity.columns = df_user_activity.columns.droplevel()

    df_deduped = df[~(df[
        ['age', 'length_of_membership_days', 'source', 'state', 'northstar_id']
    ].duplicated(keep='first'))]

    df_deduped.drop(
        columns=[
            'signup_id', 'campaign_id', 'signup_created_at',
            'post_created_at', 'post_id', 'post_type'
        ],
        inplace=True)

    df_user_final = pd.merge(
        df_user_activity, df_deduped, how='left', on='northstar_id')

    return df_user_final


def process_campaign_data(df):
    # add in campaigns with cause types provided by Tej and Jen
    df['campaign_id'] = pd.to_numeric(df['campaign_id'])
    df_other_causes = pd.read_csv('data/raw/other_campaigns.csv')

    for campaign_id in df_other_causes['campaign_id'].unique():
        cause = df_other_causes[
            df_other_causes['campaign_id'] == campaign_id][
                'campaign_cause_type'].values[0]
        df.loc[df['campaign_id'] == campaign_id, 'campaign_cause_type'] = cause

    # drop any remaining campaigns that don't have cause types
    df = df[~(df['campaign_cause_type'].isnull() | (
        df['campaign_cause_type'] == ''))]

    # expand out and stack cause types
    df['campaign_cause_type'] = df['campaign_cause_type'].apply(
        lambda x: x.split(', '))

    df_expanded = pd.DataFrame(
        df['campaign_cause_type'].values.tolist(),
        columns=['cause1', 'cause2', 'cause3', 'cause4'])

    df_user_campaigns = None

    for col in df_expanded.columns:
        if df_user_campaigns is None:
            df_user_campaigns = pd.concat(
                [df['northstar_id'], df_expanded[col]], axis=1).rename(
                    columns={col: 'cause'})
        else:
            df_cause = pd.concat(
                [df['northstar_id'], df_expanded[col]], axis=1).rename(
                    columns={col: 'cause'})
            df_user_campaigns = pd.concat(
                [df_user_campaigns, df_cause], axis=0)

    df_user_campaigns = df_user_campaigns[~(
        df_user_campaigns['cause'].isnull() |
        (df_user_campaigns['cause'] == ''))]

    # group by cause and northstar to find user participation
    df_grouped = (
        (df_user_campaigns.groupby(['cause', 'northstar_id']).size() > 0)*1)
    df_grouped = df_grouped.unstack().transpose().fillna(0)

    return df_grouped.reset_index()


def clean_data(df):
    # remove null values
    df = df[~(df['age'].isnull())]
    df = df[~(df['state'].isnull() | (df['state'] == ''))]
    df = df[~(df['source'].isnull())]
    df = df[~(df['sms_status'].isnull())]

    # group less frequent source types into 'other'
    df.loc[~df['source'].isin(
        ['phoenix', 'sms', 'niche', 'phoenix-next', 'phoenix-oauth']
    ), 'source'] = 'other'

    # clean state values
    df['state'] = df['state'].str.upper()
    df['state'] = df['state'].str.strip()
    df['state'] = df['state'].str.replace(r'{VALUE: ', '')
    df['state'] = df['state'].str.replace(r'}| -', '')
    df['state'] = df['state'].apply(
        map_state_to_abbrev)
    df = df[~(df['state'].isnull())]

    # clean age values
    df = df[~(df['age'] > 50)]

    # condense voter registration values
    df.loc[
        (df['voter_registration_status'].isnull() |
         (df['voter_registration_status'].isin(['ineligible', 'uncertain']))),
        'voter_registration_status'] = 'unregistered'

    df.loc[
        df['voter_registration_status'].isin(
            ['registration_complete', 'confirmed']),
        'voter_registration_status'] = 'registered'

    # clean sms status values
    df = df[~(df['sms_status'].isin(['undeliverable', 'stop', 'GDPR']))]

    # set null cio status to unsubscribed
    df.loc[df['cio_status'].isnull(), 'cio_status'] = 'customer_unsubscribed'

    return df
