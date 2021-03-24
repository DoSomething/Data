import os
import psycopg2


def load_connection(
        user_str='DB_USER',
        pwd_str='DB_PW',
        db_str='DB_NAME',
        host_str='DB_HOST'
):
    user = os.environ.get(user_str)
    pwd = os.environ.get(pwd_str)
    db = os.environ.get(db_str)
    host = os.environ.get(host_str)
    return psycopg2.connect(database=db, user=user, password=pwd, host=host)


def sql_stringify_list(li):
    sql_str = ''
    for l in li:
        if l != li[-1]:
            sql_str += "'" + l + "', "
        else:
            sql_str += "'" + l + "'"

    return sql_str


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


def clean_state_data(df):
    df['state'] = df['state'].str.upper()
    df['state'] = df['state'].str.strip()
    df['state'] = df['state'].str.replace(r'{VALUE: ', '')
    df['state'] = df['state'].str.replace(r'}| -', '')
    df['state'] = df['state'].apply(
        map_state_to_abbrev)
    df = df[~(df['state'].isnull())]

    return df
