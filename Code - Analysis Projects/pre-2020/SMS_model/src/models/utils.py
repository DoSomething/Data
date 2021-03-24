import numpy as np
import pandas as pd


def print_null_vals(df):
    null_vals = []
    for col in df.columns:
        if df[col].dtype == 'object':
            null_val = sum(df[col].isnull()) + (
                sum(df[col] == '') + sum(df[col] == '0'))
        if df[col].dtype == 'datetime64[ns]':
            null_val = sum(df[col].isnull()) + sum(df[col] == '')
        if df[col].dtype == 'float64' or df[col].dtype == 'int64':
            null_val = sum(df[col].isnull())
        null_vals.append(null_val)

    df_nulls = pd.DataFrame({
        'col': df.columns,
        'num_null': null_vals,
        'percent_null': list(np.array(null_vals) / len(df))
    })

    return df_nulls


def _days_since_active(val):
    if val is not None:
        return (pd.to_datetime('2018-12-31') - val).days
    return None


def _pick_most_recent(df):
    if (df['most_recent_signup'] is pd.NaT) & (
            df['most_recent_post'] is pd.NaT):
        return pd.NaT
    elif df['most_recent_post'] is pd.NaT:
        return df['most_recent_signup']
    elif df['most_recent_signup'] > df['most_recent_post']:
        return df['most_recent_signup']
    else:
        return df['most_recent_post']


def find_activity_level(df, last_active):
    '''breaks down users into three categories defined below:
            inactive: no activity;
            less active: last signup/post > last_active days;
            active: last signup/post < last_active days'''

    most_recent = _pick_most_recent(df)
    days_since_active = _days_since_active(most_recent)

    if np.isnan(days_since_active):
        return 'no_action'
    if days_since_active > last_active:
        return 'less_recent_action'
    if days_since_active < last_active:
        return 'recent_action'


region_state_mapping = {
    'NEW ENGLAND': ['CT', 'ME', 'MA', 'NH', 'RI', 'VT'],
    'MIDDLE ATLANTIC': ['DE', 'MD', 'NJ', 'NY', 'PA'],
    'SOUTH': ['AL', 'AR', 'FL', 'GA', 'KY', 'LA', 'MS', 'MO', 'NC', 'SC', 'TN',
              'VA', 'WV'],
    'MIDWEST': [
        'IL', 'IN', 'IA', 'KS', 'MI', 'MN', 'NE', 'ND', 'OH', 'SD', 'WI'],
    'SOUTHWEST': ['AZ', 'NM', 'OK', 'TX'],
    'WEST': ['AK', 'CA', 'CO', 'HI', 'ID', 'MT', 'NV', 'OR', 'UT', 'WA', 'WY']}


def map_region_to_state(val):
    for region, states in region_state_mapping.items():
        if val in states:
            return region
