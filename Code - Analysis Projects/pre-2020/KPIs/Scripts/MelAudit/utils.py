import numpy as np
import os
import pandas as pd
import psycopg2
from collections import OrderedDict


def print_null_vals(df):
    null_vals = []
    for col in df.columns:
        if df[col].dtype == 'object': 
            null_val = sum(df[col].isnull()) + sum(df[col] == '') + sum(df[col] == '0')
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


def load_connection(db_user_env, db_pw_env, db_name_env, db_host_env):
    user = os.environ.get(db_user_env)
    pwd = os.environ.get(db_pw_env)
    db = os.environ.get(db_name_env)
    host = os.environ.get(db_host_env)
    return psycopg2.connect(database=db, user=user, password=pwd, host=host)


def print_monthly_mamcount(df):
    monthly_counts = df.set_index('timestamp').groupby(
        pd.Grouper(freq='M')).agg(
            {'northstar_id': lambda x: len(x.unique())})
    return monthly_counts
