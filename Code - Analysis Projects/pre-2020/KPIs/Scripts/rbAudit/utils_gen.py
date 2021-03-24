import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import psycopg2
import seaborn as sns
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
        for l in li:
            if l != li[-1]:
                sql_str += "'" + l + "', "
            else:
                sql_str += "'" + l + "'"
    else:
        for l in li:
            if l != li[-1]:
                sql_str += str(int(l)) + ", "
            else:
                sql_str += str(int(l))
    return sql_str
