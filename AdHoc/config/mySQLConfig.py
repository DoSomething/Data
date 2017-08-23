from sqlalchemy import create_engine


def db_connect(path):
    host = 'quasar-slave-new.c9ajz690mens.us-east-1.rds.amazonaws.com'
    database = 'quasar'
    login = open(path).read()
    user_name = login.split(':')[0]
    password = login.split(':')[1]

    engine = create_engine('mysql+pymysql://' + user_name + ':' + password + '@' + host + '/' + database)
    return engine


def run_query(query, credentials):
    import pandas as pd
    engine = db_connect(credentials)

    if '.sql' in query:
        q = open(query, 'r').read()
    else:
        q = query
    df = pd.read_sql_query(q, engine)
    return df
