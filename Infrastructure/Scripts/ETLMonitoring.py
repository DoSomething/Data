import pandas as pd
from config.mySQLConfig import *
import logging
import sys
import datetime

credentials = "/Users/shasan/Documents/quasar_login.txt"

user_queries =  {
    'user_count':'SELECT count(*) FROM quasar.users',
    'user_user_count': 'SELECT count(distinct u.northstar_id) FROM quasar.users u',
    'ca_table_count': 'SELECT count(*) FROM quasar.campaign_activity c',
    'ca_post_count': 'SELECT count(distinct c.post_id) FROM quasar.campaign_activity c'
}

class QuasarException(Exception):

    def __init__(self, message):
        """Log errors with formatted messaging."""
        logging.error("ERROR: {0}".format(message))
        pass

def get_status(query):
    try:
        value = run_query(query, credentials)
        out = int(value.iloc[0])
        return out
    except:
        out = str(QuasarException(sys.exc_info()[0]))
        return out

def compile_statuses(queries):
    values = []
    descriptions = []
    ts = []
    table = []

    for query in queries.values():
        value = get_status(query)
        values.append(value)
        time = datetime.datetime.now().strftime("%m-%d-%y %H:%M:%S")
        ts.append(time)
        thisTable = query.split('FROM')[1].split(' ')[1]
        table.append(thisTable)

    for description in queries.keys():
        descriptions.append(description)

    out = pd.DataFrame(
        {'query': descriptions,
         'output': values,
         'table': table,
         'timestamp': ts
         })
    return out


test = compile_statuses(user_queries)

