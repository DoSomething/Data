import pandas as pd
from config.mySQLConfig import *
import logging
import sys

credentials = "/Users/shasan/Documents/quasar_login.txt"

uq1 = 'SELECT count(*) FROM quasar.users'
uq2 = 'SELECT count(distinct u.northstar_id) FROM quasar.users u'
bad_query = 'SELECT count(*) from quasar.campaigns'

queries = [bad_query,uq1,uq2]

class QuasarException(Exception):

    def __init__(self, message):
        """Log errors with formatted messaging."""
        logging.error("ERROR: {0}".format(message))
        pass

def return_value(query):
    try:
        value = run_query(query, credentials)
        out = int(value.iloc[0])
        return out
    except:
        out = str(QuasarException(sys.exc_info()[0]))
        return out

def user_monitoring(queries):
    values = []
    for query in queries:
        value = return_value(query)
        values.append(value)
    return values


user_monitoring(queries)

