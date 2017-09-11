import pandas as pd
from config.mySQLConfig import *
import logging
import sys
import datetime

credentials = "/Users/shasan/Documents/quasar_login.txt"

user_queries = {
    'user_count': 'SELECT count(*) FROM quasar.users',
    'user_user_count': 'SELECT count(distinct u.northstar_id) FROM quasar.users u',
    'ca_table_count': 'SELECT count(*) FROM quasar.campaign_activity c',
    'ca_post_count': 'SELECT count(distinct c.post_id) FROM quasar.campaign_activity c'
}


class QuasarException(Exception):
    def __init__(self, message):
        """Log errors with formatted messaging."""
        logging.error("ERROR: {0}".format(message))
        pass


def construct_query_dict(description, query, query_set=None):
    if query_set == None:
        query_set = {}

    query_set[description] = query

    return query_set


def get_value(query):
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
        value = get_value(query)
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


def extract_latest_value(table, desc):
    max_query = \
        "SELECT  \
            m.output  \
        FROM quasar.monitoring m  \
        INNER JOIN ( \
        SELECT \
            t.table, \
            t.query,  \
            max(t.timestamp) AS max_created \
        FROM quasar.monitoring t \
        GROUP BY t.table, t.query \
            ) m ON m.max_created = u.timestamp \
        WHERE m.table = '" + table + "'  \
        AND m.query = '" + desc + "'"
    value = get_value(max_query)
    return value

def extract_second_latest_value(table, desc):
    max_2_query = \
        "SELECT \
            m.output \
        FROM quasar.monitoring m \
        INNER JOIN \
            (SELECT \
                max(t.timestamp) AS ts_2 \
            FROM quasar.monitoring t \
            WHERE t.table = '" + table + "' \
            AND t.query = '" + desc + "' \
            AND \
            t.timestamp < (SELECT max(t1.timestamp)  \
                            FROM quasar.monitoring t1 \
                            WHERE t1.table = '" + table + "' AND t1.query = '" + desc + "') \
            ) ts2 ON ts2.ts_2 = u.timestamp \
        WHERE t1.table = '" + table + "' AND t1.query = '" + desc + "'"
    # max_2_query = "select count(*) from quasar.users"
    value = get_value(max_2_query)
    return value

extract_second_latest_value('quasar.users', 'user_count')

test = compile_statuses(user_queries)
