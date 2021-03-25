import pandas as pd
from config.mySQLConfig import *
import logging
import sys
import datetime
from pandas import DataFrame as df

credentials = "/Users/shasan/quasar_login.txt"

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
        this_table = query.split('FROM')[1].split(' ')[1]
        table.append(this_table)

    for description in queries.keys():
        descriptions.append(description)

    out = pd.DataFrame(
        {'query': descriptions,
         'output': values,
         'table': table,
         'timestamp': ts
         })

    return out


def write_to_monitoring_table(table):
    df.to_sql(table, )

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

def compare_latest_values(table, desc):
    # latest_value = extract_latest_value(table, desc)
    # second_latest_value = extract_second_latest_value(table, desc)
    latest_value=12
    second_latest_value=10
    if latest_value > second_latest_value:
        message = 'Pass'
    else:
        message = 'Fail'
    report = table + ' ' + desc + ' ' + message
    return report

extract_second_latest_value('quasar.users', 'user_count')

test = compile_statuses(user_queries)

if a<b == True:
    print('somethings wrong')
else:
    print('its fine')


from slackclient import SlackClient