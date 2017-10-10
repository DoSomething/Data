def run_query(query, credentials):
    import pandas as pd
    engine = db_connect(credentials)

    if '.sql' in query:
        q = open(query, 'r').read()
    else:
        q = query
    df = pd.read_sql_query(q, engine)
    return df

query = \
    "SELECT \
       m.query,  \
       m.output  \
    FROM quasar.monitoring m   \
    WHERE m.table = 'quasar.users' \
    AND m.timestamp = (SELECT max(t1.timestamp)  \
                       FROM quasar.monitoring t1 \
                       WHERE t1.query = 'user_count') \
    OR m.timestamp = (SELECT max(t1.timestamp)  \
                       FROM quasar.monitoring t1 \
                       WHERE t1.query = 'user_distinct_user_count')"

credentials = "/Users/shasan/quasar_login.txt"

frame = run_query(query, credentials)

frame.set_index(['query'])
int(frame[frame['query'] == 'user_count']['output'])