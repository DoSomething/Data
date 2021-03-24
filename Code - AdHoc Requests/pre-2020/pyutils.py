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
