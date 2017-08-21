from sqlalchemy import create_engine

def db_connect(path):
    HOST = 'quasar-slave-new.c9ajz690mens.us-east-1.rds.amazonaws.com'
    DATABASE = 'quasar'
    login = open(path).read()
    USER_NAME, PASSWORD = login.split(':')

    engine = create_engine('mysql+pymysql://' + USER_NAME + ':' + PASSWORD + '@' + HOST + '/' + DATABASE)
    return engine