from datetime import datetime as dt
import smtplib
import ssl
import os
from email import encoders
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.base import MIMEBase

import pandas as pd
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


def find_accounts_created(sql, min_date=None, max_date=None):
    '''logic to find the number of accounts created based on sql'''
    conn = load_connection()
    df = pd.read_sql(sql, conn)

    if min_date and max_date:
        df = df[(df['landing_datetime'] < max_date) & (
            df['landing_datetime'] >= min_date)]

    df = df[~(
        df['last_ns'].notnull() & df['created'].isnull())]

    df['created_at'] = pd.to_datetime(df['created_at'], utc=True)

    df_accounts_created = df[df['last_ns'].notnull() & (
        df['created_at'] > df['landing_datetime'])]

    df_grouped_by_week = df_accounts_created.set_index(
        'landing_datetime').groupby(
            pd.Grouper(freq='W-Sun', label='left'))['last_ns'].count()

    df_grouped_by_week.index += pd.Timedelta(days=1)

    return df_grouped_by_week


def create_email_message(fromaddr, toaddr, subj, message):
    msg = MIMEMultipart()
    msg['From'] = fromaddr
    if len(toaddr) > 1:
        msg['To'] = ', '.join(toaddr)
    else:
        msg['To'] = toaddr[0]
    msg['Subject'] = subj
    msg.attach(MIMEText(message, 'plain'))

    return msg


def send_email(
        message,
        fname,
        fname_base,
        username,
        email_app_pw=os.environ['EMAIL_PW'],
        context=ssl.create_default_context()
):
    with smtplib.SMTP_SSL("smtp.gmail.com", 465, context=context) as server:
        server.login(username, email_app_pw)
        attach = open(fname, 'rb')
        part = MIMEBase('application', 'octet-stream')
        part.set_payload(attach.read())
        encoders.encode_base64(part)
        part.add_header(
            'Content-Disposition', "attachment; filename= %s" % fname_base)
        message.attach(part)
        server.send_message(message)


def load_data_and_send_email(sql, path, fromaddr, toaddr, subject):
    ''' sql: used to generate data (sql str);
    path: full path name where data is stored (str);
    fromaddr: email address of sender (str);
    toaddr: email address(es) of recipients (str or list of str);
    subject: email subject line (str)
    '''
    # will be used to generate fname
    today = dt.now().date()
    base_path = os.path.basename(path)

    # generate csv of number of accounts created and save to path
    # for record keeping
    df_summary = find_accounts_created(sql=sql)
    df_summary.to_csv(path.format(today))

    # generate email message
    email_message = create_email_message(
        fromaddr=fromaddr,
        toaddr=toaddr,
        subj=subject.format(today),
        message='Have a great week!'
    )

    # send it!
    send_email(message=email_message,
               fname=path.format(today),
               fname_base=base_path.format(today),
               username=fromaddr)
