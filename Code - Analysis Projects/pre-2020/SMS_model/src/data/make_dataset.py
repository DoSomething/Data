# -*- coding: utf-8 -*-
import click
import logging
import os
from pathlib import Path
from dotenv import find_dotenv, load_dotenv

import pandas as pd
from sklearn.model_selection import train_test_split

import utils as u


@click.command()
@click.argument('input_filepath', type=click.Path(exists=True))
def main(input_filepath):
    """ Runs data processing scripts to turn raw data from (../raw) into
        cleaned data ready to be analyzed (saved in ../processed).
    """
    logger = logging.getLogger(__name__)
    logger.info('starting final dataset creation from raw data')

    df = pd.read_csv(input_filepath, compression='gzip')

    df.rename(columns={'user_id': 'northstar_id'}, inplace=True)

    path = input_filepath.split('/')[-1].split('.')[0]

    # load or create target variable
    if os.path.exists('data/raw/{}_user_target.csv'.format(path)):
        logger.info('''user target already exists; skipping file creation''')
        df_target = pd.read_csv('data/raw/{}_user_target.csv'.format(path))
    else:
        df_target, df_percent_response = u.build_target(df)
        df_target.to_csv(
            'data/raw/{}_user_target.csv'.format(path), index=False)
        df_percent_response.to_csv(
            'data/raw/{}_user_percent_response.csv'.format(path), index=False)
        logger.info('created target variable')

    user_ids = u.sql_stringify_list(df['northstar_id'].unique())

    # load and preprocess user data
    if os.path.exists('data/processed/{}_user_info_processed.csv.gz'.format(
            path)):
        logger.info('''user_info_processed.csv.gz already exists; skipping file creation''')  # noqa
        df_user_activity = pd.read_csv(
            'data/processed/{}_user_info_processed.csv.gz'.format(path),
            compression='gzip',
        )
    else:
        if os.path.exists('data/raw/{}_user_info.csv.gz'.format(path)):
            df_user_info = pd.read_csv(
                'data/raw/{}_user_info.csv.gz'.format(path),
                compression='gzip',
                parse_dates=['signup_created_at', 'post_created_at'],
                infer_datetime_format=True,
            )
            logger.info('loaded user info data')
        else:
            df_user_info = u.load_data(
                u.user_info_sql, 'data/raw/{}_user_info.csv.gz'.format(path),
                user_ids)
            logger.info('wrote user_info to raw/data folder')

        df_user_activity = u.process_user_data(df_user_info)
        df_user_activity.to_csv(
            'data/processed/{}_user_info_processed.csv.gz'.format(path),
            compression='gzip',
            index=False)
        logger.info(
            'wrote user_info_processed to data/processed folder')

    df_final = pd.merge(df_target, df_user_activity, on='northstar_id')

    #  load and preprocess campaigns data
    if os.path.exists(
            'data/processed/{}_user_campaigns_processed.csv.gz'.format(path)):
        logger.info('''user_campaigns_processed.csv.gz already exists; skipping file creation''')  # noqa

        df_user_campaign = pd.read_csv(
            'data/processed/{}_user_campaigns_processed.csv.gz'.format(path),
            compression='gzip',
        )

        logger.info('loaded user campaign data')
    else:
        if os.path.exists('data/raw/{}_user_campaigns.csv.gz'.format(path)):
            df_user_campaign = pd.read_csv(
                'data/raw/{}_user_campaigns.csv.gz'.format(path),
                compression='gzip')
        else:
            df_user_campaign = u.load_data(
                u.campaign_causes_sql,
                'data/raw/{}_user_campaigns.csv.gz'.format(path),
                user_ids)
            logger.info('wrote user_campaigns to data/raw folder')

        df_user_campaign = u.process_campaign_data(df_user_campaign)
        df_user_campaign.to_csv(
            'data/processed/{}_user_campaigns_processed.csv.gz'.format(path),
            compression='gzip',
            index=False
        )
        logger.info(
            'wrote user_campaigns_processed to data/processed folder')

    # finish combining all datasets
    df_final = pd.merge(
        df_final, df_user_campaign, how='left', on='northstar_id')

    # fill in null values for users that never participated in campaigns
    df_final[df_user_campaign.columns[1:]] = df_final[
        df_user_campaign.columns[1:]].fillna(0)

    # write out uncleaned data
    df_final.to_csv(
        'data/processed/{}_uncleaned.csv.gz'.format(path),
        compression='gzip',
        index=False,
    )

    # clean data
    df_final = u.clean_data(df_final)
    logger.info('cleaned data')

    # write out final dataset
    df_final.to_csv(
        'data/final/{}_cleaned.csv.gz'.format(path),
        compression='gzip', index=False)
    logger.info('wrote cleaned dataset to data/final')

    # split final dataset into train, val, test
    y = df_final['target']
    X = df_final.drop(columns=['target'])

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, stratify=y, random_state=42)

    X_train_sub, X_train_val, y_train_sub, y_train_val = train_test_split(
        X_train, y_train, test_size=0.2, stratify=y_train, random_state=42)

    pd.concat([X_train_sub, y_train_sub], axis=1).to_csv(
        'data/final/{}_train.csv.gz'.format(path),
        compression='gzip', index=False)

    pd.concat([X_train_val, y_train_val], axis=1).to_csv(
        'data/final/{}_val.csv.gz'.format(path),
        compression='gzip', index=False)

    pd.concat([X_test, y_test], axis=1).to_csv(
        'data/final/{}_test.csv.gz'.format(path),
        compression='gzip', index=False)

    logger.info(' writing train, validation, and test sets to data/final')


if __name__ == '__main__':
    log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logging.basicConfig(level=logging.INFO, format=log_fmt)

    # not used in this stub but often useful for finding various files
    project_dir = Path(__file__).resolve().parents[2]

    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())

    main()
