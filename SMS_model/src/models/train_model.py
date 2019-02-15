import click
import logging
import os
import pickle
from dotenv import find_dotenv, load_dotenv
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.model_selection import GridSearchCV

import model_params


def _write_model(model, model_type):
    files = os.listdir('models')
    fnames = [f for f in files if f.endswith('pkl') and model_type in f]
    if len(fnames):
        model_num = [int(f.split('-')[-1].split('.')[0]) for f in fnames]
        version = max(model_num) + 1
    else:
        version = 1
    fpath = 'models/{}-{}.pkl'.format(
        model_type, version)
    pickle.dump(model, open(fpath, 'wb'))

    return fpath


@click.command()
@click.argument('train_file', type=click.Path(exists=True))
@click.argument('model_type')
@click.option('--grid-search', default=None)
def main(train_file, model_type, grid_search):
    logger = logging.getLogger()

    df_train = pd.read_csv(
        train_file,
        compression='gzip',
        parse_dates=['most_recent_signup', 'most_recent_post'],
        infer_datetime_format=True,
    )

    X_train = df_train.drop(columns=['target'])
    y_train = df_train['target']

    pipeline = getattr(model_params, 'feature_pipeline')
    model = getattr(model_params, model_type)

    pipeline.steps[-1] = ('clf', model)

    if grid_search is not None:
        logger.info(' grid searching model {}'.format(model_type))
        gs_params = getattr(model_params, grid_search)

        cv = GridSearchCV(
            pipeline, gs_params, 'f1', cv=3, verbose=3, n_jobs=-1,
            return_train_score=True)
        cv.fit(X_train, y_train)

        results = cv.cv_results_
        for index in reversed(np.argsort(results['mean_test_score'])):
            print(results['params'][index])
            print("mean_train_score: {:.5f}, mean_test_score: {:.5f}".format(
                results['mean_train_score'][index],
                results['mean_test_score'][index],
                ))
            print("std_train_score:  {:.5f}, std_test_score:  {:.5f}".format(
                results['std_train_score'][index],
                results['std_test_score'][index],
                ))

    else:
        logger.info(' fitting model {}'.format(model_type))
        pipeline.fit_transform(X_train, y_train)

        fit_model = pipeline.steps[-1][1]
        fpath = _write_model(fit_model, model_type)
        logger.info(' wrote model to {}'.format(fpath))


if __name__ == '__main__':
    log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logging.basicConfig(level=logging.INFO, format=log_fmt)

    # not used in this stub but often useful for finding various files
    project_dir = Path(__file__).resolve().parents[2]

    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())

    main()
