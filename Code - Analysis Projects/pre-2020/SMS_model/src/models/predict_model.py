import click
import logging
import operator
import os
import pickle
from dotenv import find_dotenv, load_dotenv
from pathlib import Path
from subprocess import call

import pandas as pd
from sklearn.metrics import classification_report
from sklearn.metrics import f1_score
from sklearn.tree import export_graphviz

import model_params


def _load_model(model_type, model_version=None):
    if model_version:
        fpath = 'models/{}-{}.pkl'.format(model_type, model_version)
    else:
        fnames = os.listdir('models')
        fnames = [f for f in fnames if model_type in f]
        model_num = max([
            int(f.split('-')[-1].split('.')[0]) for f in fnames])
        fpath = 'models/{}-{}.pkl'.format(
            model_type, str(model_num))

    return fpath, pickle.load(open(fpath, 'rb'))


def _return_best_trees(X, y, model, num_trees):
    score_dict = {}
    for estimator in model.estimators_:
        y_pred = estimator.predict(X)
        f1 = f1_score(y, y_pred)
        score_dict[estimator] = f1
    return sorted(
        score_dict.items(),
        key=operator.itemgetter(1),
        reverse=True
    )[:num_trees]


def _extract_segment(tree, feature_names):
    left = tree.tree_.children_left
    right = tree.tree_.children_right
    threshold = tree.tree_.threshold
    impurity = tree.tree_.impurity
    features = [feature_names[i] for i in tree.tree_.feature]
    value = tree.tree_.value

    def recurse(left, right, threshold, impurity, features, node, depth=0):
        indent = "  " * depth
        if (threshold[node] != -2):
            pos_prob = value[node][0][1] / (
                value[node][0][1] + value[node][0][0])
            num_samples = int(value[node].sum())
            if depth == 1:
                print(indent, 'Probability of response: ', pos_prob)
                print(indent, 'Num samples: ', num_samples)
            elif depth > 1:
                print(indent, 'Probability of response: ', pos_prob)
                print(indent, 'Num samples: ', num_samples)
            else:
                print(indent, 'Starting samples: ', num_samples)

            if (value[left[node]][0][0] > value[left[node]][0][1]):
                # go right if the left node represents the 'no response' class
                go_right = True
            elif (value[left[node]][0][1] / value[left[node]].sum()) >= (
                    value[right[node]][0][1] / value[right[node]].sum()):
                # go left if the left node has a higher proportion of
                # 'response' class
                go_right = False
            elif (impurity[left[node]] >= impurity[right[node]]):
                # go right if yields greater reduction in impurity
                go_right = True
            else:
                print('Help, not sure what to do here!')
                return

            if go_right:
                sign = ' >= '
            else:
                sign = ' <= '

            print(
                indent,
                "if ( " + features[node] + sign + str(threshold[node]) + " ) {"
            )

            if go_right and right[node] != -1:
                recurse(
                    left, right, threshold, impurity, features, right[node],
                    depth+1)
            elif left[node] != -1:
                recurse(
                    left, right, threshold, impurity, features, left[node],
                    depth+1)
        else:
            pos_prob = (
                value[node][0][1] / (value[node][0][1] + value[node][0][0]))
            print(
                indent, "return ",
                pos_prob, 'with num_samples = ', int(value[node].sum())
            )

    recurse(left, right, threshold, impurity, features, 0)


@click.command()
@click.argument('test_file', type=click.Path(exists=True))
@click.argument('model_type')
@click.option('--model-version', default=False)
def main(test_file, model_type, model_version):
    logger = logging.getLogger()

    df_test = pd.read_csv(
        test_file,
        parse_dates=['most_recent_signup', 'most_recent_post'],
        infer_datetime_format=True,
        compression='gzip'
    )

    X_test = df_test.drop(columns=['target'])
    y_test = df_test['target']

    pipeline = getattr(model_params, 'feature_pipeline')
    X_test = pipeline.transform(X_test)

    fpath, model = _load_model(model_type, model_version)
    logger.info(' loaded model {}'.format(fpath))

    y_pred = model.predict(X_test)

    print(classification_report(y_test, y_pred))

    best_trees = _return_best_trees(X_test, y_test, model, 3)

    try:
        dirname = fpath.split('/')[1].split('.')[0]
        os.makedirs('reports/figures/{}'.format(dirname))
    except:
        pass

    for idx, tree in enumerate(best_trees):
        print('tree', idx, ':',  tree[1])
        fpath_tree = 'reports/figures/{}/tree_{}'.format(
            dirname, idx)
        export_graphviz(tree[0], out_file=fpath_tree + '.dot',
                        feature_names=X_test.columns,
                        class_names=['no_response', 'response'],
                        rounded=True, proportion=True,
                        precision=2, filled=True,
                        max_depth=3)

        call(['dot', '-Tpng', fpath_tree + '.dot',
             '-o', fpath_tree + '.png', '-Gdpi=600'])

        _extract_segment(tree[0], X_test.columns)
        print('\n')


if __name__ == '__main__':
    log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logging.basicConfig(level=logging.INFO, format=log_fmt)

    # not used in this stub but often useful for finding various files
    project_dir = Path(__file__).resolve().parents[2]

    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())

    main()
