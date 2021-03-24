import pandas as pd
from sklearn.base import BaseEstimator, TransformerMixin

# from . import utils as u
import utils as u

pd.options.mode.chained_assignment = None


class ColumnSelector(BaseEstimator, TransformerMixin):
    def __init__(self, cols):
        self.cols = cols

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        assert isinstance(X, pd.DataFrame)

        try:
            return X[self.cols]
        except KeyError:
            cols_error = list(set(self.cols) - set(X.columns))
            raise KeyError(
                "The DataFrame does not include the columns: %s" % cols_error)


class ColumnExcluder(BaseEstimator, TransformerMixin):
    def __init__(self, cols):
        self.cols = cols

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        assert isinstance(X, pd.DataFrame)

        try:
            return X.drop(columns=self.cols)
        except KeyError:
            cols_error = list(set(self.cols) - set(X.columns))
            raise KeyError(
                "The DataFrame does not include the cols: %s" % cols_error)


class ActivityLevel(BaseEstimator, TransformerMixin):
    def __init__(
        self,
        last_active,
        col='activity_level',
    ):
        self.last_active = last_active
        self.col = col

    def fit(self, X, y=None):
        return self

    def transform(self, X, y=None):
        X[self.col] = X.apply(
            u.find_activity_level,
            last_active=self.last_active,
            axis=1)

        return X


class MapRegion(BaseEstimator, TransformerMixin):
    def __init__(self, col='state'):
        self.col = col

    def fit(self, X, y=None):
        return self

    def transform(self, X, y=None):
        X['region'] = X['state'].apply(u.map_region_to_state)

        return X


class GetDummies(BaseEstimator, TransformerMixin):
    def __init__(self, dummy_cols, cols_to_drop):
        self.dummy_cols = dummy_cols
        self.cols_to_drop = cols_to_drop

    def fit(self, X, y=None):
        return self

    def transform(self, X, y=None):
        for col in self.dummy_cols:
            dummies_ser = pd.get_dummies(
                X.set_index('northstar_id')[col], prefix=col)
            X = pd.merge(
                X, dummies_ser, on='northstar_id')

        X = X.drop(columns=self.cols_to_drop)

        return X
