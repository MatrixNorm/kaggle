
import pandas as pd
from .helpers import get_character_colnames, get_numeric_colnames


colums_with_valid_na = (
    'Alley', 
    'BsmtCond', 
    'BsmtExposure', 
    'BsmtFinType1', 
    'BsmtFinType2', 
    'BsmtQual',
    'Fence',
    'FireplaceQu',
    'GarageCond',
    'GarageFinish',
    'GarageQual',
    'GarageType',
    'MasVnrType',
    'MiscFeature',
    'PoolQC',
    'Utilities'
)


def replace_with_most_common(df, columns=None):
    if columns is None:
        columns = list(df)
    tmp = (
        pd.melt(
            frame=df[columns],
            var_name='var', 
            value_name='value'
        )
        .dropna()
        .assign(
            dummy=1
        )
        .groupby(['var', 'value'], as_index=False)
        ['dummy']
        .agg('count')
        .assign(
            max_val=lambda df: df.groupby(['var'])['dummy'].transform('max')
        )
        .query('dummy == max_val')
        [['var', 'value']]
    )
    replacement_dict = dict(zip(tmp['var'].tolist(), tmp['value'].tolist()))
    return df.fillna(replacement_dict)


def replace_with_value(df, value, columns=None):
    if columns is None:
        columns = list(df)
    return df.fillna(dict((c, 1) for c in columns))


def replace_with_zero(df, columns=None):
    return


class FixMissing:
    def __init__(self, df):
        self.df = df
    
    def replace_with_zero(self):
        columns = set(get_numeric_colnames(self.df)) - set(['SalePrice'])
        self.df = replace_with_value(self.df, columns)
        return self.df

    def fix_valid(self):
        columns = (set(get_character_colnames(self.df)) & 
                        set(colums_with_valid_na))
        self.df = replace_with_value(self.df, '_none_', columns)
        return self.df

    def replace_with_most_common(self):
        columns = (set(get_character_colnames(self.df)) - 
                        set(colums_with_valid_na))
        self.df = replace_with_most_common(self.df, columns)
        return self.df
