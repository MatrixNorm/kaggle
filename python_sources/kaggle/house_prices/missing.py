
import pandas as pd

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
            frame=df[['MSZoning', 'BldgType']],
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