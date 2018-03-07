
from . import missing_tools as mtools
from .helpers import get_character_colnames, get_numeric_colnames


colums_with_valid_na = [
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
]


def fix_all(df):
    df = replace_with_most_common(df)
    df = fix_valid(df)
    df = replace_with_zero(df)
    return df


def replace_with_zero(df):
    columns = set(get_numeric_colnames(df)) - set(['SalePrice'])
    return mtools.replace_with_zero(df, list(columns))


def fix_valid(df):
    columns = (
        set(get_character_colnames(df)) & set(colums_with_valid_na)
    )
    return mtools.replace_with_value(df, '_none_', list(columns))


def replace_with_most_common(df):
    columns = (
        set(get_character_colnames(df)) - set(colums_with_valid_na)
    )
    return mtools.replace_with_most_common(df, list(columns))
