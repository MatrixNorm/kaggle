
import pandas as pd


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
    return df.fillna(dict((c, value) for c in columns))


def replace_with_zero(df, columns=None):
    return replace_with_value(df, 0, columns=columns)
