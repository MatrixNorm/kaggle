
import pandas as pd


def calc_ratings(df, categ_vars, target_var):
    """ XXX """
    df = (
        df[categ_vars + [target_var]]
        .dropna(subset=[target_var])
    )

    pd.melt(
        frame=df,
        id_vars=[target_var],
        var_name='var', 
        value_name='value'
    )
    .groupby(['var', 'value'])