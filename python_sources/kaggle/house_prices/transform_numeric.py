
import pandas as pd


def calc_tran_config_step1(df, trans):
    df = pd.melt(
        frame=df, 
        var_name='var', 
        value_name='x'
    )
    for tran_name, tran_fn in trans.items():
        df[tran_name] = tran_fn(df['x'])
    return df