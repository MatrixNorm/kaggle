
import pandas as pd


def get_categ_colnames(df):
    return [
        col[1].name for col in df.items() if col[1].dtype.kind == 'O'
    ]


def get_numeric_colnames(df):
    # possibly better way
    # https://stackoverflow.com/questions/25039626/how-do-i-find-numeric-columns-in-pandas
    return [
        col[1].name for col in df.items() if col[1].dtype.kind in ('i', 'f')
    ]


def frames_diff(df1, df2):
    ndx = (df1 == df2).apply(lambda col: all(col))
    cols = [name for name, value in ndx.items() if not value]
    t1 = df1[cols].rename(columns={c: c+'.1' for c in cols})
    t2 = df2[cols].rename(columns={c: c+'.2' for c in cols})
    res = pd.concat([t1, t2], axis=1)
    res = res[sorted(res.columns.tolist())]
    return res
