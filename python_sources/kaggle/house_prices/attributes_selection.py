
import numpy as np


def entropy(col):
    freqs = col.value_counts(dropna=False) / len(col)
    return -np.sum(freqs * np.log2(freqs))


def arrange_vars_by_entropy(df):
    return (
        df
        .apply(entropy)
        .sort_values()
        .to_frame('entropy')
        .reset_index()
        .rename(columns={"index": "var"})
    )
