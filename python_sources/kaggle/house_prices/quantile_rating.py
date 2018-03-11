
import numpy as np
import pandas as pd

from statsmodels.distributions.empirical_distribution import ECDF


def calc_ratings(df, target_var, rating_quantiles, categ_vars=None):
    """ XXX """
    if categ_vars is None:
        categ_vars = [
            col[1].name for col in df.items() if col[1].dtype.kind == 'O'
        ]

    df = (
        df[categ_vars + [target_var]]
        .dropna(subset=[target_var])
    )

    return (
        pd.melt(
            frame=df,
            id_vars=[target_var],
            var_name='var', 
            value_name='value'
        )
        .groupby(['var', 'value'])
        .apply(
            lambda df: calc_rating_for_sample(
                df[target_var], rating_quantiles)
        )
        .to_frame('rating')
        .reset_index()
        .append(
            {
                'var': np.NAN, 
                'value': np.NAN, 
                'rating': calc_default_rating(rating_quantiles)
            },
            ignore_index=True
        )
    )


def calc_quantiles(sample, probs=None):
    if probs is None:
        probs = [.25, .5, .75]
    return (
        sample
        .dropna()
        .quantile(probs)
    )


def calc_rating_for_sample(sample, rating_quantiles):
    sample = sample.dropna()
    cdf = ECDF(sample)
    cdf_points = [0] + [cdf(q) for q in rating_quantiles] + [1]
    probs = [pair[0] - pair[1] for pair in zip(cdf_points[1:], cdf_points[:-1])]
    rating = sum((i+1)*p for i, p in enumerate(probs))
    return rating


def calc_default_rating(rating_quantiles):
        num_of_levels = len(rating_quantiles) + 1
        return sum(range(1, num_of_levels + 1)) / num_of_levels
