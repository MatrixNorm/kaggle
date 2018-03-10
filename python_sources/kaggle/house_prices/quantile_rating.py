
import pandas as pd

from statsmodels.distributions.empirical_distribution import ECDF


def calc_ratings(df, categ_vars, target_var):
    """ XXX """
    global_quantiles = calc_quantiles(df)
    df = (
        df[categ_vars + [target_var]]
        .dropna(subset=[target_var])
    )

    return (
        pd.melt(
            frame=df,
            id_vars=['price_log'],
            var_name='var', 
            value_name='value'
        )
        .groupby(['var', 'value'])
        .apply(
            lambda df: calc_rating_for_sample(
                df['price_log'], global_quantiles)
        )
        .to_frame('rating')
        .reset_index()
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
    cdf_points = [0] + [cdf(q) for q in rating_quantiles.tolist()] + [1]
    probs = [pair[0] - pair[1] for pair in zip(cdf_points[1:], cdf_points[:-1])]
    rating = sum((i+1)*p for i, p in enumerate(probs))
    return rating
