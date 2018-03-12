
from . import helpers
from . import quantile_rating
from . import transform_categ


def calc_ratings(df, target):
    quantiles = quantile_rating.calc_quantiles(
        sample=df[target],
        probs=[0.25, 0.5, 0.75]
    )
    return quantile_rating.calc_ratings(
        df=df, 
        target_var=target, 
        rating_quantiles=quantiles, 
        categ_vars=helpers.get_character_colnames(df)
    )


def rating_transform(df, target):
    return transform_categ.rating_transform(
        dataset=df, 
        columns=helpers.get_character_colnames(df), 
        ratings=calc_ratings(df, target)
    )
