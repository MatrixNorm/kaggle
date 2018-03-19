
from . import helpers
from . import quantile_rating
from . import transform_categ
from . import transform_numeric


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


def get_functional_transformation_config(data, target, trans, threshold=0):
    config = transform_numeric.get_transformation_config(
        df=data.drop(columns=[target]), 
        trans=trans
    ).query('progress_score > %s' % threshold)
    return transform_numeric.filter_tran_config_by_r2(
        config=config, 
        dataset=data, 
        target_var=target
    )


def functional_transform(data, trans_config):
    return transform_numeric.apply_transform(data, trans_config)
