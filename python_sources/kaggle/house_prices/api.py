
import numpy as np

from . import helpers
from . import missing
from . import outliers
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


def stage1_transformation(dataset):
    dataset = missing.fix_all(dataset)
    dataset = outliers.remove_outliers(dataset)
    dataset = (
        dataset
        .assign(
            price_log=lambda df: np.log(df['SalePrice'])
        )
        .drop(columns=['Id', 'SalePrice'])
    )
    return dataset[sorted(dataset.columns.tolist())]


def stage2_transformation(dataset_stage1, funcs=None, threshold=20):
        if funcs is None:
            funcs = {
                'log': lambda x: np.log(x + 1),
                'sqrt': lambda x: np.sqrt(x),
                'inv3': lambda x: x**(1/3),
                'inv4': lambda x: x**(1/4)
            }      
        trans_config = get_functional_transformation_config(
            data=dataset_stage1, 
            target="price_log", 
            trans=funcs,
            threshold=threshold
        )        
        dataset_stage2 = functional_transform(dataset_stage1, trans_config)
        return (
            dataset_stage2[sorted(dataset_stage2.columns.tolist())], 
            trans_config
        )


def stage3_transformation(dataset_stage2):
    dataset_stage3 = rating_transform(
        df=dataset_stage2, 
        target="price_log"
    )
    return dataset_stage3[sorted(dataset_stage3.columns.tolist())]
