
import numpy as np
import pandas as pd


def augment(model_results):
    predictors = [
        f.name() for f in 
        model_results.model.data.design_info.factor_infos.keys()
    ]
    df_pred = model_results.model.data.orig_exog[predictors]
    df_target = model_results.model.data.orig_endog
    return (
        pd.concat([df_target, df_pred], axis=1)
        .assign(
            resid=model_results.resid,
            fitted=model_results.fittedvalues
        )
        .assign(
            resid_normed=lambda df: (
                (df['resid'] - np.mean(df['resid'])) / np.std(df['resid'])
            )
        )
    )