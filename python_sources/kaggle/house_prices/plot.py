
import pandas as pd

from rpy2 import robjects
from rpy2.robjects.lib import (grdevices, ggplot2)
from . import broom


def lm_fit_diagnostic(model, principal_predictor, target_var):
    aug = broom.augment(model)
    plt_scatter = (
        ggplot2.ggplot(aug) +
        ggplot2.geom_point(ggplot2.aes_string(x=principal_predictor, y=target_var), alpha=0.3) +
        ggplot2.geom_point(ggplot2.aes_string(x=principal_predictor, y="fitted"), alpha=0.3, color='steelblue') +
        ggplot2.theme_bw()
    )
    predicted_vs_actual = (
        ggplot2.ggplot(aug) +
        ggplot2.geom_point(ggplot2.aes_string(x="fitted", y=target_var), alpha=0.3) +
        ggplot2.geom_abline(intercept=0, slope=1, color='steelblue') +
        ggplot2.theme_bw()
    )
    resid_qq = (
        ggplot2.ggplot(aug) +
        ggplot2.stat_qq(ggplot2.aes_string(sample="resid_normed"), alpha=0.3) +
        ggplot2.geom_abline(slope=1, color="black") +
        ggplot2.theme_bw()
    )
    resid_vs_fitted = (
        ggplot2.ggplot(aug) +
        ggplot2.geom_point(ggplot2.aes_string(x="fitted", y="resid"), alpha=0.3) +
        ggplot2.geom_hline(yintercept=0, color="steelblue") +
        ggplot2.theme_bw()
    )        
    return [plt_scatter, predicted_vs_actual, resid_qq, resid_vs_fitted]


def lm_resid_vs_predictors(model, ncol, nrow):
    aug = broom.augment(model)
    predictors = [
        f.name() for f in 
        model.model.data.design_info.factor_infos.keys()
    ]
    long = pd.melt(
        frame=aug[predictors + ['resid']],
        id_vars=['resid'],
        var_name='var', 
        value_name='value'
    )
    p = (
        ggplot2.ggplot(long) +
        ggplot2.geom_point(ggplot2.aes_string(x='value', y='resid'), alpha=0.3) +
        ggplot2.geom_hline(yintercept=0, color="steelblue") +
        ggplot2.facet_wrap(robjects.Formula('~var'), ncol=ncol, nrow=nrow, scales="free") + 
        ggplot2.theme_bw()
    )    
    return p