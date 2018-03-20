
from rpy2.robjects.lib import (grdevices, ggplot2)
from . import broom


def lm_fit_diagnostic(model, principal_predictor, target_var):
    df = broom.augment(model)
    plt_scatter = (
        ggplot2.ggplot(df) +
        ggplot2.geom_point(ggplot2.aes_string(x=principal_predictor, y=target_var), alpha=0.3) +
        ggplot2.geom_point(ggplot2.aes_string(x=principal_predictor, y="fitted"), alpha=0.3, color='steelblue') +
        ggplot2.theme_bw()
    )
    predicted_vs_actual = (
        ggplot2.ggplot(df) +
        ggplot2.geom_point(ggplot2.aes_string(x="fitted", y=target_var), alpha=0.3) +
        ggplot2.geom_abline(intercept=0, slope=1, color='steelblue') +
        ggplot2.theme_bw()
    )
    resid_qq = (
        ggplot2.ggplot(df) +
        ggplot2.stat_qq(ggplot2.aes_string(sample="resid_normed"), alpha=0.3) +
        ggplot2.geom_abline(slope=1, color="black") +
        ggplot2.theme_bw()
    )
    resid_vs_fitted = (
        ggplot2.ggplot(df) +
        ggplot2.geom_point(ggplot2.aes_string(x="fitted", y="resid"), alpha=0.3) +
        ggplot2.geom_hline(yintercept=0, color="steelblue") +
        ggplot2.theme_bw()
    )        
    return [plt_scatter, predicted_vs_actual, resid_qq, resid_vs_fitted]
