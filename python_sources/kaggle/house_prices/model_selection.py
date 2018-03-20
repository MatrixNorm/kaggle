
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf


def find_initial_best_r2_predictor(data, target_var, r2_discard_level=0.05):
    predictors = list(set(data.columns.tolist()) - set([target_var]))
    formulas = ["%s ~ %s" % (target_var, pred) for pred in predictors]
    r2s = [smf.ols(formula=f, data=data).fit().rsquared for f in formulas]
    return pd.DataFrame({
        'predictor': predictors,
        'formula': formulas,
        'r2': r2s,
    }).sort_values('r2', ascending=False).reset_index(drop=True)


def find_next_best_r2_predictor(data, base_formula, predictors, 
                                r2_gain_discard_level=0.5, a_max=0, a_avg=0):
    """Docs"""
    base_predictors = [_.strip() for _ in 
                       base_formula.rpartition('~')[-1].split('+')]
    base_r2 = smf.ols(formula=base_formula, data=data).fit().rsquared
    predictors = list(set(predictors) - set(base_predictors))
    formulas = ["%s + %s" % (base_formula, p) for p in predictors]
    r2s = [smf.ols(formula=f, data=data).fit().rsquared for f in formulas]
    return (
        pd.DataFrame({
            'predictor': predictors,
            'formula': formulas,
            'r2': r2s,
            'base_r2': base_r2
        })
        .assign(
            r2_gain=lambda df: 100 * (df['r2'] - df['base_r2']) / df['base_r2'],
            cors=lambda df: df['predictor'].apply(
                lambda p: data[base_predictors].corrwith(data[p]).tolist()
            )
        )
        .assign(
            cor_abs_max=lambda df: df['cors'].apply(
                lambda cors: np.max(np.abs(cors))
            ),
            cor_abs_avg=lambda df: df['cors'].apply(
                lambda cors: np.mean(np.abs(cors))
            )
        )
        .assign(
            r2_gain_adj=lambda df: (
                df['r2_gain'] / (1 + a_max*df['cor_abs_max'] +
                                 a_avg*df['cor_abs_avg'])
            )
        )
        .drop(columns=['cors'])
        .query("r2_gain > %s" % r2_gain_discard_level)
        .sort_values('r2_gain_adj', ascending=False)
        .reset_index(drop=True)
    )


def greedy_r2_gain_adj(data, target_var, r2_gain_discard_level=0.5,
                       r2_discard_level=0.02, a_max=0, a_avg=0):

    init = find_initial_best_r2_predictor(
        data=data,
        target_var="price_log", 
        r2_discard_level=0.02
    )
    
    base_formula = init.loc[0, "formula"]
    predictors = init['predictor']
    step = 1
    report = init.assign(step=1)

    while True:
        result = find_next_best_r2_predictor(
            data=data, 
            base_formula=base_formula, 
            predictors=predictors,
            r2_gain_discard_level=r2_gain_discard_level,
            a_max=a_max,
            a_avg=a_avg
        )
        if len(result) == 0:
            break

        base_formula = result.loc[0, "formula"]
        predictors = result['predictor']
        step += 1
        report = report.append(result.assign(step=step))
        
        if len(result) == 1:
            break
    return report[['predictor', 'formula', 'r2', 'step', 
                   'base_r2', 'r2_gain', 'cor_abs_max', 
                   'cor_abs_avg', 'r2_gain_adj']]
