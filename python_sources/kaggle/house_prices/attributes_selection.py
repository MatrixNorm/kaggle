
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf


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


def arrange_vars_by_r2(df, target_var):
    return (
        pd.melt(
            frame=df.dropna(subset=[target_var]), 
            id_vars=[target_var], 
            var_name='var', 
            value_name='value'
        )
        .groupby('var')
        .apply(
            lambda df: smf.ols(
                formula='%s ~ value' % target_var, data=df
            ).fit().rsquared
        )
        .sort_values()
        .to_frame('r2')
        .reset_index()
    )


class GroupsSeparation:
    """ XXX """
    def __init__(self, df, target_var):
        self.df = df
        self.target_var = target_var
        self.df_long = pd.melt(
            frame=df.dropna(subset=[target_var]),
            id_vars=[target_var],
            var_name='var', 
            value_name='value'
        )

    def arrange_vars(self):
        precalculated = self.get_precalculated()
        return (
            precalculated
            .assign(additive=lambda df: self.calc_additive(df))
            .groupby(['var'])
            ['additive']
            .sum()
            .sort_values()
            .to_frame('score')
            .reset_index()
        )
   
    def get_precalculated(self):
        global_std = self.df[self.target_var].std()

        summ = (
            self.df_long.groupby(['var', 'value'], as_index=False)
                [self.target_var]
                .agg({
                    'n': np.count_nonzero,
                    'mean': np.mean,
                    'std': lambda vec: self.calc_std(vec, global_std)
                })
                .assign(
                    freq=lambda this: (this.groupby('var')
                                           ['n']
                                           .apply(lambda n: n / np.sum(n)))
                )
                .sort_values(['var', 'mean'])
        )

        grp = summ.groupby(['var'], group_keys=False)
        summ['lead_mean'] = grp.apply(lambda this: this['mean'].shift(-1))
        summ['lead_freq'] = grp.apply(lambda this: this['freq'].shift(-1))
        summ['lead_std'] = grp.apply(lambda this: this['std'].shift(-1))

        return summ

    @staticmethod
    def calc_std(vec, unit_default=0):
        if len(vec) == 0:
            return 0
        if len(vec) == 1:
            return unit_default
        if len(vec) > 1:
            return np.std(vec, ddof=1)
    
    @staticmethod    
    def calc_additive(df):
        up = (df['lead_mean'] - df['mean'])**2
        down = df['std']**2 / df['freq'] + df['lead_std']**2 / df['lead_freq']
        return up / down


class Anova:
    """ XXX """
    def __init__(self, df, target_var):
        self.df = df.dropna(subset=[target_var])
        self.target_var = target_var

    def Q_table(self):
        global_mean = np.mean(self.df[self.target_var])
        return (
            pd.melt(
                frame=self.df,
                id_vars=[self.target_var],
                var_name='var', 
                value_name='value'
            )
            .groupby(['var', 'value'], as_index=False)
            [self.target_var]
            .agg({
                'n': np.count_nonzero,
                'mean': np.mean,
                'Q_within_group': lambda vec: np.sum((vec - np.mean(vec))**2),
                'Q_total': lambda vec: np.sum((vec - global_mean)**2)
            })
            .assign(
                Q_of_group=lambda df: df['n'] * (df['mean'] - global_mean)**2
            )
            .groupby(['var'])
            .apply(lambda grp: pd.Series({
                'num_levels': grp.shape[0],
                'num_observ': np.sum(grp['n']),
                'Q_within_groups': np.sum(grp['Q_within_group']),
                'Q_of_groups': np.sum(grp['Q_of_group']),
                'Q_total': np.sum(grp['Q_total']),
            }))
            .reset_index()
        )

    def arrange_vars(self):
        q_table = self.Q_table()

        def calc_F(df):
            up = (df['Q_of_groups'] * (df['num_observ'] - df['num_levels']))
            down = (df['Q_within_groups'] * (df['num_levels'] - 1))
            return up / down 
        
        return (
            q_table
            .assign(
                F=calc_F
            )
            [['var', 'F']]
            .sort_values('F')
        )

    


def order_factor_by_target(df, factor_var, target_var, fn=np.mean):
    """ XXX """
    factor_ordering = (
        df
        .groupby(factor_var, as_index=False)
        .agg(fn)
        .sort_values(target_var)
        [factor_var]
    )

    return (
        df
        .assign(
            **{factor_var: (
                lambda df: df[factor_var]
                       .astype('category')
                       .cat.reorder_categories(factor_ordering, ordered=True))
        })
    )    
