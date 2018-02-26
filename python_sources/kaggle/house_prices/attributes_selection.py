
import numpy as np
import pandas as pd


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


class GroupsSeparation:
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
