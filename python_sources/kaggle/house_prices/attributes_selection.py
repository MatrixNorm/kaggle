
import numpy as np


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
            frame=df.dropna(subset=[target_var]),  # .fillna('__missing__'), 
            id_vars=[target_var],
            var_name='var', 
            value_name='value'
        )

    def arrange_vars(self):
        return 1
    
    def get_precalculated(self):
        return 1

    ###############
    
    @staticmethod
    def calc_std(vec, unit_default=0):
        if len(vec) == 0:
            return 0
        if len(vec) == 1:
            return unit_default
        if len(vec) > 1:
            return np.std(vec, ddof=1)
        
    def get_precalculated(self):
        """
            Pandas group by is not a friend with NaN. 
            So all NaNs are replaced with string `__missing__`.
        """
        if self.precalculated:
            return self.precalculated
        global_std = self.categ_data['SalePrice'].std()

        long = self.get_long().groupby(['var', 'value'])

        long = long.agg({
            'SalePrice': [('n',    np.count_nonzero), 
                          ('mean', np.mean), 
                          ('std',  lambda vec: self.calc_std(vec))]}
        )

        long.columns = long.columns.get_level_values(1)

        long = (long
                .assign(freq = long.groupby('var')['n'].apply(lambda n: n / np.sum(n)))
                .reset_index()
                .sort_values(['var', 'mean']))

        grp = long.groupby(['var'], group_keys=False)
        long['lead_mean'] = grp.apply(lambda df: df['mean'].shift(-1))
        long['lead_freq'] = grp.apply(lambda df: df['freq'].shift(-1))
        long['lead_std'] = grp.apply(lambda df: df['std'].shift(-1))
        
        self.precalculated = long
        return self.precalculated
    
    def get_separation(self):
        return (
            self.get_precalculated()
                .assign(additive = self.calc_additive)
                .groupby(['var'], group_keys=False)
                ['additive']
                .sum()
                .sort_values()
                .to_frame('score')
                .reset_index()
        )
    
    @staticmethod    
    def calc_additive(df):
        up = (df['lead_mean'] - df['mean'])**2
        down = df['std']**2 / df['freq'] + df['lead_std']**2 / df['lead_freq']
        return up / down