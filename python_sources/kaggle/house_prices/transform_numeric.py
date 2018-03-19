
import numpy as np
import pandas as pd
import scipy.stats as stats
import statsmodels.formula.api as smf


class calc_tran_config:
    @staticmethod
    def step1(df, trans):
        df = pd.melt(
            frame=df, 
            var_name='var', 
            value_name='x'
        )
        for tran_name, tran_fn in trans.items():
            df[tran_name] = tran_fn(df['x'])
        return df

    @staticmethod
    def step2(df):
        return (
            pd.melt(
                frame=df,
                id_vars=['var'],
                var_name='tran', 
                value_name='value'
            )
            .assign(
                value_normed=lambda df: (
                    df
                    .groupby(['var', 'tran'])
                    ['value']
                    .transform(lambda x: (x - x.mean()) / x.std())
                )
            )
        )

    @staticmethod
    def step3(df):
        return (
            df
            .groupby(['var', 'tran', 'value_normed'])
            .size()
            .to_frame('k')
            .sort_index()
            .reset_index()
        )

    @staticmethod
    def step4(df):
        df = df.assign(
            sum=lambda df: (
                df.groupby(['var', 'tran'])['k'].transform('sum')
            ),
            cumsum=lambda df: (
                df.groupby(['var', 'tran'])['k'].cumsum()
            )
        )
        df = df.assign(
            theoretical=lambda df: (
                df.groupby(['var', 'tran'])
                ['value_normed']
                .transform(stats.norm.cdf)
            ),
            empirical=lambda df: df['cumsum'] / df['sum']
        )
        df = df.assign(
            diff_L2=lambda df: (
                df['k']*(df['empirical'] - df['theoretical'])**2
            )
        )
        return (
            df
            .groupby(['var', 'tran'])
            ['diff_L2']
            .agg('sum')
            .to_frame()
        )

    @staticmethod
    def step5(df):
        df = (
            df
            .groupby('var')
            .apply(calc_tran_config._step5_helper)
            .sort_values('progress_score', ascending=False)
        )
        df.index = df.index.droplevel(1)
        return df.reset_index()

    @staticmethod
    def _step5_helper(gr):
        gr = gr.sort_values('diff_L2')
        gr.index = gr.index.droplevel()
        best = gr.iloc[0][0]
        vanilla = gr.loc['x'][0]
        progress_score = 100 * (vanilla - best) / vanilla
        tran_name = gr.index[0]
        if tran_name != 'x':
            return pd.DataFrame({
                'tran': [tran_name], 
                'progress_score': [progress_score]
            })

    @staticmethod
    def step6(df, trans):
        return df.assign(
            tran_fn=lambda df: [trans[x] for x in df['tran']]
        )


def get_transformation_config(df, trans):
    return (
        df
        .select_dtypes(include=[np.number]) 
        .pipe(calc_tran_config.step1, trans=trans)
        .pipe(calc_tran_config.step2)
        .pipe(calc_tran_config.step3)
        .pipe(calc_tran_config.step4)
        .pipe(calc_tran_config.step5)
        .pipe(calc_tran_config.step6, trans=trans)
    )


def filter_tran_config_by_r2(config, dataset, target_var):
    train = (
        dataset
        [list(set(config['var']) | set([target_var]))]
        .dropna(subset=[target_var])
    )

    def calc_r2_x(row):
        data = train[[row['var'], target_var]]
        return (
            smf.ols(
                formula='%s ~ %s' % (row['var'], target_var), data=data
            ).fit().rsquared
        )         

    def calc_r2_tran(row):
        var = row['var']
        fn = row['tran_fn']
        data = (
            train
            [[var, target_var]]
            .assign(**{
                var: lambda df: fn(df[var])
            })
        )
        return (
            smf.ols(
                formula='%s ~ %s' % (row['var'], target_var), data=data
            ).fit().rsquared
        )

    return (
        config
        .assign(r2_x=lambda df: df.apply(calc_r2_x, axis=1))
        .assign(r2_tran=lambda df: df.apply(calc_r2_tran, axis=1))
        .query('r2_tran > r2_x')
    )


def apply_transform(df, tran_config):
    for index, row in tran_config[['var', 'tran_fn']].iterrows():
        var_name = row['var']
        tran_fn = row['tran_fn']
        try:
            df[var_name] = tran_fn(df[var_name])
        except KeyError:
            pass
    return df
