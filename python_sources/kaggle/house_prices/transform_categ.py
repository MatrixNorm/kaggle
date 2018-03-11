
import pandas as pd


def rating_transform(dataset, columns, ratings):
    default_rating = ratings[ratings['var'].isna()]['rating'].iloc[0]

    step1 = pd.melt(
        frame=dataset[columns], 
        var_name='var', 
        value_name='value'
    )
    step2 = (
        step1
        .join(
            ratings.set_index(['var', 'value']), 
            on=['var', 'value'], 
            how='left'
        )
        .fillna({'rating': default_rating})
    )
    step3 = (
        step2
        [['var', 'rating']]
        .assign(
            id=lambda df: df.groupby('var').cumcount()
        )
        .set_index(['var', 'id'])
        .unstack(0)
    )
    step3.columns = step3.columns.get_level_values(1)
    step3.columns.name = None
    step3.index.name = None
    step3 = step3.reset_index(drop=True)
    return pd.concat([step3, dataset.drop(columns=columns)], axis=1)
