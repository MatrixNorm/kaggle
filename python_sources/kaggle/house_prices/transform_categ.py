

def rating_transform(dataset, columns, ratings):
    default_rating = ratings[ratings['var'].isna()]['rating'].iloc[0]