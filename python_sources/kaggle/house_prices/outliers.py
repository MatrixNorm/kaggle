

condition = 'GrLivArea > 4000 and SalePrice < 2e5'


def remove_outliers(dataset):
    return dataset.query("not(%s)" % condition).reset_index(drop=True)


def get_strange_cases(dataset):
    return dataset.query(condition).reset_index(drop=True)
