
import os
import pandas as pd

from numpy import NaN


def load_data():
    data_path = os.path.join(os.environ['DATA_DIR'], 'house_prices')

    training_dataset = pd.read_csv(os.path.join(data_path, 'train.csv'))
    testing_dataset = pd.read_csv(os.path.join(data_path, 'test.csv'))

    training_dataset = training_dataset[training_dataset['SalePrice'].notna()]
    training_dataset['dataSource'] = 'train'
   
    testing_dataset['dataSource'] = 'test'
    testing_dataset['SalePrice'] = NaN

    combined_dataset = training_dataset.append(testing_dataset)
    combined_dataset['MSSubClass'] = combined_dataset['MSSubClass'].astype(str)
    combined_dataset['MoSold'] = combined_dataset['MoSold'].astype(str)
    return combined_dataset


def get_character_colnames(df):
    return [
        col[1].name for col in df.items() if col[1].dtype.kind == 'O' and col[1].name != 'dataSource'
    ]


def get_numeric_colnames(df):
    return [
        col[1].name for col in df.items() if col[1].dtype.kind in ('i', 'f') and col[1].name != 'Id'
    ]
