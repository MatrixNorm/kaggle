
import os
import numpy as np
import pandas as pd
from . import utils


class Data:
    @classmethod
    def load(cls):
        cls(load_data())

    def __init__(self, combined_dataset):
        self.combined_dataset = combined_dataset

    @property
    def training_dataset(self):
        return self.combined_dataset.query("dataSource == 'train'")

    @property
    def testing_dataset(self):
        return self.combined_dataset.query("dataSource == 'test'")

    @property
    def non_data_columns(self):
        return ['dataSource', 'Id']


def load_data():
    data_path = os.path.join(os.environ['DATA_DIR'], 'house_prices')

    training_dataset = pd.read_csv(os.path.join(data_path, 'train.csv'))
    testing_dataset = pd.read_csv(os.path.join(data_path, 'test.csv'))

    training_dataset = training_dataset[training_dataset['SalePrice'].notna()]
    training_dataset['dataSource'] = 'train'
   
    testing_dataset['dataSource'] = 'test'
    testing_dataset['SalePrice'] = np.NAN

    combined_dataset = training_dataset.append(testing_dataset)
    combined_dataset['MSSubClass'] = combined_dataset['MSSubClass'].astype(str)
    combined_dataset['MoSold'] = combined_dataset['MoSold'].astype(str)
    return (
        combined_dataset
        .reset_index(drop=True)
        .rename(
            columns={
                col: "X%s" % col for col in
                combined_dataset if col[0].isdigit()
            }
        )
    )


def get_character_colnames(df):
    return [colname for colname in utils.get_categ_colnames(df) if colname != 'dataSource'] 


def get_numeric_colnames(df):

    return [
        col[1].name for col in df.items() if col[1].dtype.kind in ('i', 'f') and col[1].name != 'Id'
    ]
