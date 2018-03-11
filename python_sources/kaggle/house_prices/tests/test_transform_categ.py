
import numpy as np
import pandas as pd
from kaggle.house_prices import transform_categ


class TestTransformCateg:
    def test_1(self):
        df = pd.DataFrame(
            [
                ('a',  'x',  1.),
                ('a',  'x',  2),
                ('b',  'x',  3),
                ('b',  'x',  4),
                ('c',  'y',  5),
                ('c',  'y',  6),
                ('d',  'y',  7),
                ('d',  'y',  8),
            ],
            columns=['cat1', 'cat2', 'Y']
        )
        ratings = pd.DataFrame(
            [
                ('cat1',  'a',    1.),
                ('cat1',  'b',    2),
                ('cat1',  'c',    3),
                ('cat1',  'd',    4),
                ('cat2',  'x',    1.5),
                ('cat2',  'y',    3.5),
                (np.NAN,  np.NAN, 2.5),
            ],
            columns=['var', 'value', 'rating']
        )
        actual = transform_categ.rating_transform(
            dataset=df, 
            columns=['cat1', 'cat2'], 
            ratings=ratings
        )
        expected = pd.DataFrame(
            [
                (1.,  1.5,  1.),
                (1,  1.5,  2),
                (2,  1.5,  3),
                (2,  1.5,  4),
                (3,  3.5,  5),
                (3,  3.5,  6),
                (4,  3.5,  7),
                (4,  3.5,  8),
            ],
            columns=['cat1', 'cat2', 'Y']
        )
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )
