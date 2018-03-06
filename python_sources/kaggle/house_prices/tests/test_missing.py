
import numpy as np
import pandas as pd
from kaggle.house_prices import missing


class TestReplaceWithMostCommon:
    def test_NA_should_be_ignored(self):
        df = pd.DataFrame({
            "attr": ['1', '1', np.NAN, '2', np.NAN, np.NAN, '3']
        })
        actual = missing.replace_with_most_common(df)
        expected = pd.DataFrame({
            "attr": ['1', '1', '1', '2', '1', '1', '3']
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )

    def test_by_defaulf_all_columns_are_fixed(self):
        df = pd.DataFrame({
            "attr1": ['1', '1', np.NAN, '2', np.NAN, np.NAN, '3'],
            "attr2": ['x', 'x', 'x',    'y', np.NAN, np.NAN, 'z']
        })
        actual = missing.replace_with_most_common(df)
        expected = pd.DataFrame({
            "attr1": ['1', '1', '1', '2', '1', '1', '3'],
            "attr2": ['x', 'x', 'x', 'y', 'x', 'x', 'z']
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )

    def test_only_provided_columns_are_fixed(self):
        df = pd.DataFrame({
            "attr1": ['1', '1', np.NAN, '2', np.NAN, np.NAN, '3'],
            "attr2": ['x', 'x', 'x',    'y', np.NAN, np.NAN, 'z']
        })
        actual = missing.replace_with_most_common(df, ['attr2'])
        expected = pd.DataFrame({
            "attr1": ['1', '1', np.NAN, '2', np.NAN, np.NAN, '3'],
            "attr2": ['x', 'x', 'x', 'y', 'x', 'x', 'z']
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )