
import numpy as np
import pandas as pd
from kaggle.house_prices import missing_tools


class TestReplaceWithMostCommon:
    def test_NA_should_be_ignored(self):
        df = pd.DataFrame({
            "attr": ['1', '1', np.NAN, '2', np.NAN, np.NAN, '3']
        })
        actual = missing_tools.replace_with_most_common(df)
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
        actual = missing_tools.replace_with_most_common(df)
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
        actual = missing_tools.replace_with_most_common(df, ['attr2'])
        expected = pd.DataFrame({
            "attr1": ['1', '1', np.NAN, '2', np.NAN, np.NAN, '3'],
            "attr2": ['x', 'x', 'x', 'y', 'x', 'x', 'z']
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )


class TestReplaceWithValue:
    def test_generic_case(self):
        df = pd.DataFrame({
            "attr1": ['1', '1',    np.NAN, '2', np.NAN],
            "attr2": ['x', np.NAN, np.NAN, 'y', np.NAN]
        })
        actual = missing_tools.replace_with_value(df, '_none_')
        expected = pd.DataFrame({
            "attr1": ['1', '1',      '_none_', '2', '_none_'],
            "attr2": ['x', '_none_', '_none_', 'y', '_none_']
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )

    def test_only_provided_columns_are_fixed(self):
        df = pd.DataFrame({
            "attr1": ['1', '1',    np.NAN, '2', np.NAN],
            "attr2": ['x', np.NAN, np.NAN, 'y', np.NAN]
        })
        actual = missing_tools.replace_with_value(df, '_none_', ['attr2'])
        expected = pd.DataFrame({
            "attr1": ['1', '1',      np.NAN,   '2', np.NAN],
            "attr2": ['x', '_none_', '_none_', 'y', '_none_']
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )


class TestReplaceWithZero:
    def test_generic_case(self):
        df = pd.DataFrame({
            "attr1": [1, 1,      np.NAN, 2, np.NAN],
            "attr2": [1, np.NAN, np.NAN, 3, np.NAN]
        })
        actual = missing_tools.replace_with_zero(df)
        expected = pd.DataFrame({
            "attr1": [1., 1, 0, 2, 0],
            "attr2": [1., 0, 0, 3, 0]
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )

    def test_only_provided_columns_are_fixed(self):
        df = pd.DataFrame({
            "attr1": [1, 1,      np.NAN, 2, np.NAN],
            "attr2": [1, np.NAN, np.NAN, 3, np.NAN]
        })
        actual = missing_tools.replace_with_zero(df, ['attr1'])
        expected = pd.DataFrame({
            "attr1": [1., 1, 0, 2, 0],
            "attr2": [1, np.NAN, np.NAN, 3, np.NAN]
        })
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )