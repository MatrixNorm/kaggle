
import pandas as pd
from kaggle.house_prices import attributes_selection as attr_sel


class TestEnropy:
    def test_constant(self):
        assert attr_sel.entropy(pd.Series([1, 1, 1])) == 0

    def test_fair_coin(self):
        assert attr_sel.entropy(pd.Series([0, 1])) == 1

    def test_fair_coin2(self):
        assert attr_sel.entropy(pd.Series([0, 1, 0, 1])) == 1


class TestArrangeByEntropy:
    def test_1(self):
        df = pd.DataFrame({
            'x': ['a', 'a', 'a', 'a'],
            'y': ['a', 'a', 'b', 'b'],
        })
        expected = pd.DataFrame({
            'var': ['x', 'y'],
            'entropy': [0., 1.]
        })
        actual = attr_sel.arrange_vars_by_entropy(df)
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )
