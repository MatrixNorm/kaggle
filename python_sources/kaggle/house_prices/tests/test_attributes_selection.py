
import pandas as pd
from kaggle.house_prices import attributes_selection as attr_sel


class TestEnropy:
    def test_constant(self):
        assert attr_sel.entropy(pd.Series([1, 1, 1])) == 0

    def test_fair_coin(self):
        assert attr_sel.entropy(pd.Series([0, 1])) == 1

    def test_fair_coin2(self):
        assert attr_sel.entropy(pd.Series([0, 1, 0, 1])) == 1
