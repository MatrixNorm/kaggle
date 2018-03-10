
import pandas as pd
from kaggle.house_prices import quantile_rating


class TestCalcQuantiles:
    def test_025_05_075(self):
        sample = pd.Series([1, 2, 3, 4, 5, 6])
        actual = quantile_rating.calc_quantiles(sample).tolist()
        print(actual)
        assert 2 <= actual[0] < 3
        assert 3 <= actual[1] < 4
        assert 4 <= actual[2] < 5

    def test_default_probs(self):
        sample = pd.Series([1, 2, 3, 4, 5, 6])
        q1 = quantile_rating.calc_quantiles(sample).tolist()
        q2 = quantile_rating.calc_quantiles(sample, 
                                            probs=[.25, .5, .75]).tolist()
        assert q1 == q2
