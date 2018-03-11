
import numpy as np
import pandas as pd
from kaggle.house_prices import quantile_rating


class TestCalcQuantiles:
    def test_default_probs1(self):
        sample = pd.Series([1, 2, 3, 4, 5, 6])
        actual = quantile_rating.calc_quantiles(sample).tolist()
        assert 2 <= actual[0] < 3
        assert 3 <= actual[1] < 4
        assert 4 <= actual[2] < 5

    def test_default_probs2(self):
        sample = pd.Series([1, 2, 3, 4, 5, 6])
        q1 = quantile_rating.calc_quantiles(sample).tolist()
        q2 = quantile_rating.calc_quantiles(
            sample, 
            probs=[.25, .5, .75]
        ).tolist()
        assert q1 == q2

    def test_NAs_are_ignored(self):
        sample = pd.Series([1, 2, np.NAN, 3, 4, np.NAN, 5, 6])
        actual = quantile_rating.calc_quantiles(
            sample, 
            probs=[.25, .75]
        ).tolist()
        assert 2 <= actual[0] < 3
        assert 4 <= actual[1] < 5


class TestCalcRatingForSample:
    def test_1(self):
        sample = pd.Series([1, 2, 3, 4, 5, 6, 7, 8])
        actual = quantile_rating.calc_rating_for_sample(
            sample, 
            rating_quantiles=[2.5, 4.5, 6.5]
        )
        expected = 0.25*(1+2+3+4)
        assert actual == expected

    def test_NAs_are_ignored(self):
        sample = pd.Series([1, 2, 3, np.NAN, 4, 5, 6, np.NAN, 7, 8])
        actual = quantile_rating.calc_rating_for_sample(
            sample, 
            rating_quantiles=[2.5, 4.5, 6.5]
        )
        expected = 0.25*(1+2+3+4)
        assert actual == expected


class TestDefaultRating:
    def test_1(self):
        actual = quantile_rating.calc_default_rating([5, 10, 15])
        expected = 0.25*(1+2+3+4)
        assert actual == expected


class TestCalcRatings:
    def test_1(self):
        df = pd.DataFrame(
            [
                ('a',   'x',   10),
                ('a',   'x',   12),
                ('b',   'x',   20),
                ('b',   'x',   22),
                ('c',   'y',   30),
                ('c',   'y',   32),
                ('d',   'y',   40),
                ('d',   'y',   42)
            ], 
            columns=['cat1', 'cat2', 'Y']
        )
        rq = quantile_rating.calc_quantiles(
            df['Y'], 
            probs=[.25, .5, .75]
        )
        actual = quantile_rating.calc_ratings(
            df=df, 
            categ_vars=['cat1', 'cat2'],
            rating_quantiles=rq,
            target_var='Y',
        )
        expected = pd.DataFrame(
            [
                ('cat1', 'a',    1),
                ('cat1', 'b',    2),
                ('cat1', 'c',    3),
                ('cat1', 'd',    4),
                ('cat2', 'x',    0.5*(1+2)),
                ('cat2', 'y',    0.5*(3+4)),
                (np.NAN, np.NAN, 0.25*(1+2+3+4))
            ],
            columns=['var', 'value', 'rating']
        )
        pd.testing.assert_frame_equal(
            expected.sort_index(axis=1), 
            actual.sort_index(axis=1)
        )
