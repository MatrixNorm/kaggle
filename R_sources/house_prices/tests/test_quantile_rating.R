setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

devnull <- within(list(), {
    
    library(dplyr, warn.conflicts=FALSE)
    library(purrr, warn.conflicts=FALSE)
    library(tidyr, warn.conflicts=FALSE)
    library(testthat, warn.conflicts=FALSE)
    
    quantile_rating <- source('./quantile_rating.R', local = TRUE)$value
    
    test_that("calc_quantiles: default probs", {
        sample <- c(1, 2, 3, 4, 5, 6)
        actual <- quantile_rating$calc_quantiles(sample)
        expect_true(actual[[1]] >= 2 & actual[[1]] < 3)
        expect_true(actual[[2]] >= 3 & actual[[2]] < 4)
        expect_true(actual[[3]] >= 4 & actual[[3]] < 5)
    })
    
    test_that("calc_quantiles: default probs #2", {
        sample <- c(1, 2, 3, 4, 5, 6)
        q1 <- quantile_rating$calc_quantiles(sample)
        q2 <- quantile_rating$calc_quantiles(sample, c(.25, .5, .75))
        expect_equal(q1, q2)
    })
    
    test_that("calc_quantiles: default probs", {
        sample <- c(1, 2, 3, 4, 5, 6)
        actual <- quantile_rating$calc_quantiles(sample)
        expect_true(actual[[1]] >= 2 & actual[[1]] < 3)
        expect_true(actual[[2]] >= 3 & actual[[2]] < 4)
        expect_true(actual[[3]] >= 4 & actual[[3]] < 5)
    })
    
    test_that("calc_quantiles: NAs are ignored", {
        df <- sample <- c(1, 2, NA, 3, 4, 5, NA, 6)
        actual <- quantile_rating$calc_quantiles(sample, c(.25, .75))
        expect_true(actual[[1]] >= 2 & actual[[1]] < 3)
        expect_true(actual[[2]] >= 4 & actual[[2]] < 5)
    })
    
    test_that("calc_rating_for_sample: some case #1", {
        sample <- c(1, 2, 3, 4, 5, 6, 7, 8)
        actual <- quantile_rating$calc_rating_for_sample(sample, c(2.5, 4.5, 6.5))
        expected <- 0.25*(1+2+3+4)
        expect_equal(actual, expected)
    })
    
    test_that("calc_rating_for_sample: NAs are ignored", {
        sample <- c(1, 2, 3, NA, 4, 5, 6, NA, 7, 8)
        actual <- quantile_rating$calc_rating_for_sample(sample, c(2.5, 4.5, 6.5))
        expected <- 0.25*(1+2+3+4)
        expect_equal(actual, expected)
    })
    
    test_that("calc_ratings: #1", {
        df <-
            tribble(
                ~cat1, ~cat2, ~Y,
                'a',   'x',   10,
                'a',   'x',   12,
                'b',   'x',   20,
                'b',   'x',   22,
                'c',   'y',   30,
                'c',   'y',   32,
                'd',   'y',   40,
                'd',   'y',   42
            )
        rq <- quantile_rating$calc_quantiles(df[['Y']], c(.25, .5, .75))
        actual <- quantile_rating$calc_ratings(df, Y, rq, c('cat1', 'cat2'))
        expected <-
            tribble(
                ~var,   ~value, ~rating,
                'cat1', 'a',    1,
                'cat1', 'b',    2,
                'cat1', 'c',    3,
                'cat1', 'd',    4,
                'cat2', 'x',    0.5*(1+2),
                'cat2', 'y',    0.5*(3+4),
                NA,     NA,     0.25*(1+2+3+4)
            )
        expect_equal(actual, expected)
    })
})