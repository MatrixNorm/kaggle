setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

devnull <- within(list(), {
    
    library(dplyr, warn.conflicts=FALSE)
    library(purrr, warn.conflicts=FALSE)
    library(tidyr, warn.conflicts=FALSE)
    library(testthat, warn.conflicts=FALSE)
    
    quantile_rating <- source('./categ2numeric/quantile_rating.R', local = TRUE)$value
    
    test_that("calc_global_quantiles: #1", {
        df <- data_frame(
            Y = c(10, 12, 20, 22, 30, 32, 40, 42),
            cat1 = c('a', 'a', 'b', 'b', 'c', 'c', 'd', 'd')
        )
        actual <- quantile_rating$calc_global_quantiles(df, Y)
        expect_true(actual$q25 > 12 & actual$q25 < 20)
        expect_true(actual$q50 > 20 & actual$q50 < 30)
        expect_true(actual$q75 > 32 & actual$q75 < 40)
    })
    
    test_that("calc_global_quantiles: NAs are ignored", {
        df <- data_frame(
            Y = c(10, 12, NA, 20, 22, 30, NA, 32, 40, 42),
            cat1 = c('a', 'a', 'a', 'b', 'b', 'c', 'c', 'c', 'd', 'd')
        )
        actual <- quantile_rating$calc_global_quantiles(df, Y)
        expect_true(actual$q25 > 12 & actual$q25 < 20)
        expect_true(actual$q50 > 20 & actual$q50 < 30)
        expect_true(actual$q75 > 32 & actual$q75 < 40)
    })
    
    test_that("calc_rating: #1", {
        calc_rating <- quantile_rating$calc_rating
        global_quantiles <- list(q25=15, q50=25, q75=35)
        expect_equal(calc_rating(c(10, 12), global_quantiles), 1)
        expect_equal(calc_rating(c(16, 20), global_quantiles), 2)
        expect_equal(calc_rating(c(26, 30), global_quantiles), 3)
        expect_equal(calc_rating(c(36, 40), global_quantiles), 4)
        expect_equal(calc_rating(c(15, 16), global_quantiles), 1.5)
        expect_equal(calc_rating(c(15, 16, 17), global_quantiles), 1 * (1/3) + 2 * (2/3))
    })
    
    test_that("calc_rating_for_selected: #1", {
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
        actual <- quantile_rating$calc_rating_for_selected(df, c('cat1', 'cat2'), Y)
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