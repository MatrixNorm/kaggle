setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

devnull <- within(list(), {
    
    library(dplyr, warn.conflicts=FALSE)
    library(purrr, warn.conflicts=FALSE)
    library(tidyr, warn.conflicts=FALSE)
    library(testthat, warn.conflicts=FALSE)
    
    tran_cat <- source('./transform_categ_vars.R', local = TRUE)$value
    
    test_that("rating_transform_for_selected: #1", {
        df <- 
            tribble(
                ~cat1, ~cat2, ~other,
                'a',   'x',   1,
                'a',   'x',   2,
                'b',   'x',   3,
                'b',   'x',   4,
                'c',   'y',   5,
                'c',   'y',   6,
                'd',   'y',   7,
                'd',   'y',   8
            )
        ratings <- 
            tribble(
                ~var,   ~value, ~rating,
                'cat1', 'a',    1,
                'cat1', 'b',    2,
                'cat1', 'c',    3,
                'cat1', 'd',    4,
                'cat2', 'x',    1.5,
                'cat2', 'y',    3.5,
                NA,     NA,     2.5
            )
        actual <- tran_cat$rating_transform_for_selected(df, c('cat1', 'cat2'), ratings)
        expected <- 
            tribble(
                ~cat1, ~cat2, ~other,
                1,     1.5,   1,
                1,     1.5,   2,
                2,     1.5,   3,
                2,     1.5,   4,
                3,     3.5,   5,
                3,     3.5,   6,
                4,     3.5,   7,
                4,     3.5,   8
            )
        expect_equal(actual, expected)
    })
    
    test_that("rating_transform_for_selected: unseen values are set to global rating", {
        df <- 
            tribble(
                ~cat1, ~cat2, ~other,
                'a',   'x',   1,
                'a',   'z',   2,
                'b',   'x',   3,
                'b',   'x',   4,
                'c',   'y',   5,
                'c',   'y',   6,
                'd',   'y',   7,
                'd',   'y',   8,
                'f',   'z',   9
            )
        ratings <- 
            tribble(
                ~var,   ~value, ~rating,
                'cat1', 'a',    1,
                'cat1', 'b',    2,
                'cat1', 'c',    3,
                'cat1', 'd',    4,
                'cat2', 'x',    1.5,
                'cat2', 'y',    3.5,
                NA,     NA,     2.5
            )
        actual <- tran_cat$rating_transform_for_selected(df, c('cat1', 'cat2'), ratings)
        expected <- 
            tribble(
                ~cat1, ~cat2, ~other,
                1,     1.5,   1,
                1,     2.5,   2,
                2,     1.5,   3,
                2,     1.5,   4,
                3,     3.5,   5,
                3,     3.5,   6,
                4,     3.5,   7,
                4,     3.5,   8,
                2.5,   2.5,   9 
            )
        expect_equal(actual, expected)
    })
})