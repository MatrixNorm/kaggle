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
        ratings <- 
            tribble(
                ~var,   ~value, ~rating,
                'cat1', 'a',    1,
                'cat1', 'b',    2,
                'cat1', 'c',    3,
                'cat1', 'd',    4,
                'cat2', 'x',    1.5,
                'cat2', 'y',    3.5,
                NA,     NA,     0.25*(1+2+3+4)
            )
        actual <- tran_cat$rating_transform_for_selected(df, c('cat1', 'cat2'), ratings)
        expected <- 
            tribble(
                ~cat1, ~cat2, ~Y,
                1,   1.5,   10,
                1,   1.5,   12,
                2,   1.5,   20,
                2,   1.5,   22,
                3,   3.5,   30,
                3,   3.5,   32,
                4,   3.5,   40,
                4,   3.5,   42
            )
        expect_equal(actual, expected)
    })
})