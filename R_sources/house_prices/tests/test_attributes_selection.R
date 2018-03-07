setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

devnull <- within(list(), {
    
    library(dplyr, warn.conflicts=FALSE)
    library(purrr, warn.conflicts=FALSE)
    library(tidyr, warn.conflicts=FALSE)
    library(testthat, warn.conflicts=FALSE)
    
    attr_sel <- source('./attributes_selection.R', local = TRUE)$value
    
    test_that("constant distribution has zero entropy", {
        expect_equal(attr_sel$entropy$entropy(c(1, 1, 1, 1)), 0)
    })
    
    test_that("fair coin has entropy of 1", {
        expect_equal(attr_sel$entropy$entropy(c(0, 1)), 1)
    })
    
    test_that("arrange by entropy # 1", {
        df <- data_frame(
            x = c('a', 'a', 'a', 'a'),
            y = c('a', 'a', 'b', 'b')
        )
        expected <- data_frame(
            var = c('x', 'y'),
            entropy = c(0, 1)
        )
        actual <- attr_sel$entropy$arrange_vars(df)
        expect_equal(expected, actual)
    })
})