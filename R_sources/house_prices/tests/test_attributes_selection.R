
setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

library(testthat, warn.conflicts=FALSE)

shutup <- within(list(), { 
    source('./attributes_selection.R', local = TRUE)
    
    test_that("constant distribution has zero entropy", {
        expect_equal(attributes_selection$entropy$entropy(c(1, 1, 1, 1)), 0)
    })
    
    test_that("fair coin has entropy of 1", {
        expect_equal(attributes_selection$entropy$entropy(c(0, 1)), 1)
    })
})