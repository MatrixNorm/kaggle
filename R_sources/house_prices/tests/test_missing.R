setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

library(dplyr, warn.conflicts=FALSE)
library(purrr, warn.conflicts=FALSE)
library(tidyr, warn.conflicts=FALSE)
library(testthat, warn.conflicts=FALSE)

dummy <- within(list(), { 
    source('./fix_missing_values.R', local = TRUE)
    
    test_replace_with_most_common <- function() {
        
        replace_with_most_common <- missing$methods$replace_with_most_common
        
        test_that("replace_with_most_common: if NA is most common it is ignored", {
            df <- data_frame(
                attr = c('1', '1', NA, '2', NA, NA, '3')
            )
            actual <- replace_with_most_common(df)
            expected <- data_frame(
                attr = c('1', '1', '1', '2', '1', '1', '3')
            )
            expect_equal(actual, expected)
        })
        
        test_that("replace_with_most_common: if no columns are provided - every column is fixed", {
            df <- data_frame(
                attr1 = c('1', '1', NA,  '2', NA, NA, '3'),
                attr2 = c('x', 'x', 'x', 'y', NA, NA, 'z')
            )
            actual <- replace_with_most_common(df)
            expected <- data_frame(
                attr1 = c('1', '1', '1', '2', '1', '1', '3'),
                attr2 = c('x', 'x', 'x', 'y', 'x', 'x', 'z')
            )
            expect_equal(actual, expected)
        })
        
        test_that("replace_with_most_common: only needed columns are fixed", {
            df <- data_frame(
                attr1 = c(11,  11,  NA,  22,  NA, NA, 33),
                attr2 = c('x', 'x', 'x', 'y', NA, NA, 'z')
            )
            actual <- replace_with_most_common(df, c('attr2'))
            expected <- data_frame(
                attr1 = c(11,  11,  NA,  22,  NA, NA, 33),
                attr2 = c('x', 'x', 'x', 'y', 'x', 'x', 'z')
            )
            expect_equal(actual, expected)
        })
        
    }
    
    test_replace_with_value <- function() {
        
        replace_with_value <- missing$methods$replace_with_value
        replace_with_zero <- missing$methods$replace_with_zero
        
        test_that("replace_with_value: generic case", {
            df <- data_frame(
                attr1 = c('1', '1', NA, '2', NA),
                attr2 = c('x', NA,  NA, 'y', NA)
            )
            actual <- replace_with_value(df, '_none_')

            expected <- data_frame(
                attr1 = c('1', '1',       '_none_', '2', '_none_'),
                attr2 = c('x', '_none_',  '_none_', 'y', '_none_')
            )
            expect_equal(actual, expected)
        })
        
        test_that("replace_with_value: only needed columns are fixed", {
            df <- data_frame(
                attr1 = c('1', '1', NA, '2', NA),
                attr2 = c('x', NA,  NA, 'y', NA)
            )
            actual <- replace_with_value(df, '_none_', c('attr2'))
            
            expected <- data_frame(
                attr1 = c('1', '1',       NA,       '2', NA),
                attr2 = c('x', '_none_',  '_none_', 'y', '_none_')
            )
            expect_equal(actual, expected)
        })
        
        test_that("replace_with_zero: generic case", {
            df <- data_frame(
                attr1 = c(1, 1,  NA, 2, NA),
                attr2 = c(1, NA, NA, 3, NA)
            )
            actual <- replace_with_zero(df)
            
            expected <- data_frame(
                attr1 = c(1, 1, 0, 2, 0),
                attr2 = c(1, 0, 0, 3, 0)
            )
            expect_equal(actual, expected)
        })
        
        test_that("replace_with_value: only needed columns are fixed", {
            df <- data_frame(
                attr1 = c(1, 1,  NA, 2, NA),
                attr2 = c(1, NA, NA, 3, NA)
            )
            actual <- replace_with_zero(df, c('attr1'))
            
            expected <- data_frame(
                attr1 = c(1, 1, 0, 2, 0),
                attr2 = c(1, NA, NA, 3, NA)
            )
            expect_equal(actual, expected)
        })
        
    }
    
    test_replace_with_most_common()
    test_replace_with_value()
})