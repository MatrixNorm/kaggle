setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

devnull <- within(list(), {
    
    library(dplyr, warn.conflicts=FALSE)
    library(purrr, warn.conflicts=FALSE)
    library(tidyr, warn.conflicts=FALSE)
    library(testthat, warn.conflicts=FALSE)
    
    tran_num<- source('./transform_numeric_vars.R', local = TRUE)$value
    
    test_that("get_transformation_config: #1", {
        X <- c(qnorm(0.5), qnorm(0.6), qnorm(0.7), qnorm(0.8))
        trans <- tribble(
            ~tran_name, ~tran_defin,
            'log',      function(x) log(x),
            'sqrt',     function(x) sqrt(x)
        )
        df <- data_frame(
            aaa = exp(X),
            bbb = c(X)**2
        )
        actual <- tran_num$get_transformation_config(dataset=df, trans=trans) 
        expected <- tribble(
            ~var,  ~tran,
            'aaa', 'log',
            'bbb', 'sqrt'
        )
        expect_equal(actual %>% select(var, tran), expected)
        expect_equal(
            actual[actual$var == 'aaa', 'tran_defin']$tran_defin[[1]](exp(1)),
            1
        )
        expect_equal(
            actual[actual$var == 'bbb', 'tran_defin']$tran_defin[[1]](3**2),
            3
        )
    })
    
    test_that("apply_transform: #1", {
        X <- c(qnorm(0.5), qnorm(0.6), qnorm(0.7), qnorm(0.8))
        trans <- tribble(
            ~tran_name, ~tran_defin,
            'log',      function(x) log(x),
            'sqrt',     function(x) sqrt(x)
        )
        df <- data_frame(
            aaa = exp(X),
            bbb = c(X)**2
        )
        tran_config <- tran_num$get_transformation_config(dataset=df, trans=trans)
        
        actual <- tran_num$apply_transform(df, tran_config)
        expected <- data_frame(
            aaa = X,
            bbb = X
        )
        expect_true(max(expected - actual) < 0.0000001)
    })
})