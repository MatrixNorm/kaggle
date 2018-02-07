
loadLibraries <- function () {
    library(broom)
    library(caret)
    library(dplyr)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(Metrics)
    library(purrr)
    library(reshape2)
    library(tidyr)
    library(tibble)
    library(testthat)
}

loadData <- function () {

    df.train <- tbl_df(read.csv(paste0(Sys.getenv('DATA_DIR'), "/house_prices/train.csv"), stringsAsFactors = FALSE))
    df.test <- tbl_df(read.csv(paste0(Sys.getenv('DATA_DIR'), "/house_prices/test.csv"), stringsAsFactors = FALSE))

    df.train <- df.train %>% mutate(dataSource = "train")
    df.test <- df.test %>% mutate(dataSource = "test", SalePrice = NA)

    df.combined <- 
        bind_rows(df.train, df.test) %>%
        mutate(MSSubClass = as.character(MSSubClass))
    
    df.combined
}

getCategoricalColumnNames <- function (df) {
    df %>% purrr::map(~is.character(.)) %>% purrr::keep(~.) %>% names %>% sort
}

getNumericColumnNames <- function (df) {
    df %>% purrr::map(~is.numeric(.)) %>% purrr::keep(~.) %>% names %>% sort
}
