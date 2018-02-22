
helpers <- within(list(), 
{
    import_libs <- function () {
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
    
    load_data <- function () {
        
        data_path <- paste0(Sys.getenv('DATA_DIR'), "/house_prices/")
        
        training_dataset <- tbl_df(read.csv(
            file = paste0(data_path, "train.csv"),
            stringsAsFactors = FALSE))
        
        testing_dataset <- tbl_df(read.csv(
            file = paste0(data_path, "test.csv"),
            stringsAsFactors = FALSE))
        
        training_dataset <- 
            training_dataset %>%
            filter(!is.na(SalePrice)) %>%
            mutate(dataSource = "train")
        
        testing_dataset <- 
            testing_dataset %>% 
            mutate(dataSource = "test", SalePrice = NA)
        
        combined_dataset <- 
            bind_rows(training_dataset, testing_dataset) %>%
            mutate(MSSubClass = as.character(MSSubClass))
        
        combined_dataset
    }
    
    get_character_colnames <- function (df) {
        setdiff(
            df %>% purrr::map(~is.character(.)) %>% purrr::keep(~.) %>% names,
            'dataSource'
        ) %>% sort
    }
    
    get_numeric_colnames <- function (df) {
        setdiff(
            df %>% purrr::map(~is.numeric(.)) %>% purrr::keep(~.) %>% names,
            'Id'
        ) %>% sort
    }

})
