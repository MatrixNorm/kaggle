
helpers <- within(list(), 
{
    import_libs <- function () {
        library(broom, warn.conflicts=FALSE)
        library(caret, warn.conflicts=FALSE)
        library(dplyr, warn.conflicts=FALSE)
        library(ggplot2, warn.conflicts=FALSE)
        library(grid, warn.conflicts=FALSE)
        library(gridExtra, warn.conflicts=FALSE)
        library(Metrics, warn.conflicts=FALSE)
        library(purrr, warn.conflicts=FALSE)
        library(reshape2, warn.conflicts=FALSE)
        library(tidyr, warn.conflicts=FALSE)
        library(tibble, warn.conflicts=FALSE)
        library(testthat, warn.conflicts=FALSE)
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
            mutate(
                MSSubClass = as.character(MSSubClass),
                MoSold = as.character(MoSold)
            )
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
