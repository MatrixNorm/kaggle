
within(list(), 
{
    utils <- source('./utils.R', local = TRUE)$value
    
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
        
        training_dataset <- 
            tbl_df(read.csv(
                file = paste0(data_path, "train.csv"),
                stringsAsFactors = FALSE
            )) %>%
            filter(!is.na(SalePrice)) %>%
            mutate(dataSource = "train")
        
        testing_dataset <- 
            tbl_df(read.csv(
                file = paste0(data_path, "test.csv"),
                stringsAsFactors = FALSE
            )) %>% 
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
        setdiff(utils$get_character_colnames(df), 'dataSource') %>% sort
    }
    
    get_numeric_colnames <- function (df) {
        setdiff(utils$get_numeric_colnames(df), 'Id') %>% sort
    }

})
