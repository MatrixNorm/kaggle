
within(list(), 
{
    utils <- source('./utils.R', local = TRUE)$value
    
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

    show_table <- function(..., cols = 1) {
        list(...) %>%
        purrr::map(~repr::repr_html(.)) %>%
        purrr::map(~stringr::str_interp(
            "<div style='display:inline-block; column-count: ${cols}; padding-right:25px;'>${.}</div>"
        )) %>%
        paste0(collapse='') %>%
        (IRdisplay::display_html)
    }
    
    show_list <- function(lst) {
        ul_css <- "list-style: none; padding-left: 0"
        li_css <- "
        display: inline-block; 
        padding: 2px 4px 2px 4px;
        margin: 3px 3px 3px 3px;
        background: #efefef;
        color: #565656;
        border-radius: 3px"
        
        ul_template <- "<ul style='${ul_css}'>${li_html}</ul>"
        
        li_html <- 
            lst %>% purrr::map(function(item) {
                li_template <- "<li style='${li_css}'>${item}</li>"
                stringr::str_interp(li_template)
            }) %>% paste0(collapse='')
        
        stringr::str_interp(ul_template) %>%
            (IRdisplay::display_html)
    }
})
