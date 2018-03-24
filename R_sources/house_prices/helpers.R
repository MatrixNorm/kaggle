
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
    
    inject_css <- function() {
        styles <- "
            .rendered_html thead {
                background: #dde7fa;
            }
            .rendered_html td, th {
                position: relative;
            }
            .rendered_html  td:after, th:after {
                position: absolute;
                border-right: 1px solid #a7a7a7;
                content: '';
                top: 25%;
                bottom: 25%;
                right: -1px;
            }
            .rendered_html tr td:last-child:after, th:last-child:after {
                border-right: 0;
            }

            .__matrix_norm li {
                    display: inline-block; 
                    padding: 2px 4px 2px 4px;
                    margin: 3px 3px 3px 3px;
                    border: 1px solid #aac9db;
                    color: #2e2e2e;
                    border-radius: 3px
            }
    
            .__matrix_norm ul {
                list-style: none; 
                padding-left: 0 !important;
                margin-top: 0;
            }
    
            .__matrix_norm i {
                padding: 8px 4px 0 4px;
                display: inline-block;
                margin-bottom: px;
                margin-left: 4px;
                font-size: 0.8em;
            }
        "
        stringr::str_interp("<style>${styles}</style>") %>% 
        (IRdisplay::display_html)
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
    
    show_list <- function(lst, caption = NULL) {
        lst_html = show_list.html(lst)
        ifelse(is.null(caption),
            stringr::str_interp("<div class='__matrix_norm'>${lst_html}</div>"),
            stringr::str_interp("<div class='__matrix_norm'><i>${caption}</i>${lst_html}</div>")
        ) %>% 
        (IRdisplay::display_html)
    }
    
    show_list.html <- function(lst) {
        ul_template <- "<ul>${li_html}</ul>"
        
        li_html <- 
            lst %>% purrr::map(function(item) {
                if (item %>% length == 1) {
                    li_template <- "<li>${item}</li>"
                    stringr::str_interp(li_template)
                } else {
                    item_html = show_list.html(item)
                    stringr::str_interp("
                        <li>${item_html}</li>"
                    )
                }
            }) %>% paste0(collapse='')
        
        stringr::str_interp(ul_template)
    }
})
