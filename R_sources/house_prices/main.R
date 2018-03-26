
within(list(), {
    
    import_libs <- function () {
        library(broom, warn.conflicts=FALSE, quietly=TRUE)
        library(caret, warn.conflicts=FALSE, quietly=TRUE)
        library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
        library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
        library(grid, warn.conflicts=FALSE, quietly=TRUE)
        library(gridExtra, warn.conflicts=FALSE, quietly=TRUE)
        library(IRdisplay, warn.conflicts=FALSE, quietly=TRUE)
        library(Metrics, warn.conflicts=FALSE, quietly=TRUE)
        library(purrr, warn.conflicts=FALSE, quietly=TRUE)
        library(reshape2, warn.conflicts=FALSE, quietly=TRUE)
        library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
        library(tibble, warn.conflicts=FALSE, quietly=TRUE)
        library(testthat, warn.conflicts=FALSE, quietly=TRUE)
    }
    
    import_libs()
    
    helpers <- source('./helpers.R', local = TRUE)$value
    attributes_selection <- source('./attributes_selection.R', local = TRUE)$value
    missing <- source('./missing.R', local = TRUE)$value
    trans <- source('./transform_vars.R', local = TRUE)$value
    outliers <- source('./outliers.R', local = TRUE)$value
    model_selection <- source('./model_selection.R', local = TRUE)$value
    validate = source('./validate.R', local = TRUE)$value
    plot <- source('./plot.R', local = TRUE)$value
    
    helpers$inject_css()
    calling_env <- parent.env(environment())
    calling_env$show_table <- helpers$show_table
    calling_env$show_list <- helpers$show_list
    calling_env$show_list.html <- helpers$show_list.html
    calling_env$`@@@` <- function(w, h, r) {
        options(repr.plot.width = w, repr.plot.height = h, repr.plot.res=r)
    }
    
    stage1_transformation <- function(dataset) {
        dataset %>%
        # remove outliers
        (outliers$remove_outliers) %>%
        # fix NA values
        (missing$fix_all) %>%
        # transform Y-variable
        mutate(
            price_log = log(SalePrice)
        ) %>%
        # remove redundant variables
        select(-SalePrice) %>%
        select(order(colnames(.)))
    }
    
    stage2_transformation <- function(dataset_stage1, funcs = NULL, threshold = 20) {
        if (is.null(funcs)) {
            funcs = tribble(
                ~tran_name,  ~tran_fn,
                'log',       function(x) log(x+1),
                'sqrt',      function(x) sqrt(x),
                'inv3',      function(x) x**(1/3),
                'inv4',      function(x) x**(1/4)
            )
        }
        
        trans_config <- trans$numeric$get_transformation_config(
            data = dataset_stage1, 
            target_var = price_log, 
            trans = funcs,
            threshold = threshold
        )
        
        dataset_stage2 <-
            dataset_stage1 %>%
            trans$numeric$functional_transform(trans_config) %>%
            select(order(colnames(.)))
        
        list(trans_config=trans_config, dataset=dataset_stage2)
    }
    
    stage3_transformation <- function(dataset_stage2) {
        trans$categ$rating_transform(
            data = dataset_stage2,
            target_var = price_log
        ) %>% 
        select(order(colnames(.)))
    }
})