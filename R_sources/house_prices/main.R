
within(list(), {
    
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
        library(zeallot, warn.conflicts=FALSE)
    }
    
    import_libs()
    
    helpers <- source('./helpers.R', local = TRUE)$value
    attributes_selection <- source('./attributes_selection.R', local = TRUE)$value
    missing <- source('./missing.R', local = TRUE)$value
    trans <- source('./transform_vars.R', local = TRUE)$value
    outliers <- source('./outliers.R', local = TRUE)$value
    model_selection <- source('./model_selection.R', local = TRUE)
    # source('./validate.R', local = TRUE)
    plot <- source('./plot.R', local = TRUE)$value
})