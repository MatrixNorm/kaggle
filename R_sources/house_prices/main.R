
house_prices <- within(list(), { 
    source('./helpers.R', local = TRUE)
    source('./attributes_selection.R', local = TRUE)
    source('./fix_missing_values.R', local = TRUE)
    source('./transform_vars.R', local = TRUE)
    source('./remove_outliers.R', local = TRUE)
    source('./model_selection.R', local = TRUE)
    # source('./validate.R', local = TRUE)
    # source('./plot.R', local = TRUE)
})