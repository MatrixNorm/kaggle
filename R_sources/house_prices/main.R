
house_prices <- within(list(), { 
    source('./helpers.R', local = TRUE)
    source('./attributes_selection.R', local = TRUE)
    source('./fix_missing_values.R', local = TRUE)
    trans <- source('./transform_vars.R', local = TRUE)$value
    source('./outliers.R', local = TRUE)
    source('./model_selection.R', local = TRUE)
    # source('./validate.R', local = TRUE)
    # source('./plot.R', local = TRUE)
})