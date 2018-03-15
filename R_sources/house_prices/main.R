
house_prices <- within(list(), { 
    helpers <- source('./helpers.R', local = TRUE)$value
    attributes_selection <- source('./attributes_selection.R', local = TRUE)$value
    missing <- source('./missing.R', local = TRUE)$value
    trans <- source('./transform_vars.R', local = TRUE)$value
    outliers <- source('./outliers.R', local = TRUE)$value
    source('./model_selection.R', local = TRUE)
    # source('./validate.R', local = TRUE)
    # source('./plot.R', local = TRUE)
})