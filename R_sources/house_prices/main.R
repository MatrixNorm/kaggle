
kaggle.house <- list()

kaggle.house <- within(kaggle.house, { 
    source('./helpers.R', local = TRUE)
    loadLibraries()
    source('./fix_missing_values.R', local = TRUE)
    source('./remove_outliers.R', local = TRUE)
    source('./transform_vars.R', local = TRUE)
    source('./validate.R', local = TRUE)
    source('./plot.R', local = TRUE)
})