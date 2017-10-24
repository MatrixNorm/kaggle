
kaggle.house <- list()

kaggle.house <- within(kaggle.house, { 
    source('./helpers.R', local = TRUE)
    source('./fix_missing_values.R', local = TRUE)
    source('./transform_vars.R', local = TRUE)
})