

within(list(), 
{
    numeric <- source('./transform_numeric_vars.R', local = TRUE)$value
    categ <- source('./transform_categ_vars.R', local = TRUE)$value
})
