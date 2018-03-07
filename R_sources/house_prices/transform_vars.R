

within(list(), 
{
    source('./helpers.R', local = TRUE)
    
    numeric <- source('./transform_numeric_vars.R', local = TRUE)$value
    categ <- source('./transform_categ_vars.R', local = TRUE)$value
    quantile_rating <- source('./categ2numeric/quantile_rating.R', local = TRUE)$value
    
    rating_transform <- function(data, target_var) {
        target_var <- enquo(target_var)
        ratings <- quantile_rating$calc_rating_for_all(data, !!target_var)
        categ$rating_transform_for_selected(
            data, 
            helpers$get_character_colnames(data),
            ratings
        )
    }
    
    
    functional_transform <- function(data) {
        
    }
})
