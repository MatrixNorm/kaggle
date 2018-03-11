

within(list(), 
{
    numeric <- source('./transform_numeric_vars.R', local = TRUE)$value
    
    categ <- within(list(), 
    {
        Helpers <- source('./helpers.R', local = TRUE)$value
        QuantileRating <- source('./quantile_rating.R', local = TRUE)$value
        Tran <- source('./transform_categ_vars.R', local = TRUE)$value
        
        calc_ratings <- function(df, target_var) {
            target_var <- enquo(target_var)
            target_var_char <- as.character(target_var)[2]
            
            quantiles <- 
                QuantileRating$calc_quantiles(
                    df[[target_var_char]],
                    c(0.25, 0.5, 0.75)
                )
            
            QuantileRating$calc_ratings(
                df, 
                !!target_var,
                quantiles,
                Helpers$get_character_colnames(df)
            )
        }

        rating_transform <- function(data, target_var) {
            target_var <- enquo(target_var)
            ratings <- calc_ratings(data, !!target_var)
            Tran$rating_transform_for_selected(
                data, 
                helpers$get_character_colnames(data),
                ratings
            )
        }
    })
})
