

within(list(), 
{
    Helpers <- source('./helpers.R', local = TRUE)$value
    
    numeric <- within(list(),
    {
        Tran <- source('./transform_numeric_vars.R', local = TRUE)$value
        
        example_trans <- tribble(
                ~tran_name,  ~tran_fn,
                'log',       function(x) log(x+1),
                'sqrt',      function(x) sqrt(x),
                'invcube',   function(x) x**(1/3)
            )
        
        functional_transform <- function(data, trans, threshold = 25) {
            tran_config <- Tran$get_transformation_config(
                dataset = data %>% select_if(is.numeric) %>% select(-one_of(c("SalePrice", "price_log"))), 
                trans = trans
            ) %>% filter(progress_score > threshold)
            Tran$apply_transform(data, tran_config)
        }
    })
        
    categ <- within(list(), 
    {
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
