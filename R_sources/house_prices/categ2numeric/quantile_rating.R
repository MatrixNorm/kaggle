
within(list(), 
{
    calc_rating_for_all <- function(df, target_var) {
        target_var <- enquo(target_var)
        calc_rating_for_selected(
            df, 
            helpers$get_character_colnames(df),
            !!target_var
        )
    }
    
    calc_rating_for_selected <- function(df, categ_vars, target_var) {
        target_var <- enquo(target_var)
        target_var_char <- as.character(target_var)[2]

        df <- 
            df %>%
            select(categ_vars, !!target_var) %>%
            filter(!is.na(!!target_var))
        
        global_quantiles <- calc_global_quantiles(df, !!target_var)
        
        df %>%
        gather(var, value, -!!target_var) %>%
        group_by(var, value) %>%
        nest %>%
        mutate(
           rating = map_dbl(data, function (df) {
               calc_rating(df[[target_var_char]], global_quantiles)
           })
        ) %>%
        select(-data) %>%
        rbind(list(NA, NA, 0.25*(1+2+3+4)))
    }
    
    calc_global_quantiles <- function(df, target_var) {
        target_var <- enquo(target_var)
        df %>%
        select(!!target_var) %>%
        na.omit %>%
        summarise(
            q25 = quantile(!!target_var, 0.25),
            q50 = quantile(!!target_var, 0.5),
            q75 = quantile(!!target_var, 0.75)
        ) %>%
        as.list
    }
    
    
    calc_rating <- function(sample, global_quantiles) {
        cdf = ecdf(sample)
        prob_rating_1 <- cdf(global_quantiles$q25)
        prob_rating_2 <- cdf(global_quantiles$q50) - cdf(global_quantiles$q25)
        prob_rating_3 <- cdf(global_quantiles$q75) - cdf(global_quantiles$q50)
        prob_rating_4 <- 1 - cdf(global_quantiles$q75)
        rating <- prob_rating_1 * 1 + prob_rating_2 * 2 + prob_rating_3 * 3 + prob_rating_4 * 4
        rating
    }
})