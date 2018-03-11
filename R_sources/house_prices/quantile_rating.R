
within(list(), 
{
    calc_ratings <- function(df, target_var, rating_quantiles, categ_vars) {
        target_var <- enquo(target_var)
        target_var_char <- as.character(target_var)[2]
        
        if (is.null(categ_vars)) {
            categ_vars <- 
                df %>% 
                purrr::map(~is.character(.)) %>% 
                purrr::keep(~.) %>% 
                names
        }

        df <- 
            df %>%
            select(categ_vars, !!target_var) %>%
            filter(!is.na(!!target_var))
        
        df %>%
        gather(var, value, -!!target_var) %>%
        group_by(var, value) %>%
        nest %>%
        mutate(
           rating = map_dbl(data, function (df) {
               calc_rating_for_sample(df[[target_var_char]], rating_quantiles)
           })
        ) %>%
        select(-data) %>%
        rbind(list(NA, NA, calc_default_rating(rating_quantiles)))
    }
    
    calc_quantiles <- function(sample, probs = NULL) {
        if (is.null(probs)) {
            probs <- c(.25, .5, .75)
        }
        quantile(sample, probs, na.rm=TRUE)
    }
    
    calc_rating_for_sample <- function(sample, quantiles) {
        sample <- sample[!is.na(sample)]
        cdf = ecdf(sample)
        cdf_points <- c(0, cdf(quantiles), 1)
        probs <- tail(cdf_points, -1) - head(cdf_points, -1)
        rating <- sum(probs * c(1:length(probs)))
        rating
    }
    
    calc_default_rating <- function(rating_quantiles) {
        num_of_levels <- length(rating_quantiles) + 1
        sum(1:num_of_levels) / num_of_levels
    }
})