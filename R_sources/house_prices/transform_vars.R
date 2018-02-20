

trans <- within(list(), 
{
    numeric <- within(list(), {
        
        get_transformation_config <- function(numeric_data) {
            numeric_data %>%
            select(-MoSold) %>%
            gather(var, x) %>%
            filter(!is.na(x)) %>%
            mutate(
                log = log(x + 1),
                sqrt = sqrt(x)
            ) %>%
            gather(predictor, value, -var) %>%
            group_by(var, predictor) %>%
            mutate(
                value_normed = (value - mean(value)) / sd(value)
            ) %>%
            group_by(var, predictor, value_normed) %>%
            arrange(var, predictor, value_normed) %>%
            summarise(
                k = n()
            ) %>%
            mutate(
                empirical = cumsum(k) / sum(k),
                theoretical = pnorm(value_normed),
                diff_L2 = k*(empirical - theoretical)**2
            ) %>%
            group_by(var, predictor) %>%
            summarise(
                L2_distance = sum(diff_L2)
            ) %>%
            group_by(var) %>%
            nest %>%
            mutate(
                best_predictor = map(data, function(df) {
                    best <- df %>% arrange(L2_distance) %>% head(1)
                    x <- df %>% filter(predictor == 'x')
                    score <- 100 * (x$L2_distance - best$L2_distance) / x$L2_distance
                    data_frame(predictor = best$predictor, score = score)
                })
            ) %>%
            select(var, best_predictor) %>%
            unnest(best_predictor) %>%
            filter(predictor != 'x' & score > 30)
        }
        
        for_qq_plot <- function(numeric_data, transformation_config) {
            numeric_data %>%
            select(one_of(transformation_config$var)) %>%
            gather(var, value) %>%
            filter(!is.na(value)) %>%
            inner_join(transformation_config %>% select(var, predictor), by='var') %>%
            mutate(
                value_transformed = recode(predictor,
                                           log = log(value + 1),
                                           sqrt = sqrt(value)
                )
            ) %>%
            select(var, value, value_transformed) %>%
            gather(tran, value, -var) %>%
            mutate(
                tran = ifelse(tran == 'value', 'vanilla', 'transformed')
            ) %>%
            group_by(var, tran) %>%
            mutate(
                normed_value = (value - mean(value)) / sd(value)
            )
        }
        
        transform <- function(df, transformation_config) {
            colnames(df) %>% 
            map_dfc(function (col) {
                
                tran <- transformation_config[transformation_config$var == col, 'predictor'][[1]]
                
                if ( !identical(tran, character(0)) ) {
                    switch(tran,
                           log = log(df[, col] + 1),
                           sqrt = sqrt(df[, col]))
                } else {
                    df[, col]
                }   
            })
        }
    })
    
 
    categ <- within(list(), {
        
        calc_rating <- function(df, target_var) {
            
            target_var <- enquo(target_var)
            target_var_char <- as.character(target_var)[2]
            
            global_quantiles <- 
                df %>%
                select(!!target_var) %>%
                summarise(
                    q25 = quantile(!!target_var, 0.25),
                    q50 = quantile(!!target_var, 0.5),
                    q75 = quantile(!!target_var, 0.75)
                ) 
            
            df %>%
            gather(var, value, -!!target_var) %>%
            group_by(var, value) %>%
            nest %>%
            mutate(
                distrib = map(data, ~ecdf(.[[target_var_char]])),
                rating = map_dbl(distrib, function (cdf) {
                    prob_rating_1 <- cdf(global_quantiles$q25)
                    prob_rating_2 <- cdf(global_quantiles$q50) - cdf(global_quantiles$q25)
                    prob_rating_3 <- cdf(global_quantiles$q75) - cdf(global_quantiles$q50)
                    prob_rating_4 <- 1 - cdf(global_quantiles$q75)
                    rating <- prob_rating_1 * 1 + prob_rating_2 * 2 + prob_rating_3 * 3 + prob_rating_4 * 4
                    rating
                })
            ) %>%
            select(-distrib, -data)
        }
        
       rating_transform <- function(data, columns, ratings) {
           columns %>% 
           map_dfc(function (col) {
               
               tmp <-
                   ratings %>%
                   filter(var == col) %>%
                   select(value, rating)
               
               mapping <- structure(as.list(tmp$rating), names = as.list(tmp$value))
               
               new_col_df <- data_frame(
                   map_dbl(data[, col] [[1]], ~mapping[[.]])
               )
               names(new_col_df) <- col
               new_col_df
           })
       }
    })
})
