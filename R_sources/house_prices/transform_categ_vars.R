
within(list(), 
{
    quantile_rating <- source('./categ2numeric/quantile_rating.R', local = TRUE)$value
    
    rating_transform <- function(data, target_var) {
        
        target_var <- enquo(target_var)
        
        ratings <- quantile_rating$calc_rating_for_all(data, !!target_var)
        
        rating_transform_for_selected(
            data, 
            helpers$get_character_colnames(data),
            ratings
        )
    }
    
    
    rating_transform_for_selected <- function(data, columns, ratings) {
        
        global_rating <- ratings[ratings$var == '_global_',]$rating
        
        working <- 
            data %>% 
            select(columns) %>%
            gather(var, value) %>%
            left_join(ratings, by=c('var', 'value')) %>%
            select(var, rating) %>%
            group_by(var) %>%
            mutate(id = row_number()) %>%
            spread(var, rating) %>%
            select(-id)
        
        working[is.na(working)] <- global_rating
        
        cbind(working, data %>% select(-one_of(columns)))
    }
    
    
    rating_transform_for_selected2 <- function(data, columns, ratings) {
        
        global_rating <- ratings[ratings$var == '_global_',]$rating
        
        columns %>% 
            map_dfc(function (col) {
                
                tmp <-
                    ratings %>%
                    filter(var == col) %>%
                    select(value, rating)
                
                mapping <- structure(as.list(tmp$rating), names = as.list(tmp$value))
                
                transformed_col <- map_dbl(
                    data[, col] [[1]], 
                    ~ifelse(is.null(mapping[[.]]), global_rating, mapping[[.]])
                )
                
                as_df <- as_data_frame(transformed_col)
                names(as_df) <- col
                as_df
            }) %>%
            cbind(data %>% select(-one_of(columns)))
    }
    
    
    rating_transform_for_selected3 <- function(data, columns, ratings) {
        
        global_rating <- ratings[ratings$var == '_global_',]$rating
        
        columns %>% 
            map_dfc(function (col) {
                
                mapping <-
                    ratings %>%
                    filter(var == col) %>%
                    select(value, rating)
                
                new <- 
                    left_join(data %>% select(value=col), mapping, by='value') %>%
                    select(!!col := rating)
                new[is.na(new)] <- global_rating
                new
            }) %>%
            cbind(data %>% select(-one_of(columns)))
    }
})