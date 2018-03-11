
within(list(), 
{
    rating_transform_for_selected <- function(data, columns, ratings) {
        default_rating <- ratings[is.na(ratings$var),]$rating
        
        working <- 
            data %>% 
            select(columns) %>%
            gather(var, value) %>%
            left_join(ratings, by=c('var', 'value')) %>%
            replace_na(list(rating=default_rating)) %>%
            select(var, rating) %>%
            group_by(var) %>%
            mutate(id = row_number()) %>%
            spread(var, rating) %>%
            select(-id)

        bind_cols(working, data %>% select(-one_of(columns)))
    }
    
    
    rating_transform_for_selected2 <- function(data, columns, ratings) {
        global_rating <- ratings[is.na(ratings$var),]$rating
        
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
        global_rating <- ratings[is.na(ratings$var),]$rating
        
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