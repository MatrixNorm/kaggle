

within(list(), 
{
    get_character_colnames <- function(df) {
        df %>% purrr::map(~is.character(.)) %>% purrr::keep(~.) %>% names
    }
    
    get_numeric_colnames <- function(df) {
        df %>% purrr::map(~is.numeric(.)) %>% purrr::keep(~.) %>% names
    }
    
    frames_diff <- function(df1, df2) {
        df1 <- df1[, order(colnames(df1))]
        df2 <- df2[, order(colnames(df2))]
        
        ndx <- apply(df1 != df2, 2, any)
        cols <- ndx[!is.na(ndx) & ndx] %>% names
        
        df1 <- df1 %>% select(cols)
        names(df1) <- paste0(cols, '.1')
        
        df2 <- df2 %>% select(cols)
        names(df2) <- paste0(cols, '.2')
        
        combined <- bind_cols(df1, df2)
        combined[,order(colnames(combined))]
    }
    
    L2_avg_loss = function (vec) {
        sum(vec**2) / length(vec)
    }
})
