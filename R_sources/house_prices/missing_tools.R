
within(list(), 
{
   replace_with_most_common <- function(df, columns = NULL) {
       if (!is.null(columns)) {
           df_for_fix <- df %>% select(one_of(columns))
       } else {
           df_for_fix <- df
       }
       tmp <-
           df_for_fix %>%
           gather(var, value) %>%
           na.omit %>%
           group_by(var, value) %>%
           count %>%
           group_by(var) %>%
           filter(n == max(n)) %>%
           select(var, value)
       
       replacement_list <- structure(as.list(tmp$value), 
                                     names = as.list(tmp$var))
       
       df %>%
       replace_na(replacement_list)
   }
   
   replace_with_value <- function(df, value, columns = NULL) {
       if (is.null(columns)) {
           columns <- colnames(df)
       }
       
       replacement_list <- structure(
           as.list(rep(value, length(columns))),
           names = columns
       )
       
       df %>%
       replace_na(replacement_list)
   }
   
   replace_with_zero <- function(df, colnames = NULL) {
       replace_with_value(df, 0, colnames)
   }
})
