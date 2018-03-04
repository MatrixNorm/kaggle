
missing <- within(list(), 
{
    source('./helpers.R', local = TRUE)
    
    colums_with_valid_na <- c('Alley', 
                              'BsmtCond', 
                              'BsmtExposure', 
                              'BsmtFinType1', 
                              'BsmtFinType2', 
                              'BsmtQual',
                              'Fence',
                              'FireplaceQu',
                              'GarageCond',
                              'GarageFinish',
                              'GarageQual',
                              'GarageType',
                              'MasVnrType',
                              'MiscFeature',
                              'PoolQC',
                              'Utilities')
    
    categ <- within(list(), {
        
        replace_with_most_common <- function(df) {
            
            columns <- setdiff(
                helpers$get_character_colnames(df),
                colums_with_valid_na
            )
            
            methods$replace_with_most_common(df, columns)
        }
        
        fix_valid <- function(df) {
            
            cat_colums_with_valid_na <- intersect(colums_with_valid_na, helpers$get_character_colnames(df))

            replacement_list <- structure(
                as.list(rep('_none_', length(cat_colums_with_valid_na))),
                names = cat_colums_with_valid_na
            )
            
            df %>%
            replace_na(replacement_list)
        }
    })
    
    
    numeric <- within(list(), {
        
        replace_with_zero <- function(df) {
            
            columns <- setdiff(
                helpers$get_numeric_colnames(df),
                'SalePrice'
            )
            
            replacement_list <- structure(
                as.list(rep(0, length(columns))), 
                names = columns
            )
            
            df %>%
            replace_na(replacement_list)
        }
    })
    
    
    methods <- within(list(), {
        
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
            cat_colums_with_valid_na <- intersect(colums_with_valid_na, helpers$get_character_colnames(df))
            
            replacement_list <- structure(
                as.list(rep('_none_', length(cat_colums_with_valid_na))),
                names = cat_colums_with_valid_na
            )
            
            df %>%
                replace_na(replacement_list)
        }
    })
})
