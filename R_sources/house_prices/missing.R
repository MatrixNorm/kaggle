
within(list(), 
{
    helpers <- source('./helpers.R', local = TRUE)$value
    tools <- source('./missing_tools.R', local = TRUE)$value
    
    colums_with_valid_na <- c(
        'Alley',        'BsmtCond',     'BsmtExposure', 'BsmtFinType1', 
        'BsmtFinType2', 'BsmtQual',     'Fence',        'FireplaceQu',
        'GarageCond',   'GarageFinish', 'GarageQual',   'GarageType',
        'MasVnrType',   'MiscFeature',  'PoolQC',       'Utilities'
    )
    
    fix_all <- function(df) {
        df %>%
        (categ$replace_with_most_common) %>%
        (categ$fix_valid) %>%
        (numeric$replace_with_zero)
    }
    
    categ <- within(list(), {
        replace_with_most_common <- function(df) {
            columns <- setdiff(
                helpers$get_character_colnames(df),
                colums_with_valid_na
            )
            tools$replace_with_most_common(df, columns)
        }
        
        fix_valid <- function(df) {
            cat_colums_with_valid_na <- intersect(
                colums_with_valid_na, 
                helpers$get_character_colnames(df)
            )
            tools$replace_with_value(df, '_none_', cat_colums_with_valid_na)
        }
    })
    
    numeric <- within(list(), {
        replace_with_zero <- function(df) {
            columns <- setdiff(
                helpers$get_numeric_colnames(df),
                'SalePrice'
            )
            tools$replace_with_zero(df, columns)
        }
    })
})
