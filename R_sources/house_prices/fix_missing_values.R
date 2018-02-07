
missing <- within(list(), 
{
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
                              'Utilities',
                              'SalePrice',
                              'dataSource')
    
    replace_na_with_value <- function (col_name, value) {
        function (df) {
            df[df[, col_name] %>% `[[`(1) %>% is.na, col_name] <- value
            df
        }
    }

    replace_na_with_zero <- purrr::partial(replace_na_with_value, value = 0)
    
    registerFixer <- function (col_name, fixer, ...) {
        assign(col_name, fixer(col_name, ...), parent.frame())
     }
    
    fixerContainer <- within(list(), 
    {
        # character
        registerFixer("Electrical",   replace_na_with_value, 'SBrkr')
        registerFixer("Exterior1st",  replace_na_with_value, 'VinylSd')
        registerFixer("Exterior2nd",  replace_na_with_value, 'VinylSd')
        registerFixer("Functional",   replace_na_with_value, 'Typ')
        registerFixer("KitchenQual",  replace_na_with_value, 'TA')
        registerFixer("MSZoning",  replace_na_with_value, 'RL')
        registerFixer("SaleType",  replace_na_with_value, 'Oth')
        registerFixer("SaleType",  replace_na_with_value, 'WD')
        
        # numeric
        
        # basement
        registerFixer("BsmtFinSF1",   replace_na_with_zero)
        registerFixer("BsmtFinSF2",   replace_na_with_zero)
        registerFixer("BsmtFullBath", replace_na_with_zero)
        registerFixer("BsmtHalfBath", replace_na_with_zero)
        registerFixer("BsmtUnfSF",    replace_na_with_zero)
        registerFixer("TotalBsmtSF",  replace_na_with_zero)
        # garage
        registerFixer("GarageArea",  replace_na_with_zero)
        registerFixer("GarageCars",  replace_na_with_zero)
        registerFixer("GarageYrBlt",  replace_na_with_zero)
        
        registerFixer("LotFrontage",  replace_na_with_zero)
        registerFixer("MasVnrArea",  replace_na_with_zero)

    })
    
    fixGoodNa <- function(df) {
        for (attr in kaggle.house$na$colums.with.good.na) {
            column <- df[[attr]]
            if ( is.character(column) ) {
                df[is.na(column), attr] <- "_none_"
            } else {
                df[is.na(column), attr] <- 0
            }
        }
        df
    }
    
    fixBadNa <-  do.call(purrr::compose, fixerContainer)
    
    fixAll <- function(df) {
        df <- fixGoodNa(df)
        df <- fixBadNa(df)
        df
    }
})
