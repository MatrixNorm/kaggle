source('./helpers.R')


kaggle.house <- within(kaggle.house, {
  
    na <- within(list(), 
    {
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
        
        FixerContainer <- within(list(), 
        {
            registerFixer("BsmtFinSF1",  replace_na_with_zero)
            registerFixer("BsmtFinSF2",   replace_na_with_zero)
            registerFixer("BsmtFullBath", replace_na_with_zero)
            registerFixer("BsmtHalfBath", replace_na_with_zero)
            registerFixer("BsmtUnfSF",    replace_na_with_zero)
            registerFixer("Electrical",   replace_na_with_value, 'SBrkr')
            registerFixer("Exterior1st",  replace_na_with_value, 'VinylSd')
            registerFixer("Exterior2nd",  replace_na_with_value, 'VinylSd')
            registerFixer("Functional",   replace_na_with_value, 'Typ')
            registerFixer("GarageYrBlt",  replace_na_with_zero)
            registerFixer("BsmtUnfSF",    replace_na_with_zero)
            registerFixer("KitchenQual",  replace_na_with_value, 'TA')
            registerFixer("LotFrontage",  replace_na_with_zero)

            MasVnrArea <- function (df) {
                df[df$MasVnrType == 'None' &
                     !is.na(df$MasVnrArea) & df$MasVnrArea > 0, "MasVnrArea"] <- 0

                df[is.na(df$MasVnrArea), "MasVnrArea"] <- 0

                df
            }

            MasVnrType <- function (df) {
                df[is.na(df$MasVnrType) &
                     !is.na(df$MasVnrArea) & df$MasVnrArea > 0, "MasVnrType"] <- 'BrkFace'

                df[!is.na(df$MasVnrArea) & df$MasVnrArea == 0 &
                     df$MasVnrType != 'None', "MasVnrType"] <- 'None'

                df[is.na(df$MasVnrType), "MasVnrType"] <- 'None'

                df
            }

            registerFixer("MSZoning",  replace_na_with_value, 'RL')
            registerFixer("SaleType",  replace_na_with_value, 'Oth')

            TotalBsmtSF <- function (df) {
                df[is.na(df$TotalBsmtSF) & is.na(df$BsmtCond), "TotalBsmtSF"] <- 0
                df
            }
        })
    })
})

