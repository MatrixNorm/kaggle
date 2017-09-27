source('./helpers.R')


kaggle.house <- within(kaggle.house, {
  
  na <- list()
  
  na <- within(na, {
    FixerContainer <- list()
    
    registerFixer <- function (col_name, fixer, ...) {
      container <- FixerContainer
      container[[col_name]] <- fixer(col_name, ...)
      container
    }
    
    replace_na_with_value <- function (col_name, value) {
      
      function (df) {
        df[df[, col_name] %>% `[[`(1) %>% is.na, col_name] <- value
        df
      }
    }
    
    replace_na_with_zero <- purrr::partial(kaggle.house$na$replace_na_with_value, value = 0)
    
    FixerContainer <- register("BsmtFinSF1",   kaggle.house$na$replace_na_with_zero)
    FixerContainer <- register("BsmtFinSF2",   kaggle.house$na$replace_na_with_zero)
    FixerContainer <- register("BsmtFullBath", kaggle.house$na$replace_na_with_zero)
    FixerContainer <- register("BsmtHalfBath", kaggle.house$na$replace_na_with_zero)
    FixerContainer <- register("BsmtUnfSF",    kaggle.house$na$replace_na_with_zero)
    FixerContainer <- register("Electrical",   kaggle.house$na$replace_na_with_value, 'SBrkr')
    FixerContainer <- register("Exterior1st",  kaggle.house$na$replace_na_with_value, 'VinylSd')
    FixerContainer <- register("Exterior2nd",  kaggle.house$na$replace_na_with_value, 'VinylSd')
    FixerContainer <- register("Functional",   kaggle.house$na$replace_na_with_value, 'Typ')
    FixerContainer <- register("GarageYrBlt",  kaggle.house$na$replace_na_with_zero)
    FixerContainer <- register("BsmtUnfSF",    kaggle.house$na$replace_na_with_zero)
    FixerContainer <- register("KitchenQual",  kaggle.house$na$replace_na_with_value, 'TA')
    FixerContainer <- register("LotFrontage",  kaggle.house$na$replace_na_with_zero)
    
    FixerContainer$MasVnrArea <- function (df) {
      df[df$MasVnrType == 'None' & 
           !is.na(df$MasVnrArea) & df$MasVnrArea > 0, "MasVnrArea"] <- 0
      
      df[is.na(df$MasVnrArea), "MasVnrArea"] <- 0
      
      df
    }
    
    FixerContainer$MasVnrType <- function (df) {
      df[is.na(df$MasVnrType) & 
           !is.na(df$MasVnrArea) & df$MasVnrArea > 0, "MasVnrType"] <- 'BrkFace'
      
      df[!is.na(df$MasVnrArea) & df$MasVnrArea == 0 &
           df$MasVnrType != 'None', "MasVnrType"] <- 'None'
      
      df[is.na(df$MasVnrType), "MasVnrType"] <- 'None'
      
      df
    }
    
    FixerContainer <- register("MSZoning",  kaggle.house$na$replace_na_with_value, 'RL')
    FixerContainer <- register("SaleType",  kaggle.house$na$replace_na_with_value, 'Oth')
    
    FixerContainer$TotalBsmtSF <- function (df) {
      df[is.na(df$TotalBsmtSF) & is.na(df$BsmtCond), "TotalBsmtSF"] <- 0
      df
    }
  })
})

