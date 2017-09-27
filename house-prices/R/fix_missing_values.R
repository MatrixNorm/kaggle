source('./helpers.R')


kaggle.house$na <- list()

kaggle.house$na$FixerContainer <- list()

kaggle.house$na$registerFixer <- function (col_name, fixer, ...) {
  container <- kaggle.house$na$FixerContainer
  container[[col_name]] <- fixer(col_name, ...)
  container
}


kaggle.house$na$replace_na_with_value <- function (col_name, value) {
  
  function (df) {
    df[df[, col_name] %>% `[[`(1) %>% is.na, col_name] <- value
    df
  }
}

kaggle.house$na$replace_na_with_zero <- purrr::partial(kaggle.house$na$replace_na_with_value, value = 0)


kaggle.house$na$FixerContainer <- register("BsmtFinSF1",   kaggle.house$na$replace_na_with_zero)
kaggle.house$na$FixerContainer <- register("BsmtFinSF2",   kaggle.house$na$replace_na_with_zero)
kaggle.house$na$FixerContainer <- register("BsmtFullBath", kaggle.house$na$replace_na_with_zero)
kaggle.house$na$FixerContainer <- register("BsmtHalfBath", kaggle.house$na$replace_na_with_zero)
kaggle.house$na$FixerContainer <- register("BsmtUnfSF",    kaggle.house$na$replace_na_with_zero)
kaggle.house$na$FixerContainer <- register("Electrical",   kaggle.house$na$replace_na_with_value, 'SBrkr')
kaggle.house$na$FixerContainer <- register("Exterior1st",  kaggle.house$na$replace_na_with_value, 'VinylSd')
kaggle.house$na$FixerContainer <- register("Exterior2nd",  kaggle.house$na$replace_na_with_value, 'VinylSd')
kaggle.house$na$FixerContainer <- register("Functional",   kaggle.house$na$replace_na_with_value, 'Typ')
kaggle.house$na$FixerContainer <- register("GarageYrBlt",  kaggle.house$na$replace_na_with_zero)
kaggle.house$na$FixerContainer <- register("BsmtUnfSF",    kaggle.house$na$replace_na_with_zero)
kaggle.house$na$FixerContainer <- register("KitchenQual",  kaggle.house$na$replace_na_with_value, 'TA')
kaggle.house$na$FixerContainer <- register("LotFrontage",  kaggle.house$na$replace_na_with_zero)

kaggle.house$na$FixerContainer$MasVnrArea <- function (df) {
  df[df$MasVnrType == 'None' & 
       !is.na(df$MasVnrArea) & df$MasVnrArea > 0, "MasVnrArea"] <- 0
  
  df[is.na(df$MasVnrArea), "MasVnrArea"] <- 0
  
  df
}

kaggle.house$na$FixerContainer$MasVnrType <- function (df) {
  df[is.na(df$MasVnrType) & 
       !is.na(df$MasVnrArea) & df$MasVnrArea > 0, "MasVnrType"] <- 'BrkFace'
  
  df[!is.na(df$MasVnrArea) & df$MasVnrArea == 0 &
       df$MasVnrType != 'None', "MasVnrType"] <- 'None'
  
  df[is.na(df$MasVnrType), "MasVnrType"] <- 'None'
  
  df
}

kaggle.house$na$FixerContainer <- register("MSZoning",  kaggle.house$na$replace_na_with_value, 'RL')
kaggle.house$na$FixerContainer <- register("SaleType",  kaggle.house$na$replace_na_with_value, 'Oth')

kaggle.house$na$FixerContainer$TotalBsmtSF <- function (df) {
  df[is.na(df$TotalBsmtSF) & is.na(df$BsmtCond), "TotalBsmtSF"] <- 0
  df
}
