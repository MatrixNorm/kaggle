source('fix_missing_values.R')


kaggle.house$na$test_na_fixers <- function(data) {
    
    print("run tests ...")
    
    with(kaggle.house$na$FixerContainer, {
        test_that("BsmtFinSF1", {
            df <- BsmtFinSF1(data)
            
            expect_equal(df %>% filter(is.na(BsmtFinSF1)) %>% nrow, 0)
            
        })
        
        test_that("MasVnrType & MasVnrArea", {
            df <- MasVnrType(data)
            df <- MasVnrArea(df)
            
            expect_equal(df %>% filter(is.na(MasVnrType)) %>% nrow, 0)
            expect_equal(df %>% filter(is.na(MasVnrArea)) %>% nrow, 0)
            expect_equal(df %>% filter(MasVnrArea == 0 & MasVnrType != 'None') %>% nrow, 0)
            expect_equal(df %>% filter(MasVnrType == 'None' & MasVnrArea > 0) %>% nrow, 0)
        })
        
        test_that("MSZoning", {
            df <- MSZoning(data)
            
            expect_equal(df %>% filter(is.na(MSZoning)) %>% nrow, 0)
            
        })
        
        test_that("TotalBsmtSF", {
            df <- TotalBsmtSF(data)
            
            expect_equal(df %>% filter(is.na(TotalBsmtSF)) %>% nrow, 0)
            expect_equal(df %>% filter(TotalBsmtSF == 0 & !is.na(BsmtCond)) %>% nrow, 0)
            
        })
    })
}


