

test_trans <- function(df.training, df.testing) {
    
    print("run tests ...")
    
    with(kaggle.house$trans, {
        
        with(list(), {
            df.train <- data_frame(
                categAttr =       c('a', 'a', 'b', 'b', 'b', 'c', 'c'),
                sale_price_log  = c(1 ,   2,   3,   4,   5,   6,   7)
            )
            df.test <- data_frame(
                categAttr = c('a', 'a', 'b', 'b', 'c', 'c', 'd', 'd', 'd')
            )
            
            trainTransformator <- groupAveragingTranFactory(sale_price_log, categAttr, "categAttr.new")
            tmp <- trainTransformator(df.train)    
            
            test_that("train set tranformation", {
                
                df.train.new <- tmp$df.new
                
                x <- df.train.new %>% group_by(categAttr) %>% 
                    summarise(min = min(categAttr.new), max = max(categAttr.new))
                
                y <- df.train %>% group_by(categAttr) %>% 
                    summarise(median = median(sale_price_log))
                
                expect_true(all(x[, "min"] == x[, "max"]))
                expect_true(all(x[, "min"] == y[, "median"]))
                expect_equal(
                    df.train.new$categAttr.new, 
                    c(1.5, 1.5, 4, 4, 4, 6.5, 6.5)
                )
            })    
            
            test_that("test set tranformation", {
                
                testTransformator <- tmp$testset.transformator
                df.test.new <- testTransformator(df.test)
                
                expect_equal(df.test.new$categAttr, df.test$categAttr)
                expect_equal(
                    df.test.new$categAttr.new, 
                    c(1.5, 1.5, 4, 4, 6.5, 6.5, 4, 4, 4)
                )
            })
        })
        
        with(transformatorContainer, {
            test_that("Alley", {
                df.new <- Alley(df.training)
                
                expect_equal(df.new %>% filter(is.na(Alley) & has_alley_access != 0) %>% nrow, 0)
                expect_equal(df.new %>% filter(!is.na(Alley) & has_alley_access != 1) %>% nrow, 0)
            })
            
            test_that("CentralAir", {
                df <- CentralAir(df.training)
                
                expect_equal(df %>% filter(CentralAir == 'Y' & has_central_air != 1) %>% nrow, 0)
                expect_equal(df %>% filter(CentralAir != 'Y' & has_central_air != 0) %>% nrow, 0)
            })
            
            test_that("Electrical", {
                df <- Electrical(df.training)
                
                expect_equal(df %>% filter(Electrical == 'SBrkr' & standard_electrical != 1) %>% nrow, 0)
                expect_equal(df %>% filter(is.na(Electrical) & standard_electrical != 1) %>% nrow, 0)
                expect_equal(df %>% filter(Electrical != 'SBrkr' & standard_electrical != 0) %>% nrow, 0)
            })
            
            test_that("Functional", {
                df <- Functional(df.training)
                
                expect_equal(df %>% filter(Functional == 'Typ' & is_full_functional != 1) %>% nrow, 0)
                expect_equal(df %>% filter(Functional != 'Typ' & is_full_functional != 0) %>% nrow, 0)
            })
            
            test_that("BldgType", {
                res <- BldgType(df.training)
                df.training.new <- res$df.new
                testTran <- res$testset.transformator
                
                res$df.new %>% group_by(BldgType) %>% 
                    summarise(min = min(building_type), max = max(building_type), avg = median(sale_price_log)) %>% print
                
                df.test.new <- res$testset.transformator(df.testing)
                
                df.test.new %>% group_by(BldgType) %>% 
                    summarise(min = min(building_type), max = max(building_type)) %>% print
                
                #expect_equal(df %>% filter(Functional == 'Typ' & is_full_functional != 1) %>% nrow, 0)
                #expect_equal(df %>% filter(Functional != 'Typ' & is_full_functional != 0) %>% nrow, 0)
            })
        })
        
    })
}


