source('transform_vars.R')


kaggle.house$na$test_na_fixers <- function(data) {
    
    print("run tests ...")
    
    with(kaggle.house$trans, {
        
        test_that("groupAveragingTranFactory", {
            df.train <- data.frame(
                categ = c('a', 'a', 'b', 'b', 'b', 'c', 'c'),
                sale_price_log  = c(1 ,   2,   3,   4,   5,   6,   7)
            )
            df.test <- data.frame(
                categ = c('a', 'a', 'b', 'c', 'c', 'd', 'd')
            )
            
            trainTransformator <- groupAveragingTranFactory(categ, "categ.transed")
            tmp <- trainTransformator(df.train)
            df.train.transed <- tmp$df.new
            testTransformator <- tmp$testset.transformator
            df.test.transed <- testTransformator(df.test)
            
            x <- df.train.transed %>% group_by(categ) %>% summarise(min = min(categ.transed), max = max(categ.transed))
            y <- df.train %>% group_by(categ) %>% summarise(avg = median(sale_price_log))
            expect_true(all(x[, "min"] == x[, "max"]))
            expect_true(all(x[, "min"] == y[, "median"]))
        })
        
    })
}


