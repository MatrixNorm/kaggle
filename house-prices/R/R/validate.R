
validate <- within(list(), 
{
    trainMany <- function (dataset, N=5, sample.share=0.75, trainset.share=0.5, modelFactory) {
        c(1:N) %>% map(function (i) {
            
            sample <- dataset %>% sample_n(round(sample.share * nrow(dataset)))
            sample.partition.index <- caret::createDataPartition(y=sample$sale_price_log, 
                                                                 p=trainset.share, list=F, times=1)
            
            trainset <- sample[sample.partition.index,] %>% select(-SalePrice)
            testset <- sample[-sample.partition.index,] %>% select(-sale_price_log)
            
            stopifnot(setdiff(trainset %>% colnames, testset %>% colnames) == 'sale_price_log')
            stopifnot(setdiff(testset %>% colnames, trainset %>% colnames) == 'SalePrice')
            
            tranform.results <- kaggle.house$trans$doItAll(trainset, testset)
            trainset <- tranform.results$trainset
            testset <- tranform.results$testset
            
            stopifnot(0 == trainset %>% purrr::map(function (col) { !is.numeric(col) }) %>% unlist %>% sum)
            stopifnot(0 == testset %>% purrr::map(function (col) { !is.numeric(col) }) %>% unlist %>% sum)

            model <- modelFactory(trainset)
            y_predicted <- predict(model, testset %>% select(-SalePrice)) %>% as.vector
            y_actual <- testset %>% mutate(sale_price_log = log(SalePrice)) %>% select(sale_price_log) %>% `[[`(1)
            
            list(model=model, y_test_predicted=y_predicted, y_test_actual=y_actual)
        })
    }
})
