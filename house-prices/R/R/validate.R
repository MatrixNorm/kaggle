
validate <- within(list(), 
{
    trainMany <- function (dataset, N=5, sample.share=0.75, trainset.share=0.5, modelFactory) {
        c(1:N) %>% map(function (i) {
            
            sample = dataset
            if ( sample.share != 1 ) {
                sample <- dataset %>% sample_n(round(sample.share * nrow(dataset)))
            }
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
    
    trainSingle <- function(dataset, y.var, N=5, sample.share=0.75, trainset.share=0.5, modelFactory, transformFactory) {
        
        stopifnot(sample.share <= 1 | sample.share >= 0.1)
        stopifnot(trainset.share <= 1 | trainset.share >= 0.1)
        
        sample = dataset
        if ( sample.share < 1 ) {
            sample <- dataset %>% sample_n(round(sample.share * nrow(dataset)))
        }
        sample.partition.index <- caret::createDataPartition(y=sample %>% `$`(y.var), 
                                                             p=trainset.share, list=F, times=1)
        
        trainset <- sample[sample.partition.index,]
        testset  <- sample[-sample.partition.index,] %>% select(-one_of(y.var))
        y_test_actual <- sample[-sample.partition.index, y.var]
        
        stopifnot(setdiff(trainset %>% colnames, testset %>% colnames) == y.var)
        stopifnot(setdiff(testset %>% colnames, trainset %>% colnames) == '')
        
        trainset.ready <- trainset
        testset.ready  <- testset
        if ( !missing(transformFactory) ) {
            tranform.results <- transformFactory(trainset, testset)
            trainset.ready <- tranform.results$trainset
            testset.ready <- tranform.results$testset        
        }
        
        model <- modelFactory(trainset.ready)
        y_test_predicted <- predict(model, testset.ready) %>% as.vector

        list(model=model, y_test_predicted=y_test_predicted, y_test_actual=y_test_actual)
    }
})
