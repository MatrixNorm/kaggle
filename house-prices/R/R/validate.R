
validate <- within(list(), 
{
    trainMany.old <- function (dataset, target.var, N=5, trainset.share=0.5, modelFactory, transformFactory) {
        
        c(1:N) %>% map(function (i) {
            
            partition.index <- caret::createDataPartition(y=dataset %>% `$`(target.var), 
                                                            p=trainset.share, list=F, times=1)
            
            trainset <- sample[partition.index,]
            testset <- sample[-partition.index,]
            
            target_test_actual <- testset %>% select(one_of(target.var))
            testset <- testset %>% select(-one_of(target.var))
            
            res <- transformFactory(trainset, testset)
            trainset.after.tran <- res$trainset
            testset.after.tran <- res$testset
            
            model <- modelFactory(trainset.after.tran)
            
            target_test_predicted <- predict(model, testset.after.tran) %>% as.vector
 
            list(model=model, target_test_predicted=target_test_predicted, target_test_actual=target_test_actual)
        })
    }
    
    trainAndTest <- function(dataset, target.var, trainset.share=0.5, modelFactory, transformFactory=NULL) {
        
        partition.index <- caret::createDataPartition(y=dataset[,target.var] %>% `[[`(1), 
                                                      p=trainset.share, list=F, times=1)
        
        trainset <- dataset[partition.index,]
        testset <- dataset[-partition.index,]
        
        target_test_actual <- testset %>% select(one_of(target.var)) %>% `[[`(1)
        testset <- testset %>% select(-one_of(target.var))
        
        stopifnot(setdiff(trainset %>% colnames, testset %>% colnames) == target.var)
        stopifnot(setdiff(testset %>% colnames, trainset %>% colnames) == '')
        
        trainset.ready <- trainset
        testset.ready  <- testset
        if ( !is.null(transformFactory) ) {
            res <- transformFactory(trainset, testset)
            trainset.ready <- res %>% filter(dataSource == "train") %>% select(-dataSource)
            testset.ready <- res %>% filter(dataSource == "test") %>% select(-dataSource)
        }
        
        model <- modelFactory(trainset.ready)
        
        test.results = tibble(
            actual = target_test_actual,
            predicted = predict(model, testset.ready) %>% as.vector
        )

        tibble(model=list(model), test.results=list(test.results))
    }
    
    trainAndTestMany <- function(dataset, target.var, N, trainset.share=0.5, modelFactory, transformFactory=NULL) {
        
        fits <- c(1:N) %>% map(function (i) {

            fit <- trainAndTest(
                dataset=dataset, 
                target.var=target.var, 
                trainset.share=trainset.share, 
                modelFactory=modelFactory,
                transformFactory=transformFactory
            )
            
            fit$iteration.num <- i
            fit
        })
        bind_rows(fits)
    }
})
