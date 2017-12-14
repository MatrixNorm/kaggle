
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
            trainAndTest(
                dataset=dataset, 
                target.var=target.var, 
                trainset.share=trainset.share, 
                modelFactory=modelFactory,
                transformFactory=transformFactory
            )
        })
        bind_rows(fits) %>%
        mutate(
            
            formula = map_chr(model, function (mod) { as.list(mod$call)$formula %>% as.character %>% `[[`(3) }),
            
            glance = map(model, broom::glance),
            
            tidy = map(model, broom::tidy),
            
            L2.test.score = map_dbl(test.results, function (df) {
                sqrt(sum((df$actual - df$predicted)^2) / nrow(df))
            }),
            
            L2.train.score = map_dbl(model, function (mod) {
                target.var.name <- as.list(mod$call)$formula %>% as.list %>% `[[`(2) %>% as.character
                augment = broom::augment(mod)
                sqrt(sum((augment[,target.var.name] - augment$.fitted)^2) / nrow(augment))
            }),
            
            R2 = map_dbl(model, function (mod) {
                summary(mod)$r.squared
            })
        )
    }
    
    plot.terms <- function (simulation.results, bins=15, facet.cols=3) {
        
        terms <- simulation.results %>% select(tidy) %>% unnest %>% group_by(term)
        
        hist <-
            terms %>%
            ggplot() +
            geom_histogram(aes(estimate, y=..density..), bins=bins, alpha=0.5) +
            geom_density(aes(estimate), color="blue") +
            facet_wrap(~term, ncol=facet.cols, scales="free") +
            theme_bw()
        
        qq <-
            terms %>%
            group_by(term) %>%
            mutate(estimate.normed = (estimate - mean(estimate)) / sd(estimate)) %>%
            ggplot() +
            geom_qq(aes(sample=estimate.normed), alpha=0.4) +
            geom_abline(slope=1, color="blue") +
            facet_wrap(~term, ncol=facet.cols, scales="free") +
            theme_bw()
        
        list(hist=hist, qq=qq)
    }
    
    plot.scores <- function (simulation.results, bins=15, facet.cols=3) {
        
        hist <-
            scores %>%
            gather(name, val) %>%
            group_by(name) %>%
            ggplot() +
            geom_histogram(aes(val, y=..density..), bins=bins, alpha=0.5) +
            geom_density(aes(val), color="blue") +
            facet_wrap(~name, ncol=facet.cols, scales="free") +
            theme_bw()
        
        qq <-
            scores %>%
            gather(name, val) %>%
            group_by(name) %>%
            mutate(val.normed = (val - mean(val)) / sd(val)) %>%
            ggplot() +
            geom_qq(aes(sample=val.normed), alpha=0.4) +
            geom_abline(slope=1, color="blue") +
            facet_wrap(~name, ncol=facet.cols, scales="free") +
            theme_bw()
        
        arrangeGrob(
            scores %>%
                ggplot() +
                geom_point(aes(L2.train.score, L2.test.score), alpha=0.5) +
                theme_bw(),
            
            scores %>%
                ggplot() +
                geom_point(aes(R2, L2.train.score), alpha=0.5) +
                theme_bw(),
            
            scores %>%
                ggplot() +
                geom_point(aes(R2, L2.test.score), alpha=0.5) +
                theme_bw(),
            
            hist,
            
            qq,
            
            layout_matrix=rbind(c(1, 2, 3),
                                c(4, 4, 4),
                                c(5, 5, 5)), 
            
            widths=c(100, 100, 100)
        )
    }
})
