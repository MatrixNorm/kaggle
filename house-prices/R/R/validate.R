
validate <- within(list(), 
{
    L2score = function (X, Y) {
        sqrt(sum((X - Y)^2) / length(X))
    }
    
    trainAndTest <- function(dataset, target.var, formulas, trainset.share=0.5, N=5) {
        
        caret::createDataPartition(y=dataset[,target.var] %>% `[[`(1), p=trainset.share, list=F, times=N) %>% 
        as.data.frame %>%
        gather(sample, index) %>%
        group_by(sample) %>%
        nest %>%
        rename(sample.index=data) %>%
        mutate(fit = map(sample.index, function (index) {
            trainset <- dataset[index$index,]
            testset <- dataset[-index$index,]
            target_test_actual <- testset %>% select(one_of(target.var)) %>% `[[`(1)
            testset <- testset %>% select(-one_of(target.var))
            
            tibble(formula = formulas) %>%
                mutate(
                    model = map(formula, ~lm(as.formula(.), trainset)),
                    glance = map(model, broom::glance),
                    tidy = map(model, broom::tidy),
                    L2.test.score = map_dbl(model, function (mod) {
                        target.test.predicted = predict(mod, testset)
                        L2score(target_test_actual, target.test.predicted)
                    }),
                    L2.train.score = map_dbl(model, function (mod) {
                        augment = broom::augment(mod)
                        L2score(augment[,target.var], augment$.fitted)
                    }),
                    R2 = map_dbl(model, function (mod) {
                        summary(mod)$r.squared
                    })
                ) %>% 
                select(-model)
        })) %>%
        select(-sample.index) %>%
        unnest
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
