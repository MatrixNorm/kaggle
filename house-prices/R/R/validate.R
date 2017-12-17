
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
    
    plot.terms <- function (data, bins=15, facet.cols=3) {
        
        terms <- data %>% 
                select(formula, tidy) %>% 
                unnest %>% 
                group_by(formula, term) %>%
                mutate(estimate.normed = (estimate - mean(estimate)) / sd(estimate)) 
        
        hist <-
            terms %>%
            ggplot() +
            geom_histogram(aes(estimate, y=..density..), bins=bins, alpha=0.5) +
            geom_density(aes(estimate), color="blue") +
            facet_wrap(formula~term, ncol=facet.cols, scales="free") +
            theme_bw()
        
        qq <-
            terms %>%
            ggplot() +
            geom_qq(aes(sample=estimate.normed), alpha=0.4) +
            geom_abline(slope=1, color="blue") +
            facet_wrap(formula~term, ncol=facet.cols, scales="free") +
            theme_bw()
        
        list(hist=hist, qq=qq)
    }
    
    plot.scores.scatter <- function(data) {
        arrangeGrob(
            data %>%
                ggplot() +
                geom_point(aes(L2.train.score, L2.test.score, color=formula), alpha=0.5) +
                theme_bw() +
                theme(legend.position="none", legend.direction="vertical"),
            
            data %>%
                ggplot() +
                geom_point(aes(R2, L2.train.score, color=formula), alpha=0.5) +
                theme_bw() +
                theme(legend.position="none", legend.direction="vertical"),
            
            data %>%
                ggplot() +
                geom_point(aes(R2, L2.test.score, color=formula), alpha=0.5) +
                theme_bw() +
                theme(legend.position="bottom", legend.direction="vertical"),
            
            layout_matrix=rbind(c(1, 2, 3)), 
            
            widths=c(100, 100, 100)
        )
    }
    
    plot.scores.in.depth <- function (data, bins=15, facet.cols=3) {
        
        data.long <-
            data %>% 
            select(formula, L2.test.score, L2.train.score, R2) %>%
            gather(name, val, -formula) %>%
            group_by(formula, name) %>%
            mutate(val.normed = (val - mean(val)) / sd(val))
        
        hist <-
            data.long %>%
            ggplot() +
            geom_histogram(aes(val, y=..density.., fill=formula), bins=bins, alpha=0.2, position="identity") +
            geom_density(aes(val, color=formula)) +
            facet_wrap(~name, ncol=facet.cols, scales="free") +
            theme_bw() +
            theme(legend.position="bottom", legend.direction="vertical")
        
        qq <-
            data.long %>%
            ggplot() +
            geom_qq(aes(sample=val.normed, color=formula), alpha=0.4) +
            geom_abline(slope=1, color="black") +
            facet_wrap(~name, ncol=facet.cols, scales="free") +
            theme_bw() +
            theme(legend.position="bottom", legend.direction="vertical")

        list(hist=hist, qq=qq)
    }
})
