
within(list(), 
{
    Utils <- source('./utils.R', local = TRUE)$value
    
    iterate <- function(formulas_for_validation, sample_index, totalset, target_var, categ_transform) {
        
        target_var <- enquo(target_var)
        target_var_char <- as.character(target_var)[2]

        test_y <- totalset[-sample_index, target_var_char][[1]]

        totalset <- 
            totalset %>% 
            mutate(!!target_var_char := replace(!!target_var, -sample_index, NA))
        
        totalset <-
            categ_transform(
                data = totalset, 
                target_var = !!target_var
            )

        trainset <- totalset[sample_index,]
        testset <- totalset[-sample_index,]
        
        formulas_for_validation %>%
        mutate(
            model = map(formula, ~lm(as.formula(.), data=trainset)),
            
            r2 = map_dbl(model, function (mod) {
                summary(mod)$r.squared
            }),
            
            L2_train = map_dbl(model, function (mod) {
                augment <- broom::augment(mod)
                Utils$L2_avg_loss(augment[['price_log']] - augment$.fitted)
            }),
            
            L2_test = map_dbl(model, function (mod) {
                test_predicted <- predict(mod, testset)
                Utils$L2_avg_loss(test_predicted - test_y)
            })
        ) %>%
        select(-model)
    }
    
    get_avg_report <- function(report) {
        report %>%
        group_by(formula) %>%
        summarise(
            r2 = mean(r2),
            L2_train = mean(L2_train),
            L2_test = mean(L2_test),
            step = max(step)
        ) %>%
        mutate(
            L2_test_gain = lag(L2_test) - L2_test
        )
    }
    
    plot_report <- function(report) {
        avg_report <- report %>% get_avg_report
        
        p1 <-
            avg_report %>%
            ggplot() +
            geom_line(aes(x=step, y=L2_train, group=1)) +
            geom_line(aes(x=step, y=L2_test, group=1), color='red') +
            scale_x_continuous(breaks=c(1:max(avg_report$step))) +
            theme_bw()
        
        p2 <- 
            report %>% 
            ggplot() +
            geom_line(aes(x=step, y=L2_train, group=sample)) +
            geom_line(aes(x=step, y=L2_test, group=sample), color='red') +
            scale_x_continuous(breaks=c(1:max(avg_report$step))) +
            theme_bw()
        
        grid.arrange(
            p1, p2,
            layout_matrix=rbind(
                c(1, 2)
            )
        )
    }
})
