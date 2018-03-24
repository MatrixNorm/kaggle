
within(list(), 
{
    Utils <- source('./utils.R', local = TRUE)$value
    
    iterate <- function(formulas_for_validation, sample_index, totalset, target_var, categ_transform) {
        
        target_var <- enquo(target_var)
        target_var_char <- as.character(target_var)[2]

        valid_y <- totalset[-sample_index, target_var_char][[1]]

        totalset <- 
            totalset %>% 
            mutate(!!target_var_char := replace(!!target_var, -sample_index, NA))
        
        totalset <-
            categ_transform(
                data = totalset, 
                target_var = !!target_var
            )

        trainset <- totalset[sample_index,]
        validset <- totalset[-sample_index,]
        
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
            
            L2_valid = map_dbl(model, function (mod) {
                Utils$L2_avg_loss(predict(mod, validset) - valid_y)
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
            L2_valid = mean(L2_valid),
            step = max(step)
        ) %>%
        mutate(
            L2_valid_gain = lag(L2_valid) - L2_valid
        )
    }
    
    plot_report <- function(report) {
        avg_report <- report %>% get_avg_report
        
        p1 <-
            avg_report %>%
            select(L2_train, L2_valid, step) %>%
            gather(var, value, -step) %>%
            ggplot() +
            geom_line(aes(x=step, y=value, color=var)) +
            scale_x_continuous(breaks=c(1:max(avg_report$step))) +
            theme_bw() +
            ylab("L2 error") + xlab("Formula Number") +
            theme(legend.position="bottom")
        
        p2 <- 
            report  %>%
            select(L2_train, L2_valid, step, sample) %>%
            gather(var, value, -step, -sample) %>%
            mutate(
                gr = paste(sample, var, sep='_')
            ) %>%
            ggplot() +
            geom_line(aes(x=step, y=value, group=gr, color=var)) +
            scale_x_continuous(breaks=c(1:max(avg_report$step))) +
            theme_bw() +
            ylab("L2 error") + xlab("Formula Number") +
            theme(legend.position="bottom")
        
        grid.arrange(
            p1, p2,
            layout_matrix=rbind(
                c(1, 2)
            )
        )
    }
})
