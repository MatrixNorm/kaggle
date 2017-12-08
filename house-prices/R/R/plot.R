
plot <- within(list(), 
{
        jitter <- function (data, x.var, y.var, color.var) {
        data %>%
            ggplot(aes_string(x=x.var, y=y.var)) +
            geom_jitter(aes_string(color=color.var), alpha=0.3, width=0.1, height=0) +
            stat_summary(fun.y = mean, geom="line", colour = "red") +
            theme_bw() +
            theme(legend.position="bottom")
    } 
    
    qq <- function (df, var.name, alpha=0.2) {
        
        var.name <- enquo(var.name)
        var.name.char <- as.character(var.name)[2]
        var.vector <- df[,var.name.char]
        if ( !is.numeric(var.vector) ) {
            var.vector <- var.vector[[1]]
        }
        df %>% 
        select(!!var.name) %>%
        mutate(
            normed := (var.vector - mean(!!var.name)) / sd(!!var.name)
        ) %>%
        ggplot() +
        geom_qq(aes(sample=normed), alpha=alpha) +
        geom_abline(slope=1) +
        theme_bw()
    }
   
    compareFormulas <- function (df, formulas, target.var) {
        
        target.var <- enquo(target.var)
        target.var.char <- as.character(target.var)[[2]]

        X <- 
            tibble(
                formula = formulas
            ) %>%
            mutate(
                mod = map(formula, ~lm(as.formula(.), df)),
                glance = map(mod, broom::glance),
                augment = map(mod, broom::augment),
                r2 = map_dbl(glance, 'r.squared')
            )
        
        X.wide <-
            X %>% 
            select(formula, augment) %>% 
            unnest %>% 
            select(formula, !!target.var, fitted=.fitted, resid=.resid) %>%
            group_by(formula) %>%
            mutate(
                resid.normed = (resid - mean(resid)) / sd(resid)
            )
        
        X.long <-
            X %>% 
            select(formula, augment) %>% 
            unnest %>% 
            select(-.se.fit, -.hat, -.sigma, -.cooksd, -.std.resid) %>%
            rename(fitted=.fitted, resid=.resid) %>%
            gather(name, value, -formula, -!!target.var, -fitted, -resid) %>%
            filter(!is.na(value)) %>%
            group_by(formula, name)
        
        resid_qq <-
            X.wide %>%
            ggplot(aes(sample=resid.normed, color=formula)) +
            geom_qq(alpha=0.4) +
            geom_abline(slope=1) +
            theme_bw() +
            theme(legend.position="bottom", legend.direction="vertical")

        resid_vs_target <-
            X.wide %>%
            ggplot(aes_string(x=target.var.char, y="resid", color="formula")) +
            geom_point(alpha=0.7, shape='o', size=2) +
            geom_hline(yintercept=0) +
            theme_bw() +
            theme(legend.position="bottom", legend.direction="vertical")

        actual_vs_predicted <-
            X.wide %>%
            ggplot(aes_string(x=target.var.char, y="fitted", color="formula")) +
            geom_point(alpha=0.7, shape='o', size=2) +
            geom_abline(slope=1, color="red") +
            theme_bw() +
            theme(legend.position="bottom", legend.direction="vertical")
        
        resid_vs_predictors <-
            X.long %>%
            ggplot() +
            geom_point(aes(x=value, y=resid), alpha=0.3) +
            theme_bw()
        
        target_vs_predictors <-
            X.long %>%
            ggplot() +
            geom_point(aes_string(x="value", y=target.var.char), alpha=0.3) +
            geom_point(aes(x=value, y=fitted), alpha=0.3, color="red") +
            theme_bw()

        grob1 <- arrangeGrob(resid_qq, resid_vs_target, actual_vs_predicted,
                             layout_matrix=rbind(c(1, 2, 3)))
        
        list(X=X, X.long=X.long, X.wide=X.wide, grob1=grob1, 
             resid_vs_predictors=resid_vs_predictors, 
             target_vs_predictors=target_vs_predictors)    
    }
    
    regressionDiagnostic <- function (mod, target.var) {
        
        target.var <- enquo(target.var)
        target.var.char <- as.character(target.var)[[2]]
        
        X.wide <-
            mod %>% augment %>%
            select(!!target.var, fitted=.fitted, resid=.resid) %>%
            mutate(
                resid.normed = (resid - mean(resid)) / sd(resid)
            )

        X.long <-
            mod %>% augment %>%
            select(-.se.fit, -.hat, -.sigma, -.cooksd, -.std.resid) %>%
            rename(resid=.resid, fitted=.fitted) %>%
            gather(predictor.name, predictor.value, -!!target.var, -fitted, -resid) %>%
            group_by(predictor.name)

        resid_vs_predictors <-
            X.long %>%
            ggplot(aes(x=predictor.value, y=resid)) +
            geom_point(alpha=0.4) +
            geom_hline(yintercept=0) +
            theme_bw()

        target_vs_predictors <-
            X.long %>%
            ggplot() +
            geom_point(aes_string(x="predictor.value", y=target.var.char), alpha=0.2) +
            geom_point(aes(x=predictor.value, y=fitted), color="red", alpha=0.2) +
            theme_bw()

        resid_qq <-
            X.wide %>%
            ggplot() +
            geom_qq(aes(sample=resid.normed), alpha=0.3) +
            geom_abline(slope=1) +
            theme_bw()

        resid_vs_target <-
            X.wide %>%
            ggplot(aes_string(x=target.var.char, y="resid")) +
            geom_point(alpha=0.3) +
            geom_abline(slope=1, color="red") +
            theme_bw()

        target_vs_fitted <-
            X.wide %>%
            ggplot(aes_string(x=target.var.char, y="fitted")) +
            geom_point(alpha=0.3) +
            geom_abline(slope=1, color="red") +
            theme_bw()
        
        environment() %>% as.list
    }
})
