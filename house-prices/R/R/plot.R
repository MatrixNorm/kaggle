
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
   
    compareFormulas <- function (formulas, targer.var) {
        
        targer.var <- enquo(targer.var)
        targer.var.char <- as.character(targer.var)[[2]]
        
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
            select(formula, !!targer.var, fitted=.fitted, resid=.resid) %>%
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
            gather(name, value, -formula, -!!targer.var, -fitted, -resid) %>%
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
            ggplot(aes_string(x=targer.var.char, y="resid", color="formula")) +
            geom_point(alpha=0.7, shape='o', size=2) +
            geom_hline(yintercept=0) +
            theme_bw() +
            theme(legend.position="bottom", legend.direction="vertical")

        actual_vs_predicted <-
            X.wide %>%
            ggplot(aes_string(x=targer.var.char, y="fitted", color="formula")) +
            geom_point(alpha=0.7, shape='o', size=2) +
            geom_abline(slope=1, color="red") +
            theme_bw() +
            theme(legend.position="bottom", legend.direction="vertical")

        grob1 <- arrangeGrob(resid_qq, resid_vs_target, actual_vs_predicted,
                             layout_matrix=rbind(c(1, 2, 3)))
        
        list(X=X, X.long=X.long, X.wide=X.wide, grob1=grob1)    
    }
})
