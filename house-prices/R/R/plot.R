
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
   
})
