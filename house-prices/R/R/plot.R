
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
   
})
