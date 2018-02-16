
attributes_selection <- within(list(), 
{
    
    entropy <- within(list(), {
        
        entropy <- function(col) {
            counts = table(col, useNA="ifany")
            freqs <- counts / length(col)
            -sum(freqs * log2(freqs))
        }
        
        arrange_vars <- function(df) {
            df %>%
                purrr::map(entropy) %>% 
                as_data_frame %>% 
                gather(var, entropy) %>%
                arrange(entropy)
        }
    })
    
    
    groups_separation <- within(list(), {
        
        arrange_vars <- function (precalculated) {
            precalculated %>%
            mutate(
                additive = (lead_mean - mean)^2 / (std^2/freq + lead_std^2/lead_freq)
            ) %>%
            summarise(
                score = sum(additive, na.rm = TRUE)
            ) %>%
            arrange(score)
        }
        
        precalculated <- function (df, target_var) {
            
            target_var <- enquo(target_var)
            target_var_char <- as.character(target_var)[2]
            
            global_std <- sd(categ_data[,target_var_char][[1]], na.rm=TRUE)
            
            df %>%
            gather(var, value, -!!target_var) %>%
            group_by(var, value) %>%
            summarise(
                n = n(),
                mean = mean(!!target_var),
                std = ifelse(n > 1, sd(!!target_var), global_std)
            ) %>%
            mutate(
                freq = n / sum(n)
            ) %>% 
            arrange(var, mean) %>%
            mutate(
                lead_mean = lead(mean, 1),
                lead_freq = lead(freq, 1),
                lead_std  = lead(std, 1)
            )
        }
    })
    
    Rsquared <- within(list(), {
        
        arrange_vars <- function(df, target_var) {
            
            target_var <- enquo(target_var)
            target_var_char <- as.character(target_var)[2]
            
            formula <- reformulate(termlabels = c('value'), response = target_var_char)
            
            df %>%
            filter(!is.na(!!target_var)) %>%
            gather(var, value, -!!target_var) %>%
            group_by(var) %>%
            nest %>%
            mutate(
                mod = map(data, ~lm(formula, .)),
                glance = map(mod, broom::glance),
                r2 = map_dbl(glance, 'r.squared')
            ) %>%
            select(var, r2) %>%
            arrange(r2)
        }
    })
    
    
    order_factor_by_target <- function(df, factor_var, target_var) {
        factor_var <- enquo(factor_var)
        factor_var_char <- as.character(factor_var)[2]
        target_var <- enquo(target_var)
        
        factor_ordering <- 
            df %>%
            group_by(!!factor_var) %>%
            summarise(
                n = n(),
                mean = mean(!!target_var)
            ) %>%
            arrange(mean) %>%
            select(!!factor_var) %>%
            `[[`(1)
        
        df %>%
        mutate(
            !!factor_var_char := factor(
                !!factor_var,
                levels=factor_ordering
            )
        )
    }
    
})