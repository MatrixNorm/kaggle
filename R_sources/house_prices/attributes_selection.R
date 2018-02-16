
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
    
    
    
})