
model_selection <- within(list(), 
{
    
    lm <- within(list(), {
        
        iterate <- function(base_formula_str, dataset, R2_GAIN_INFIMUM, MAX_CORR_SUPREMUM, CORR_SENSITIVITY) {
            base_formula <- as.formula(base_formula_str)
            target <- all.vars(base_formula)[1]
            all_predictors <- setdiff(colnames(dataset), target)
            base_predictors <- labels(terms(base_formula))
            remaining_predictors <- setdiff(all_predictors, base_predictors)
            
            data_frame(
                base_formula = base_formula_str,
                var_name = remaining_predictors
            ) %>%
                mutate(
                    formula = paste0(base_formula, ' + ', var_name),
                    base_r2 = summary(lm(as.formula(base_formula), data=training_dataset))$r.squared
                ) %>%
                mutate(    
                    model = map(formula, ~lm(as.formula(.), data=dataset)),
                    
                    r2 = map_dbl(model, function (mod) {
                        summary(mod)$r.squared
                    }),
                    
                    r2_gain = 100 * (r2 - base_r2) / base_r2,
                    
                    max_corr = map2_dbl(base_formula, var_name, function(base_formula, var_name) {
                        base_predictors <- labels(terms(as.formula(base_formula)))
                        max(abs(cor(dataset[base_predictors], dataset[var_name])))
                    })        
                ) %>%
                select(-model) %>%
                filter(r2_gain > R2_GAIN_INFIMUM, max_corr < MAX_CORR_SUPREMUM) %>%
                mutate(
                    r2_gain_adj = r2_gain / (1 + CORR_SENSITIVITY * max_corr)
                ) %>%
                arrange(desc(r2_gain_adj))
        }
        
        greedy_r2_adj <- function(base_formula_str, dataset, 
                                  R2_GAIN_INFIMUM = 1, MAX_CORR_SUPREMUM = 0.4, CORR_SENSITIVITY = 1) {
            
            report <- NULL
            
            repeat {
                
                result <- iterate(base_formula_str, dataset, 
                                  R2_GAIN_INFIMUM, MAX_CORR_SUPREMUM, CORR_SENSITIVITY) 
                
                if( nrow(result) < 1 ){
                    break
                }
                
                base_formula_str <- result[['formula']][[1]]
                
                if (is.null(report)) {
                    report <- result
                } else {
                    report <- bind_rows(report, result)
                }
            }
            report
        }    
    })

})