
within(list(), 
{
    
    lm <- within(list(), {
        
        find_initial_best_r2_predictor <- function(data, target_var, r2_discard_level = 0.05) {
            target_var <- enquo(target_var)
            target_var_char <- as.character(target_var)[2]
            data_frame(
                predictor = setdiff(data %>% colnames, target_var_char),
                formula = paste0(target_var_char, ' ~ ', predictor)
            ) %>%
            mutate(
                model = map(formula, ~lm(as.formula(.), data=data)),
                r2 = map_dbl(model, function (mod) {
                    summary(mod)$r.squared
                })
            ) %>%
            select(predictor, formula, r2) %>%
            filter(r2 > r2_discard_level) %>%
            arrange(desc(r2))
        }
        
        find_next_best_r2_predictor <- function(data, base_formula_str, predictors,
                                                r2_gain_discard_level = 0.5, a_max = 0, a_avg = 0) {
            base_formula <- as.formula(base_formula_str)
            base_predictors <- labels(terms(base_formula))
            base_model <- lm(base_formula, data=data)
            base_r2 <- summary(base_model)$r.squared
            predictors <- setdiff(predictors, base_predictors)

            data_frame(
                predictor = predictors,
                formula = paste0(base_formula_str, ' + ', predictors),
                base_r2 = base_r2
            ) %>%
                mutate(
                    model = map(formula, ~lm(as.formula(.), data=data)),
                    r2 = map_dbl(model, function (mod) {
                        summary(mod)$r.squared
                    }),
                    r2_gain = 100 * (r2 - base_r2) / base_r2,
                    cors = map(predictor, function(predictor) {
                        cor(data[base_predictors], data[predictor]) %>% as.vector
                    }),
                    cor_abs_max = map_dbl(cors, function(cors) {
                        max(abs(cors))
                    }),
                    cor_abs_avg = map_dbl(cors, function(cors) {
                        mean(abs(cors))
                    }),
                    r2_gain_adj = r2_gain / (1 + a_max*cor_abs_max + a_avg*cor_abs_avg)
                ) %>%
                select(predictor, formula, base_r2, r2, r2_gain, cor_abs_max, cor_abs_avg, r2_gain_adj) %>%
                filter(r2_gain > r2_gain_discard_level) %>%
                arrange(desc(r2_gain_adj))
        }
        
        greedy_r2_gain_adj <- function(data, target_var, 
                                       r2_gain_discard_level = 0.5, r2_discard_level = 0.02, 
                                       a_max = 0, a_avg = 0) {
            target_var <- enquo(target_var)
            
            init <- find_initial_best_r2_predictor(
                data = data,
                target_var = !!target_var, 
                r2_discard_level = r2_discard_level
            )

            base_formula_str <- init[[1, 'formula']]
            predictors <- init$predictor
            step <- 1
            report <- init %>% mutate(step = step)

            repeat {
                result <- find_next_best_r2_predictor(
                    data = data, 
                    base_formula_str = base_formula_str,
                    predictors = predictors,
                    r2_gain_discard_level = r2_gain_discard_level,
                    a_max = a_max, 
                    a_avg = a_avg
                )

                if( nrow(result) < 1 ){
                    break
                }
                
                base_formula_str <- result[['formula']][[1]]
                predictors <- result$predictor
                step <- step + 1
                report <- bind_rows(report, result %>% mutate(step = step))
                
                if( nrow(result) == 1 ){
                    break
                }
            }
            report
        }    
    })

})