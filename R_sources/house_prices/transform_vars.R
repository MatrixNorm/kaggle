

trans <- within(list(), 
{
    numeric <- within(list(), {
        
        get_transformation_config <- function(numeric_data) {
            numeric_data %>%
            select(-MoSold) %>%
            gather(var, x) %>%
            filter(!is.na(x)) %>%
            mutate(
                log = log(x + 1),
                sqrt = sqrt(x)
            ) %>%
            gather(predictor, value, -var) %>%
            group_by(var, predictor) %>%
            mutate(
                value_normed = (value - mean(value)) / sd(value)
            ) %>%
            group_by(var, predictor, value_normed) %>%
            arrange(var, predictor, value_normed) %>%
            summarise(
                k = n()
            ) %>%
            mutate(
                empirical = cumsum(k) / sum(k),
                theoretical = pnorm(value_normed),
                diff_L2 = k*(empirical - theoretical)**2
            ) %>%
            group_by(var, predictor) %>%
            summarise(
                L2_distance = sum(diff_L2)
            ) %>%
            group_by(var) %>%
            nest %>%
            mutate(
                best_predictor = map(data, function(df) {
                    best <- df %>% arrange(L2_distance) %>% head(1)
                    x <- df %>% filter(predictor == 'x')
                    score <- 100 * (x$L2_distance - best$L2_distance) / x$L2_distance
                    data_frame(predictor = best$predictor, score = score)
                })
            ) %>%
            select(var, best_predictor) %>%
            unnest(best_predictor) %>%
            filter(predictor != 'x' & score > 30)
        }
        
        for_qq_plot <- function(numeric_data, transformation_config) {
            numeric_data %>%
            select(one_of(transformation_config$var)) %>%
            gather(var, value) %>%
            filter(!is.na(value)) %>%
            inner_join(transformation_config %>% select(var, predictor), by='var') %>%
            mutate(
                value_transformed = recode(predictor,
                                           log = log(value + 1),
                                           sqrt = sqrt(value)
                )
            ) %>%
            select(var, value, value_transformed) %>%
            gather(tran, value, -var) %>%
            mutate(
                tran = ifelse(tran == 'value', 'vanilla', 'transformed')
            ) %>%
            group_by(var, tran) %>%
            mutate(
                normed_value = (value - mean(value)) / sd(value)
            )
        }
    })
    
    
 
    binaryTransform <- function (dataset) {
        dataset %>% 
        mutate(
            Alley = ifelse(Alley != '_none_', 1, 0),
            
            CentralAir = ifelse(CentralAir == 'Y', 1, 0),
            
            Electrical = ifelse(Electrical == 'SBrkr', 1, 0),
            
            Functional = ifelse(Functional == 'Typ', 1, 0),
            
            Heating = ifelse(Heating == 'GasA', 1, 0),
            
            LandContour = ifelse(LandContour == 'Lvl', 1, 0),
            
            LandSlope = ifelse(LandSlope != 'Gtl', 1, 0),
            
            LotShape = ifelse(LotShape == 'Reg', 1, 0),
            
            MiscFeature = ifelse(MiscFeature != '_none_', 1, 0),
            
            PavedDrive = ifelse(PavedDrive == 'Y', 1, 0),
            
            PoolQC = ifelse(PoolQC != '_none_', 1, 0),
            
            RoofMatl = ifelse(RoofMatl == 'CompShg', 1, 0),
            
            Street = ifelse(Street == 'Pave', 1, 0),
            
            Utilities = ifelse(Utilities == 'AllPub', 1, 0)
        )
    }
    
    averagingTransform <- function (dataset, y.var, id.var, src.var, stat.fun=mean, diff=F) {
        
        y.var <- enquo(y.var)
        id.var <- enquo(id.var)
        src.var <- enquo(src.var)
        
        long <- 
            dataset %>% 
            gather(var.name, var.value, -!!y.var, -!!id.var, -!!src.var) %>%
            group_by(var.name, var.value) %>%
            mutate(avg_ = stat.fun(!!y.var, na.rm=T)) %>%
            group_by(var.name) %>%
            mutate(avg_ = ifelse(is.na(avg_), stat.fun(!!y.var, na.rm=T), avg_))
        
        if ( diff ) {
            long <- long %>% mutate(avg_ = avg_ - stat.fun(!!y.var, na.rm=T))
        }

        long %>% 
        select(-var.value) %>% 
        spread(var.name, avg_)
    }
    
    transformCombindDataset <- function (dataset) {
        
        dataset.after.binary <- dataset %>% binaryTransform
        
        binary.vars <- setdiff(
            dataset %>% (kaggle.house$getCategoricalColumnNames), 
            dataset.after.binary %>% (kaggle.house$getCategoricalColumnNames)
        )
        averaging.vars <- 
            dataset.after.binary %>% 
            select(-dataSource) %>% 
            (kaggle.house$getCategoricalColumnNames)
        
        dataset.after.averaging <- 
            averagingTransform(
                dataset = dataset.after.binary %>% select(one_of(averaging.vars), price.log, dataSource, Id), 
                y.var = price.log, 
                id.var = Id,
                src.var=dataSource,
                stat.fun = mean, 
                diff = TRUE
            ) %>% select(-price.log)
        
        inner_join(
            dataset.after.binary %>% select(-one_of(averaging.vars)),
            dataset.after.averaging,
            by=c("dataSource", "Id")
        )
    }
    
    combine.levels <- within(list(), 
    {
        OverallQual <- function (df) {
            df %>%
                mutate(
                    quality = case_when(
                        OverallQual %in% c(1, 2, 3, 4) ~ 1,
                        OverallQual %in% c(9, 10) ~ 6,
                        TRUE ~ OverallQual - 3
                    )
                )
        }
    })
})
