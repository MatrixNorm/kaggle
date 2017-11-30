

trans <- within(list(), 
{
 
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
})
