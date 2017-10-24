

trans <- within(list(), 
{
    source('./test_transform_vars.R', local = TRUE)
    
    removify <- function(trans, attr_name) {
        function(df, remove=T) {
            df <- trans(df)
            if ( remove ) {
                df[, attr_name] <- NULL
            }
            df
        }
    }
    
    groupAveragingTranFactory <- function (y_attr_name, attr_name, new_attr_name) {
        attr_name <- enquo(attr_name)
        y_attr_name <- enquo(y_attr_name)
        function (df) {
            df.new <- df %>%
            group_by(!!attr_name) %>%
            mutate(!!new_attr_name := median(!!y_attr_name))

            train_group_avg <- df %>% group_by(!!attr_name) %>% summarise(!!new_attr_name := median(!!y_attr_name))
            train_global_avg <- df %>% summarise(avg = median(!!y_attr_name)) %>% `$`('avg')
            
            testset.transformator <- function (testset) {
                attr_name_as_char <- as.character(attr_name)[2]
                testset.new <- testset %>% left_join(train_group_avg, by=attr_name_as_char)
                testset.new[is.na(testset.new[[new_attr_name]]), new_attr_name] <- train_global_avg
                testset.new
            }
            list(df.new = df.new, testset.transformator = testset.transformator)
        }
    }
    
    registerTranformation <- function (col_name, tranformator) {
        assign(col_name, tranformator, parent.frame())
    }
    
    
    transformatorContainer <- within(list(), 
    {
        registerTranformation("Alley", function (df) {
            df %>% mutate(has_alley_access = ifelse(!is.na(Alley), 1, 0))
        })
        
        registerTranformation("CentralAir", function (df) {
            df %>% mutate(has_central_air = ifelse(CentralAir == 'Y', 1, 0))
        })

        registerTranformation("Electrical", function (df) {
            df %>% mutate(standard_electrical = ifelse(Electrical == 'SBrkr', 1, 0))
        })
        
        registerTranformation("Functional", function (df) {
            df %>% mutate(is_full_functional = ifelse(Functional == 'Typ', 1, 0))
        })
        
        registerTranformation("Heating", function (df) {
            df %>% mutate(heating_air_furnace = ifelse(Heating == 'GasA', 1, 0))
        })
        
        registerTranformation("LandContour", function (df) {
            df %>% mutate(is_land_level = ifelse(LandContour == 'Lvl', 1, 0))
        })
        
        registerTranformation("LandSlope", function (df) {
            df %>% mutate(is_slope = ifelse(LandSlope != 'Gtl', 1, 0))
        })
        
        registerTranformation("LotShape", function (df) {
            df %>% mutate(is_lotshape_regular = ifelse(LotShape == 'Reg', 1, 0))
        })
        
        registerTranformation("MiscFeature", function (df) {
            df %>% mutate(has_misc_feature = ifelse(!is.na(MiscFeature), 1, 0))
        })
        
        registerTranformation("PavedDrive", function (df) {
            df %>% mutate(has_paved_drive = ifelse(PavedDrive == 'Y', 1, 0))
        })
        
        registerTranformation("PoolQC", function (df) {
            df %>% mutate(has_pool = ifelse(!is.na(PoolQC), 1, 0))
        })
        
        registerTranformation("RoofMatl", function (df) {
            df %>% mutate(standard_roof_material = ifelse(RoofMatl == 'CompShg', 1, 0))
        })
        
        registerTranformation("Street", function (df) {
            df %>% mutate(is_street_paved = ifelse(Street == 'Pave', 1, 0))
        })
        
        BldgType     <- groupAveragingTranFactory(sale_price_log, BldgType, "building_type")
        BsmtCond     <- groupAveragingTranFactory(sale_price_log, BsmtCond, "basement_condition")
        BsmtExposure <- groupAveragingTranFactory(sale_price_log, BsmtExposure, "basement_exposure")
        BsmtFinType1 <- groupAveragingTranFactory(sale_price_log, BsmtFinType1, "basement_finish1")
        BsmtFinType2 <- groupAveragingTranFactory(sale_price_log, BsmtFinType2, "basement_finish2")
        BsmtQual     <- groupAveragingTranFactory(sale_price_log, BsmtQual, "basement_height_quality")
        Fence        <- groupAveragingTranFactory(sale_price_log, Fence, "fence")
        FireplaceQu  <- groupAveragingTranFactory(sale_price_log, FireplaceQu, "fireplace_qual")
        Foundation   <- groupAveragingTranFactory(sale_price_log, Foundation, "foundation")
        GarageFinish <- groupAveragingTranFactory(sale_price_log, GarageFinish, "garage_finish")
        GarageType <- groupAveragingTranFactory(sale_price_log, GarageType, "garage_type")
        
    
    })
})
