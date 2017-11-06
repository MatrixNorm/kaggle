

trans <- within(list(), 
{
    #source('./test_transform_vars.R', local = TRUE)
    
    # removify <- function(trans, attr_name) {
    #     function(df, remove=T) {
    #         df <- trans(df)
    #         if ( remove ) {
    #             df[, attr_name] <- NULL
    #         }
    #         df
    #     }
    # }
    
    groupAveragingTranFactory <- function (y_attr_name, attr_name, new_attr_name) {
        # attr_name <- enquo(attr_name)
        # y_attr_name <- enquo(y_attr_name)
        function (df) {
            df.new <- df %>%
                group_by(!!attr_name) %>%
                mutate(!!new_attr_name := median(!!y_attr_name))

            train_group_avg <- df.new %>% summarise(!!new_attr_name := median(!!y_attr_name))
            train_global_avg <- df %>% summarise(avg = median(!!y_attr_name)) %>% `$`('avg')

            testsetTransformator <- function (testset) {
                attr_name_as_char <- as.character(attr_name)[2]
                testset.new <- testset %>% left_join(train_group_avg, by=attr_name_as_char)
                testset.new[is.na(testset.new[[new_attr_name]]), new_attr_name] <- train_global_avg
                testset.new
            }
            list(df.new = df.new, testsetTransformator = testsetTransformator)
        }
    }
    
    groupAveragingTranFactory2 <- function (y_attr_name, attr_name) {
        new_attr_name <- paste0(as.character(enquo(attr_name))[2], '.new')
        groupAveragingTranFactory(enquo(y_attr_name), enquo(attr_name), new_attr_name)
    }
    
    registerTranformation <- function (col_name, tranformator) {
        assign(col_name, tranformator, parent.frame())
    }
    
    type1TransContainer <- within(list(), 
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
        
        registerTranformation("Utilities", function (df) {
            df %>% mutate(all_utilities = ifelse(Utilities == 'AllPub', 1, 0))
        })
    })
    
    type2TransContainer <- within(list(), 
    {
        BldgType     <- groupAveragingTranFactory2(sale_price_log, BldgType)
        BsmtCond     <- groupAveragingTranFactory2(sale_price_log, BsmtCond)
        BsmtExposure <- groupAveragingTranFactory2(sale_price_log, BsmtExposure)
        BsmtFinType1 <- groupAveragingTranFactory2(sale_price_log, BsmtFinType1)
        BsmtFinType2 <- groupAveragingTranFactory2(sale_price_log, BsmtFinType2)
        BsmtQual     <- groupAveragingTranFactory2(sale_price_log, BsmtQual)
        Condition1   <- groupAveragingTranFactory2(sale_price_log, Condition1)
        Condition2   <- groupAveragingTranFactory2(sale_price_log, Condition2)
        ExterCond    <- groupAveragingTranFactory2(sale_price_log, ExterCond)
        ExterQual    <- groupAveragingTranFactory2(sale_price_log, ExterQual)
        Exterior1st  <- groupAveragingTranFactory2(sale_price_log, Exterior1st)
        Exterior2nd  <- groupAveragingTranFactory2(sale_price_log, Exterior2nd)
        Fence        <- groupAveragingTranFactory2(sale_price_log, Fence)
        FireplaceQu  <- groupAveragingTranFactory2(sale_price_log, FireplaceQu)
        Foundation   <- groupAveragingTranFactory2(sale_price_log, Foundation)
        GarageCond   <- groupAveragingTranFactory2(sale_price_log, GarageCond)
        GarageFinish <- groupAveragingTranFactory2(sale_price_log, GarageFinish)
        GarageQual   <- groupAveragingTranFactory2(sale_price_log, GarageQual)
        GarageType   <- groupAveragingTranFactory2(sale_price_log, GarageType)
        HeatingQC    <- groupAveragingTranFactory2(sale_price_log, HeatingQC)
        HouseStyle   <- groupAveragingTranFactory2(sale_price_log, HouseStyle)
        KitchenQual  <- groupAveragingTranFactory2(sale_price_log, KitchenQual)
        LotConfig    <- groupAveragingTranFactory2(sale_price_log, LotConfig)
        MasVnrType   <- groupAveragingTranFactory2(sale_price_log, MasVnrType)
        MSSubClass   <- groupAveragingTranFactory2(sale_price_log, MSSubClass)
        MSZoning     <- groupAveragingTranFactory2(sale_price_log, MSZoning)
        Neighborhood <- groupAveragingTranFactory2(sale_price_log, Neighborhood)
        RoofStyle    <- groupAveragingTranFactory2(sale_price_log, RoofStyle)
        SaleCondition <- groupAveragingTranFactory2(sale_price_log, SaleCondition)
        SaleType     <- groupAveragingTranFactory2(sale_price_log, SaleType)
    })
})
