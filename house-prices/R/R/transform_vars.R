

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
    tran.column.map <- list()
    
    registerTranformation <- function (col_name, new_col_name, tranformator) {
        
        wrappedTran <- function (df) {
            if ( new_col_name %in% colnames(df) ) {
                stop(paste0("Column <", new_col_name, "> already presents in data frame"))
            }
            new.df <- tranformator(df)
            if ( !(new_col_name %in% colnames(new.df)) ) {
                stop(paste0("Column <", new_col_name, "> should have been added in data frame but it hasn't"))
            }
            new.df
        }

        parentEnv <- parent.env(environment())
        if ( col_name %in% names(parentEnv$tran.column.map) ) {
            stop(paste0("Transformation <", col_name, "> is already registered"))
        }
        parentEnv$tran.column.map[[col_name]] <- new_col_name

        assign(col_name, wrappedTran, parent.frame())
    }
    
    type1TransContainer <- within(list(), 
    {
        registerTranformation("Alley", "has_alley_access", function (df) {
            df %>% mutate(has_alley_access = ifelse(Alley != '_none_', 1, 0))
        })
        
        registerTranformation("CentralAir", "has_central_air", function (df) {
            df %>% mutate(has_central_air = ifelse(CentralAir == 'Y', 1, 0))
        })

        registerTranformation("Electrical", "standard_electrical", function (df) {
            df %>% mutate(standard_electrical = ifelse(Electrical == 'SBrkr', 1, 0))
        })

        registerTranformation("Functional", "is_full_functional", function (df) {
            df %>% mutate(is_full_functional = ifelse(Functional == 'Typ', 1, 0))
        })

        registerTranformation("Heating", "heating_air_furnace", function (df) {
            df %>% mutate(heating_air_furnace = ifelse(Heating == 'GasA', 1, 0))
        })

        registerTranformation("LandContour", "is_land_level", function (df) {
            df %>% mutate(is_land_level = ifelse(LandContour == 'Lvl', 1, 0))
        })

        registerTranformation("LandSlope", "is_slope", function (df) {
            df %>% mutate(is_slope = ifelse(LandSlope != 'Gtl', 1, 0))
        })

        registerTranformation("LotShape", "is_lotshape_regular", function (df) {
            df %>% mutate(is_lotshape_regular = ifelse(LotShape == 'Reg', 1, 0))
        })

        registerTranformation("MiscFeature", "has_misc_feature", function (df) {
            df %>% mutate(has_misc_feature = ifelse(MiscFeature != '_none_', 1, 0))
        })

        registerTranformation("PavedDrive", "has_paved_drive", function (df) {
            df %>% mutate(has_paved_drive = ifelse(PavedDrive == 'Y', 1, 0))
        })

        registerTranformation("PoolQC", "has_pool", function (df) {
            df %>% mutate(has_pool = ifelse(PoolQC != '_none_', 1, 0))
        })

        registerTranformation("RoofMatl", "standard_roof_material", function (df) {
            df %>% mutate(standard_roof_material = ifelse(RoofMatl == 'CompShg', 1, 0))
        })

        registerTranformation("Street", "is_street_paved", function (df) {
            df %>% mutate(is_street_paved = ifelse(Street == 'Pave', 1, 0))
        })

        registerTranformation("Utilities", "all_utilities", function (df) {
            df %>% mutate(all_utilities = ifelse(Utilities == 'AllPub', 1, 0))
        })
    })
    
    groupAveragingTranFactory <- function (y_attr_name, attr_name, new_attr_name, stat.avg=mean) {
        function (df) {
            train_group_avg <- df.new %>% summarise(!!new_attr_name := stat.avg(!!y_attr_name))
            train_global_avg <- df %>% ungroup %>% summarise(avg = stat.avg(!!y_attr_name)) %>% `$`('avg')
            
            df.new <- df %>%
                group_by(!!attr_name) %>%
                mutate(!!new_attr_name := stat.avg(!!y_attr_name) - train_global_avg)
            
            testsetTransformator <- function (testset) {
                attr_name_as_char <- as.character(attr_name)[2]
                testset.new <- testset %>% left_join(train_group_avg - train_global_avg, by=attr_name_as_char)
                #print(train_global_avg)
                testset.new[is.na(testset.new[[new_attr_name]]), new_attr_name] <- train_global_avg
                testset.new
            }
            list(df.new = df.new %>% ungroup, testsetTransformator = testsetTransformator)
        }
    }
    
    groupAveragingTranFactory2 <- function (attr_name) {
        new_attr_name <- paste0(as.character(enquo(attr_name))[2], '.new')
        groupAveragingTranFactory(quo(sale_price_log), enquo(attr_name), new_attr_name)
    }
    
    type2TransContainer <- within(list(), 
    {
        BldgType     <- groupAveragingTranFactory2(BldgType)
        BsmtCond     <- groupAveragingTranFactory2(BsmtCond)
        BsmtExposure <- groupAveragingTranFactory2(BsmtExposure)
        BsmtFinType1 <- groupAveragingTranFactory2(BsmtFinType1)
        BsmtFinType2 <- groupAveragingTranFactory2(BsmtFinType2)
        BsmtQual     <- groupAveragingTranFactory2(BsmtQual)
        Condition1   <- groupAveragingTranFactory2(Condition1)
        Condition2   <- groupAveragingTranFactory2(Condition2)
        ExterCond    <- groupAveragingTranFactory2(ExterCond)
        ExterQual    <- groupAveragingTranFactory2(ExterQual)
        Exterior1st  <- groupAveragingTranFactory2(Exterior1st)
        Exterior2nd  <- groupAveragingTranFactory2(Exterior2nd)
        Fence        <- groupAveragingTranFactory2(Fence)
        FireplaceQu  <- groupAveragingTranFactory2(FireplaceQu)
        Foundation   <- groupAveragingTranFactory2(Foundation)
        GarageCond   <- groupAveragingTranFactory2(GarageCond)
        GarageFinish <- groupAveragingTranFactory2(GarageFinish)
        GarageQual   <- groupAveragingTranFactory2(GarageQual)
        GarageType   <- groupAveragingTranFactory2(GarageType)
        HeatingQC    <- groupAveragingTranFactory2(HeatingQC)
        HouseStyle   <- groupAveragingTranFactory2(HouseStyle)
        KitchenQual  <- groupAveragingTranFactory2(KitchenQual)
        LotConfig    <- groupAveragingTranFactory2(LotConfig)
        MasVnrType   <- groupAveragingTranFactory2(MasVnrType)
        MSSubClass   <- groupAveragingTranFactory2(MSSubClass)
        MSZoning     <- groupAveragingTranFactory2(MSZoning)
        Neighborhood <- groupAveragingTranFactory2(Neighborhood)
        RoofStyle    <- groupAveragingTranFactory2(RoofStyle)
        SaleCondition <- groupAveragingTranFactory2(SaleCondition)
        SaleType     <- groupAveragingTranFactory2(SaleType)
    })
    
    type2TranWrapper <- function (tran) {
        function (prevRes) {
            res <- tran(prevRes$df)
            list(df = res$df.new, tran = function (df) { res$testsetTransformator(prevRes$tran(df)) })
        }
    }
    
    allType1Transform <- do.call(purrr::compose, type1TransContainer)
    allType2Transform <- do.call(purrr::compose, purrr::map(type2TransContainer, type2TranWrapper))
    
    doItAll <- function(df.training, df.testing) {
        has_df_testing <- !missing(df.testing)

        df.training <- allType1Transform(df.training)
        if (has_df_testing) {
            df.testing <- allType1Transform(df.testing)
        }
        
        result <- kaggle.house$trans$allType2Transform(list(df = df.training, tran = function(df) { df }))
        df.training <- result$df
        if (has_df_testing) {
            df.testing <- result$tran(df.testing)
        }
        
        df.training <- 
            df.training %>% 
            select(-dplyr::one_of(kaggle.house$trans$type1TransContainer %>% names)) %>%
            select(-dplyr::one_of(kaggle.house$trans$type2TransContainer %>% names))
        
        if (has_df_testing) {
            df.testing <- 
                df.testing %>% 
                select(-dplyr::one_of(kaggle.house$trans$type1TransContainer %>% names)) %>%
                select(-dplyr::one_of(kaggle.house$trans$type2TransContainer %>% names))
        }
        
        if (has_df_testing) {
            return_value <- list(trainset = df.training, testset = df.testing)
        } else {
            return_value <- list(trainset = df.training, testTransform = result$tran)
        }
        return_value
    }
})
