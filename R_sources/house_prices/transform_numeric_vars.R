
within(list(), 
{
    calc_tran_config_step1 <- function(dataset, columns, trans) {
        # returns tibble
        # var         | x  | log   | sqrt
        # LotFrontage | 65 | 4.18  | 8.06
        # ...
        df1 <-
            dataset %>%
            select(columns) %>%
            select_if(is.numeric) %>%
            gather(var, x) %>%
            filter(!is.na(x))
        
        for (row in 1:nrow(trans)) {
            df1[trans[row, "tran_name"]$tran_name] <- (trans[row, "tran_defin"]$tran_defin)[[1]](df1$x)
        }
        df1
    }
    
    calc_tran_config_step2 <- function(dataset) {
        # returns tibble
        # var          | tran  | L2_distance
        # BedroomAbvGr | log   | 104.29
        # BedroomAbvGr | sqrt  | 111.96
        # BedroomAbvGr | x     | 134.35
        # ...
        dataset %>%
        gather(tran, value, -var) %>%
        group_by(var, tran) %>%
        mutate(
            value_normed = (value - mean(value)) / sd(value)
        ) %>%
        group_by(var, tran, value_normed) %>%
        arrange(var, tran, value_normed) %>%
        summarise(
            k = n()
        ) %>%
        mutate(
            empirical = cumsum(k) / sum(k),
            theoretical = pnorm(value_normed),
            diff_L2 = k*(empirical - theoretical)**2
        ) %>%
        group_by(var, tran) %>%
        summarise(
            L2_distance = sum(diff_L2)
        )
    }
    
    calc_tran_config_step3 <- function(dataset) {
        # returns tibble
        # var       | tran | progress_score
        # GrLivArea | log  | 91.15
        # X1stFlrSF | log  | 90.77
        # BsmtUnfSF | sqrt | 79.97
        # ...
        #  0 <= progress_score <= 100
        #
        dataset %>%
        group_by(var) %>%
        nest %>%
        mutate(
            best_tran = map(data, function(df) {
                best <- df %>% arrange(L2_distance) %>% head(1)
                vanilla <- df %>% filter(tran == 'x')
                progress_score <- 100 * (vanilla$L2_distance - best$L2_distance) / vanilla$L2_distance
                data_frame(tran = best$tran, progress_score = progress_score)
            })
        ) %>%
        select(var, best_tran) %>%
        unnest(best_tran) %>%
        filter(tran != 'x') %>%
        arrange(desc(progress_score))
    }
    
    get_transformation_config <- function(dataset, columns = NULL, trans) {
        if (is.null(columns)) {
            columns <- colnames(dataset)
        }
        calc_tran_config_step1(dataset, columns, trans) %>%
        calc_tran_config_step2 %>%
        calc_tran_config_step3 %>%
        inner_join(trans, by=c("tran" = "tran_name"))
    }
    
    
    for_qq_plot <- function(dataset, transformation_config) {
        dataset %>%
        select(one_of(transformation_config$var)) %>%
        gather(var, value) %>%
        filter(!is.na(value)) %>%
        inner_join(transformation_config %>% select(var, tran), by='var') %>%
        mutate(
            value_transformed = recode(tran,
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
    
    
    apply_transform <- function(df, transformation_config) {
        colnames(df) %>% 
        map_dfc(function (col) {
            
            tran <- transformation_config[transformation_config$var == col, 'predictor'][[1]]
            
            if ( !identical(tran, character(0)) ) {
                switch(tran,
                       log = log(df[, col] + 1),
                       sqrt = sqrt(df[, col]))
            } else {
                df[, col]
            }   
        })
    }
})
