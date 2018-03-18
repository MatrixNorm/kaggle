
within(list(), 
{
    calc_tran_config_step1 <- function(dataset, trans) {
        # var	         x	      log	    sqrt   invcube
        # LotFrontage	65	 4.189655	8.062258   4.020726
        # LotFrontage	80	 4.394449	8.944272   4.308869
        # ...
        df <- dataset %>% gather(var, x) %>% filter(!is.na(x))
        
        for (row in 1:nrow(trans)) {
            tran_name <- trans[[row, "tran_name"]]
            tran_fn <- trans[[row, 'tran_fn']]
            df[tran_name] <- tran_fn(df$x)
        }
        df
    }
    
    calc_tran_config_step2 <- function(dataset) {
        # var	    tran	value	      value_normed
        # LotArea	x	    8450.000000	  -0.21639955
        # LotArea	x	    9600.000000	  -0.06909653
        # ...
        # LotArea	log	    9.042040	  -0.10174374
        # ...
        # LotArea	sqrt	91.923882	  -0.21081495
        dataset %>%
        gather(tran, value, -var) %>%
        group_by(var, tran) %>%
        mutate(
            value_normed = (value - mean(value)) / sd(value)
        )
    }
        
    calc_tran_config_step3 <- function(dataset) {
        # var	    tran	  value_normed	 k
        # YrSold    invcube	  -1.3633378	 619
        # YrSold	invcube	  -0.6027227	 691
        # ...
        # YrSold	log	      -1.3634510	 619
        # ...
        # YrSold	sqrt	  -1.3632811	 619
        dataset %>%
        group_by(var, tran, value_normed) %>%
        summarise(
            k = n()
        ) %>%
        arrange(var, tran, value_normed)
    }
        
    calc_tran_config_step4 <- function(dataset) {  
        # var	    tran	  L2_distance
        # LotArea	invcube	  10.78862
        # LotArea	log	      13.31376
        # LotArea	sqrt	  13.39006
        # LotArea	x	      50.03278
        dataset %>%
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
    
    calc_tran_config_step5 <- function(dataset) {
        # var	        best_tran
        # GrLivArea	    log , 91.1579415746179
        # LotArea	    invcube , 78.436890821096
        # OverallQual	log , 29.9443565176069
        dataset %>%
        group_by(var) %>%
        nest %>%
        mutate(
            best_tran = map(data, function(df) {
                best <- df %>% arrange(L2_distance) %>% head(1)
                vanilla <- df %>% filter(tran == 'x')
                progress_score <- 100 * (vanilla$L2_distance - best$L2_distance) / vanilla$L2_distance
                data_frame(tran_name = best$tran, progress_score = progress_score)
            })
        ) %>%
        select(-data)
    }
    
    calc_tran_config_step6 <- function(dataset, trans) {
        # var	      tran_name	 progress_score	  tran_fn
        # GrLivArea	  log	     91.15794157	  function (x) , log(x + 1)
        # X1stFlrSF	  log	     90.77165686	  function (x) , log(x + 1)
        # BsmtUnfSF	  sqrt	     79.97603070	  function (x) , sqrt(x)
        dataset %>%
        unnest(best_tran) %>%
        filter(tran_name != 'x') %>%
        inner_join(trans, by=c("tran_name")) %>%
        arrange(desc(progress_score))
    }
    
    get_transformation_config <- function(dataset, trans, columns = NULL) {
        if (is.null(columns)) {
            columns <- colnames(dataset)
        }
        calc_tran_config_step1(dataset %>% select_if(is.numeric), trans) %>%
        calc_tran_config_step2 %>%
        calc_tran_config_step3 %>%
        calc_tran_config_step4 %>%
        calc_tran_config_step5 %>%
        calc_tran_config_step6(trans)
    }
    
    apply_transform <- function(df, tran_config) {
        tran_config <- 
            tran_config %>% 
            filter(var %in% c(df %>% colnames))
        
        for (row in 1:nrow(tran_config)) {
            var_name <- tran_config[[row, "var"]]
            tran_fn <- tran_config[[row, "tran_fn"]]
            df[[var_name]] <- tran_fn(df[[var_name]])
        }
        df
    }
    
    apply_transform2 <- function(df, tran_config) {
        
        cols <- intersect(tran_config$var, colnames(df))

        df[cols] <- purrr::lmap(df[cols], function (col) {
            fn <- 
                tran_config[
                    tran_config$var == colnames(col), 
                    "tran_fn"
                ]$tran_fn[[1]]
            fn(col) %>% as_data_frame
        })
        df
    }
    
    for_qq_plot <- function(dataset, transformation_config) {
        dataset %>%
        select(one_of(transformation_config$var)) %>%
        gather(var, value) %>%
        filter(!is.na(value)) %>%
        inner_join(transformation_config %>% select(var, tran_name, tran_fn), by='var') %>%
        mutate(
            value_transformed = map2_dbl(value, tran_fn, function(val, tran_fn) tran_fn(val))
        ) %>%
        select(var, value, value_transformed) %>%
        gather(tran, value, -var) %>%
        mutate(
            tran = ifelse(tran == 'value', 'original', 'transformed')
        ) %>%
        group_by(var, tran) %>%
        mutate(
            normed_value = (value - mean(value)) / sd(value)
        )
    }
})
