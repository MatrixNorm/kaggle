trans <- transFn

df <-
    combined_dataset %>%
    filter(!is.na(SalePrice)) %>%
    mutate(price_log = log(SalePrice)) %>%
    select(-SalePrice) %>%
    select_if(is.numeric) %>%
    gather(var, x, -price_log) %>%
    filter(!is.na(x))

for (row in 1:nrow(trans)) {
    tran_name <- trans[[row, "tran_name"]]
    tran_fn <- trans[[row, 'tran_fn']]
    df[tran_name] <- tran_fn(df$x)
}

x <- 
    df %>%
    group_by(var) %>%
    nest %>%
    mutate(
        qq = map(data, function(data) {
            data %>%
                gather(tran, val, -price_log) %>%
                group_by(tran) %>%
                nest %>%
                mutate(
                    r2 = map_dbl(data, function(data) {
                        model = lm(price_log ~ val, data=data)
                        summary(model)$r.squared
                    })
                ) %>%
                select(-data)
        })
    ) %>%
    select(-data) %>%
    unnest

x %>% group_by(var) %>%
    nest %>%
    mutate(
        best_tran = map(data, function(df) {
            best <- df %>% arrange(desc(r2)) %>% head(1)
            vanilla <- df %>% filter(tran == 'x')
            progress_score <- 100 * (best$r2 - vanilla$r2) / vanilla$r2
            data_frame(
                tran_name = best$tran, 
                progress_score = progress_score,
                r2_vanilla = vanilla$r2,
                r2_best = best$r2
            )
        })
    ) %>%
    select(-data) %>%
    unnest %>%
    filter(tran_name != 'x') %>%
    inner_join(trans, by=c("tran_name")) %>%
    arrange(desc(progress_score)) %>%
    filter(r2_best > 0.02)