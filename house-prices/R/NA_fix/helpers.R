
# XXX
load_header = function () {
    library(dplyr)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(reshape2)

    df.train = tbl_df(read.csv("../../data/train.csv", stringsAsFactors = FALSE))
    df.test = tbl_df(read.csv("../../data/test.csv", stringsAsFactors = FALSE))
    df.combined = rbind(within(df.train, rm('Id','SalePrice')), within(df.test, rm('Id')))
    df.combined	
}

# XXX
peek_tibble = function (tibble) {
	head(as.data.frame(nonzero_df), 3)
}

# XXX
get_slim_df = function (fat_df, factor_column) {
    fat_df %>%
        filter(!is.na(LotFrontage)) %>%
        rename_(Factor=factor_column) %>%
        select(LotFrontage, Factor) %>%
        filter(!is.na(Factor)) %>%
        mutate(FactorIsNotZero=(Factor != 0))
}

# XXX
get_nonzero_factor_df = function (slim_fd) {

    df = slim_fd %>%
            filter(Factor > 0) %>%
	    select(-FactorIsNotZero) %>%	
            mutate(FactorSqrt=Factor^0.5, FactorLog=log10(Factor))

    fit = lm(LotFrontage ~ Factor, data = df)
    fitSqrt = lm(LotFrontage ~ FactorSqrt, data = df)
    fitLog = lm(LotFrontage ~ FactorLog, data = df)

    df %>%
     mutate(predicted=predict(fit), resudials=residuals(fit)) %>%
     mutate(predictedSqrt=predict(fitSqrt), resudialsSqrt=residuals(fitSqrt)) %>%
     mutate(predictedLog=predict(fitLog), resudialsLog=residuals(fitLog))
}

# XXX
expore_zero_vs_nonzero = function (slim_fd) {

    df2 = slim_fd %>%
            group_by(FactorIsNotZero) %>%
            summarise(n(), meanLotFrontage=mean(LotFrontage))
    model.lm <- lm(LotFrontage ~ FactorIsNotZero, data = slim_fd)

    print(df2)
    print(summary(model.lm))
}

# XXX
visualize_zero_vs_nonzero = function (slim_fd, binwidth) {

    g1 = ggplot(slim_fd , aes(LotFrontage, fill=FactorIsNotZero)) +
            geom_histogram(binwidth = binwidth) +
            theme(legend.position="top")
    g2 = ggplot(slim_df, aes(x=FactorIsNotZero, y=LotFrontage)) +
            geom_jitter(width=0.1)

    arrangeGrob(g1, g2, layout_matrix=rbind(c(1, 2)))
}

# XXX
explore_nonzero = function (slim_fd, x_factor) {
    nonzero_df = get_nonzero_factor_df(slim_fd)
    formula_str = paste("LotFrontage ~ ", x_factor)
    model.lm <- lm(as.formula(formula_str), data = nonzero_df)
    print(summary(model.lm))
}

# XXX
visualize_nonzero = function (nonzero_df, binwidth) {

    g1 = ggplot(nonzero_df, aes(Factor)) + geom_histogram(binwidth = binwidth[1])
    g2 = ggplot(nonzero_df, aes(FactorSqrt)) + geom_histogram(binwidth = binwidth[2])
    g3 = ggplot(nonzero_df, aes(FactorLog)) + geom_histogram(binwidth = binwidth[3])

    q1 = ggplot(nonzero_df, aes(sample=Factor)) + stat_qq()
    q2 = ggplot(nonzero_df, aes(sample=FactorSqrt)) + stat_qq()
    q3 = ggplot(nonzero_df, aes(sample=FactorLog)) + stat_qq()

    s1 = ggplot(nonzero_df, aes(x=Factor, y=LotFrontage)) +
            geom_point(shape=1) +
            stat_smooth(method="lm", se=FALSE)
    s2 = ggplot(nonzero_df, aes(x=FactorSqrt, y=LotFrontage)) +
            geom_point(shape=1) +
            stat_smooth(method="lm", se=FALSE)
    s3 = ggplot(nonzero_df, aes(x=FactorLog, y=LotFrontage)) +
            geom_point(shape=1) +
            stat_smooth(method="lm", se=FALSE)

    r1 = ggplot(nonzero_df, aes(sample=resudials)) + stat_qq()
    r2 = ggplot(nonzero_df, aes(sample=resudialsSqrt)) + stat_qq()
    r3 = ggplot(nonzero_df, aes(sample=resudialsLog)) + stat_qq()

    arrangeGrob(g1, g2, g3,
                 q1, q2, q3,
                 s1, s2, s3,
                 r1, r2, r3,
                 layout_matrix=rbind(c(1,   2,  3),
                                     c(4,   5,  6),
                                     c(7,   8,  9),
                                     c(10, 11, 12)))
}
