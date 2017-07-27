
# XXX
kaggle.house.loadLibraries = function () {
    library(dplyr)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(reshape2)
    library(tidyr)
    library(magrittr)
    library(broom)
}


kaggle.house.loadData = function (data.dir) {
  df.train = tbl_df(read.csv("../../data/train.csv", stringsAsFactors = FALSE)) %>% mutate(dataSource = "train")
  df.test = tbl_df(read.csv("../../data/test.csv", stringsAsFactors = FALSE)) %>% mutate(dataSource = "test")
  df.combined = rbind(within(df.train, rm('Id','SalePrice')), within(df.test, rm('Id')))
  df.combined
}


kaggle.house.PrepareCombinedDataSet = function () {
  df.combined = kaggle.house.loadData() %>% 
    mutate(
      # Convert categorical numeric vars to char
      MSSubClass = as.character(MSSubClass), 
      OverallQual = as.character(OverallQual),
      OverallCond = as.character(OverallCond),
      # Add new vars
      LotArea.Log = log(LotArea),
      LotFrontage.Log = log(LotFrontage),
      X1stFlrSF.Log = log(X1stFlrSF),
      GarageArea.Log = log(GarageArea),
      TotRmsAbvGrd.Log = log(TotRmsAbvGrd),
      GrLivArea.Log = log(GrLivArea),
      LotShape2=ifelse(LotShape == 'Reg', 'Reg', 'Ireg'),
      LotConfig2=ifelse(LotConfig %in% c('CulDSac', 'FR2', 'FR3'), 'CulDSac+FR2+FR3', LotConfig),
      GarageCarsChar = as.character(GarageCars)
    ) 
  df.combined
}


kaggle.house.getTrainData =  function () {
  df.combined %>% filter(dataSource == "train") %>% mutate(LotFrontageCalc = NA)
}


kaggle.house.meanVariationSplitByCategoricalAttrs = function (df.data, targetColumn) {
  targetColumn <- enquo(targetColumn)
  colNames = names(which(sapply(df.data, is.character)))
  colNames = c(colNames, paste(targetColumn)[2])
  
  df.data %>% 
          select(colNames) %>% 
          gather(attr, attr_val, -!!targetColumn) %>% 
          group_by(attr, attr_val) %>% 
          summarise(var=var(!!targetColumn), n=n()) %>%
          mutate(freq = n / sum(n), freq.var = var * freq) %>%
          summarise(var.expect = sum(freq.var, na.rm=TRUE)) %>%
          arrange(var.expect)
}



# XXX
peek_tibble = function (tibble, rows=3) {
	head(as.data.frame(tibble), rows)
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
get_nonzero_factor_df = function (slim_df) {

    df = slim_df %>%
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
get_nonzero_factor_df2 = function (slim_df) {

    df = slim_df %>%
            filter(Factor > 0) %>%
	    select(-FactorIsNotZero) %>%	
            mutate(FactorSqrt=Factor^0.5, FactorLog=log10(Factor)) %>%
	    gather(transform, value, c(Factor, FactorSqrt, FactorLog))

    fit = lm(LotFrontage ~ value, data = df %>% filter(transform == 'Factor'))
    fitSqrt = lm(LotFrontage ~ value, data = df %>% filter(transform == 'FactorSqrt'))
    fitLog = lm(LotFrontage ~ value, data = df %>% filter(transform == 'FactorLog'))

    df$resudial = NA
    df$predicted = NA

    df[df$transform == 'Factor', ]$resudial = residuals(fit)
    df[df$transform == 'FactorSqrt', ]$resudial = residuals(fitSqrt)
    df[df$transform == 'FactorLog', ]$resudial = residuals(fitLog)

    df[df$transform == 'Factor', ]$predicted = predict(fit)
    df[df$transform == 'FactorSqrt', ]$predicted = predict(fitSqrt)
    df[df$transform == 'FactorLog', ]$predicted = predict(fitLog)	

    df
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
