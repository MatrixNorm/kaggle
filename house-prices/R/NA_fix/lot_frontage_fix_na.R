
FixNaLotFrontage.Blmngtn = function (df.train) {
  df.train[df.train$Neighborhood == 'Blmngtn' & df.train$LotAreaSqrt < 59, "LotFrontageCalc"] = 43
  df.train[df.train$Neighborhood == 'Blmngtn' & df.train$LotAreaSqrt >= 59, "LotFrontageCalc"] = 53
  df.train
}

FixNaLotFrontage.BrkSide = function (df.train, df.data.BrkSide) {
  
  df.train.BrkSide    = df.train %>% filter(Neighborhood == 'BrkSide') 
  df.train.BrkSide.RL = df.train.BrkSide %>% filter(MSZoning == "RL")
  df.train.BrkSide.RM = df.train.BrkSide %>% filter(MSZoning == "RM")
  
  df.data.BrkSide.RL = df.data.BrkSide %>% filter(MSZoning == "RL")
  df.data.BrkSide.RM = df.data.BrkSide %>% filter(MSZoning == "RM")
  
  lm.BrkSide.RL = lm(LotFrontage ~ LotAreaSqrt, data = df.data.BrkSide.RL)
  
  median.RM = (df.data.BrkSide.RM %>% 
                  select(LotFrontage) %>% 
                  summarise(median=median(LotFrontage))
              )$median
  
  df.train[df.train$Neighborhood == 'BrkSide' & df.train$MSZoning == 'RL', "LotFrontageCalc"] = predict(lm.BrkSide.RL, df.train.BrkSide.RL)
  df.train[df.train$Neighborhood == 'BrkSide' & df.train$MSZoning == 'RM', "LotFrontageCalc"] = median.RM
  df.train
}