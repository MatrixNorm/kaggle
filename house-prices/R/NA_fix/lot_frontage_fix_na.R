
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


FixNaLotFrontage.ClearCr = function (df.train, df.data.ClearCr) {
  
  df.data.ClearCr.Reg = df.data.ClearCr %>% filter(LotShape == "Reg")
  df.data.ClearCr.Ireg = df.data.ClearCr %>% filter(LotShape != "Reg")
  
  median.Reg = (df.data.ClearCr.Reg %>% 
                 select(LotFrontage) %>% 
                 summarise(median=median(LotFrontage)))$median
  
  median.Ireg = (df.data.ClearCr.Ireg %>% 
                  select(LotFrontage) %>% 
                  summarise(median=median(LotFrontage)))$median
  
  df.train[df.train$Neighborhood == 'ClearCr' & df.train$LotShape == 'Reg', "LotFrontageCalc"] = median.Reg
  df.train[df.train$Neighborhood == 'ClearCr' & df.train$LotShape != 'Reg', "LotFrontageCalc"] = median.Ireg
  df.train
}


FixNaLotFrontage.CollgCr = function (df.train, df.data.CollgCr) {
  
  df.train.CollgCr      = df.train %>% filter(Neighborhood == 'CollgCr') 
  df.train.CollgCr.Reg  = df.train.CollgCr %>% filter(LotShape2 == "Reg")
  df.train.CollgCr.Ireg = df.train.CollgCr %>% filter(LotShape2 == "Ireg")
  
  df.data.CollgCr.Reg  = df.data.CollgCr %>% filter(LotShape2 == "Reg")
  df.data.CollgCr.Ireg = df.data.CollgCr %>% filter(LotShape2 == "Ireg")
  
  lm.CollgCr.Reg = lm(LotFrontage ~ LotAreaSqrt, data = df.data.CollgCr.Reg)
  
  median.Ireg = (df.data.CollgCr.Ireg %>% 
                 select(LotFrontage) %>% 
                 summarise(median=median(LotFrontage))
  )$median
  
  df.train[df.train$Neighborhood == 'CollgCr' & df.train$LotShape2 == 'Reg', "LotFrontageCalc"] = predict(lm.CollgCr.Reg, df.train.CollgCr.Reg)
  df.train[df.train$Neighborhood == 'CollgCr' & df.train$LotShape2 == 'Ireg', "LotFrontageCalc"] = median.Ireg
  df.train
}


FixNaLotFrontage.Crawfor = function (df.train, df.data) {
  
  df.train.nei      = df.train %>% filter(Neighborhood == 'Crawfor') 
  df.train.nei.Reg  = df.train.nei %>% filter(LotShape2 == "Reg")
  df.train.nei.Ireg = df.train.nei %>% filter(LotShape2 == "Ireg")
  
  df.data.Reg  = df.data %>% filter(LotShape2 == "Reg")
  df.data.Ireg = df.data %>% filter(LotShape2 == "Ireg")
  
  lm.Reg  = lm(LotFrontage ~ LotAreaSqrt, data = df.data.Reg)
  lm.Ireg = lm(LotFrontage ~ LotAreaSqrt, data = df.data.Ireg)
  
  df.train[df.train$Neighborhood == 'Crawfor' & df.train$LotShape2 == 'Reg', "LotFrontageCalc"] = predict(lm.Reg, df.train.nei.Reg)
  df.train[df.train$Neighborhood == 'Crawfor' & df.train$LotShape2 == 'Ireg', "LotFrontageCalc"] = predict(lm.Ireg, df.train.nei.Ireg)
  df.train
}


FixNaLotFrontage.Edwards = function (df.train, df.data) {
  
  df.train.nei = df.train %>% filter(Neighborhood == 'Edwards') 

  lm.model  = lm(LotFrontage ~ LotAreaSqrt, data = df.data)
 
  df.train[df.train$Neighborhood == 'Edwards', "LotFrontageCalc"] = predict(lm.model, df.train.nei)
  df.train
}


FixNaLotFrontage.Gilbert = function (df.train, df.data) {
  
  df.train.nei = df.train %>% filter(Neighborhood == 'Gilbert') 
  
  lm.model  = lm(LotFrontage ~ LotAreaSqrt, data = df.data)
  
  df.train[df.train$Neighborhood == 'Gilbert', "LotFrontageCalc"] = predict(lm.model, df.train.nei)
  df.train
}



