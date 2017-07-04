

simpleLmFixerMaker = function (condition) {
  
  condition_call = substitute(condition)
  
  function (df.train, df.data) {
    df.train.nei = df.train[ eval(condition_call, df.train), ]
    df.data.nei  = df.data [ eval(condition_call, df.data), ]
    
    lm.model = lm(LotFrontage ~ LotAreaSqrt, data = df.data.nei)
    
    df.train[ eval(condition_call, df.train), "LotFrontageCalc"] = predict(lm.model, df.train.nei)
    df.train
  }
}

medianFixerMaker = function (condition) {
  
  condition_call = substitute(condition)
  
  function (df.train, df.data) {
    df.train.nei = df.train[ eval(condition_call, df.train), ]
    df.data.nei  = df.data [ eval(condition_call, df.data), ]
    
    med = (df.data.nei %>% 
              select(LotFrontage) %>% 
              summarise(median=median(LotFrontage))
          )$median
    
    df.train[ eval(condition_call, df.train), "LotFrontageCalc"] = med
    df.train
  }
}


FixNaLotFrontage.Blmngtn = function (df.train) {
  df.train[ eval( substitute(Neighborhood == 'Blmngtn' & LotAreaSqrt < 59), df.train ), "LotFrontageCalc"] = 43
  df.train[ eval( substitute(Neighborhood == 'Blmngtn' & LotAreaSqrt >= 59), df.train ), "LotFrontageCalc"] = 53
  df.train
}


FixNaLotFrontage.BrkSide = function (df.train, df.data.BrkSide) {
  
  df.data.BrkSide.RL = df.data.BrkSide %>% filter(MSZoning == "RL")
  df.data.BrkSide.RM = df.data.BrkSide %>% filter(MSZoning == "RM")
  
  FixRL = simpleLmFixerMaker(Neighborhood == 'BrkSide' & MSZoning == "RL")
  FixRM = medianFixerMaker(Neighborhood == 'BrkSide' & MSZoning == "RM")
  
  median.RM = (df.data.BrkSide.RM %>% 
                  select(LotFrontage) %>% 
                  summarise(median=median(LotFrontage))
              )$median
  
  df.train = FixRL(df.train, df.data.BrkSide.RL)
  df.train = FixRM(df.train, df.data.BrkSide.RM)
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
  
  FixReg = medianFixerMaker(Neighborhood == 'ClearCr' & LotShape == "Reg")
  FixIreg = medianFixerMaker(Neighborhood == 'ClearCr' & LotShape != "Reg")
  
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


FixNaLotFrontage.Edwards = simpleLmFixerMaker(Neighborhood == 'Edwards')
FixNaLotFrontage.Gilbert = simpleLmFixerMaker(Neighborhood == 'Gilbert')
FixNaLotFrontage.IDOTRR  = simpleLmFixerMaker(Neighborhood == 'IDOTRR')


FixNaLotFrontage.MeadowV = function (df.train) {
  df.train[df.train$Neighborhood == 'MeadowV' & df.train$LotAreaSqrt < 47, "LotFrontageCalc"] = 21
  df.train[df.train$Neighborhood == 'MeadowV' & df.train$LotAreaSqrt >= 47 & df.train$LotAreaSqrt < 51, "LotFrontageCalc"] = 34
  df.train[df.train$Neighborhood == 'MeadowV' & df.train$LotAreaSqrt >= 51, "LotFrontageCalc"] = 41
  df.train
}