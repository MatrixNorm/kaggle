

universalFixer = function (patchMaker) {
  
  function (condition) {
    #condition_call = substitute(condition)
    
    function (df.train, df.data) {
      #browser()
      df.data.subset  = df.data [ eval(condition, df.data), ]
      df.train.subset = df.train[ eval(condition, df.train), ]

      patch = patchMaker(df.data.subset)
      if ( class(patch) == "lm" ) {
        patch = predict(patch, df.train.subset)
      }
      df.train[ eval(condition, df.train), "LotFrontageCalc"] = patch
      df.train
    }
  }
}


simpleLmFixerMaker = universalFixer(function (data) {
  lm(LotFrontage ~ LotAreaSqrt, data = data)
})

medianFixerMaker = universalFixer(function (data) {
  median(data$LotFrontage)
})


meanFixerMaker = universalFixer(function (data) {
  mean(data$LotFrontage)
})

FixNaLotFrontage.Blmngtn = function (df.train) {
  df.train[ eval( substitute(Neighborhood == 'Blmngtn' & LotAreaSqrt < 59), df.train), "LotFrontageCalc"] = 43
  df.train[ eval( substitute(Neighborhood == 'Blmngtn' & LotAreaSqrt >= 59), df.train), "LotFrontageCalc"] = 53
  df.train
}


qwerty = function (...) {
  # cond1, fixerMAker1, cond2, fixermaker2, ...
  # e.g.
  # MSZoning == "RL", simpleLmFixerMaker, MSZoning == "RM", medianFixerMaker
  
  args = as.list(match.call())[-1]
  argsLength = length(args)
  
  function (df.train, df.data) {
    for ( i in 1:(argsLength/2) ) {
      
      condition = args[[2*i - 1]]
      fixerMaker = args[[2*i]]
      #browser()
      df.data.slice = df.data %>% filter(eval(condition))
      fixer = eval(fixerMaker)(condition)
      
      df.train = fixer(df.train, df.data.slice)
    }
    df.train
  }
}

FixNaLotFrontage.BrkSide = qwerty(Neighborhood == 'BrkSide' & MSZoning == "RL", simpleLmFixerMaker, 
                                  Neighborhood == 'BrkSide' & MSZoning == "RM", medianFixerMaker)

# FixNaLotFrontage.BrkSide = function (df.train, df.data.BrkSide) {
#   
#   df.data.BrkSide.RL = df.data.BrkSide %>% filter(MSZoning == "RL")
#   df.data.BrkSide.RM = df.data.BrkSide %>% filter(MSZoning == "RM")
#   
#   FixRL = simpleLmFixerMaker(Neighborhood == 'BrkSide' & MSZoning == "RL")
#   FixRM = medianFixerMaker(Neighborhood == 'BrkSide' & MSZoning == "RM")
#   
#   df.train = FixRL(df.train, df.data.BrkSide.RL)
#   df.train = FixRM(df.train, df.data.BrkSide.RM)
#   df.train
# }


FixNaLotFrontage.ClearCr = function (df.train, df.data) {
  
  df.data.Reg = df.data %>% filter(LotShape2 == "Reg")
  df.data.Ireg = df.data %>% filter(LotShape2 != "Reg")
  
  FixReg = meanFixerMaker(Neighborhood == 'ClearCr' & LotShape2 == "Reg")
  FixIreg = meanFixerMaker(Neighborhood == 'ClearCr' & LotShape2 != "Reg")
  
  df.train = FixReg(df.train, df.data.Reg)
  df.train = FixIreg(df.train, df.data.Ireg)
  df.train
}


FixNaLotFrontage.CollgCr = function (df.train, df.data) {
  
  df.data.Reg  = df.data %>% filter(LotShape2 == "Reg")
  df.data.Ireg = df.data %>% filter(LotShape2 == "Ireg")
  
  FixReg = simpleLmFixerMaker(Neighborhood == 'CollgCr' & LotShape2 == "Reg")
  FixIreg = medianFixerMaker(Neighborhood == 'CollgCr' & LotShape2 == "Ireg")
  
  df.train = FixReg(df.train, df.data.Reg)
  df.train = FixIreg(df.train, df.data.Ireg)
  df.train
}


FixNaLotFrontage.Crawfor = function (df.train, df.data) {
  
  df.data.Reg  = df.data %>% filter(LotShape2 == "Reg")
  df.data.Ireg = df.data %>% filter(LotShape2 == "Ireg")
  
  FixReg = simpleLmFixerMaker(Neighborhood == 'Crawfor' & LotShape2 == "Reg")
  FixIreg = simpleLmFixerMaker(Neighborhood == 'Crawfor' & LotShape2 == "Ireg")
  
  df.train = FixReg(df.train, df.data.Reg)
  df.train = FixIreg(df.train, df.data.Ireg)
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