

fixerMakerFactory = function (patchMaker) {
  
  function (condition) {

    function (df.train, df.data) {
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

simpleLmFixerMaker = fixerMakerFactory(function (data) {
  lm(LotFrontage ~ LotAreaSqrt, data = data)
})

medianFixerMaker = fixerMakerFactory(function (data) {
  median(data$LotFrontage)
})

meanFixerMaker = fixerMakerFactory(function (data) {
  mean(data$LotFrontage)
})


multyFixerFactory = function (...) {
  # cond1, fixerMAker1, cond2, fixermaker2, ...
  # e.g.
  # MSZoning == "RL", simpleLmFixerMaker, MSZoning == "RM", medianFixerMaker
  
  args = as.list(match.call())[-1]
  argsLength = length(args)
  
  function (df.train, df.data) {
    for ( i in 1:(argsLength/2) ) {
      
      condition = args[[2*i - 1]]
      fixerMaker = args[[2*i]]
      df.data.slice = df.data %>% filter(eval(condition))
      fixer = eval(fixerMaker)(condition)
      
      df.train = fixer(df.train, df.data.slice)
    }
    df.train
  }
}


FixNaLotFrontage.Blmngtn = function (df.train) {
  df.train[ eval( substitute(Neighborhood == 'Blmngtn' & LotAreaSqrt < 59), df.train), "LotFrontageCalc"] = 43
  df.train[ eval( substitute(Neighborhood == 'Blmngtn' & LotAreaSqrt >= 59), df.train), "LotFrontageCalc"] = 53
  df.train
}

FixNaLotFrontage.MeadowV = function (df.train) {
  df.train[df.train$Neighborhood == 'MeadowV' & df.train$LotAreaSqrt < 47, "LotFrontageCalc"] = 21
  df.train[df.train$Neighborhood == 'MeadowV' & df.train$LotAreaSqrt >= 47 & df.train$LotAreaSqrt < 51, "LotFrontageCalc"] = 34
  df.train[df.train$Neighborhood == 'MeadowV' & df.train$LotAreaSqrt >= 51, "LotFrontageCalc"] = 41
  df.train
}

FixNaLotFrontage.NPkVill = function (df.train) {
  df.train[ eval( substitute(Neighborhood == 'NPkVill' & LotAreaSqrt < 55), df.train), "LotFrontageCalc"] = 23
  df.train[ eval( substitute(Neighborhood == 'NPkVill' & LotAreaSqrt >= 55), df.train), "LotFrontageCalc"] = 53
  df.train
}


FixNaLotFrontage.BrkSide = multyFixerFactory(
    Neighborhood == 'BrkSide' & MSZoning == "RL", simpleLmFixerMaker, 
    Neighborhood == 'BrkSide' & MSZoning == "RM", medianFixerMaker)

FixNaLotFrontage.ClearCr = multyFixerFactory(
    Neighborhood == 'ClearCr' & LotShape2 == "Reg", meanFixerMaker, 
    Neighborhood == 'ClearCr' & LotShape2 != "Reg", meanFixerMaker)

FixNaLotFrontage.CollgCr = multyFixerFactory(
  Neighborhood == 'CollgCr' & LotShape2 == "Reg", simpleLmFixerMaker, 
  Neighborhood == 'CollgCr' & LotShape2 == "Ireg", medianFixerMaker)

FixNaLotFrontage.Crawfor = multyFixerFactory(
  Neighborhood == 'Crawfor' & LotShape2 == "Reg", simpleLmFixerMaker, 
  Neighborhood == 'Crawfor' & LotShape2 == "Ireg", simpleLmFixerMaker)

FixNaLotFrontage.NoRidge = multyFixerFactory(
  Neighborhood == 'NoRidge' & LotShape2 == "Reg", simpleLmFixerMaker, 
  Neighborhood == 'NoRidge' & LotShape2 == "Ireg", simpleLmFixerMaker)


FixNaLotFrontage.Edwards = simpleLmFixerMaker(substitute(Neighborhood == 'Edwards'))
FixNaLotFrontage.Gilbert = simpleLmFixerMaker(substitute(Neighborhood == 'Gilbert'))
FixNaLotFrontage.IDOTRR  = simpleLmFixerMaker(substitute(Neighborhood == 'IDOTRR'))
FixNaLotFrontage.Mitchel  = simpleLmFixerMaker(substitute(Neighborhood == 'Mitchel'))

