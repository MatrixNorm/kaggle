
FixNaLotFrontageNeighborhoodIsBlmngtn = function (df.train) {
  df.train %>%
    mutate(LotFrontageCalc = ifelse(Neighborhood == 'Blmngtn', 
                                      ifelse(LotAreaSqrt < 59, 43, 53), 
                                      LotFrontageCalc))
}

FixNaLotFrontageNeighborhoodIsBrkSide = function (df.train, df.data) {
  
  df.train.BrkSide = df.train %>% filter(Neighborhood == 'BrkSide')
  df.train.BrkSide.RL = df.train.BrkSide %>% filter(MSZoning == "RL")
  df.train.BrkSide.RM = df.train.BrkSide %>% filter(MSZoning == "RM")
  
  df.data.BrkSide = df.data %>% filter(Neighborhood == 'BrkSide')
  df.data.BrkSide.RL = df.data.BrkSide %>% filter(MSZoning == "RL")
  df.data.BrkSide.RM = df.data.BrkSide %>% filter(MSZoning == "RM")
  
  lm.BrkSide.RL = lm(LotFrontage ~ LotAreaSqrt, data = df.data.BrkSide.RL)
  median.RM = (df.data.BrkSide.RM %>% 
                  select(LotFrontage) %>% 
                  summarise(median=median(LotFrontage))
              )$median
  
  df.train %>%
    mutate(LotFrontageCalc = 
             ifelse( Neighborhood == 'BrkSide', 
                                
                  ifelse( MSZoning == "RL",
                      
                      predict(lm.BrkSide.RL, df.train.BrkSide.RL), 
                      
                      ifelse( MSZoning == "RM",
                          median.RM,
                          LotFrontageCalc
                      )
                  ),
                  LotFrontageCalc
             )
    )
}