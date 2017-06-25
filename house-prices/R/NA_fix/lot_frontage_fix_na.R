
FixNaLotFrontageNeighborhoodIsBlmngtn = function (df) {
  df %>%
    mutate(LotFrontageCalc = ifelse(Neighborhood == 'Blmngtn', 
                                      ifelse(LotAreaSqrt < 59, 43, 53), 
                                      LotFrontageCalc))
}

FixNaLotFrontageNeighborhoodIsBrkSide = function (df) {
  df %>%
    mutate(LotFrontageCalc = ifelse(Neighborhood == 'Blueste', 
                                    ifelse(LotAreaSqrt < 59, 24, 35), 
                                    LotFrontageCalc))
}