
FixNaLotFrontageNeighborhoodIsBlmngtn = function (df) {
  df %>% mutate(LotFrontageCalc = ifelse(LotAreaSqrt < 59, 43, 53))
}