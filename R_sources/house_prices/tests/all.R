setwd(paste0(Sys.getenv('R_SOURCES'), '/house_prices'))

library(testthat, warn.conflicts=FALSE)
testthat::test_dir('./tests')

