

loadLibraries <- function () {
    library(broom)
    library(caret)
    library(dplyr)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(Metrics)
    library(purrr)
    library(reshape2)
    library(tidyr)
    library(tibble)
    library(testthat)
}


loadData <- function () {
  df.train <- tbl_df(read.csv(paste0(Sys.getenv('ROOT'), "/data/train.csv"), stringsAsFactors = FALSE))
  df.test <- tbl_df(read.csv(paste0(Sys.getenv('ROOT'), "/data/test.csv"), stringsAsFactors = FALSE))

  df.train$Id <- NULL
  df.test$Id <- NULL
  
  df.train <- df.train %>% mutate(MSSubClass = as.character(MSSubClass))
  df.test <- df.test %>% mutate(MSSubClass = as.character(MSSubClass))

  list(test=df.test, train=df.train)
}


getCombinedDataset <- function (df.train, df.test) {
  rbind(
    df.train %>% mutate(dataSource = "train"), 
    df.test %>% mutate(dataSource = "test", SalePrice = NA)
  )
}


getCategoricalColumnNames <- function (df) {
    df %>% purrr::map(~is.character(.)) %>% purrr::keep(~.) %>% names %>% sort
}

getNumericColumnNames <- function (df) {
    df %>% purrr::map(~is.numeric(.)) %>% purrr::keep(~.) %>% names %>% sort
}

# kaggle.house.loadData.old = function () {
#   df.train <- tbl_df(read.csv("./data/train.csv", stringsAsFactors = FALSE)) %>% mutate(dataSource = "train")
#   df.test <- tbl_df(read.csv("./data/test.csv", stringsAsFactors = FALSE)) %>% mutate(dataSource = "test")
#   
#   df.train <- df.train %>% mutate(MSSubClass = as.character(MSSubClass))
#   df.test <- df.test %>% mutate(MSSubClass = as.character(MSSubClass))
#   
#   df.combined <- rbind(within(df.train, rm('Id','SalePrice')), within(df.test, rm('Id')))
#   list(combined=df.combined, test=df.test, train=df.train)
# }
# 
# 
# get_covariate_hist <- function(df, col_name, type="hist") {
#   
#   col_df = df %>% select_(col_name) %>% na.omit()
#   
#   q_tmpl = paste("stats::quantile(", col_name, ",", "probs=")
#   
#   qs = col_df %>% summarise_(
#     q25=paste(q_tmpl, 0.25, ")"),
#     q75=paste(q_tmpl, 0.75, ")"),
#     q99=paste(q_tmpl, 0.99, ")"),
#     n=~n())
#   q25 = qs[['q25']]
#   q75 = qs[['q75']]
#   q99 = qs[['q99']]
#   n = qs[['n']]
#   bin_width = 2 * (q75 - q25) * (n ** (- 1/3))
#   
#   g = ggplot(col_df %>% filter_(paste(col_name, "<", q99)), aes_string(col_name))
#   
#   if( type == "dens"){
#     g = g + geom_density(fill = "red", alpha = "0.7")
#   } else {
#     g = g + geom_histogram(binwidth=bin_width)
#   }
#   
#   return(g)
# }
