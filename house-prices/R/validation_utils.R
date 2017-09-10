

kaggle.house.validation.do_many_partitions <- function (data, formula, p, numrep) {
  
  fn <- function (i) { 
    train_index <- caret::createDataPartition(
      data[, toString(terms(formula)[[2]])] %>% `[[`(1), 
      p=0.8, times=1, list=FALSE)
    df.train <- data[train_index,]
    model.lm <- lm(LotFrontage.log ~ LotArea.log, data = df.train)
    list(model = model.lm, train_index = train_index)
  }
  
  1:200 %>% map(fn)
}


kaggle.house.validation.get_train_test_score <- function (data, model, index) {
  df.train <- data[index,]
  df.test <- data[-index,]
  
  y_test_predicted <- predict(model, df.test)
  y_test_actual <- df.test$LotFrontage.log
  
  train_score <- sum(model$residuals^2) / length(model$residuals)
  test_score <- sum( (y_test_predicted - y_test_actual)^2 ) / length(y_test_actual)
  
  list(train_score=train_score, test_score=test_score)
}


kaggle.house.validation.get_scores <- function (model_index_pairs, data) {
  scores <- model_index_pairs %>% map(~kaggle.house.validation.get_train_test_score(data, .$model, .$train_index))
  do.call(rbind.data.frame, scores)
}