

kaggle.house.validation.do_many_partitions <- function (data, formula, p, numrep) {
  
  fn <- function (i) { 
    train_index <- caret::createDataPartition(
      data[, toString(terms(formula)[[2]])] %>% `[[`(1), 
      p=p, times=1, list=FALSE)
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


kaggle.house.validation.plot_scores <- function (scores) {
  
  scores %>%
    ggplot() +
    geom_point(aes(x=train_score, y=test_score), alpha=0.1, color="black", size=1) +
    theme_bw() -> p1
  
  scores %>%
    ggplot() +
    geom_density(aes(train_score), fill = "red", alpha = "0.2") +
    geom_density(aes(test_score), fill = "blue", alpha = "0.2") +
    theme_bw() -> p2
  
  scores %>%
    ggplot() +
    geom_point(aes(seq_along(test_score), test_score), color="black", alpha=0.5) +
    geom_point(aes(seq_along(train_score), train_score), color="blue", alpha=0.5) +
    theme_bw() -> p3
  
  grid.arrange(p1, p2, p3,
               layout_matrix=rbind(c(1, 2, 3))
  )
  
}