
eval_sets <- evaluationScheme(data = steam_matrix, method = "split",
                              train = percentage_training, given = 5, goodRating = rating_threshold, k = n_eval) 

#Горизонтальное нормирование, IBCF, cosine

model_to_evaluate <- "IBCF"
model_parameters <- list(method = "cosine")

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy


RMSE      MSE      MAE 
2.143687 4.595393 1.496097 


#Горизонтальное нормирование, IBCF, pearson

model_to_evaluate <- "IBCF"
model_parameters <- list(method = "pearson")

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy


RMSE      MSE      MAE 
1.824452 3.328624 1.495561 



#Горизонтальное нормирование, UBCF, cosine

model_to_evaluate <- "UBCF"
model_parameters <- list(method = "cosine")

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy

RMSE      MSE      MAE 
1.534847 2.355754 1.319176 



#Горизонтальное нормирование, UBCF, pearson

model_to_evaluate <- "UBCF"
model_parameters <- list(method = "pearson")

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy

RMSE      MSE      MAE 
1.529873 2.340510 1.312541


#Горизонтальное нормирование, POPULAR, cosine

model_to_evaluate <- "POPULAR"
model_parameters <- list(method = "cosine")

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy

RMSE       MSE       MAE 
1.0553994 1.1138678 0.8787259


#Горизонтальное нормирование, POPULAR, pearson

model_to_evaluate <- "POPULAR"
model_parameters <- list(method = "pearson")

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy
RMSE       MSE       MAE 
1.0553994 1.1138678 0.8787259 





#вертикальное нормировапние
sqmatrix1 = sqmatrix
for (i in (1:dim(sqmatrix)[2])) {
  sqmatrix1[,i] = cut(sqmatrix[,i], breaks = unique(quantile(sqmatrix[,i], c(.0, .20, .40, .60, 0.80, 1), na.rm = TRUE)),include.lowest = TRUE) }


steam_matrix_v <- as(sqmatrix1, "realRatingMatrix")




eval_sets_v <- evaluationScheme(data = steam_matrix_v, method = "split",
                              train = percentage_training, given = 5, goodRating = rating_threshold, k = n_eval) 


#Вертикальное нормирование, IBCF, cosine

model_to_evaluate <- "IBCF"
model_parameters <- list(method = "cosine")

eval_recommender <- Recommender(data = getData(eval_sets_v, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy


RMSE       MSE       MAE 
1.1238255 1.2629837 0.8127017 


#Вертикальное нормирование, IBCF, peasron

model_to_evaluate <- "IBCF"
model_parameters <- list(method = "pearson")

eval_recommender <- Recommender(data = getData(eval_sets_v, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy


RMSE      MSE      MAE 
1.172073 1.373755 0.863341



#Вертикальное нормирование, UBCF, cosine

model_to_evaluate <- "UBCF"
model_parameters <- list(method = "cosine")

eval_recommender <- Recommender(data = getData(eval_sets_v, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy

RMSE       MSE       MAE 
1.0400984 1.0818047 0.8738766 



#Вертикальное нормирование, UBCF, pearson

model_to_evaluate <- "UBCF"
model_parameters <- list(method = "pearson")

eval_recommender <- Recommender(data = getData(eval_sets_v, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy

RMSE       MSE       MAE 
1.0541992 1.1113361 0.8934165




#Вертикальное нормирование, POPULAR, cosine

model_to_evaluate <- "POPULAR"
model_parameters <- list(method = "cosine")

eval_recommender <- Recommender(data = getData(eval_sets_v, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy

RMSE       MSE       MAE 
1.0347990 1.0708090 0.8558049




#Вертикальное нормирование, POPULAR, pearson

model_to_evaluate <- "POPULAR"
model_parameters <- list(method = "pearson")

eval_recommender <- Recommender(data = getData(eval_sets_v, "train"),
                                method = model_to_evaluate, parameter = model_parameters)
items_to_recommend <- 6
eval_prediction <- predict(object = eval_recommender, newdata =
                             getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")

eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction, data = getData(eval_sets, "unknown"), byUser =
    FALSE)
eval_accuracy


RMSE       MSE       MAE 
1.0484830 1.0993166 0.8729619
