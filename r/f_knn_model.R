# knn model

knn_model <- function(mod, pp, k) {
  
  # Fit KNN model
  mod_knn <- caret::train(
    pp, 
    data = mod |> drop_na(), 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "RMSE"
  )
  
  mod_knn
  
}