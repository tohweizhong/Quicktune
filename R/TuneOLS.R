
# TuneOLS

TuneOLs <- function(xgb){
    
    results <- xgb$results[,1:7]
    tune_ols <- lm(data = xgb$results[,1:7], xgb$results[,7] ~ eta + max_depth + gamma + colsample_bytree + min_child_weight + nrounds)
    return(tune_ols)
    
}