
# TuneOLS

TuneOLS <- function(xgb, metric){
    params <- c("eta", "max_depth", "gamma", "colsample_bytree", "min_child_weight", "subsample", "nrounds")
    tune_ols <- lm(data = xgb$results, xgb$results[,metric] ~ eta + max_depth + gamma + colsample_bytree + min_child_weight + subsample + nrounds)
    return(tune_ols)
    
}