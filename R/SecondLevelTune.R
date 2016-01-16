
# SecondLevelTune: function to invoke second level tuning from TLT

# input:
# @ data: data.frame consisting of the training set (with response var)
# @ frml: formula of model
# @ prop_config: (0, 1], top proportion of hyperparameter configuration to take to second level tuning
# @ tr_ctrl1: caret::trainControl() object from caret for first level tuning
# @ tr_ctrl2: caret::trainControl() object from caret for second level tuning
# @ objective: objective parameter in xgboost
# @ tg1: expand.grid(), first level tuning grid
# @ nrounds: vector for number of rounds to tune
# @ eta: vector for various eta to tune
# @ nthread: nthread argument for caret::train()
# @ verbose: verbose argument for caret::train()
# @ metric: metric argument for caret::train()

SecondLevelTune <- function(data, frml,
                            tr_ctrl2,
                            objective = "binary:logistic", tg2,
                            nthread = 4, verbose = 1,
                            metric = "Accuracy"){
    
    cat("Remember to set the classProbs and summary function in trainControl() for classification problems\n")
    
    cat("==== Second level tuning ====\n")
    
    xgb2 <- train(form = formula(frml),
                  data = data,
                  method = "xgbTree",
                  trControl = tr_ctrl2,
                  tuneGrid = tg2,
                  objective = objective,
                  nthread = nthread,
                  verbose = verbose,
                  metric = metric)
    
    cat("==== Second level tuning completed ====\n")
    
    return(list(mod2 = xgb2))
}
