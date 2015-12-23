# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# ====

library(caret)


# TwoLevelTune: function to invoke the two-level tuning method for xgb
# Works for classification only for now
# Using non-xy convention
# learn to use ... for additional arguments for caret::train()


# input:
# @ .Xytrain: data.frame consisting of the training set (with response var)
# @ .frml: formula of model
# @ .prop_config: (0, 1], top proportion of hyperparameter configuration to take to second level tuning
# @ .tr_ctrl1: caret::trainControl() object from caret for first level tuning
# @ .tr_ctrl2: caret::trainControl() object from caret for second level tuning
# @ .objective: objective parameter in xgboost
# @ .tg1: expand.grid(), first level tuning grid
# @ .nrounds: vector for number of rounds to tune
# @ .eta: vector for various eta to tune
# @ .nthread: nthread argument for caret::train()
# @ .verbose: verbose argument for caret::train()


TwoLevelTune <- function(.Xytrain, .frml, .prop_config,
                         .tr_ctrl1, .tr_ctrl2, .objective,
                         .tg1, .nrounds, .eta, .nthread = 4, .verbose, .metric = "Accuracy"){

    require(caret)

    cat("==== First level tuning ====\n")

    xgb1 <- train(form = formula(.frml),
                  data = .Xytrain,
                  method = "xgbTree",
                  trControl = .tr_ctrl1,
                  tuneGrid = .tg1,
                  objective = .objective,
                  nthread = .nthread,
                  verbose = .verbose,
                  metric = .metric)

    tmp <- which(colnames(xgb1$results) == .metric)

    tune_df1 <- xgb1$results[order(xgb1$results[,tmp], decreasing = T),]

    View(tune_df1)

    num_config <- ceiling(.prop_config * nrow(tune_df1))
    tune_df1 <- tune_df1[seq(1:num_config),]
    #tune_df1 <- subset(tune_df1, select = -c(nrounds, eta, Accuracy, AccuracySD, Kappa, KappaSD))

    # populate second tuning grid
    tg2 <- list()
    for(i in seq(nrow(tune_df1))){
        for(nr in .nrounds){
            for(et in .eta){
                tmp <- as.vector(c(tune_df1[i,c("max_depth", "gamma", "colsample_bytree", "min_child_weight")], nr, et))
                tg2 <- list.push(tg2, tmp)
            }
        }
    }

    tg2 <- do.call(rbind.data.frame, tg2)
    colnames(tg2) <- c("max_depth", "gamma", "colsample_bytree",
                       "min_child_weight", "nrounds", "eta")

    cat("==== Second level tuning ====\n")

    xgb2 <- train(form = formula(.frml),
                  data = .Xytrain,
                  method = "xgbTree",
                  trControl = .tr_ctrl2,
                  tuneGrid = tg2,
                  objective = .objective,
                  nthread = .nthread,
                  verbose = .verbose,
                  metric = .metric)

    return(list(mod1 = xgb1, mod2 = xgb2))
}
