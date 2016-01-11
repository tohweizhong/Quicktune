
# test_TwoLevelTune.R
# test on adult dataset

library(caret)

Xtt <- read.csv("data/adult.csv", header = T, stringsAsFactors = F, strip.white = T)
Xtt$income <- unlist(sapply(Xtt$income, FUN = function(x){
    if(x == ">50K") return("MoreThan50K")
    else if (x == "<=50K") return("NotMoreThan50K")
}))
Xtt$income <- factor(Xtt$income)

tr_idx <- createDataPartition(Xtt$income, p = 0.7, list = F)
Xtrain <- Xtt[tr_idx,]; Xtest <- Xtt[-tr_idx,]


# ====
# tlt
tlt <- function(){
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE,
                            classProbs = TRUE, summaryFunction = twoClassSummary)
    tg <- expand.grid(nrounds = c(15),
                      eta = c(0.1),
                      max_depth        = seq(1, 10, by = 1),
                      min_child_weight = seq(5, 50, by = 5),
                      colsample_bytree = seq(0.8, 1, by = 0.1),
                      gamma            = seq(0, 120, by = 20))
    nrow(tg)
    mods <- TwoLevelTune(data = Xtrain, frml = "income ~.",
                         prop_config = 0.2, tr_ctrl1 = tr_ctrl, tr_ctrl2 = tr_ctrl,
                         tg1 = tg, nrounds = seq(100, 1000, by = 100),
                         eta = c(0.1, 0.05, 0.01), nthread = 4, verbose = 1, metric = "ROC")
}
tlt.time <- system.time(tlt())

# ====
# Exhaustive search
exh <- function(){
    
    tr_ctrl <- trainControl(method = "boot", number = 1,
                            verboseIter = TRUE, returnData = FALSE,
                            classProbs = TRUE, summaryFunction = twoClassSummary)
    
    tg <- expand.grid(nrounds = seq(100, 1000, by = 100),
                      eta = c(0.1, 0.05, 0.01),
                      max_depth        = seq(1, 10, by = 1),
                      min_child_weight = seq(5, 50, by = 5),
                      colsample_bytree = seq(0.8, 1, by = 0.1),
                      gamma            = seq(0, 120, by = 20))
    nrow(tg)
    
    xgb <- train(form = formula(frml),
                 data = Xtrain,
                 method = "xgbTree",
                 trControl = tr_ctrl,
                 tuneGrid = tg,
                 objective = "binary:logistic",
                 nthread = 4,
                 verbose = 1,
                 metric = "ROC")
}
exh.time <- system.time(exh())
