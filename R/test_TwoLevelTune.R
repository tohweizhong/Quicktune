
# test_TwoLevelTune.R

library(caret)

Xtt <- read.csv("data/adult.csv", header = T, stringsAsFactors = F, strip.white = T)
Xtt$income <- unlist(sapply(Xtt$income, FUN = function(x){
    if(x == ">50K") return("MoreThan50K")
    else if (x == "<=50K") return("NotMoreThan50K")
}))
Xtt$income <- factor(Xtt$income)

tr_idx <- createDataPartition(Xtt$income, p = 0.7, list = F)
Xtrain <- Xtt[tr_idx,]; Xtest <- Xtt[-tr_idx,]



tr_ctrl <- trainControl(method = "boot", number = 1,
                        verboseIter = TRUE, returnData = FALSE)


tg <- expand.grid(nrounds = c(15),
                   eta = c(0.1),
                   max_depth        = seq(1, 1, by = 1),
                   min_child_weight = seq(5, 50, by = 5),
                   colsample_bytree = seq(1, 1, by = 0.1),
                   gamma            = seq(10, 20, by = 10))
nrow(tg)



mods <- TwoLevelTune(.Xytrain = Xtrain, .frml = "income ~.",
                    .prop_config = 0.1, .tr_ctrl1 = tr_ctrl, .tr_ctrl2 = tr_ctrl,
                    .tg1 = tg, .nrounds = seq(100, 1000, by = 100),
                    .eta = c(0.1, 0.05, 0.01), .nthread = 4, .verbose = 1)
