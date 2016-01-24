
# Function to populate a tuning grid
# based on the OLS model built on previous tuning results

PopulateNext <- function(tg0, xgb){
    
    require(Standard)
    
    # define granularity of hyperparameters and factor to traverse by
    gran <- c(0.01, 1, 1, 0.1, 5, 50)
    names(gran) <- c("eta", "max_depth", "gamma", "colsample_bytree", "min_child_weight", "nrounds")
    k <- 1
    
    # rearrange columns in tg0
    tg0 <- subset(tg0, select = c(eta, max_depth, gamma, colsample_bytree, min_child_weight, nrounds))
    params <- colnames(tg0)
    
    # construct OLS model
    ols <- lm(data = xgb$results[,1:7], xgb$results[,7] ~ eta + max_depth + gamma + colsample_bytree + min_child_weight + nrounds)
    
    coe <- ols$coefficients[-1]
    pvals <- summary(ols)$coefficients[-1,4]
    
    # check for any NAs in the model
    if(length(coe) != length(pvals)){
        
        
        idx <- which(is.na(match(names(coe), names(pvals))))
        tmp <- append(pvals[1:idx-1], NA)
        pvals <- append(tmp, pvals[idx:length(pvals)])
        names(pvals) <- params
    }
    
    directn <- sign(coe)
    
    # populate second tuning grid
    # can populate based on mean or
    # based on granularity
    tg1 <- list()
    for(i in seq_along(coe)){
        
        col <- tg0[,i]
        if(is.na(pvals[i])) tg1 <- PushList(tg1, col)
        else if(pvals[i] > 0.05) tg1 <- PushList(tg1, col)
        else if(pvals[i] <= 0.05){
            
            # traverse
            g <- gran[which(names(gran) == params[i])]
            if(directn[i] == 1) col <- col + (k * g) # positive coefficient
            else if(directn[i] == -1) col <- col - k * g # negative coefficient
            
            tg1 <- PushList(tg1, col)
        }
        
    }
    tg1 <- do.call(cbind.data.frame, tg1)
    colnames(tg1) <- params
    
    # because colsample_bytree has a range of (0.1],
    # need to check whether this is fulfiled in
    # the tuning grid
    
    CheckColSampleByTree <- function(tg1){
        if(min(tg1$colsample_bytree) > 0 && max(tg1$colsample_bytree) <= 1){
            return(tg1)
        }
        
        # four conditions , but two of them should not happen at all
        else if(min(tg1$colsample_bytree) < 0){
            if(coe["colsample_bytree"] < 0){
                idx <- which(tg1$colsample_bytree < 0)
                tg1$colsample_bytree[idx] <- 0.1  
            }
            else if(coe["colsample_bytree"] > 0){ # this case should not happen
                idx <- which(tg1$colsample_bytree < 0)
                tg1$colsample_bytree[idx] <- 0.1              
            }
        }
        else if(max(tg1$colsample_by) > 1){
            if(coe["colsample_bytree"] < 0){ # this case should not happen
                idx <- which(tg1$colsample_bytree > 1)
                tg1$colsample_bytree[idx] <- 1    
            }
            else if(coe["colsample_bytree"] > 0){
                idx <- which(tg1$colsample_bytree > 1)
                tg1$colsample_bytree[idx] <- 1
            }
        }
        return(tg1)
    }
    tg1 <- CheckColSampleByTree(tg1)
    
    # Sanity check for the other hyperparameters
    # Cannot be negative or zero
    
    # leaving out gamma
    
    tmp_min <- min(tg1$eta)
    if(tmp_min <= 0) tg1$eta[which(tg1$eta == tmp_min)] <- gran["eta"]
    
    tmp_min <- min(tg1$max_depth)
    if(tmp_min <= 0) tg1$max_depth[which(tg1$max_depth == tmp_min)] <- gran["max_depth"]
    
    tmp_min <- min(tg1$colsample_bytree)
    if(tmp_min <= 0) tg1$colsample_bytree[which(tg1$colsample_bytree == tmp_min)] <- gran["colsample_bytree"]
    
    tmp_min <- min(tg1$min_child_weight)
    if(tmp_min <= 0) tg1$min_child_weight[which(tg1$min_child_weight == tmp_min)] <- gran["min_child_weight"]
    
    tmp_min <- min(tg1$nrounds)
    if(tmp_min <= 0) tg1$nrounds[which(tg1$nrounds == tmp_min)] <- gran["nrounds"]
    
    return(tg1)
}

#tg3 <- PopulateNext(tg0 = tg1, xgb = xgb_tlt)
