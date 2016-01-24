
PlotTune <- function(tunedf){
    
    require(ggplot2)
    require(gridExtra)
    
    metric_name <- colnames(tunedf)[7]
    
    # first seven columns of tunedf
    q1 <- qplot(y = tunedf[,7], x = tunedf[,1]) + geom_smooth(method = lm, aes(y = tunedf[,7], x = tunedf[,1])) + ylab(metric_name) + xlab("eta") + theme_bw()
    q2 <- qplot(y = tunedf[,7], x = tunedf[,2]) + geom_smooth(method = lm, aes(y = tunedf[,7], x = tunedf[,2])) + ylab(metric_name) + xlab("max_depth") + theme_bw()
    q3 <- qplot(y = tunedf[,7], x = tunedf[,3]) + geom_smooth(method = lm, aes(y = tunedf[,7], x = tunedf[,3])) + ylab(metric_name) + xlab("gamma") + theme_bw()
    q4 <- qplot(y = tunedf[,7], x = tunedf[,4]) + geom_smooth(method = lm, aes(y = tunedf[,7], x = tunedf[,4])) + ylab(metric_name) + xlab("colsample_bytree") + theme_bw()
    q5 <- qplot(y = tunedf[,7], x = tunedf[,5]) + geom_smooth(method = lm, aes(y = tunedf[,7], x = tunedf[,5])) + ylab(metric_name) + xlab("min_child_weight") + theme_bw()
    q6 <- qplot(y = tunedf[,7], x = tunedf[,6]) + geom_smooth(method = lm, aes(y = tunedf[,7], x = tunedf[,6])) + ylab(metric_name) + xlab("nrounds") + theme_bw()
    
    grid.arrange(q1, q2, q3, q4, q5, q6, ncol = 3)
    
}
