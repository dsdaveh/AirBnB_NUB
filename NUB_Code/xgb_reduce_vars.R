# userf1 must exist ... load externally (and manipulate)
# make sure imp_mat matches

stopifnot( exists("userf1") )
stopifnot( nrow(imp_mat) < ncol(userf1))  #not a perfect indicator

#####  xgb_reduce_vars.R 
# 
#  imp_mat  exists from a previous xgb.R run and contains the importance of the vars
#  keep the first 25 only and retune as an experiment
#
#load(file="imp_mat_2016_01_26_140755.RData")
#load(file='../intermediate_results/xgb_last_full.RData')
gain <- 0
gain_thresh <- .01  # how much gain are we willing to throw out
for( i in nrow(imp_mat):1) {
    gain <- gain + imp_mat$Gain[i]
    if (gain > gain_thresh) break
}
cat ( sprintf("%dth feature and below combine to add on %f gain and will be removed\n", i, gain))
remove_vars <- imp_mat$Feature[i:nrow(imp_mat)]
print(remove_vars)
userf1 <- userf1[,-c(which(colnames(userf1) %in% remove_vars))]
