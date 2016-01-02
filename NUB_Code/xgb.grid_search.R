# load libraries
library(xgboost)
library(stringr)
library(caret)
library(car)
library(plyr)
library(dplyr)
library(lubridate)
library(data.table)
library(bit64)

source('nub_utils.R')
if (! exists("userf1")) load(file="../userf1.RData") # source('features.R')

tcheck(0) ####
### parameters
if ( exists("set_run_id") ) {
    run_id <- set_run_id
    rm( set_run_id )
} else {
    run_id <- format(Sys.time(), "xgb_grid_search_%Y_%m_%d_%H%M%S")
}
tcheck.print <- TRUE
set.seed(1)
kfold <- 5   #set to -1 to skip
only1 <- TRUE  
create_csv <- TRUE

xgb_params <- list( 
    eta = 0.1,
    max_depth = 9, 
    subsample = 0.5,
    colsample_bytree = 0.5,
    eval_metric = "merror",
    objective = "multi:softprob",
    num_class = 12,
    nthreads = 4
    )
xgb_nrounds <- 53
###

# prep
tcheck( desc="begin data prep") ####

# split train and test
X <- userf1 %>% filter( source == "train")
labels <- as.character( X$country_destination )
X$country_destination <- NULL
X$source <- NULL

y <- recode(labels,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")

X_test <- userf1 %>% filter( source == "test")
X_test$country_destination <- NULL
X_test$source <- NULL

ix_shuffle <- sample( 1: nrow(X))
ix_upper <- 0
ix_inc <- ceiling( length(ix_shuffle) / kfold )
ho_scores <- numeric(kfold)

top5_preds <- function (xgb_pred) {
    predictions <- as.data.frame(matrix(xgb_pred, nrow=12))
    rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
    as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
}

tcheck( desc="begin kfold cross validation scores") ####
# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
    nrounds = 1000,
    eta = c(0.2, 0.1, 0.01, 0.001),
    max_depth = c(2, 4, 6, 8, 10)
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    returnData = FALSE,
    returnResamp = "all",                                                        # save losses across all models
    allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
levels(X$dev_last) <- c(levels(X$dev_last), "NA")
levels(X$dev_first) <- c(levels(X$dev_first), "NA")
levels(X$age_check) <- c(levels(X$age_check), "NA")

for (icol in 2:ncol(X)) {
    na_idx <- which( is.na( X[, icol] ))
    cat("Col ",icol," has ", length(na_idx), "NAs\n")
    if ( class(  X[[1,icol]] ) == "integer") X[ na_idx, icol] <- -99
    if ( class(  X[[1,icol]] ) == "numeric") X[ na_idx, icol] <- -99
    if ( class(  X[[1,icol]] ) == "factor") X[ na_idx, icol] <- "NA"
}
xgb_train_1 = train(
    x = data.matrix(X[ ,-1]),
    y = y,
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree"
)
# Aggregating results
# Selecting tuning parameters
# Fitting nrounds = 1000, max_depth = 4, eta = 0.01 on full training set

# This section is taken for the stack exchange example - TODO: adapt and replace
# scatter plot of the AUC against max_depth and eta
# ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
#     geom_point() + 
#     theme_bw() + 
#     scale_size_continuous(guide = "none")

# Use the settings above to modify the params
xgb_params_2 <- list( 
    eta = 0.01,                      #from xgb_train_1
    max_depth = 4,                   #from xgb_train_1
    subsample = 0.5,
    colsample_bytree = 0.5,
    eval_metric = "merror",
    objective = "multi:softprob",
    num_class = 12,
    nthreads = 4
)

cv <- xgb.cv(data = data.matrix(X[ ,-1]) #, missing = NA
               , label = y
               , params = xgb_params_2
               , nrounds = 1000
               , early.stop.round = 10
               , nfold = 10
)
# Stopping. Best iteration: 3    ### Seriousl??

## predict whether this will do better before submitting  70/30 
set.seed(168); tcheck()
scores <- numeric()
for (i in 1:7) {
    idx_trn <- sample( 1:nrow(X), round(.7 * nrow(X)))
    xgb.trn <- xgboost(data = data.matrix(X[ idx_trn,-1]) #, missing = NA
                       , label = y[ idx_trn ]
                       , params = xgb_params_2
                       , nrounds = 3  
    )
    y_ho_pred <- predict(xgb.trn, data.matrix(X[ -idx_trn,-1]))
    y_ho_top5 <- as.data.frame( matrix( top5_preds( y_ho_pred ), ncol=5, byrow = TRUE)) %>% tbl_df
    truth_ho <- labels[ -idx_trn]
    y_ho_score <- score_predictions( y_ho_top5, truth_ho)
    scores[i] <- mean( score_predictions( y_ho_top5, truth_ho) )
    print( scores[i]); tcheck()
}
# [1] 0.8433242 0.8436520 0.8402001 0.8389522 0.8417098 0.8401625 0.8394671
mean(scores) # 0.8410669
sd(scores) # 0.001861524

# 1/5 score for this model and orig params was 0.849507  ... so this looks to be an improvement
#train on the full model
xgb <- xgboost(data = data.matrix(X[ ,-1]) #, missing = NA
                   , label = y[  ]
                   , params = xgb_params_2
                   , nrounds = 3  
); tcheck()

imp_mat <- xgb.importance( feature_names = colnames(X)[-1], model=xgb); tcheck()
xgb.plot.importance(imp_mat)
print(imp_mat)

y_trn_pred <- predict(xgb, data.matrix(X[,-1])) #, missing = NA)
y_trn_top5 <- as.data.frame( matrix( top5_preds( y_trn_pred ), ncol=5, byrow = TRUE)) %>% tbl_df
y_trn_score <- score_predictions( y_trn_top5, labels)
cat( sprintf( "Mean score (full training set)= %f\n", mean(y_trn_score)) ) ; tcheck()  #0.842840  
# compare above to 0.880791 and it seems it would be an improvement (still doesn't pass sniff test because nrounds=3)

# Test
levels(X_test$dev_last) <- c(levels(X_test$dev_last), "NA")
levels(X_test$dev_first) <- c(levels(X_test$dev_first), "NA")
levels(X_test$age_check) <- c(levels(X_test$age_check), "NA")

for (icol in 2:ncol(X_test)) {
    na_idx <- which( is.na( X_test[, icol] ))
    cat("Col ",icol," has ", length(na_idx), "NAs\n")
    if ( class(  X_test[[1,icol]] ) == "integer") X_test[ na_idx, icol] <- -99
    if ( class(  X_test[[1,icol]] ) == "numeric") X_test[ na_idx, icol] <- -99
    if ( class(  X_test[[1,icol]] ) == "factor") X_test[ na_idx, icol] <- "NA"
}

y_pred <- predict(xgb, data.matrix(X_test[,-1])) #, missing = NA)
y_top5 <- top5_preds( y_pred )
submission <- data.frame( id= rep(X_test$id, each=5), country=y_top5)
subfile <- sprintf("../submissions/submission_%s.csv", run_id)
write.csv(submission, file=subfile , quote=FALSE, row.names = FALSE); tcheck( desc= subfile)
# Kaggle = 0.86842  (versus 0.87204) so no improvement after all

