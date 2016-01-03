# load libraries
library(xgboost)
library(stringr)
library(caret)
library(car)
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
    run_id <- format(Sys.time(), "xgb_%Y_%m_%d_%H%M%S")
}
tcheck.print <- TRUE
set.seed(1)
kfold <- 5   #set to -1 to skip
only1 <- TRUE  
create_csv <- TRUE

xgb_params <- list( 
    eta = 0.01,      # was .1
    max_depth = 4,   # was 9
    subsample = 0.5,
    colsample_bytree = 0.5,
    eval_metric = "merror",
    objective = "multi:softprob",
    num_class = 12,
    nthreads = 4
    )
xgb_nrounds <- 360   #was 53
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
for (i in 1:kfold) {
    
    # split out a hold out set
    ix_lower <- ix_upper + 1
    ix_upper <- ifelse( i == kfold, length(ix_shuffle), ix_lower + ix_inc - 1)
    iho <- ix_shuffle[ix_lower:ix_upper]

    # train xgboost
    xgb <- xgboost(data = data.matrix(X[-iho ,-1]) , missing = NA
                   , label = y[-iho]
                   , params = xgb_params
                   , nrounds = xgb_nrounds  
    )

    # predict values in hold out set
    y_ho_pred <- predict(xgb, data.matrix(X[iho,-1]), missing = NA)
    y_ho_top5 <- as.data.frame( matrix( top5_preds( y_ho_pred ), ncol=5, byrow = TRUE)) %>% tbl_df
    truth_ho <- labels[iho]
    y_ho_score <- score_predictions( y_ho_top5, truth_ho)
    pred_eval <- y_ho_top5 %>% bind_cols( data.frame(truth_ho, y_ho_score))
    
    ho_scores[i] <- mean(y_ho_score)
    cat( sprintf( "%d/%d: Mean score = %f\n", i, kfold, ho_scores[i]) ) ; tcheck()
    
    if (only1) break
}

if (i > 1) {
    tcheck( t=kfold, desc= 'K-f cross validation')
    # [1] "303.310000 elapsed from K-f cross validation:t=3" (Brazil)
    
    cat( sprintf( "%d-fold summary: Mean = %f, sd = %f", 
                  mean(ho_scores), sd(ho_scores)))
}  

## 80% 1fold validation run records:
## nrounds=25       1/5: Mean score = 0.825033   12/13 ~ 1AM    (~5min)
## nrounds=100      1/5: Mean score = 0.825753   12/13          (~12min)  ( full train 0.84231 is much higher)
## nrounds=25  (validated same as above)
## eta=.003         1/5: Mean score = 0.818456  
## nrounds=100      1/5: Mean score = 0.819815
## nrounds=50 eta=0.01       1/5: Mean score = 0.821307
## add hour         1/5: Mean score = 0.821307
## eta=.1           1/5: Mean score = 0.826301
## eta=.05          1/5: Mean score = 0.825543
## eta=.1           1/5: Mean score = 0.826301                  (~8min) Mean score (full training set)= 0.837861
## add wday         1/5: Mean score = 0.825606      Mean score (full training set)= 0.839281

## xgb:initial      1/5: Mean score = 0.849507      Mean score (full training set)= 0.880791  
## nrnd=360, eta=.01 max_d=4; 1/5: Mean score = 0.847303      Mean score (full...)= 0.852272  Kaggle: 0.87311 
stopifnot( create_csv )

# cv <- xgb.cv(data = data.matrix(X[ ,-1]) , missing = NA
#                , label = y
#                , params = xgb_params
#                , nrounds = 200
#                , early.stop.round = 10
#                , nfold = 10
# )
# Stopping. Best iteration: 53

#retrain on full X
xgb <- xgboost(data = data.matrix(X[ ,-1]) , missing = NA
               , label = y
               , params = xgb_params
               , nrounds = xgb_nrounds  
)

imp_mat <- xgb.importance( feature_names = colnames(X)[-1], model=xgb); tcheck()
xgb.plot.importance(imp_mat)
print(imp_mat)

y_trn_pred <- predict(xgb, data.matrix(X[,-1]), missing = NA)
y_trn_top5 <- as.data.frame( matrix( top5_preds( y_trn_pred ), ncol=5, byrow = TRUE)) %>% tbl_df
y_trn_score <- score_predictions( y_trn_top5, labels)
cat( sprintf( "Mean score (full training set)= %f\n", mean(y_trn_score)) ) ; tcheck()  

# Test
y_pred <- predict(xgb, data.matrix(X_test[,-1]), missing = NA)
y_top5 <- top5_preds( y_pred )
submission <- data.frame( id= rep(X_test$id, each=5), country=y_top5)
subfile <- sprintf("../submissions/submission_%s.csv", run_id)
write.csv(submission, file=subfile , quote=FALSE, row.names = FALSE); tcheck( desc= subfile)

tcheck_df <- get_tcheck()
print( tcheck_df )
print( sum(tcheck_df$delta))
