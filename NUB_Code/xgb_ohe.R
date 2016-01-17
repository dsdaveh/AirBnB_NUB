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
    run_id <- format(Sys.time(), "xgb_ohe_%Y_%m_%d_%H%M%S")
}
tcheck.print <- TRUE
set.seed(1)
kfold <- 5   #set to -1 to skip
only1 <- FALSE  
create_csv <- TRUE
ndcg_mean <- function(preds, dtrain) {
    truth <- getinfo(dtrain, "label")
    pred12 <- matrix( preds, nrow=12) 
    pred5 <- apply(pred12, 2, order)[12:8,] %>% t() -1
    score <- apply(pred5, 2, function(x) as.integer( x == truth ))
    
    ndcg <- apply(score, 1, ndcg_at_k )
    return(list(metric = "mean-ndcg", value = mean(ndcg)))
}
xgb_params <- list( 
    eta = 0.01,      # was .1
    max_depth = 6,   # was 4 # was 9
    gamma = 0.5,     # new
    min_child_weight = 5, #new
    subsample = 0.5,
    colsample_bytree = 0.5, 
    eval_metric = "ndcg@5",
    objective = "multi:softprob",
    num_class = 12,
    nthreads = 4,
    maximize = TRUE
    )
xgb_nrounds <- 94 # was 334   
###

# prep
tcheck( desc="begin data prep") ####

# one-hot-encoding features
factor_cols <- which(lapply( userf1, class) == "factor") 
factor_cols <- factor_cols[ -which(names(factor_cols) == 'country_destination')]  #there's probably a cooler way to remove one element
factor_names <- names(factor_cols)
ohe_formula <- as.formula( paste0( "~  ", paste( factor_names, collapse=" + ")))
##df_all <- as.data.frame(df_all)
##ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
##dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
dummies <- dummyVars( ohe_formula, data = userf1)
user_ohe <- cbind( userf1[ ,-factor_cols],
                   as.data.frame(predict(dummies, newdata = userf1)) )

# split train and test
X <- user_ohe %>% filter( source == "train")
labels <- as.character( X$country_destination )
X$country_destination <- NULL
X$source <- NULL

y <- country_to_int( labels ) 



X_test <- user_ohe %>% filter( source == "test")
X_test$country_destination <- NULL
X_test$source <- NULL

if (kfold > 0) {
    ix_shuffle <- sample( 1: nrow(X))
    ix_upper <- 0
    ix_inc <- ceiling( length(ix_shuffle) / kfold )
    ho_scores <- numeric(kfold)
}

top5_preds <- function (xgb_pred) {
    predictions <- as.data.frame(matrix(xgb_pred, nrow=12))
    rownames(predictions) <- c('NDF','US','other','FR','IT','GB','ES','CA','DE','NL','AU','PT')
    as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
}

tcheck( desc="begin kfold cross validation scores") ####
for (i in 1:kfold) {
    if (kfold < 0) break
    
    # split out a hold out set
    ix_lower <- ix_upper + 1
    ix_upper <- ifelse( i == kfold, length(ix_shuffle), ix_lower + ix_inc - 1)
    iho <- ix_shuffle[ix_lower:ix_upper]

    # train xgboost
    dtrain <- xgb.DMatrix(data.matrix(X[ -iho ,-1]), label = y[-iho], missing = NA)
    xgb <- xgb.train(dtrain
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
    
    cat( sprintf( "%d-fold summary: Mean = %f, sd = %f\n", 
                  kfold, mean(ho_scores), sd(ho_scores)))
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
## max_d=6, gamma=.5, min_c=5; 1/5: Mean score = 0.848526     Mean score (full...)= 0.857748  Kaggle: 0.87394 
## nrnd=218                                                   Mean score (full...)= 0.855208  Kaggle: 0.87275
## *1 nrnd=360 + new features      Mean score (full training set)= 0.860487                      Kaggle: 0.87609 (#72)
## feval = ndcg(custom) eval_metric=merror (not sure what was used)  Mean score (full... = 0.860403 )
## eval_metric="ndcg" (no feval)                                     Mean score (full... = 0.860403
## nrnd=281                                     ...full ts = 0.859302  Kaggle=0.87569,
## *1    return  (verified full = 0.860403 )
## add fea: e_n12hr                                                          full= 0.860451 Kaggle=	0.87650  (+1 -> #89)
## add fea: e_n30hr, e_n6d                                                   full= 0.860660 Kaggle= 0.87582
## eval="ncdg@5" 5-fold summary:             Mean = 0.852913, sd = 0.001647  full= 0.860352 Kaggle: 0.87593
## n=334                                                                     full= 0.860222  
## added mf_rat features ... 5-fold summary: Mean = 0.852747, sd = 0.001544  full= 0.859594 Kaggle: 0.87502
## check that mf_rat as numeric versus char makes no difference (confirmed)
## ohe_encoding              5-fold summary: Mean = 0.853098, sd = 0.001529
## n=94                      5-fold summary: Mean = 0.852094, sd = 0.001925  full= 0.856644 Kaggle: 0.87528
 
stopifnot( create_csv )

dtrain <- xgb.DMatrix(data.matrix(X[ ,-1]), label = y, missing = NA)
tcheck( desc="begin train full model") ####

# cv <- xgb.cv(data = data.matrix(X[ ,-1]) , missing = NA
#                , label = y
#                , params = xgb_params
#                , nrounds = 1000
#                , early.stop.round = 15
#                , nfold = 5
#                , feval = ndcg_mean
#                , maximize = TRUE
# )
#Stopping. Best iteration: 94

#retrain on full X
# xgb <- xgboost(data = data.matrix(X[ ,-1]) , missing = NA
#                , label = y
#                , params = xgb_params
#                , nrounds = xgb_nrounds  
#                , feval = ndcg
# )

xgb <- xgb.train(dtrain , missing = NA
               , label = y
               , params = xgb_params
               , nrounds = xgb_nrounds  
)

# imp_mat <- xgb.importance( feature_names = colnames(X)[-1], model=xgb); tcheck()
# print(xgb.plot.importance(imp_mat))
# print(imp_mat)

y_trn_pred <- predict(xgb, data.matrix(X[,-1]), missing = NA)
y_trn_top5 <- as.data.frame( matrix( top5_preds( y_trn_pred ), ncol=5, byrow = TRUE)) %>% tbl_df
y_trn_score <- score_predictions( y_trn_top5, labels)
cat( sprintf( "Mean score (full training set)= %f\n", mean(y_trn_score)) ) ; tcheck()  
trn_csv <- sprintf("../submissions/train_pred_%s.csv", run_id)
trn_pred <- data.frame( id= rep(X$id, each=5), country=top5_preds(y_trn_pred) )
write.csv(trn_pred, file=trn_csv , quote=FALSE, row.names = FALSE); tcheck( desc= trn_csv)

# Test
y_pred <- predict(xgb, data.matrix(X_test[,-1]), missing = NA)
y_top5 <- top5_preds( y_pred )
submission <- data.frame( id= rep(X_test$id, each=5), country=y_top5)
subfile <- sprintf("../submissions/submission_%s.csv", run_id)
write.csv(submission, file=subfile , quote=FALSE, row.names = FALSE); tcheck( desc= subfile)

tcheck_df <- get_tcheck()
print( tcheck_df )
print( sum(tcheck_df$delta))
