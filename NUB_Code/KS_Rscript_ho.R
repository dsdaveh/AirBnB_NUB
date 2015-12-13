# This R script is based on Sandro's python script, which produces a LB score of 0.8655
# This script should produce a LB score of 0.86547

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

### parameters
tcheck.print <- TRUE
set.seed(1)
kfold <- 5
###

tcheck(0) ####
# load data
df_train = fread("../input/train_users_2.csv")
df_test = fread("../input/test_users.csv")

tcheck( desc="begin data prep") ####
labels = df_train$country_destination
df_train$country_destination <- NULL

df_all = rbind(df_train,df_test) # combine train and test data
df_all$date_first_booking <- NULL # remove date_first_booking
df_all[is.na(df_all)] <- -1 # replace missing values

# split date_account_created in year, month and day
df_all <- df_all %>% 
    mutate ( dac_year = year(date_account_created)
               , dac_month = month(date_account_created)
               , dac_day = day(date_account_created)
               )
df_all$date_account_created <- NULL

# split timestamp_first_active in year, month and day
df_all <- df_all %>% 
    mutate ( tfa = ymd_hms( timestamp_first_active)) %>%
    mutate ( tfa_year = year( tfa ) 
             , tfa_month = month( tfa ) 
             , tfa_day = day( ymd_hms( tfa ) )
    )
df_all$timestamp_first_active <- NULL
df_all$tfa <- NULL

# clean Age by removing values
df_all[df_all$age < 14 | df_all$age > 100, age:= -1 ]

# one-hot-encoding features
df_all <- as.data.frame(df_all)
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

# split train and test
X = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")

X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

ix_shuffle <- sample( 1: nrow(X))
ix_upper <- 0
ix_inc <- ceiling( length(ix_shuffle) / kfold )
ho_scores <- numeric(kfold)

tcheck( desc="begin kfold cross validation scores") ####
for (i in 1:kfold) {
    
    # split out a hold out set
    ix_lower <- ix_upper + 1
    ix_upper <- ifelse( i == kfold, length(ix_shuffle), ix_lower + ix_inc - 1)
    iho <- ix_shuffle[ix_lower:ix_upper]

    # train xgboost
    xgb <- xgboost(data = data.matrix(X[-iho ,-1]), 
                   label = y[-iho], 
                   eta = 0.1,
                   max_depth = 9, 
                   nround=25, 
                   subsample = 0.5,
                   colsample_bytree = 0.5,
                   eval_metric = "merror",
                   objective = "multi:softprob",
                   num_class = 12,
                   nthread = 3
    )
    
    top5_preds <- function (xgb_pred) {
        predictions <- as.data.frame(matrix(xgb_pred, nrow=12))
        rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
        as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
    }
    
    # predict values in hold out set
    y_ho_pred <- predict(xgb, data.matrix(X[iho,-1]))
    y_ho_top5 <- as.data.frame( matrix( top5_preds( y_ho_pred ), ncol=5, byrow = TRUE)) %>% tbl_df
    truth_ho <- labels[iho]
    y_ho_score <- score_predictions( y_ho_top5, truth_ho)
    pred_eval <- y_ho_top5 %>% bind_cols( data.frame(truth_ho, y_ho_score))
    
    ho_scores[i] <- mean(y_ho_score)
    cat( sprintf( "%d/%d: Mean score = %f10.8\n", i, kfold, ho_scores[i]) ) ; tcheck()
}
tcheck( t=kfold, desc= 'K-f cross validation')
# [1] "303.310000 elapsed from K-f cross validation:t=3" (Brazil)

summary(ho_scores)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8226  0.8241  0.8242  0.8246  0.8250  0.8269 
sd(ho_scores)
# [1] 0.00157869

# pred_ho <- data.frame( id= rep(X[iho,'id'], each= 5)
#                        , country= top5_preds( y_ho_pred) )

# 
# # predict values in test set
# y_pred <- predict(xgb, data.matrix(X_test[,-1]))
# 
# # extract the 5 classes with highest probabilities
# predictions <- as.data.frame(matrix(y_pred, nrow=12))
# rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
# predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
# 
# # create submission 
# ids <- NULL
# for (i in 1:NROW(X_test)) {
#     idx <- X_test$id[i]
#     ids <- append(ids, rep(idx,5))
# }
# submission <- NULL
# submission$id <- ids
# submission$country <- predictions_top5
# 
# # generate submission file
# submission <- as.data.frame(submission)
# write.csv(submission, "submission.csv", quote=FALSE, row.names = FALSE)