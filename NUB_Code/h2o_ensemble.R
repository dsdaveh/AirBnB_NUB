# load libraries
library(stringr)
library(caret)
library(car)
library(dplyr)
library(lubridate)
library(data.table)
library(bit64)
library(cvAUC)
library(h2oEnsemble)  # This will load the `h2o` R package as well

h2o.init(nthreads = -1, max_mem_size = "9G")  # Start an H2O cluster with nthreads = num cores on your machine
h2o.removeAll() # Clean slate - just in case the cluster was already running
#h2o.shutdown(prompt = F)

source('nub_utils.R')
if (! exists("userf1")) load(file="../userf1.RData") # source('features.R')

tcheck(0) ####
### parameters
if ( exists("set_run_id") ) {
    run_id <- set_run_id
    rm( set_run_id )
} else {
    run_id <- format(Sys.time(), "h2o_stack_%Y_%m_%d_%H%M%S")
}
tcheck.print <- TRUE
set.seed(1)
kfold <- 5   #set to -1 to skip
only1 <- TRUE  
create_csv <- TRUE

learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"


###  end params ####

# prep
tcheck( desc="begin data prep") ####


userf1$isNDF <- userf1$country_destination == 'NDF'  # turn this into a binomial problem
userf1$dac_tfa <- NULL  # H2O can't handle this type, and there is only 1 value != 0 anyway

# split train and test
train <- userf1 %>% filter( source == "train") %>% replace_na()

ignore <- c("id", "country_destination", "source")
y <- "isNDF"
x <- setdiff(names(train), c(y, ignore))

test <- userf1 %>% filter( source == "test")%>% replace_na()


if (kfold > 0) {
    ix_shuffle <- sample( 1: nrow(train))
    ix_upper <- 0
    ix_inc <- ceiling( length(ix_shuffle) / kfold )
    ho_scores <- numeric(kfold)
}

tcheck( desc="begin kfold cross validation scores") ####
for (i in 1:kfold) {
    if (kfold < 0) break
    
    # split out a hold out set
    ix_lower <- ix_upper + 1
    ix_upper <- ifelse( i == kfold, length(ix_shuffle), ix_lower + ix_inc - 1)
    iho <- ix_shuffle[ix_lower:ix_upper]
    
    trn.k.name <- paste0( "trn", i )
    h2o.trn.k <- as.h2o( train[-iho, ], destination_frame = paste0( "trn", i ) )
    h2o.tst.k <- as.h2o( train[ iho, ], destination_frame = paste0( "tst", i ) )

    fit.k <- h2o.ensemble(x = x, y = y, 
                        training_frame = h2o.trn.k, 
                        family = "binomial", 
                        learner = learner, 
                        metalearner = metalearner,
                        cvControl = list(V = 5))
    
    #### Predict 
    #Generate predictions on the training set.
    pred <- predict(fit.k, h2o.trn.k)
    predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
    labels <- as.data.frame(train[-iho,y])[,1]
    auc_trn <- cvAUC::AUC(predictions = predictions, labels = labels)
    roc <- cvAUC(predictions, labels)
    plot(roc$perf, col="red", avg="vertical")
    tcheck( desc= sprintf( 'k-fold %d: Training AUC = %f\n', i, auc_trn))
    
    #Generate predictions on the training set.
    pred <- predict(fit, h2o.tst.k)
    predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
    labels <- as.data.frame(train[iho,y])[,1]
    ho_scores[i] <- cvAUC::AUC(predictions = predictions, labels = labels)
    roc <- cvAUC(predictions, labels)
    plot(roc$perf, col="red", avg="vertical", add=TRUE)
    tcheck( desc= sprintf( 'k-fold %d: Holdout AUC = %f\n', i, ho_scores[i]))
    
    if (only1) break
}

if (i > 1) {
    tcheck( t=kfold, desc= 'K-f cross validation')
    # [1] "303.310000 elapsed from K-f cross validation:t=3" (Brazil)
    
    cat( sprintf( "%d-fold summary: Mean = %f, sd = %f", 
                  kfold, mean(ho_scores), sd(ho_scores)))
}  
## xgb comments..
## 80% 1fold validation run records:
...
## add fea: e_n30hr, e_n6d (full training set)= 0.860660  Kaggle=   0.87582
## begin h2o_ensemble records:
## 5-fold summary (ho): Mean = 0.832481, sd = 0.001740  (trn) Mean=0.903837 sd=0.00326 full AUC = 0.901269
 
stopifnot( create_csv )

h2o.trn <- as.h2o( train, destination_frame = "trn" )
h2o.tst <- as.h2o( test, destination_frame = "tst" )

fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = h2o.trn, 
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

#### Predict 
#Generate predictions on the training set.
pred <- predict(fit, h2o.trn)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(train[,y])[,1]
auc_trn <- cvAUC::AUC(predictions = predictions, labels = labels)
roc <- cvAUC(predictions, labels)
plot(roc$perf, col="red", avg="vertical", add=TRUE)
tcheck( desc= sprintf( 'Training AUC = %f\n', auc_trn))
trn_csv <- sprintf("../intermediate_results/train_ndf_pred_%s.csv", run_id)
trn_pred <- data.frame( id= train$id, pNDF = predictions, truth = labels )
write.csv(trn_pred, file=trn_csv , quote=FALSE, row.names = FALSE); tcheck( desc= trn_csv)

#Generate predictions on the training set.
pred <- predict(fit, h2o.tst)
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(test[,y])[,1]
tst_csv <- sprintf("../intermediate_results/test_ndf_pred_%s.csv", run_id)
tst_pred <- data.frame( id= test$id, pNDF = predictions, truth = labels )
write.csv(tst_pred, file=tst_csv , quote=FALSE, row.names = FALSE); tcheck( desc= tst_csv)

tcheck_df <- get_tcheck()
print( tcheck_df )
print( sum(tcheck_df$delta))
