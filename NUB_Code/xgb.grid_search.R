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
    run_id <- format(Sys.time(), "xgb_%Y_%m_%d_%H%M%S")
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
    if ( class(  X[[1,icol]] ) == "factor") X[ na_idx, icol] <- "NA"
}
xgb_train_1 = train(
    x = data.matrix(X[ ,-1]),
    y = y,
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree"
)

# scatter plot of the AUC against max_depth and eta
# ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
#     geom_point() + 
#     theme_bw() + 
#     scale_size_continuous(guide = "none")


