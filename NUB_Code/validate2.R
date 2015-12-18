# Building on previous script for internal scoring 
# https://www.kaggle.com/datadave/airbnb-recruiting-new-user-bookings/ndcg-score-r
# 
# Kaggle Script: Z-scores for LB benchmarks vs traininig

# find the variance within the training set to verify it explains difference 
# between local score and LB score

dcg_at_k <- function (r, k=min(5, length(r)) ) {
    #only coded alternative formulation of DCG (used by kaggle)
    r <- as.vector(r)[1:k]
    sum(( 2^r - 1 )/ log2( 2:(length(r)+1)) )
} 

ndcg_at_k <- function(r, k=min(5, length(r)) ) {
    r <- as.vector(r)[1:k]
    if (sum(r) <= 0) return (0)     # no hits (dcg_max = 0)
    dcg_max = dcg_at_k(sort(r, decreasing=TRUE)[1:k], k)
    return ( dcg_at_k(r, k) / dcg_max )
}

score_predictions <- function(preds, truth) {
    # preds: matrix or data.frame
    # one row for each observation, one column for each prediction.
    # Columns are sorted from left to right descending in order of likelihood.
    # truth: vector
    # one row for each observation.
    preds <- as.matrix(preds)
    truth <- as.vector(truth)
    
    stopifnot( length(truth) == nrow(preds))
    r <- apply( cbind( truth, preds), 1
                , function(x) ifelse( x == x[1], 1, 0))[ -1, ]
    if ( ncol(preds) == 1) r <-  rbind( r, r)  #workaround for 1d matrices
    as.vector( apply(r, 2, ndcg_at_k) )
}
# 


# Read in training data for more benchmarks
train <- read.csv('../input/train_users.csv')

# All NDF  -- compare to submission script of 0.67909
cat ("Full Dataset scores:\n")
score <- score_predictions( rep("NDF", nrow(train)), train$country_destination )
cat('Training set mean score for all NDF = ', mean(score), ' compare to LB 0.67909\n')   # 0.5834735
 
# Global probabilities
top5 <- sort( table(train$country_destination) , decreasing = TRUE)[1:5]
score <- score_predictions( matrix( rep(names(top5), nrow(train)), ncol=5, byrow=TRUE)
                            ,  train$country_destination)
cat('Training set mean score for top 5 global prob = ', mean(score), ' compare to LB 0.85359\n')   # 0.8067654

N <- 25
size <- 62096   #size of the test set

set.seed(1)
scores <- data.frame ( score=numeric(), type=character()) 
for (i in 1:N) {
    truth <- sample( train$country_destination, size )
    print( sort( table(truth), decreasing = TRUE) )
    score_NDF <- mean( score_predictions( rep("NDF", size), truth))
    score_top5 <-mean( score_predictions( matrix( rep(names(top5), size), ncol=5, byrow=TRUE), truth) )
    scores <- rbind( scores, data.frame( score=score_NDF, type="NDF"))
    scores <- rbind( scores, data.frame( score=score_top5, type="top5"))
    cat(i, ':Training subset mean score for all NDF = ', score_NDF, '\n')   # 0.5834735
    cat(i, ':Training subset mean score for top 5 global prob = ', score_top5, '\n')   # 0.8067654
}

#scores %>% filter( type == "NDF" ) %>% mutate(i=row_number()) %>% ggvis( ~i, ~score) %>% layer_points()
#scores %>% filter( type == "top5" ) %>% mutate(i=row_number()) %>% ggvis( ~i, ~score) %>% layer_points()

LB_ndf <- 0.67909
LB_top5 <- 0.85359

sndf <-scores[ scores$type=="NDF", 'score']
zndf <- (LB_ndf - mean(sndf)) / sd(sndf)
cat( sprintf(" Leaderboard Z-score for NDF method is %4.1f\n", zndf))

stop5 <-scores[ scores$type=="top5", 'score']
ztop5 <- (LB_top5 - mean(stop5)) / sd(stop5)
cat( sprintf(" Leaderboard Z-score for top5 method is %4.1f\n", ztop5))

plot ( c(1, 2, N), c( max(sndf)+ 2*sd(sndf), min(sndf)- 2*sd(sndf), LB_ndf)
       , type="n", xlab = 'Sample Iteration', ylab= "mean nCDG" )
points( sndf)
abline(h = mean(sndf), lwd = 2)
abline(h = mean(sndf)+2*sd(sndf), lty=2)
abline(h = mean(sndf)-2*sd(sndf), lty=2)
abline(h = LB_ndf, lwd=2, col="red")
title("mean nDCG for Training samples -vs- Test (Leaderboard)\nAll predictions = NDF")
legend( "topright", legend= c("Training", "Test"), col=c("black", "red"), lty=1, lwd=2)
