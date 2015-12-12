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
                , function(x) ifelse( x == x[1], 1, 0))[-1, ]
    as.vector( apply(r, 2, ndcg_at_k) )
}
# 
# cat ('Examples from NDCG example\n')
# print(ndcg_at_k(c(0)))
# print(ndcg_at_k(c(1)))
# print(ndcg_at_k(c(1,0)))
# print(ndcg_at_k(c(0,1)))
# print(ndcg_at_k(c(0,1,1)))
# print(ndcg_at_k(c(0,1,1,1)))
# 
# cat ('\nExamples from Score predictions using NDCG\n')
# preds <- matrix( c('US', 'FR', 'FR', 'US', 'FR', 'FR'), nrow=3, byrow= TRUE) 
# truth <- c('US','US','FR')
# cat("preds\n")
# print(as.data.frame(preds))
# score <- score_predictions( preds, truth)
# print(data.frame( truth=truth, score=score ))
# 
# #Assuming final score is a mean based on this from Wikipedia:
# # The nDCG values for all queries can be averaged to obtain a measure
# # of the average performance of a search engine's ranking algorithm.
# cat('mean score = ', mean(score), '\n')

