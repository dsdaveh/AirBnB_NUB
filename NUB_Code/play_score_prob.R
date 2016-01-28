#rescore base on the known proportions
#starts from xgb.R solve in memory

#proportions for the training set
p_train <- userf1 %>% 
    filter( source == 'train') %>% 
    group_by( country_destination) %>% 
    summarize( p=n()/nrow(.) ) %>% 
    arrange(desc(p))

#proportions for the hold out set
p_ho <- data.frame( country_destination = names(y[iho])) %>% 
    group_by (country_destination) %>% 
    summarize( p_ho=n()/length(iho)) %>% 
    arrange(desc(p_ho)) %>%
    left_join( p_train, by="country_destination") %>%
    mutate( diff = p-p_ho )

# this is section taken from xgb
y_ho_pred <- predict(xgb.k, data.matrix(X[iho,-1]), missing = NA)
y_ho_top5 <- as.data.frame( matrix( top5_preds( y_ho_pred ), ncol=5, byrow = TRUE), stringsAsFactors=FALSE ) %>% tbl_df
truth_ho <- labels[iho]
y_ho_score <- score_predictions( y_ho_top5, truth_ho)
pred_eval <- y_ho_top5 %>% bind_cols( data.frame(truth_ho, y_ho_score))
mean(y_ho_score) # 0.8517255

#reconstruct V1 based on the p_train probabilities
y_ho_pred_mat <- matrix(y_ho_pred, ncol=12, byrow=TRUE)
y_ho_pred_rank <- apply(y_ho_pred_mat, 2, function(x) length(x) + 1 - rank(x) ) %>% as.data.frame()
colnames(y_ho_pred_rank) <- names(country_rank)
y_ho_pred_rank$ix <- 1:nrow(y_ho_pred_rank)

#Forward Ranking
nvals <- round( p_train$p[1:12] * nrow(y_ho_pred_mat))
names(nvals) <- names(country_rank)
nvals[12] <- nrow(y_ho_pred_mat) - sum(nvals[1:11]) #correct rounding error

idx_ranks <- y_ho_pred_rank
V1 <- character()  # factor( levels=names(country_rank))
for (i in 1:12 ) {
    rank_ix <- order( idx_ranks[,i])[1:nvals[i]]
    frame_ix <- idx_ranks[ rank_ix, 13 ] #
    V1[frame_ix] <- rep( names(nvals)[i], nvals[i] )  #fill in the appropriate rows
    idx_ranks <- idx_ranks[-rank_ix, ]  #remove the assigned rows
}

fix_v2to5 <- function ( new_v1, v1to5) {
    stopifnot( length(v1to5) == 5 )
#     print(new_v1)
#     print(v1to5)
    v1to5 <- v1to5 %>% unlist() %>% as.character()
    v1pos <- which( v1to5 == new_v1)
    if (length(v1pos) == 0)  return ( c(new_v1, v1to5[1:4])) #shift
    if ( v1pos == 1 )        return (v1to5)  #nothing changed
                             return ( c(new_v1, v1to5[-v1pos])) #shuffle
}

fwd_rank <- y_ho_top5
for ( i in 1:nrow(fwd_rank)) {
    fwd_rank[i,] <- fix_v2to5( V1[i], y_ho_top5[i, ])
    # print(fwd_rank[i,])
}
fwd_score <- score_predictions( fwd_rank, truth_ho)
mean(fwd_score) # 0.8316345
