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
mean(y_ho_score) # 0.8515544

#reconstruct V1 based on the p_train probabilities
y_ho_pred_mat <- matrix(y_ho_pred, ncol=12, byrow=TRUE)
y_ho_pred_rank <- apply(y_ho_pred_mat, 2, function(x) length(x) + 1 - rank(x) ) %>% as.data.frame()
colnames(y_ho_pred_rank) <- names(country_rank)
y_ho_pred_rank$ix <- 1:nrow(y_ho_pred_rank)

#Forward Ranking
nvals <- round( p_train$p[1:12] * nrow(y_ho_pred_mat))
names(nvals) <- names(country_rank)
nvals[12] <- nrow(y_ho_pred_mat) - sum(nvals[1:11]) #correct rounding error


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

idx_ranks <- y_ho_pred_rank
V1 <- character()  # factor( levels=names(country_rank))
for (i in 1:12 ) {
    rank_ix <- order( idx_ranks[,i])[1:nvals[i]]
    frame_ix <- idx_ranks[ rank_ix, 13 ] #
    V1[frame_ix] <- rep( names(nvals)[i], nvals[i] )  #fill in the appropriate rows
    idx_ranks <- idx_ranks[-rank_ix, ]  #remove the assigned rows
}
sort( table( V1), decreasing = TRUE)

      
fwd_rank <- y_ho_top5
for ( i in 1:nrow(fwd_rank)) {
    fwd_rank[i,] <- fix_v2to5( V1[i], y_ho_top5[i, ])
    # print(fwd_rank[i,])
}
fwd_score <- score_predictions( fwd_rank, truth_ho)
mean(fwd_score) # 0.8314388
v1.fwd_cd <- V1
v1.fwd <- V1 == truth_ho
mean(v1.fwd) # 0.6538009
    
#Backward Ranking
idx_ranks <- y_ho_pred_rank
V1 <- character()  # factor( levels=names(country_rank))
for (i in 12:1 ) {
    rank_ix <- order( idx_ranks[,i])[1:nvals[i]]
    frame_ix <- idx_ranks[ rank_ix, 13 ] #
    V1[frame_ix] <- rep( names(nvals)[i], nvals[i] )  #fill in the appropriate rows
    idx_ranks <- idx_ranks[-rank_ix, ]  #remove the assigned rows
}
sort( table( V1), decreasing = TRUE)


bwd_rank <- y_ho_top5
for ( i in 1:nrow(bwd_rank)) {
    bwd_rank[i,] <- fix_v2to5( V1[i], y_ho_top5[i, ])
    # print(fwd_rank[i,])
}
bwd_score <- score_predictions( bwd_rank, truth_ho)
mean(bwd_score) # 0.8315415
v1.bwd_cd <- V1
v1.bwd <- V1 == truth_ho
mean(v1.bwd) # 0.6486327

v1.xgb <- y_ho_top5$V1 == truth_ho
mean(v1.xgb) # 0.6940338

v1.vote <- round( (v1.fwd + v1.bwd + v1.xgb) / 3) 
mean(v1.vote) # 0.6639409

pred_cmp <- data.frame( xgb=y_ho_top5$V1, fwd=v1.fwd_cd, bwd=v1.bwd_cd, truth_ho, 
                        xgb1=v1.xgb, fwd1=v1.fwd, bwd1=v1.bwd, stringsAsFactors = F) %>%
    mutate( any = xgb==truth_ho | fwd==truth_ho | bwd==truth_ho ,
            all = xgb==truth_ho & fwd==truth_ho & bwd==truth_ho) %>%
    mutate( some = any & ! all)
head(pred_cmp)
pred_cmp %>% group_by(truth_ho) %>%
    summarize( xgb=mean( xgb1),
               fwd=mean( fwd1),
               bwd=mean( bwd1))

## OBSERVATION ... bwd does better then forward for the non-NDF/US, but still not good overall
## experiment:  What happens if we use backward as the 3rd place for only those
dbg_cnt <- 0
insert_obscures <- function( new_v3, v1to5) {
    if ( new_v3 %in% c("NDF", "US")) return (v1to5)
    stopifnot( length(v1to5) == 5 )
    #     print(new_v3)
    #     print(v1to5)
    v1to5 <- v1to5 %>% unlist() %>% as.character()
    first2 <- v1to5[1:2]
    last3 <- v1to5[3:5]
    v3pos <- which( v1to5 == new_v3)
    if (length(v3pos) == 0 & new_v3==truth_ho[i])  { dbg_cnt <<- dbg_cnt+1 ; cat(sprintf("%3d: new_v3=%s truth=%s\n", dbg_cnt, new_v3, truth_ho[i])) }
    
    if (length(v3pos) == 0)  return ( c(first2, new_v3, last3[1:2])) #shift
    if ( v3pos <= 3 )        return ( v1to5)  #nothing changed
                             return ( c(first2, new_v3, last3[-(v3pos-2)])) #shuffle)
}

optv3 <- y_ho_top5
for ( i in 1:nrow(optv3)) {
    optv3[i,] <- insert_obscures( v1.bwd_cd[i], y_ho_top5[i, ])
    #print(optv3[i,] ,  v1.bwd_cd[i], y_ho_top5[i, ])
}
optv3_score <- score_predictions( optv3, truth_ho)
mean(optv3_score) # 0.85109

