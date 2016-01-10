library(fastmatch)

#Fast %in%
`%fin%` <- function(x, lkup) {
    fmatch(x, lkup, nomatch = 0L) > 0L
}

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
    if ( ncol(preds) == 1) r <-  rbind( r, r)  #workaround for 1d matrices
    as.vector( apply(r, 2, ndcg_at_k) )
}

replace_na <- function( df, verbose=FALSE ) {
    colClasses <- lapply(df, class) %>% unlist()
    #Add NA as a level to the appropriate factor variables 
    for ( ix in which(colClasses == "factor")) {
        if ( any( is.na(df[ ,ix]))) {
            levels(df[ ,ix][[1]]) <- c(levels(df[ ,ix][[1]]), "NA")
        }
    }
    
    for (icol in 1:ncol(df)) {
        na_idx <- which( is.na( df[, icol] ))
        if ( length(na_idx) <= 1 ) next
        if (verbose) cat("...replacing Col ",icol,": ", length(na_idx), "NAs\n")
        if ( colClasses[icol] == "integer") df[ na_idx, icol] <- -99
        if ( colClasses[icol] == "numeric") df[ na_idx, icol] <- -99
        if ( colClasses[icol] == "factor") df[ na_idx, icol] <- "NA"
    }
    return(df)
}


if (! exists("tcheck.print")) tcheck.print = FALSE
if (! exists("tcheck.df")) tcheck.df <- data.frame( stringsAsFactors = FALSE)
tcheck.default_string <- function() sprintf( "t=%d", nrow(tcheck.df))
tcheck.tx <- list( proc.time()) 
tcheck <- function(t=1, desc = tcheck.default_string() ) {
    # t=0 to reset counter, t=1 incremental time output,  t=n time difference from n intervals
    #
    # use:
    # tcheck(0) #reset the counter
    # <computation 1>
    # tcheck()
    # <computation 2>
    # tcheck()
    # tcheck(2)  # for total time
    #
    t <- min( t, length(tcheck.tx))
    pt <- proc.time()
    if (t == 0) { 
        tcheck.tx <<- list( proc.time()) 
        tcheck.df <<- data.frame( elapsed = pt[3], desc = desc,stringsAsFactors = FALSE )
    } else {
        tcheck.tx <<- c( tcheck.tx, list(pt))
        tcheck.df <<- rbind( tcheck.df, data.frame( elapsed = pt[3], desc = desc, stringsAsFactors = FALSE ) )
        tn <- nrow( tcheck.df )
        elapsed_delta <- diff( tcheck.df[ c(tn-t, tn),]$elapsed )
        out_str <- ifelse ( t == 1
                            , sprintf("%f elapsed for %s", elapsed_delta
                                      , tcheck.df[tn, "desc"] )
                            , sprintf("%f elapsed from %s:%s", elapsed_delta
                                      , tcheck.df[tn, "desc"], tcheck.df[tn-t, "desc"]) )
        if (tcheck.print) print( out_str)
        return( out_str )
        #         tn <- length(tcheck.tx)
        #         print ( tcheck.tx[[tn]] - tcheck.tx[[tn-t]]) 
    }
}
get_tcheck <- function() tcheck.df %>% mutate( delta=c( 0, diff(elapsed)) ) %>% select( desc, delta)
