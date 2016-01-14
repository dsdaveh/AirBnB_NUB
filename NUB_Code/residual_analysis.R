library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)

source('nub_utils.R')
if (! exists("userf1")) load(file="../userf1.RData") # source('features.R')

run_id = "xgb_2016_01_09_202503"
trn_csv <- sprintf("../submissions/train_pred_%s.csv", run_id)
subfile <- sprintf("../submissions/submission_%s.csv", run_id)

preds <- read.csv(trn_csv) %>% 
    bind_cols( data.frame( rank = rep( paste0("V",1:5), nrow(.)/5 ))) %>%
    spread( rank, country) %>%
    tbl_df()

top5_truth <- userf1 %>% 
    filter(source == "train") %>% 
    select(country_destination) %>% extract2(1) %>%
    table() %>% sort() %>% rev() %>% extract(1:5)

# Do any prediction NOT contain NDF or US?
ndf_cols <- apply( preds[ ,-1], 2, function(x) sum( x == "NDF")) 
ndf_cols
nrow(preds)-sum(ndf_cols)   #0

ndf_cols <- apply( preds[ ,-1], 2, function(x) sum( x == "US")) 
ndf_cols
nrow(preds)-sum(ndf_cols)   #0

ndf_cols <- apply( preds[ ,-1], 2, function(x) sum( x == "other")) 
ndf_cols
nrow(preds)-sum(ndf_cols)   #1

ndf_cols <- apply( preds[ ,-1], 2, function(x) sum( x == "FR")) 
ndf_cols
nrow(preds)-sum(ndf_cols)   #3437

pred2 <- preds %>% left_join( userf1 %>% select( id, country_destination), by="id") 
# How many NDF's/US did we get correct in the first position
pred2 %>% filter( country_destination == "NDF" ) %>% summarize( V1.NDF = mean( V1 == "NDF"))

# How many NDF/US correct in the first column
pred2 %>% filter( country_destination == "NDF" ) %>% summarize( V1.NDF = mean( V1 == "NDF")) #0.87639
pred2 %>% filter( country_destination == "US" ) %>% summarize( V1.US = mean( V1 == "US")) #0.6088937

# Which Column did we guess correctly in
find_pcol <- function(row) {
    guess_cols <- as.character( row[2:6])
    pcol <- grep( row[[7]], as.character( row[2:6]))
    ifelse( length(pcol) > 0, pcol, 0)
}
pred2$pcol <- apply( pred2, 1, find_pcol)

pred2 %>% 
    mutate( truth = ifelse( country_destination %in% names(top5_truth) 
                            , as.character(country_destination), "elsewhere")) %>%
    ggplot( aes(x=as.factor(pcol), fill=truth)) + geom_bar(position="dodge")

#TODO what if we rais the NDF/US numbers only???

