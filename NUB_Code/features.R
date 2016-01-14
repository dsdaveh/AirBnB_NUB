library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)

if (! exists("users"))    load(file="../users.RData") # source('data_prep.R')
if (! exists("sessions")) load(file="../sessions.RData") # source('data_prep.R')

countries <- read.csv('../input/countries.csv') %>% tbl_df()
age_gender_bkts <- read.csv('../input/age_gender_bkts.csv') %>% tbl_df()

userf1 <- users

#### features from the raw user data

#see EDA_1.R for reasoning
userf1 <- userf1 %>% 
    mutate ( age_check = as.factor( 
        ifelse( age < 18, "<18", 
                ifelse( age == 105, "=105", 
                        ifelse( age == 2014, "=2014",  
                                ifelse( age > 90, ">90","OK") )))))


#### features from the aggregates sessions data

count_occurance <- function(x, str) length( grep(str, x))
first_occurance <- function(x, str) grep(str,x)[1]

ses_agg <- sessions %>% group_by( id ) %>%
    mutate( e_12hr = secs_elapsed > 12*3600, 
            e_30hr = secs_elapsed > 30*3600,
            e_6d  = secs_elapsed > 6*24*3600) %>%
    summarise(
        e0 = first(secs_elapsed),
        e_total = sum(secs_elapsed, na.rm=TRUE),
        e_max = max(secs_elapsed, na.rm=TRUE),
        e_n12hr = sum( e_12hr, na.rm=TRUE ),
        e_n30hr = sum( e_12hr, na.rm=TRUE ),
        e_n6d = sum( e_12hr, na.rm=TRUE ),
        nrec = n(),
        n_actions = n_distinct(action),
        n_act_type = n_distinct(action_type),
        n_act_det = n_distinct(action_detail),
        n_dev = n_distinct(device_type),
        dev_first = first( device_type),
        dev_last = last( device_type),
        n_contact_host = count_occurance( action_detail, "contact_host"),
        w_contact_host = first_occurance( action_detail, "contact_host"),
        n_manage_listing = count_occurance( action_detail, "manage_listing"),
        w_manage_listing = first_occurance( action_detail, "manage_listing"),
        n_wishlist = count_occurance( action_detail, "wishlist"),
        w_wishlist = first_occurance( action_detail, "wishlist"),
        n_translate = count_occurance( action_detail, "translate"),
        w_translate = first_occurance( action_detail, "translate"),
        n_book = count_occurance( action_detail, "book"),
        w_book = first_occurance( action_detail, "book"),
        n_user_reviews = count_occurance( action_detail, "user_reviews"),
        w_user_reviews = first_occurance( action_detail, "user_reviews"),
        n_photos = count_occurance( action_detail, "photos"),
        w_photos = first_occurance( action_detail, "photos"),
        n_reviews = count_occurance( action_detail, "reviews"),
        w_reviews = first_occurance( action_detail, "reviews") 
    ) %>%
    mutate ( nrec_ln = log(nrec)) %>%
    mutate( lne0 = log1p(e0) ) %>%
    mutate( dev_int1 = n_dev * as.integer( dev_first )) %>%
    mutate( dev_int2 = n_dev * as.integer( dev_last )) 


ses_agg[ is.na(ses_agg$lne0), 'lne0' ] <- -999  # separate these for NA's from join   
userf1 <- userf1 %>% left_join( ses_agg, by='id') 

save(userf1, file='../userf1.RData')

# EDA and verification used along the way

# first elapsed might have value so lets capture it
# elapsed_0 <- sessions %>% group_by( id ) %>%
#     slice(1) %>% ungroup() %>% 
#     mutate( lne0 = log1p(secs_elapsed) ) %>%
#     select( id, lne0 ) 
# users %>% left_join( elapsed_0, by='id') %>%
#     ggplot( aes( lne0 )) + geom_histogram()

#what's the range on number of actions  -- nrec
# n_actions <- sessions %>% group_by( id ) %>% summarize( count=n() ) %>% extract2(2)
# hist(n_actions)
# hist(log(n_actions))

# ses_cnts <- sessions %>% group_by( id ) %>% summarize( n_distinct(action),
#                                            n_distinct(action_type),
#                                            n_distinct(action_detail),
#                                            n_distinct(device_type)) 
# summary(ses_cnts)


# age_gender ... place the male/female ratio for the age group for the user (or 1)
age2 <- age_gender_bkts %>% 
    spread( gender, population_in_thousands) %>%
    mutate( mf_ratio = male/female ) %>%
    mutate( age_bucket = as.numeric( gsub( "[^0-9].*", "", age_bucket ) ))

age_agg <- age2 %>% group_by(age_bucket) %>% summarise(
    mf_rat_min = country_destination[ which.min( mf_ratio) ] ,
    mf_rat_max = country_destination[ which.max( mf_ratio) ] ,
    mf_rat_diff = max(mf_ratio) - min(mf_ratio)
)
    
userf1 <- userf1 %>%
    mutate( age_impute = ifelse( is.na(age), 30, age)) %>%   # mf_ratio for 30 is very close to 1.0 anyway
    mutate( age_bucket = ifelse( age_impute > 104, 30, floor(age_impute / 5) * 5 )) %>%
    left_join( age_agg, by = "age_bucket")
userf1$age_impute <- NULL


