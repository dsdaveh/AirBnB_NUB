library(dplyr)
library(ggplot2)
library(stringr)

if (! exists("sessions") & (nrow(sessions) != 10567737 |
                            nrow(users) != 138526) 
) source( 'data_prep.R')

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

# first elapsed might have value so lets capture it
ses_agg <- sessions %>% group_by( id ) %>%
    summarise(
        e0 = first(secs_elapsed),
        nrec = n(),
        n_actions = n_distinct(action),
        n_act_type = n_distinct(action_type),
        n_act_det = n_distinct(action_detail),
        n_dev = n_distinct(device_type),
        dev_first = first( device_type),
        dev_last = last( device_type)
    ) %>%
    mutate ( nrec_ln = log(nrec)) %>%
    mutate( lne0 = log1p(e0) ) %>%
    mutate( dev_int1 = n_dev * as.integer( dev_first )) %>%
    mutate( dev_int2 = n_dev * as.integer( dev_last )) 


ses_agg[ is.na(elapsed_0$lne0), 'lne0' ] <- -999  # separate these for NA's from join   
userf1 <- userf1 %>% left_join( ses_agg, by='id') 

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
