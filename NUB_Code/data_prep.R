library(magrittr)
library(dplyr)
library(readr)
library(lubridate)

cTypes <- c( "character", "Date", "character",  "character", "factor", "numeric", rep("factor", 9) )
users_trn <- read.csv( '../input/train_users_2.csv', colClasses = c(cTypes, "factor")) %>% mutate( source = "train" ) %>% tbl_df() 
users_tst <- read.csv('../input/test_users.csv', colClasses = cTypes) %>% mutate( source = "test" ) %>% tbl_df() 

trn_dest <- users_trn %>% select( id, country_destination )
users_tst$country_destination <- NA
users <- rbind( users_trn, users_tst)

#users$date_first_booking <- NULL

countries <- read.csv('../input/countries.csv') %>% tbl_df()
age_gen <- read.csv('../input/age_gender_bkts.csv') %>% tbl_df()
sessions <- read_csv('../input/sessions.csv') %>% tbl_df()
sessions$action <- as.factor( sessions$action )
sessions$action_type <- as.factor( sessions$action_type )
sessions$action_detail <- as.factor( sessions$action_detail )
sessions$device_type <- as.factor( sessions$device_type )

#transforms
users$tfa <- ymd_hms( users$timestamp_first_active)
users$timestamp_first_active <- NULL

users <- users %>% mutate( dac_tfa = as.Date(date_account_created) - as.Date(tfa) )
users$date_account_created <- NULL

sessions <- sessions %>% rename( id = user_id ) 


#divide into data w/without sessions
older <- which( year( users$tfa) < 2014 ) 
users_older <- users[ older, ]
users <- users[ -older, ]

elapsed_0 <- sessions %>% group_by( id ) %>%
    slice(1) %>% ungroup() %>% select( id, secs = secs_elapsed )
elapsed_0[ is.na(elapsed_0$secs), 'secs' ] <- -1    
users <- users %>% left_join( elapsed_0, by='id') %>% rename( elapsed_0 = secs)




