library(dplyr)
library(readr)
library(lubridate)

cTypes <- c( "character", "Date", "character",  "character", "factor", "numeric", "factor", "integer", rep("factor", 7) )
users_trn <- read.csv( '../input/train_users_2.csv', colClasses = c(cTypes, "factor")) %>% tbl_df() 
users_tst <- read.csv('../input/test_users.csv', colClasses = cTypes) %>% tbl_df() 
users_trn$date_first_booking <- NULL
users_tst$date_first_booking <- NULL

countries <- read.csv('../input/countries.csv') %>% tbl_df()
age_gen <- read.csv('../input/age_gender_bkts.csv') %>% tbl_df()
sessions <- read_csv('../input/sessions.csv') %>% tbl_df()
sessions$action <- as.factor( sessions$action )
sessions$action_type <- as.factor( sessions$action_type )
sessions$action_detail <- as.factor( sessions$action_detail )
sessions$device_type <- as.factor( sessions$device_type )

#transforms
users_trn$tfa <- ymd_hms( users_trn$timestamp_first_active)
users_tst$tfa <- ymd_hms( users_tst$timestamp_first_active)
users_trn$timestamp_first_active <- NULL
users_tst$timestamp_first_active <- NULL

users_trn <- users_trn %>% mutate( dac_tfa = as.Date(date_account_created) - as.Date(tfa) )
users_trn$date_account_created <- NULL
users_trn_2014 <- users_trn[ year( users_trn$tfa ) == 2014, ]
users_tst <- users_tst %>% mutate( dac_tfa = as.Date(date_account_created) - as.Date(tfa) )
users_tst$date_account_created <- NULL

sessions <- sessions %>% rename( id = user_id )
ses_trn <- users_trn %>% inner_join( sessions, by= "id") %>% group_by( id )
ses_tst <- users_tst %>% inner_join( sessions, by= "id") %>% group_by( id )

user_ses_trn <- slice( ses_trn, 1) %>% ungroup() %>% rename( elapsed0 = secs_elapsed) %>% group_by( id )
user_ses_tst <- slice( ses_tst, 1) %>% ungroup() %>% rename( elapsed0 = secs_elapsed) %>% group_by( id )


