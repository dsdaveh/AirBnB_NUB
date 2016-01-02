library(magrittr)
library(dplyr)
library(readr)
library(lubridate)

cTypes <- c( "character", "Date", "character",  "character", "factor", "numeric", rep("factor", 9) )
users_trn <- read.csv( '../input/train_users_2.csv', colClasses = c(cTypes, "factor")) %>% mutate( source = "train" ) %>% tbl_df() 
users_tst <- read.csv('../input/test_users.csv', colClasses = cTypes) %>% mutate( source = "test" ) %>% tbl_df() 

countries <- read.csv('../input/countries.csv') %>% tbl_df()
age_gen <- read.csv('../input/age_gender_bkts.csv') %>% tbl_df()
sessions <- read_csv('../input/sessions.csv') %>% tbl_df()
sessions$action <- as.factor( sessions$action )
sessions$action_type <- as.factor( sessions$action_type )
sessions$action_detail <- as.factor( sessions$action_detail )
sessions$device_type <- as.factor( sessions$device_type )

users_tst$country_destination <- NA
users <- rbind( users_trn, users_tst)

#transforms
users$tfa <- ymd_hms( users$timestamp_first_active)
users$timestamp_first_active <- NULL

users <- users %>% mutate( dac_tfa = as.Date(date_account_created) - as.Date(tfa) )

users <- users %>% mutate(
    dac_year = year( date_account_created),
    dac_mon = month( date_account_created),
    dac_wday = wday( date_account_created),
    tfa_year = year( tfa ),
    tfa_mon = month( tfa ),
    tfa_wday = wday( tfa ),
    tfa_hour = hour(tfa)
)
users$date_account_created <- NULL
users$tfa <- NULL
users$date_first_booking <- NULL

sessions <- sessions %>% 
    rename( id = user_id ) %>%
    filter( id != "" )

#divide into data w/without sessions (based on date)
older <- which( users$tfa_year < 2014 ) 
users_older <- users[ older, ]
users <- users[ -older, ]

save(users, file='../users.RData')
save(users_older, file='../users_older.RData')
save(sessions, file='../sessions.RData')

