library(dplyr)
library(readr)
library(lubridate)

users_trn <- read_csv( '../input/train_users_2.csv')
users_tst <- read_csv('../input/test_users.csv')

countries <- read_csv('../input/countries.csv')
age_gen <- read_csv('../input/age_gender_bkts.csv')
sessions <- read_csv('../input/sessions.csv')

#transforms
users_trn$tfa <- ymd_hms( users_trn$timestamp_first_active)
users_tst$tfa <- ymd_hms( users_tst$timestamp_first_active)

users_trn <- users_trn %>% mutate( dac_tfa = date_account_created - as.Date(tfa) )
users_tst <- users_tst %>% mutate( dac_tfa = date_account_created - as.Date(tfa) )

############

#how many users:
length( unique(users_trn$id) )  #213451
length( unique(users_tst$id) )  #62096

table( year(users_trn$date_account_created))  #2010-2014
table( year(users_tst$date_account_created))  #2014 only

table( year(users_trn$tfa))
table( year(users_tst$tfa))
 
users_trn %>% group_by( country_destination) %>% filter(dac_tfa > 0) %>% 
    ggvis(~as.numeric(dac_tfa)) %>%
    layer_histograms( fill = ~country_destination, width = 100)  #looks significant
users_trn %>% filter(dac_tfa > 0) %>% 
    ggvis(~as.numeric(dac_tfa)) %>%
    layer_histograms( width = 100) 
users_tst %>% filter(dac_tfa > 0) %>% 
    ggvis(~as.numeric(dac_tfa)) %>%
    layer_histograms( width = 100)  #  or not ... DAMN!

sessions <- sessions %>% rename( id = user_id )
ses_trn <- users_trn %>% inner_join( sessions, by= "id")
ses_tst <- users_tst %>% inner_join( sessions, by= "id")

#how many uses in the sessions data
length( unique( ses_trn$id ))  
length( unique( ses_tst$id ))  # most 

#the handful that don't have session data
trn_2014 <- users_trn[ year( users_trn$tfa ) == 2014, ]
trn_no_ses <- setdiff( trn_2014$id, unique( sessions$id ))  #2615 members
tst_no_ses <- setdiff( users_tst$id, unique( sessions$id ))  # 428 

trn_2014 %>% filter (id %in% trn_no_ses) %>%
    ggvis( ~country_destination) %>% layer_bars()

top5_no_ses <- sort(table( trn_2014[ trn_2014$id %in% trn_no_ses, 'country_destination']), decreasing = T)[1:5]
barplot(top5_no_ses)
top5 <- sort(table( trn_2014[ , 'country_destination']), decreasing = T)[1:5]
barplot(top5)
100*top5_no_ses/sum(top5_no_ses)
100*top5/sum(top5)


