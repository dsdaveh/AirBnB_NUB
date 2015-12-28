library(dplyr)
library(ggvis)
library(ggplot2)
library(lubridate)
library(fastmatch)


source( 'data_prep.R')

############

#how many users:
length( unique(users_trn$id) )  #213451
length( unique(users_tst$id) )  #62096

table( year(users_trn$date_account_created))  #2010-2014
table( year(users_tst$date_account_created))  #2014 only

users %>% group_by( source, year(tfa)) %>% summarise( count=n() )
users_older %>% group_by( source, year(tfa)) %>% summarise( count=n() )

n_trn <- users %>% filter( source == "train") %>% nrow()  #n training users since 2014
n_tst<- users %>% filter( source == "test") %>% nrow()

#this would have been good if test had any entries ... as it is, its dangerous
users_older %>% filter(dac_tfa > 0) %>%
    ggplot( aes( country_destination, fill=source)) + geom_bar( position = "dodge")

#how many uses in the sessions data
length( unique( sessions$id ))    #135484

#the handful that don't have session data
trn_no_ses <- setdiff( users %>% filter( source=="train") %>% select(id) %>% extract2(1)
                       , unique( sessions$id ))  #2615 members
tst_no_ses <- setdiff( users %>% filter( source=="test") %>% select(id) %>% extract2(1)
                       , unique( sessions$id ))  #2615 members  # 428 

users %>% filter (id %in% trn_no_ses) %>%
    ggvis( ~country_destination) %>% layer_bars()

users %>% filter (id %in% trn_no_ses) %>% group_by( country_destination) %>% summarise( count=n())
    

# More NDF for the missing sessions.  This is really small though so not using it    
# top5_diff %>% group_by( source) %>% ggvis( ~Country, ~top5 ) %>% layer_bars( fill= ~source, stack=FALSE)
# ggplot( top5_diff, aes( Country, top5, fill = source)) + geom_bar( position='dodge', stat='identity')

#gender
users %>% 
    ggplot( aes( country_destination, fill=gender)) + geom_bar( position = "dodge")

users %>% 
    ggplot( aes( gender, fill=source)) + geom_bar( position = "dodge")

#age
userp <- users %>% mutate ( too_young = ifelse( age < 18, "Yes", "No"))
ggplot( userp, aes( country_destination, fill=too_young)) + geom_bar( position = "dodge")
table( userp$too_young)

ggplot( userp, aes( country_destination, fill=signup_method)) + geom_bar( position = "dodge")
table(userp$signup_method)

ggplot( userp, aes( country_destination, fill=signup_flow)) + geom_bar( position = "dodge")
table(userp$signup_flow)

ggplot( userp, aes( country_destination, fill=language)) + geom_bar( position = "dodge")
table( userp$language)

ggplot( userp, aes( country_destination, fill=affiliate_channel)) + geom_bar( position = "dodge")
table( userp$affiliate_channel)

ggplot( userp, aes( country_destination, fill=affiliate_provider)) + geom_bar( position = "dodge")
table( userp$affiliate_provider)


