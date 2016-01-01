library(dplyr)
library(ggvis)
library(ggplot2)
library(lubridate)
library(fastmatch)

if (! exists("sessions") & (nrow(sessions) != 10567737 |
                            nrow(users) != 138526) 
    ) source( 'data_prep.R')

############

#how many users:
length( unique(users_trn$id) )  #213451
length( unique(users_tst$id) )  #62096

table( year(users_trn$date_account_created))  #2010-2014
table( year(users_tst$date_account_created))  #2014 only

users %>% group_by( source, tfa_year) %>% summarise( count=n() )
users_older %>% group_by( source, tfa_year) %>% summarise( count=n() )

n_trn <- users %>% filter( source == "train") %>% nrow()  #n training users since 2014
n_tst<- users %>% filter( source == "test") %>% nrow()

#this would have been good if test had any entries ... as it is, its dangerous
users %>% group_by(source)  %>% summarise( count=n())
users %>% group_by(source) %>% filter( dac_tfa > 0) %>% summarise( count=n())

users_older %>% filter(dac_tfa > 0) %>%
    ggplot( aes( country_destination, fill=source)) + geom_bar( position = "dodge")

#how many users in the sessions data
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

#gender
users %>% 
    ggplot( aes( country_destination, fill=gender)) + geom_bar( position = "dodge")

users %>% 
    ggplot( aes( gender, fill=source)) + geom_bar( position = "dodge")

#age
users %>% filter( age > 90 ) %>% select(age) %>% extract2(1)  %>% table  # high values at 105 & 2014
users %>% filter( age > 90 ) %>% 
    ggplot( aes(x=age, fill=source)) + geom_dotplot( binwidth = 10)
users %>% mutate( gt1k = age > 1000) %>% filter( age > 90 ) %>%
    ggplot( aes( x=as.factor(age), fill=source)) + 
        geom_bar( binwidth = 1) +
        facet_grid( ". ~ gt1k")
                
userp <- users %>% 
    mutate ( age_check = as.factor( 
        ifelse( age < 18, "<18", 
                ifelse( age == 105, "=105", 
                        ifelse( age == 2014, "=2014",  
                                ifelse( age > 90, ">90","OK") )))))
userp %>% group_by( source, age_check) %>% summarise( count=n())
userp %>% filter( ! ( is.na(age) | age_check == "OK" )) %>%
    ggplot( aes( country_destination, fill=age_check)) + 
        geom_bar( position = "dodge")
table( userp$age_check)
### the above seems useful adding age_check to users in features.R

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


