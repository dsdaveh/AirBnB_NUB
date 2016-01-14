age2 <- age_gender_bkts %>% 
    spread( gender, population_in_thousands) %>%
    mutate( mf_ratio = male/female )

age3 <- age2 %>% group_by(country_destination) %>%
    summarise( female = sum(female),
               male = sum(male)) %>%
    mutate( mf_ratio = male/female )

age2$age_bucket <- as.numeric( gsub( "[^0-9].*", "", age2$age_bucket ) )
age2 %>% 
    ggplot( aes( x=age_bucket, y=mf_ratio , colour=country_destination, group=country_destination)) +
    geom_point() +
    geom_line( ) +
    theme( axis.text.x = element_text( angle=330, hjust=0 ))
    

