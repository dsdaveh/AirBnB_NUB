library(dplyr)
library(ggvis)
library(ggplot2)
library(lubridate)
library(fastmatch)

if (! exists("sessions") & (nrow(sessions) != 10567737 |
                            nrow(users) != 138526) 
    ) source( 'data_prep.R')

############

actions <- unique( sessions$action)
action_types <- unique( sessions$action_type)
action_details <- unique( sessions$action_detail)
device_types <- unique( sessions$device_type)

# not getting much out of the following ... probably need idf or something
plot(table(sessions$action))
sessions %>% group_by( action ) %>% summarise( count=n()) %>% arrange( desc(count) ) %>%
    head(  50) %>% 
    ggplot( aes(action, count)) + geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))



