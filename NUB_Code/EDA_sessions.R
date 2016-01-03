source('nub_utils.R')

# users that booked
booked <- userf1 %>% filter( source=="train" & country_destination != "NDF")
length( unique( booked$id)) #29217

bids <- sample( unique(booked$id))
xx <- sessions %>% filter( id == bids[1])

count_occurance <- function(x, str) length( grep(str, x))
first_occurance <- function(x, str) grep(str,x)[1]

sessions %>% group_by( id ) %>%
    filter( id %fin% bids) %>% 
    summarise( n_contact_host = count_occurance( action_detail, "contact_host"),
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
               w_reviews = first_occurance( action_detail, "reviews") )

unique( sessions$action_detail )
