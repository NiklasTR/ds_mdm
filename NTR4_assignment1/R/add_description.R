add_description <- function(df){
  df %>% 
    select(type, a1) %>% 
    distinct() %>% 
    spread(type, a1) %>% 
    cbind(.,
          df %>% 
            mutate(type = paste(type, group, sep = "_")) %>%
            select(type, ratio) %>% 
            spread(type, ratio)
    ) %>% 
    return()
}