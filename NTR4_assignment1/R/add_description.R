add_description <- function(allele){
  
  if(n_miss(allele) > 0 | nrow(allele) != 4){return(tibble(major = NA,
                                       minor = NA, 
                                       major_ctrl = NA, 
                                       major_disease = NA,
                                       minor_ctrl = NA, 
                                       minor_disease = NA))}
  
  allele %>% 
    select(type, a1) %>% 
    distinct() %>% 
    spread(type, a1) %>% 
    cbind(.,
          allele %>%
            mutate(type = paste(type, group, sep = "_")) %>%
            select(type, ratio) %>% 
            spread(type, ratio)
    ) %>% 
    return()
  
  #print("getting description")
}
