get_genotype <- function(df){
  df %>% group_by(group, genotype) %>% 
    count() %>% ungroup() %>% 
    drop_na %>% 
    group_by(group) %>% 
    mutate(sum = sum(n)) %>%
    ungroup() %>%
    separate(genotype, c("a1", "a2"), sep = " ", remove = FALSE) %>% 
    mutate(status = if_else(a1 == a2, "homo", "hetero")) %>% 
    return()
}
